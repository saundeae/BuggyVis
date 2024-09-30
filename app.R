# resource bank ----------------------------------------------------------------

library(readr)
library(shiny)
library(tidyverse)
library(shinythemes)
library(dplyr)
library(packcircles)
library(ggplot2)
library(ggiraph)
library(shinyWidgets)
library(rgbif)
library(httr)
library(jsonlite)

# globals ----------------------------------------------------------------------

#make dataset from imported csv of ewa buggy data
buggyDataCSV <- read_csv("ewa_buggy-2024-07-05_09.55.11.csv")
buggyData <- buggyDataCSV %>%
  filter(grepl(x=`___Review_(Admin_Only)`, pattern="Reviewed"), !is.na(`___Host_plant_species`)) %>%
  select(id=row_id, url=observation_url, observed, municipality, bug_group=`___Arthropod_group`, bug_taxon=`___Arthropod_taxon`, quantity=`___Quantity`, body_length=`___Body_length_(mm)`, host_plant=`___Host_plant_species`)

#for biomass calculation (not currently accurate, just a placeholder to make sure I can do the calculations)
biomassCalc <- read_csv("biomassCalc.csv")

#string wrangling
getGenus <- function(str){
  return(str_extract(string=str, pattern="(^\\w+)"))
}
getSp <- function(str){
  secondWord=str_extract(string=str, pattern='\\s+([^\\s]+)')
  ifelse(test=grepl(x=secondWord, pattern=")"), yes="spp.", no=secondWord)
}
getCN <- function(str){
  ifelse(test=grepl(x=str, pattern=")"), yes=str_extract(string=str, pattern="\\((.+)\\)"), no="")
}
getFamPlants <- function(genus, sp){
  info <- name_backbone(paste(genus, " ", sp), limit=1, kingdom="Plantae")
  if(info$matchType == "NONE"){
    return("Other")
  }
  return(info$family)
}
getFamBugs <- function(genus, sp){
  info <- name_backbone(paste(genus, " ", sp), limit=1, phylum="Arthropoda")
  if(info$matchType == "NONE"){
    return("Other")
  }
  return(info$family)
}

#make plants dataframe
plantsData <- buggyData %>%
  select(host_plant, bug_taxon) %>%
  group_by(host_plant) %>%
  mutate(hp_genus="", hp_sp="", hp_cn="", number_obs=n(), number_bug_sp=NA) %>%
  distinct(host_plant, bug_taxon, .keep_all=TRUE) %>%
  mutate(number_bug_sp=n()) %>%
  select(host_plant, hp_genus, hp_sp, hp_cn, number_obs, number_bug_sp) %>%
  distinct(host_plant, .keep_all=TRUE) %>%
  mutate(hp_genus=getGenus(host_plant), hp_sp=getSp(host_plant), hp_cn=getCN(host_plant), hp_family="")
for(i in 1:nrow(plantsData)){
  fam_name <- getFamPlants(plantsData$hp_genus[i], plantsData$hp_sp[i])
  plantsData$hp_family[i] <- paste(fam_name)
} #this refuses to work in mutate </3

#make bugs dataframe
bugsData <- buggyData %>%
  select(bug_taxon, bug_group, host_plant, quantity) %>%
  group_by(bug_taxon) %>%
  mutate(bug_genus="", bug_sp="", bug_cn="", bug_family="", number_obs=NA, number_plant_sp=NA) %>%
  distinct(bug_taxon, .keep_all=TRUE) %>%
  mutate(number_plant_sp=sum(quantity)) %>%
  ungroup() %>%
  mutate(bug_genus=getGenus(bug_taxon), bug_sp=getSp(bug_taxon), bug_cn=getCN(bug_taxon))
for(i in 1:nrow(bugsData)){
  fam_name <- getFamBugs(bugsData$bug_genus[i], bugsData$bug_sp[i])
  bugsData$bug_family[i] <- paste(fam_name)
} #this refuses to work in mutate </3

#make visitors dataframe
visitorsList <- bugsData %>%
  select(host_plant, bug_taxon, bug_genus, bug_sp, bug_cn, bug_family, quantity) %>%
  group_by(host_plant, bug_taxon) %>%
  mutate(total=sum(quantity)) %>%
  distinct(host_plant, bug_taxon, .keep_all=TRUE) %>%
  select(host_plant, bug_taxon, total, bug_genus, bug_sp, bug_cn, bug_family)

#make biomass dataframe
biomassStats <- buggyData %>%
  select(bug_group, body_length) %>%
  group_by(bug_group) %>%
  mutate(x=sum(body_length)) %>%
  left_join(y=biomassCalc, by="bug_group") %>%
  mutate(biomass_in_g=(a*x^b)/1000)%>%
  distinct(bug_group, .keep_all=TRUE) %>%
  select(bug_group, group_taxon, a, x, b, biomass_in_g)

#host plant bubble chart for number of observations
hppacking <- circleProgressiveLayout(plantsData$number_obs, sizetype='area')
hpdata <- cbind(plantsData, hppacking)
hpdata$text <- paste(plantsData$hp_genus, " ", plantsData$hp_sp, "\n", plantsData$hp_cn, "\n", "Number: ", plantsData$number_obs)
hpdat.gg <- circleLayoutVertices(hppacking, npoints=50)
hostPlantBubblesChart <- ggplot() +
  geom_polygon_interactive(data=hpdat.gg, aes(x, y, group=id, fill=id, tooltip=hpdata$text[id], data_id=id), color="black", alpha=.6) +
  geom_text(data=hpdata, aes(x, y, label=paste(hp_genus, " ", hp_sp)), size=3, color="black") +
  scale_size_continuous(range=c(1,7)) +
  theme_void() +
  theme(legend.position="none") +
  coord_equal()

#host plant bubble chart for number of species
hpspacking <- circleProgressiveLayout(plantsData$number_bug_sp, sizetype="area")
hpsdata <- cbind(plantsData, hpspacking)
hpsdata$text <- paste(plantsData$hp_genus, " ", plantsData$hp_sp, "\n", plantsData$hp_cn, "\n", "Number of bug species: ", plantsData$number_bug_sp)
hpsdat.gg <- circleLayoutVertices(hpspacking, npoints=50)
hpsChart <- ggplot() +
  geom_polygon_interactive(data=hpsdat.gg, aes(x, y, group=id, fill=id, tooltip=hpsdata$text[id], data_id=id), color="black", alpha=.6) +
  geom_text(data=hpsdata, aes(x, y, label=paste(hp_genus, " ", hp_sp)), size=3, color="black") +
  scale_size_continuous(range=c(1,7)) +
  theme_void() +
  theme(legend.position="none") +
  coord_equal()

trees <- c("Quercus rubra (northern red oak)", "Quercus ilicifolia (bear oak)", "Tsuga canadensis (Canada hemlock)", "Prunus serotina (black cherry)", "Betula nigra (river birch)")
verbenas <- c("Verbena urticifolia (white vervain)")
gentianas <- c("Asclepias syriaca (common milkweed)")
asters <- c("Solidago (goldenrod)", "Cirsium arvense (Californian thistle)", "Coreopsis (tickseed)", "	
Coreopsis lanceolata (lanceleaf tickseed)", "Rudbeckia triloba (browneyed Susan)")
vines <- c("Vitis (grape)")
roses <- c("Rosa multiflora (multiflora rose)", "Spiraea japonica (Japanese meadowsweet)", "Rubus (blackberry)")
hydrangeas <- c("Hydrangea quercifolia (oakleaf hydrangea)")
mints <- c("Stachys byzantina (woolly hedgenettle)", "Melissa officinalis (common balm)")
honeysuckles <- c("Lonicera (honeysuckle)")
nettles <- c("Urtica dioica (California nettle)")
legumes <- c("Trifolium repens (Dutch clover)")

# UI ---------------------------------------------------------------------------

ui <- fluidPage(

  # title 
    titlePanel("Buggy Data Interactive App"),
    navbarPage("",
               tabPanel("Host Plant Bubble Chart",
                        mainPanel(
                          switchInput(inputId="hpBubblesType", onLabel="Number of plant observations", offLabel="Number of bug species observed", value=TRUE),
                          htmlOutput("hpBubbles")
                          )
                        ),
               tabPanel("Host Plant Visitors",
                        sidebarPanel(
                          # checkboxGroupInput(inputId="selectedPlants", choices=plantsData$host_plant, label="Choose host plant:", selected=plantsData$host_plant[1:3]),
                          virtualSelectInput(
                            inputId = "selectedPlants", label = "Select:",
                            choices = list(
                              "Asteraceae (asters)" = asters,
                              "Rosaceae (roses)" = roses,
                              "Lamiaceae (mints)"= mints,
                              "Vitaceae (vines)" = vines,
                              "Fabiaceae (legumes)" = legumes,
                              "Verbenaceae (verbenas)" = verbenas,
                              "Gentianaceae (gentianas)" = gentianas,
                              "Urticaceae (nettles)" = nettles,
                              "Caprifoliaceae (honeysuckles)" = honeysuckles,
                              "Hydrangeacea (hydrangeas)" = hydrangeas,
                              "Trees" = trees
                            ),
                            search = TRUE, multiple = TRUE, selected=asters[1]
                          )
                        ),
                        mainPanel(
                          htmlOutput("plantVisitors")
                        )
               ),
               tabPanel("Biomass Calculator",
                        mainPanel(
                          plotOutput("biomassPlot")
                          )
                        )
               )
    )

# server -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  observe({
    input$selectedPlants
    req(input$selectedPlants)
      selectedVisitors <- visitorsList %>%
        filter(host_plant %in% input$selectedPlants) %>%
        group_by(bug_taxon) %>%
        mutate(number_bugs=sum(total)) %>%
        select(bug_taxon, number_bugs, bug_genus, bug_sp, bug_cn) %>%
        distinct(bug_taxon, .keep_all=TRUE)
      vpacking <- circleProgressiveLayout(selectedVisitors$number_bugs, sizetype="area")
      vdata <- cbind(selectedVisitors, vpacking)
      vdata$text <- paste(vdata$bug_taxon, " ", vdata$number_bugs)
      vdat.gg <- circleLayoutVertices(vpacking, npoints=50)
      visitorsChart <- ggplot() +
          geom_polygon_interactive(data=vdat.gg, aes(x, y, group=id, fill=id, tooltip=vdata$text[id], data_id=id), color="black", alpha=.6) +
          geom_text(data=vdata, aes(x, y, label=paste(bug_genus, bug_sp)), size=3, color="black") +
          scale_size_continuous(range=c(1,7)) +
          theme_void() +
          theme(legend.position="none") +
          coord_equal()
      output$plantVisitors <- renderUI(
        girafe(ggobj=visitorsChart, width_svg=7, height_svg=7)
      )
  })
  
  hpBubbles <- reactive(
    if(input$hpBubblesType == TRUE){
      girafe(ggobj = hostPlantBubblesChart, width_svg = 6, height_svg = 6)
    } else {
      girafe(ggobj=hpsChart, width_svg=6, height_svg=6)
    }
  )
  
  biomassPlot <- reactive(
    ggplot(biomassStats, aes(x="", y=biomass_in_g, fill=bug_group)) +
      geom_col()+
      coord_polar(theta="y") +
      theme_void()
  )
  
  output$hpBubbles <- renderUI(hpBubbles())
  output$biomassPlot <- renderPlot(biomassPlot())
}

# run --------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
