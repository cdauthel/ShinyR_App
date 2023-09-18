
### Définition des classes ###

#permet de lire des données et de bien les typées
Environnement <- setRefClass("Loading Data",
                             fields = list(path = "character", file = "character", strVec = "character", factVect = "factor",
                                           numVec = "numeric", doubleVec = "double", intVec = "integer", dateVec = "Date"),
                             methods = list(loadingData = function(path, file, strVec, factVect, numVec, doubleVec, intVec, dateVec){
                                            require(parallel)
                                            require(doParallel)
                                            require(openxlsx)
                                            require(foreach)
                                            cl <- parallel::makeCluster(2)
                                            doParallel::registerDoParallel(cl)
                                            data <- openxlsx::read.xlsx(paste(path, file, sep = "/"), check.names = FALSE, na.strings = c("NA", "<NA>", "na", "Na", "nA", "NULL", "null", "Null"))
                                            foreach::foreach(i = 1:length(strVec)) %:% when (length(strVec) != 0) %do% (data[strVec[i]] = as.character(unlist(data[strVec[i]])))
                                            foreach::foreach(i = 1:length(factVect)) %:% when (length(factVect) != 0) %do% (data[factVect[i]] = as.factor(unlist(data[factVect[i]])))
                                            foreach::foreach(i = 1:length(numVec)) %:% when (length(numVec) != 0) %do% (data[numVec[i]] = as.numeric(unlist(data[numVec[i]])))
                                            foreach::foreach(i = 1:length(doubleVec)) %:% when (length(doubleVec) != 0) %do% (data[doubleVec[i]] = as.double(unlist(data[doubleVec[i]])))
                                            foreach::foreach(i = 1:length(intVec)) %:% when (length(intVec) != 0) %do% (data[intVec[i]] = as.integer(unlist(data[intVec[i]])))
                                            foreach::foreach(i = 1:length(dateVec)) %:% when (length(dateVec) != 0) %do% (data[dateVec[i]] = as.Date(unlist(data[dateVec[i]])))
                                            parallel::stopCluster(cl)
                                            data
                            }
              ))

suppressWarnings(data <- Environnement("/users/cdauthel/desktop/ACO/M2/Data_Science/P1/Massive_Data_Analysis/Projet/Data", "Wood_Nutrients.xlsx", c(), c("Sample_ID", "Plot_ID", "Tree_species", "Species_name", "Dutch_soil_code", "Soil_catagory"), c("Stem_bark_Ca", "Stem_bark_K", "Stem_bark_Mg", "Stem_bark_P", "Stem_bark_S", "Stem_bark_N", "Stem_bark_C", "Stem_sapwood_Ca", "Stem_sapwood_K", "Stem_sapwood_Mg", "Stem_sapwood_P", "Stem_sapwood_S", "Stem_sapwood_N", "Stem_sapwood_C", "Stem_heartwood_Ca", "Stem_heartwood_K", "Stem_heartwood_Mg", "Stem_heartwood_P", "Stem_heartwood_S", "Stem_heartwood_N", "Stem_heartwood_C", "Stem_wood_Ca", "Stem_wood_K", "Stem_wood_Mg", "Stem_wood_P", "Stem_wood_S", "Stem_wood_N", "Stem_wood_C", "Coarse_branch_bark_Ca", "Coarse_branch_bark_K", "Coarse_branch_bark_Mg", "Coarse_branch_bark_P", "Coarse_branch_bark_S", "Coarse_branch_bark_N", "Coarse_branch_bark_C", "Coarse_branch_wood_without_bark_Ca", "Coarse_branch_wood_without_bark_K", "Coarse_branch_wood_without_bark_Mg", "Coarse_branch_wood_without_bark_P", "Coarse_branch_wood_without_bark_S", "Coarse_branch_wood_without_bark_N", "Coarse_branch_wood_without_bark_C", "Fine_branches_Ca", "Fine_branches_K", "Fine_branches_Mg", "Fine_branches_P", "Fine_branches_S", "Fine_branches_N", "Fine_branches_C"), c("Lat", "Long"), c(), c())$loadingData("/users/cdauthel/desktop/ACO/M2/Data_Science/P1/Massive_Data_Analysis/Projet/Data", "Wood_Nutrients.xlsx", c(), c("Sample_ID", "Plot_ID", "Tree_species", "Species_name", "Dutch_soil_code", "Soil_catagory"), c("Stem_bark_Ca", "Stem_bark_K", "Stem_bark_Mg", "Stem_bark_P", "Stem_bark_S", "Stem_bark_N", "Stem_bark_C", "Stem_sapwood_Ca", "Stem_sapwood_K", "Stem_sapwood_Mg", "Stem_sapwood_P", "Stem_sapwood_S", "Stem_sapwood_N", "Stem_sapwood_C", "Stem_heartwood_Ca", "Stem_heartwood_K", "Stem_heartwood_Mg", "Stem_heartwood_P", "Stem_heartwood_S", "Stem_heartwood_N", "Stem_heartwood_C", "Stem_wood_Ca", "Stem_wood_K", "Stem_wood_Mg", "Stem_wood_P", "Stem_wood_S", "Stem_wood_N", "Stem_wood_C", "Coarse_branch_bark_Ca", "Coarse_branch_bark_K", "Coarse_branch_bark_Mg", "Coarse_branch_bark_P", "Coarse_branch_bark_S", "Coarse_branch_bark_N", "Coarse_branch_bark_C", "Coarse_branch_wood_without_bark_Ca", "Coarse_branch_wood_without_bark_K", "Coarse_branch_wood_without_bark_Mg", "Coarse_branch_wood_without_bark_P", "Coarse_branch_wood_without_bark_S", "Coarse_branch_wood_without_bark_N", "Coarse_branch_wood_without_bark_C", "Fine_branches_Ca", "Fine_branches_K", "Fine_branches_Mg", "Fine_branches_P", "Fine_branches_S", "Fine_branches_N", "Fine_branches_C"), c("Lat", "Long"), c(), c()))

require(shiny)
require(plotly)
wood.site <- c("Stem_bark", "Stem_sapwood", "Stem_heartwood", "Stem_wood", "Coarse_branch_bark", "Coarse_branch_wood_without_bark", "Fine_branches")
composition <- c("Ca", "K", "Mg", "P", "S", "N", "C")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(" "),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Group ----
      checkboxGroupInput("species", "Filter by species",
                         choices = unique(data$Species_name),
                         selected = unique(data$Species_name)
      ),
      checkboxGroupInput("soil", "Filter by soil code",
                         choices = unique(data$Dutch_soil_code),
                         selected = unique(data$Dutch_soil_code)
      ),
      checkboxGroupInput("quality", "Filter by soil quality",
                         choices = unique(data$Soil_catagory),
                         selected = unique(data$Soil_catagory)
      ),
      
      selectInput("wood", "Filter by wood site", wood.site),
      selectInput("composition", "Filter by composition type", composition),
      
      hr(), # Add a horizontal rule
      
      checkboxInput("by_species", "Show species", TRUE),
      checkboxInput("show_margins", "Show marginal plots", TRUE),
      checkboxInput("smooth", "Add smoother")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput("distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output){
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    subsetted <- reactive({
      req(input$species)
      data |> filter(Species_name %in% input$species)
    })
    p <- plot_ly(subsetted(), y = as.formula(paste("~", input$wood, "_", input$composition, sep = "")), color = I("black"), 
                 alpha = 0.1, boxpoints = "suspectedoutliers")
    p1 <- p %>% add_boxplot(x = "Overall")
    p2 <- p %>% add_boxplot(x = ~Species_name)
    subplot(
      p1, p2, shareY = TRUE,
      widths = c(0.2, 0.8), margin = 0
    ) %>% hide_legend()
    
  })
  
}

#Main
shinyApp(ui, server)

#Calcul parralel
require(foreach)
require(doParallel)
require(parallel)
f <- function(x, .export = NULL){
  cl <- makeCluster(2)
  registerDoParallel(cl)
  
  res <- foreach(dta = x, .packages = "") %dopar% {
    randomForest(dep_delay ~ day + dep_time + hour + min, dta, ntree = 30)
  }
  #OU
  res <- foreach(i = x, .export = .export) %dopar% (i + y)
  stopCluster(cl)
  res
}

#Microbenchmark
suppressWarnings(require(microbenchmark, quietly = TRUE))
microbenchmark(f1(arg = ??), f2(arg = ??), times = 1000)

#Profilage du code via Rprof 
Rprof("Rprof.out", interval = 0.001)
x <- f(arg = ??)
Rprof(NULL)
summaryRprof("Rprof.out")

#Détermination du nombre de coeur optimale(s)

#Impact mémoire
require(pryr)
object_size(f(arg = ??))
mem_change(x <- 1:1e6) ; mem_change(rm(x))

#GESTION DES ERREURS ET DES MESSAGES
#Déboggage

#
