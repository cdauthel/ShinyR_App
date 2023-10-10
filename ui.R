# Define UI for application
shiny::shinyUI(
  # navbarPage (6)
  navbarPage("Ecology : Effect of specie, soil type and soil quality on tree mineral composition - Effect of nitrogen concentration increased in three compartments on three nutrients",
             br(),
             theme = shinythemes::shinytheme("superhero"),
             
             # first page 
             tabPanel("Study",

                      navlistPanel(widths = c(2, 10),
                                   
                                   tabPanel("General",
                                            h3("Objective"),
                                            hr(),
                                            p("The study aims to assess the impact of specie, soil type and soil quality on tree mineral composition, and the effect of nitrogen concentration increased in three compartments on three nutrients, in Dutch forests exposed to high nitrogen deposition."),
                                            p("The research focuses on essential nutrients - nitrogen (N), phosphorus (P), sulphur (S), calcium (Ca), potassium (K), and magnesium (Mg) - in stem wood and branch wood of seven major tree species in the Netherlands."),
                                            br(),
                                            
                                            h3("Context"),
                                            hr(),
                                            p("The Dutch Climate Agreement (2019) sets reduction targets for CO2 emissions and emphasizes increasing the use of sustainable biomass. Forest biomass harvesting practices need to align with sustainability goals, ensuring soil nutrients are not depleted."),
                                            p("Dutch forests, mainly on poor sandy soils, have experienced nutrient depletion historically due to grazing and sod cutting. The study considers the impact of specie, soil type and soil quality on tree mineral composition, particularly in the context of elevated nitrogen deposition over 40 years. Results that may be useful in understanding tree harvesting consequences."),
                                            br(),
                                            
                                            absolutePanel(top = "100%", right = "88%",
                                                          tags$a(href = "https://annforsci.biomedcentral.com/articles/10.1186/s13595-022-01149-5", "- Link to the article -")
                                                          ),
                                            absolutePanel(top = "100%", right = "1%",
                                                          tags$a(href = "https://data.4tu.nl/articles/_/19705156/1", "- Link to download data -")
                                                          )
                                   ),
                                   
                                   tabPanel("Method",
                                            h3("Sampling and calculation"),
                                            hr(),
                                            p("Nutrient concentrations were measured in different wood compartments of seven tree species. Sampling included stem wood (heartwood, sapwood, bark) and branches (coarse and fine branches). The average concentrations are exprimed in g of compound by kg of dry matter."),
                                            br(),
                                            
                                            column(width = 5,
                                              h3("Tree compartiments"),
                                              tags$figure(
                                                class = "centerGettyImage",
                                                tags$img(src = "Tree_Compartiments.png", height = 400, width = 380),
                                                tags$figcaption("Tree compartiments scheme")
                                              )
                                            ),
                                            br(),
                                            
                                            column(width = 4,
                                                   br(),
                                                   br(),
                                                   p(". Stem bark"),
                                                   p(". Stem sapwood"),
                                                   p(". Stem heartwood"),
                                                   p(". Stem wood"),
                                                   p(". Coarse branch bark"),
                                                   p(". Coarse branch wood without bark"),
                                                   p(". Fine branches")
                                            ),
                                            
                                            column(width = 2,
                                                   br(),
                                                   br(),
                                                   p(". Ca"),
                                                   p(". K"),
                                                   p(". Mg"),
                                                   p(". P"),
                                                   p(". S"),
                                                   p(". N"),
                                                   p(". C")
                                            )
                                            
                                   ),
                                   
                                   tabPanel("Species",
                                            h3("Looking on tree species"),
                                            hr(),
                                            column(width = 6,
                                                   tags$figure(
                                                     class = "centerGettyImage",
                                                     tags$img(src = "Betula_Pendula_at_Stockholm_University.jpg", height = 400, width = 380),
                                                     tags$figcaption("Silver birch (Betula pendula Roth)")
                                                   ),
                                                   br(),
                                                   tags$figure(
                                                     class = "centerGettyImage",
                                                     tags$img(src = "Désert_Rousseau_Cœur_Hêtre.jpg", height = 400, width = 380),
                                                     tags$figcaption("Beech (Fagus sylvatica L.)")
                                                   ),
                                                   br(),
                                                   tags$figure(
                                                     class = "centerGettyImage",
                                                     tags$img(src = "Larix_kaempferi.jpg", height = 400, width = 380),
                                                     tags$figcaption("Japanese larch (Larix kaempferi Lamb.)")
                                                   ),
                                                   br(),
                                                   tags$figure(
                                                     class = "centerGettyImage",
                                                     tags$img(src = "Picea_abies.jpg", height = 400, width = 380),
                                                     tags$figcaption("Norway spruce (Picea abies L. Karst.)")
                                                   ),
                                                   br()
                                            ),
                                            
                                            column(width = 6,
                                                   tags$figure(
                                                     class = "centerGettyImage",
                                                     tags$img(src = "Pinus_sylvestris_Nethybridge.jpg", height = 400, width = 380),
                                                     tags$figcaption("Scots pine (Pinus sylvestris L.)")
                                                   ),
                                                   br(),
                                                   tags$figure(
                                                     class = "centerGettyImage",
                                                     tags$img(src = "Pseudotsuga_menziesii.jpg", height = 400, width = 380),
                                                     tags$figcaption("Douglas fir (Pseudotsuga menziesii Mirb.)")
                                                   ),
                                                   br(),
                                                   tags$figure(
                                                     class = "centerGettyImage",
                                                     tags$img(src = "Quercus_robur.jpg", height = 400, width = 380),
                                                     tags$figcaption("Q oak (Quercus robur L.)")
                                                   )
                                            )
                                    
                                   )
                                   
                      )
             ),
             
             # second page
             tabPanel("Data base",
                      # panel list
                      navlistPanel(widths = c(2, 10),
                        
                        # table
                        tabPanel("Table", dataTableOutput("table")),
                        
                        # summary
                        tabPanel("Summary",
                                 # first column
                                 column(width = 6,
                                        checkboxGroupInput("va_quali_summary", h4("Qualitatives variables to summarise"),
                                                           choices = colnames(data[, 4:6]), selected = colnames(data[, 4:6])
                                        )
                                 ),
                                 # second column
                                 column(width = 6,
                                        checkboxGroupInput("va_quanti_summary", h4("Quantitatives variables to summarise"),
                                                                  choices = wood.site, selected = wood.site[1]
                                        )
                                 ),
                                 # show text output 
                                 verbatimTextOutput("summary"),
                                 
                                 # horizontal space
                                 br()
                        ),
                        
                        # collection
                        tabPanel("Sample", verbatimTextOutput("sample"),
                                 radioButtons("radio_sample_plot", h4("Show"),
                                              choices = list("Species" = "Species_name", "Soil type" = "Dutch_soil_code", "Soil quality" = "Soil_category"),
                                              selected = "Species_name"),
                                 
                                 # show plot output
                                 mainPanel(
                                   plotlyOutput("plot_sample"),
                                   br(),
                                   verbatimTextOutput("table_sample"),
                                   br()
                                 )
                        ),
                        
                        # NAs
                        tabPanel("NAs",
                                 column(width = 6,
                                        radioButtons("group_na_plot", h4("Group by"),
                                                     choices = list("Species" = "Species_name", "Soil type" = "Dutch_soil_code", "Soil quality" = "Soil_category"),
                                                     selected = "Species_name"),
                                        br(),
                                        plotlyOutput("plot_na"),
                                        br(),
                                        verbatimTextOutput("table_na"),
                                        br()
                                 ),
                                 column(width = 6,
                                        h4("Quantitative variable"),
                                        selectInput("wood_na", "Filter by wood site", wood.site),
                                        selectInput("composition_na", "Filter by composition type", composition),
                                        plotOutput("plot_na_view")
                                 )
                          
                        )

                      )
             ),
             
             # third page
             tabPanel("Geographic distribution of individuals",
                      fluidRow(
                        
                        # left panel
                        column(width = 6,
                               actionButton("go_point", "Sampled elements"),
                               radioButtons("radio_map", h3("Visualisation"),
                                            choices = list("Classic" = 1, "Number of indivuals" = 2, "Group" = 3),
                                            selected = 1),
                               
                               conditionalPanel(
                                 condition = "input.radio_map == 3",
                                 selectInput("var_quali_map", "Filter by", c("Species", "Soil type", "Soil quality")),
                                 selectInput("wood_map", "Filter by wood site", wood.site),
                                 selectInput("composition_map", "Filter by composition type", composition)
                               )
                               
                        ),
                        
                        # right panel
                        column(width = 6, h4("Coordonnées"),
                               textOutput("coord"),
                               conditionalPanel(condition = "input.radio_map == 3",
                                                sliderInput("slider_map", h3("Intensity"),
                                                            min = 1, max = 100, value = 50)
                               )
                        )
                        
                      ),
                      
                      # show a map
                      mainPanel(
                        leafletOutput("map_points", width = "150%", height = 400),
                        br()
                      )
             ),
             
             # fourth page
             tabPanel("Data visualisation", 
                      fluidRow(

                        # right panel
                        column(width = 3,
                               wellPanel(
                                 conditionalPanel(condition = "input.tabset == 'Boxplot'",
                                   
                                   # select filled and visualised boxplot groups
                                   selectInput("filled_group", "Filled group", c("Species", "Soil type", "Soil quality")),
                                   conditionalPanel("input.filled_group == 'Species'",
                                                    selectInput("selected_group1", "Visualised group", c("Soil type", "Soil quality"), selected = "Soil type")
                                   ),
                                   conditionalPanel("input.filled_group == 'Soil type'",
                                                    selectInput("selected_group2", "Visualised group", c("Species", "Soil quality"), selected = "Species")
                                   ),
                                   conditionalPanel("input.filled_group == 'Soil quality'",
                                                    selectInput("selected_group3", "Visualised group", c("Species", "Soil type"), selected = "Species")
                                   ),
                                   
                                   
                                   # filter group
                                   checkboxGroupInput("species", "Filter by species",
                                                      choices = unique(data$Species_name),
                                                      selected = unique(data$Species_name)
                                   ),
                                   checkboxGroupInput("soil", "Filter by soil code",
                                                      choices = unique(data$Dutch_soil_code),
                                                      selected = unique(data$Dutch_soil_code)
                                   ),
                                   checkboxGroupInput("quality", "Filter by soil quality",
                                                      choices = unique(data$Soil_category),
                                                      selected = unique(data$Soil_category)
                                   ),
                                   
                                   # select site and composition
                                   selectInput("wood", "Filter by wood site", wood.site),
                                   selectInput("composition", "Filter by composition type", composition),
                                   
                                   # horizontal rule
                                   hr(),
                                   radioButtons("radio_results_box", h5("Show quantiles"),
                                                choices = list("Yes" = 1, "No" = 2),
                                                selected = 2),
                                   radioButtons("radio_results_outsiders", h5("Show global outsiders"),
                                                choices = list("Yes" = 1, "No" = 2),
                                                selected = 1)
                                 ),
                                 
                                 conditionalPanel(condition = "input.tabset == 'Scatter plot'",
                                   h5("Abscissa"),
                                   selectInput("wood1", "Wood site on abscissa", wood.site),
                                   selectInput("composition1", "Nutrient on abscissa", composition),
                                   hr(),
                                   
                                   h5("Ordinate"),
                                   selectInput("wood2", "Wood site on ordinate", wood.site),
                                   selectInput("composition2", "Nutrient on ordinate", composition),
                                   hr(),
                                   
                                   selectInput("group_scatter", "Color by", c("Species", "Soil type", "Soil quality"), selected = "Species"),
                                   radioButtons("smooth", h5("Add smoother"),
                                                choices = list("Yes" = 1, "No" = 2),
                                                selected = 2),
                                   conditionalPanel("input.smooth == 1",
                                                    selectInput("method_smooth", "Smoothing method", c("lm", "loess"), selected = "lm")
                                   ),
                                   radioButtons("marginal", h5("Add marginal plot"),
                                                choices = list("Yes" = 1, "No" = 2),
                                                selected = 2),
                                   hr(),
                                   
                                   radioButtons("show_results_scatter", h5("Show average concentrations and standard-error of the mean"),
                                                choices = list("Yes" = 1, "No" = 2),
                                                selected = 2)
                                   
                                 )
                                 
                               )
                        ),
                        
                        column(width = 9, 
                               tabsetPanel(
                                 id = "tabset",
                                 tabPanel("Boxplot",
                                          plotOutput("boxplot"),
                                          br(),
                                          verbatimTextOutput("boxplot_results"),
                                          br(),
                                          dataTableOutput("outsiders_results")
                                 ),
                                 
                                 tabPanel("Scatter plot",
                                          plotOutput("scatter"),
                                          br(),
                                          dataTableOutput("scatter_results")
                                 )
                               )
                        )
                        
                      )
             ),
             
             # fifth page
             tabPanel("Factor analysis",
               radioButtons("factor_analysis", h4("Method"),
                            choices = list("PCA" = 1, "MCA" = 2, "MFA" = 3),
                            selected = 1
               ),
               
               conditionalPanel("input.factor_analysis == 1",
                                h3("Data exploration by quantitative variables : PCA"),
                                hr(),
                                
                                sidebarLayout(
                                  
                                  sidebarPanel(
                                    radioButtons("impute_PCA", h5("Imputation of Na values by an PCA model"),
                                                 choices = list("Yes" = 1, "No" = 2),
                                                 selected = 2),
                                    conditionalPanel("input.impute_PCA == 1",
                                                     sliderInput("slider_pca", h5("Number of dimensions keep for the imputation"),
                                                                 min = 1, max = 10, value = 5)
                                    ),
                                    radioButtons("radio_PCA", h4("Visualisation"),
                                                 choices = list("Individual factor map" = 1, "Correlation circle" = 2),
                                                 selected = 1),
                                    
                                    hr(),
                                    
                                    radioButtons("radio_results_PCA", h4("Show results"),
                                                 choices = list("Yes" = 1, "No" = 2),
                                                 selected = 2),
                                    conditionalPanel("input.radio_PCA == 1 && input.radio_results_PCA == 1",
                                                     selectInput("results_ind_pca", "",
                                                                 c("Eign values", "Contribution", "Cos2"))
                                    ),
                                    
                                    conditionalPanel("input.radio_PCA == 2 && input.radio_results_PCA == 1",
                                                     selectInput("results_var_pca", "",
                                                                 c("Correlation", "Contribution", "Cos2"))
                                    )
                                    
                                  ),
                                  
                                  # show a plot and text ouput
                                  mainPanel(
                                    plotOutput("pca_plot"),
                                    br(),
                                    verbatimTextOutput("results_PCA")
                                  )
                                  
                                )
               ),
               
               conditionalPanel("input.factor_analysis == 2",
                                h3("Data exploration by qualitative variables : MCA"),
                                hr(),
                                
                                sidebarLayout(
                                  
                                  sidebarPanel(
                                    radioButtons("impute_MCA", h5("Imputation of Na values by an MCA model"),
                                                 choices = list("Yes" = 1, "No" = 2),
                                                 selected = 2),
                                    conditionalPanel("input.impute_MCA == 1",
                                                     sliderInput("slider_mca", h5("Number of dimensions keep for the imputation"),
                                                                 min = 1, max = 10, value = 5)
                                    ),
                                    radioButtons("radio_MCA", h4("Visualisation"),
                                                 choices = list("Individual factor map" = 1, "Modality" = 2, "Variables representation" = 3),
                                                 selected = 1),
                                    
                                    hr(),
                                    
                                    radioButtons("radio_results_MCA", h4("Show results"),
                                                 choices = list("Yes" = 1, "No" = 2),
                                                 selected = 2),
                                    conditionalPanel("input.radio_MCA == 1 && input.radio_results_MCA == 1",
                                                     selectInput("results_ind_mca", "",
                                                                 c("Eign values", "Contribution", "Cos2"))
                                    ),
                                    
                                    conditionalPanel("input.radio_MCA == 2 && input.radio_results_MCA == 1",
                                                     selectInput("results_mod_mca", "",
                                                                 c("Contribution", "Cos2", "V Test"))
                                    )
                                    
                                  ),
                                  
                                  # show a plot and text ouput
                                  mainPanel(
                                    plotOutput("mca_plot"),
                                    br(),
                                    verbatimTextOutput("results_MCA")
                                  )
                                  
                                )
               ),
               
               conditionalPanel("input.factor_analysis == 3",
                                h3("Data exploration by groups of variables : MFA"),
                                hr(),
                                
                                sidebarLayout(
                                  
                                  # Sidebar panel for inputs
                                  sidebarPanel(
                                    radioButtons("impute_MFA", h5("Imputation of Na values by an MFA model"),
                                                 choices = list("Yes" = 1, "No" = 2),
                                                 selected = 2),
                                    conditionalPanel("input.impute_MFA == 1",
                                                     sliderInput("slider_mfa", h4("Number of dimensions keep for the imputation"),
                                                                 min = 1, max = 10, value = 5)
                                    ),
                                    radioButtons("radio_MFA", h4("Visualisation"),
                                                 choices = list("Individual factor map" = 1, "Correlation circle" = 2,
                                                                "Groups representation" = 3, "Partial axes" = 4, "Partial individuals" = 5),
                                                 selected = 1),
                                    conditionalPanel("input.radio_MFA == 1", 
                                                     selectInput("habillage_ind", "Color by",
                                                                 c("Species", "Soil type", "Soil quality"))
                                                     
                                    ),
                                    
                                    hr(),
                                    
                                    radioButtons("radio_results_MFA", h4("Show results"),
                                                 choices = list("Yes" = 1, "No" = 2),
                                                 selected = 2),
                                    conditionalPanel("input.radio_MFA == 1 && input.radio_results_MFA == 1",
                                                     selectInput("results_ind", "",
                                                                 c("Eign values", "Inertia ratio", "Contribution", "Cos2", "Within inertia"))
                                    ),
                                    
                                    conditionalPanel("input.radio_MFA == 2 && input.radio_results_MFA == 1",
                                                     radioButtons("results_var", h3("Variable type"),
                                                                  choices = list("Qualitative" = 1, "Quantitative" = 2),
                                                                  selected = 1),
                                                     conditionalPanel(condition = "input.results_var == 1",
                                                                      selectInput("results_var_quali", "",
                                                                                  c("Contribution", "Cos2", "Within inertia", "V Test"))
                                                     ),
                                                     conditionalPanel(condition = "input.results_var == 2",
                                                                      selectInput("results_var_qaunti", "",
                                                                                  c("Correlation", "Contribution", "Cos2"))
                                                     )
                                    ),
                                    
                                    conditionalPanel("input.radio_MFA == 3 && input.radio_results_MFA == 1",
                                                     selectInput("results_groups", "",
                                                                 c("Lg", "RV", "Correlation", "Contribution", "Cos2"))
                                    ),
                                    conditionalPanel("input.radio_MFA == 4 && input.radio_results_MFA == 1",
                                                     selectInput("results_partial_axes", "",
                                                                 c("Correlation", "Contribution"))
                                    )
                                    
                                  ),
                                  
                                  # show a plot and text ouput
                                  mainPanel(
                                    plotOutput("mfa_plot"),
                                    br(),
                                    verbatimTextOutput("results_MFA")
                                  )
                                  
                                )
               )
             ),

             # sixth page
             tabPanel("Data modelisation and prediction",
                      # right panel
                      column(width = 3,
                             wellPanel(h4("Data"),
                               radioButtons("impute_mod", h5("Imputation of Na values by an MFA model"),
                                            choices = list("Yes" = 1, "No" = 2),
                                            selected = 2),
                               conditionalPanel("input.impute_mod == 1",
                                                sliderInput("slider_mod", h5("Number of dimensions keep for the imputation"),
                                                min = 1, max = 10, value = 5),
                                                radioButtons("neg_mod", h5("Supression of negative(s) value(s) generated by the MFA imputation on global data set"),
                                                             choices = list("Yes" = 1, "No" = 2),
                                                             selected = 2),
                               ),
                               conditionalPanel("input.tabset1 = 'Prediction'",
                                                radioButtons("neg_obs_pred", h5("Negative(s) value(s) supression of obs-pred couples (after prediction)"),
                                                             choices = list("Yes" = 1, "No" = 2),
                                                             selected = 2)
                               ),
                               br(),
                               hr(),
                               
                               h4("Modeling"),
                               selectInput("model", "Model choice", c("Linear model", "Generalized linear model"), selected = "Generalized linear model"),
                               
                               conditionalPanel("input.model == 'Generalized linear model'",
                                                conditionalPanel("input.radio_Y_type_glm == 1",
                                                                 selectInput("family_glm1", "Family", c("gaussian", "Gamma", "inverse.gaussian", "poisson", "quasi", "quasipoisson"))
                                                ),
                                                conditionalPanel("input.radio_Y_type_glm == 2",
                                                                 selectInput("family_glm2", "Family", c("binomial"))
                                                ),
                                                radioButtons("type_glm", "Test of interaction",
                                                             choices = list("Yes" = 1, "No" = 2),
                                                             selected = 2
                                                ),
                                                br(),
                                                hr(),
                                                
                                                conditionalPanel("input.radio_Y_type_glm == 2",
                                                                 h5("Target for predictions"),
                                                                 conditionalPanel("input.radio_Y_quali_glm == 'Species_name'",
                                                                                  selectInput("target1", "Modality", levels(data[, "Species_name"]))
                                                                 ),
                                                                 conditionalPanel("input.radio_Y_quali_glm == 'Dutch_soil_code'",
                                                                                  selectInput("target2", "Modality", levels(data[, "Dutch_soil_code"]))
                                                                 ),
                                                                 conditionalPanel("input.radio_Y_quali_glm == 'Soil_category'",
                                                                                  selectInput("target3", "Modality", levels(data[, "Soil_category"]))
                                                                 )
                                                ),

                                                h3("Response variable"),
                                                radioButtons("radio_Y_type_glm", h5("Type"),
                                                             choices = list("Quantitative" = 1, "Qualitative" = 2)
                                                ),
                                                
                                                conditionalPanel("input.radio_Y_type_glm == 1",
                                                                 h5("Variable choice"),
                                                                 selectInput("wood_glm_Y", "Wood site", wood.site),
                                                                 selectInput("composition_glm_Y", "Nutrient", composition)
                                                ),
                                                conditionalPanel("input.radio_Y_type_glm == 2",
                                                                 radioButtons("radio_Y_quali_glm", h5("Variable choice"), 
                                                                              choices = list("Species" = "Species_name", "Soil type" = "Dutch_soil_code", "Soil quality" = "Soil_category"),
                                                                              selected = "Species_name"
                                                                 )
                                                ),
                                                br(),
                                                hr(),
                                                
                                                h3("Explicative(s) variable"),
                                                checkboxGroupInput("quali_X_glm", "Qualitative variable(s) choice",
                                                                   choices = c("Species", "Soil type", "Soil quality"),
                                                                   selected = "Species"
                                                ),
                                                radioButtons("radio_X_add_glm", h5("Add quantitative(s) variable(s)"),
                                                             choices = list("Yes" = 1, "No" = 2), selected = 2
                                                ),
                                                conditionalPanel("input.radio_X_add_glm == 1",
                                                                 checkboxGroupInput("quanti_X_glm", "Supplementray quantitative variables",
                                                                                    choices = colnames(data[, 9:length(data)]),
                                                                                    selected = colnames(data[, 9:length(data)])[1]
                                                                 )
                                                )
                                                
                               ),
                               
                               conditionalPanel("input.model == 'Linear model'",
                                                radioButtons("type_lm", "Test of interaction",
                                                             choices = list("Yes" = 1, "No" = 2),
                                                             selected = 2
                                                ),
                                                br(),
                                                hr(),
                                                
                                                h3("Response variable"),
                                                selectInput("wood_lm_Y", "Wood site", wood.site),
                                                selectInput("composition_lm_Y", "Nutrient", composition),
                                                br(),
                                                hr(),
                                                
                                                checkboxGroupInput("quanti_X_lm", h3("Explicative(s) variable"),
                                                                   choices = colnames(data[, 9:length(data)]),
                                                                   selected = colnames(data[, 9:length(data)])[1]
                                                )
                                                
                               )

                             )
                      ),
                      
                      column(width = 9, 
                               tabsetPanel(
                                 
                                 tabPanel("Summary",
                                          verbatimTextOutput("summary_mod"),
                                          plotOutput("plot_mod")
                                 ),
                                 
                                 tabPanel("Coefficients",
                                          verbatimTextOutput("coeff_mod")
                                 ),
                                 
                                 tabPanel("Anova",
                                          verbatimTextOutput("anova")
                                 ),
                                 
                                 tabPanel("Confidence interval",
                                          verbatimTextOutput("interval_mod")
                                 ),
                                 
                                 tabPanel("Fitted values",
                                          verbatimTextOutput("fitted_mod")
                                 ),
                                 
                                 tabPanel("Residuals",
                                          verbatimTextOutput("residuals_mod")
                                 ),
                                 
                                 tabPanel("All pairwise comparisons",
                                          verbatimTextOutput("tukey_mod")
                                 ),
                                 
                                 tabPanel("Prediction",
                                          plotOutput("prediction_plot"),
                                          br(),
                                          verbatimTextOutput("prediction_mod"),
                                          br(),
                                          dataTableOutput("prediction_table"),
                                 )
                               )
                      )
                      
             )
        )
 )
