# Define server
server <- shiny::shinyServer(function(input, output){
  
  # data table second page
  output$table <- renderDataTable({
    data
  })
  
  # summary second page first page
  output$summary <- renderPrint({
    va_quanti_summary <- assemble(input$va_quanti_summary)
    summary(data[, c(input$va_quali_summary, va_quanti_summary)])
  })
  
  # plot sample count by group second page
  output$plot_sample <- renderPlotly({
    p <- ggplot2::ggplot(data, aes_string(x = input$radio_sample_plot)) +
          geom_bar(aes_string(fill = input$radio_sample_plot)) +
          scale_fill_manual(values = color[1:length(levels(data[, input$radio_sample_plot]))]) +
          labs(x = input$radio_sample_plot, y = "Specimen count") +
          theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 0.5))
    plotly::ggplotly(p)
  })
  
  # table sample count by group second page
  output$table_sample <- renderPrint({
    table(data[, input$radio_sample_plot])
  })
  
  # plot Na count by group second page
  output$plot_na <- renderPlotly({
    na <- data[is.na(data[, paste(input$wood_na, input$composition_na, sep = "_")]), ]
    p <- ggplot2::ggplot(na, aes_string(x = input$group_na_plot)) +
          geom_bar(aes_string(fill = input$group_na_plot)) +
          scale_fill_manual(values = color[1:length(levels(data[, input$group_na_plot]))]) +
          labs(x = input$group_na_plot, y = "Na count") +
          theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 0.5))
    plotly::ggplotly(p)
  })
  
  # visuel of na values
  output$plot_na_view <- renderPlot({
    VIM::aggr(data)
  })
  
  # table Na count by group second page
  output$table_na <- renderPrint({
    formula <- as.formula(paste(paste(input$wood_na, input$composition_na, sep = "_"), input$group_na_plot, sep = "~"))
    natable <- aggregate(formula, data = data, function(x) {sum(is.na(x))}, na.action = NULL)
    natable
  })

  # map third page
  observe({
    radio_map <- input$radio_map
    var_quali_map <- conversion(input$var_quali_map)
    wood_map <- input$wood_map
    composition_map <- input$composition_map
    relative_radius <- input$slider_map
    output$map_points <- renderLeaflet({
      input$go_point
      isolate({
        map_points <- leaflet::leaflet()
        map_points <- leaflet::addTiles(map_points)
    
        # classic
        if (as.integer(radio_map) == 1) {
          points <- data[, c("Lat", "Long")]
          points$id <- paste0(points$Long, " - ", points$Lat)
          map_points <- leaflet::addMarkers(map = map_points, lng = ~Long, lat = ~Lat,
                                            data = points, popup = paste(". ID : ", data$Sample_ID, "\n", "   . Species : ", data$Species_name,
                                                                         "   . Soil type : ", data$Dutch_soil_code, "   . Soil quality : ", data$Soil_category, sep = "")
          )
          
          # number of individuals
        }
        else if (as.integer(radio_map) == 2) {
          points <- data[, c("Lat", "Long")]
          points$id <- paste0(points$Long, " - ", points$Lat)
          map_points <- leaflet::addMarkers(map = map_points, lng = ~Long, lat = ~Lat,
                                            data = points, popup = paste(". ID : ", data$Sample_ID, "\n", "   . Species : ", data$Species_name,
                                                                         "   . Soil type : ", data$Dutch_soil_code, "   . Soil quality : ", data$Soil_category, sep = ""),
                                            clusterOptions = markerClusterOptions()
          )
          
          # group
        }
        else {
          points <- data[!is.na(data[, paste(wood_map, composition_map, sep = "_")]), c("Lat", "Long")]
          points$Lat <- points$Lat + runif(nrow(points), min = -0.001, max = 0.001)
          points$Long <- points$Long + runif(nrow(points), min = -0.001, max = 0.001)
          color_fact <- leaflet::colorFactor(color[1:length(levels(data[, var_quali_map]))],
                                             domain = levels(data[, var_quali_map]))
          map_points <- leaflet::addCircleMarkers(map = map_points, lng = ~Long, lat = ~Lat,
                                                  data = points, popup = paste(". ID : ", data$Sample_ID, "\n",
                                                                               "   . Species : ", data$Species_name,
                                                                               "   . Soil type : ", data$Dutch_soil_code,
                                                                               "   . Soil quality : ", data$Soil_category,
                                                                               "   . ", wood_map, "_", composition_map, " : ", data[, paste(wood_map, composition_map, sep = "_")], " g.kg-1", sep = ""),
                                                  color = color_fact(data[, var_quali_map]),
                                                  radius = intensity(composition_map, data[, paste(wood_map, composition_map, sep = "_")], relative_radius)
          )
          value_fact <- factor(data[, var_quali_map], levels = levels(data[, var_quali_map]))
          map_points <- leaflet::addLegend(map = map_points, position = "topright",
                                           pal = color_fact, values = levels(data[, var_quali_map]),
                                           title = paste(var_quali_map, sep = ""), opacity = 1
          )
        }
        
        map_points
        
      })
      
    })
    
  })
  
  # coordo map third page
  output$coord <- renderText({
    clic <- input$map_points_click
    txt <- paste0("Coordonnées : ", clic$lat, " - ", clic$lng)
    txt
  })
  
  # box plot fourth page
  output$boxplot <- renderPlot({
    
    # sort data by Species
    subsetted <- reactive({
      data |> plotly::filter((Species_name %in% input$species) & (Dutch_soil_code %in% input$soil) & (Soil_category %in% input$quality))
    })
    
    filled <- conversion(input$filled_group)
    if (filled == "Species_name"){
      group <- conversion(input$selected_group1)
    }
    else if (filled == "Dutch_soil_code") {
      group <- conversion(input$selected_group2)
    }
    else {
      group <- conversion(input$selected_group3)
    }
    
    nutrient <- paste(input$wood, input$composition, sep = "_")
    
    # color
    col <- c()
    if (filled == "Soil_category") {
      for (i in 1:length(input$quality)) {
        if (input$quality[i] == "Poor") {
          col <- c(col, "red")
        }
        else if (input$quality[i] == "Moderate") {
          col <- c(col, "orange")
        }
        else {
          col <- c(col, "green")
        }
      }
    }
    else if (filled == "Species_name") {
      col <- color[1:length(input$species)]
    }
    else {
      col <- color[1:length(input$soil)]
    }
    
    # boxplot composition by Species & Soil_category & Dutch_soil_code
    ggplot2::ggplot(subsetted(), aes_string(group, nutrient)) +
      geom_boxplot(aes_string(fill = filled), outlier.shape = NA) +
      geom_jitter(aes_string(fill = filled), pch = 21, color = "black") +
      theme_bw() +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = col) +
      scale_colour_manual(values = col) +
      labs(x = group, y = paste(nutrient, "g.kg-1", sep = " "), width = 6) +
      theme(axis.text.x = element_text(angle = 50, hjust = 0.5, vjust = 0.5))
  })
  
  # box plot results fourh page
  output$boxplot_results <- renderPrint({
    if (input$radio_results_box == 1) {
      filled <- conversion(input$filled_group)
      if (filled == "Species_name"){
        group <- conversion(input$selected_group1)
      }
      else if (filled == "Dutch_soil_code") {
        group <- conversion(input$selected_group2)
      }
      else {
        group <- conversion(input$selected_group3)
      }
      
      vapply(split(data[, paste(input$wood, input$composition, sep = "_")], list(data[, group], data[, filled])),
             FUN = quantile,
             FUN.VALUE = c(Min. = 0, "1st Qu." = 0,
             Median = 0, "3rd Qu." = 0, Max. = 0), na.rm = TRUE
      )
    }
  })
  
  output$outsiders_results <- renderDataTable({
    if (input$radio_results_outsiders == 1) {
      
      subsetted <- reactive({
        data |> plotly::filter((Species_name %in% input$species) & (Dutch_soil_code %in% input$soil) & (Soil_category %in% input$quality))
      })
      
      outlier_val <- boxplot.stats(subsetted()[, paste(input$wood, input$composition, sep = "_")])$out
      outlier_idx <- which(subsetted()[, paste(input$wood, input$composition, sep = "_")] %in% c(outlier_val))
      subsetted()[outlier_idx, ]
      
    }
  })
  
  # scatter plot fourth page
  output$scatter <- renderPlot({
    
    x <- paste(input$wood1, input$composition1, sep = "_")
    y <- paste(input$wood2, input$composition2, sep = "_")
    group <- conversion(input$group_scatter)
    col <- color[1:length(levels(data[, group]))]
    
    p <- ggplot2::ggplot(data, aes_string(x, y, group = group, color = group)) +
           geom_point(aes_string(fill = group), shape = 16, cex = 2) +
           theme_bw() +
           theme(legend.position = "bottom") + 
           scale_colour_manual(values = col) +
           labs(x = paste(x, "g.kg-1", sep = " "), y = paste(y, "g.kg-1", sep = " "), width = 6)
    
    if (input$smooth == 1) {
      p <- p + ggplot2::geom_smooth(method = input$method_smooth, aes_string(fill = group), show.legend = FALSE, alpha = 0.2)
    }
    
    if (input$marginal == 1) {
      ggExtra::ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE, color = col)
    }
    else {
      p
    }

  })
  
  # average concentrations and std-error fourth page
  output$scatter_results <- renderDataTable({
    if (input$show_results_scatter == 1) {
      col_num <- c(paste(input$wood1, input$composition1, sep = "_"), paste(input$wood2, input$composition2, sep = "_"))
      group <- conversion(input$group_scatter)
      
      # calcul mean for each group
      mean.group <- lapply(col_num, function(col) {
        lapply(split(data[[col]], data[, group]), function(x) {
          round(mean(x), 3)
        })
      })
      
      # calcul std error of the mean (SEM) for each group
      std.error.group <- lapply(col_num, function(col) {
        lapply(split(data[[col]], data[, group]), function(x) {
          round(plotrix::std.error(x), 3)
        })
      })
      
      me <- as.data.frame(do.call(cbind, lapply(mean.group, function(x) unlist(x))))
      colnames(me) <- paste("AC", col_num, sep = "_")
      
      se <- as.data.frame(do.call(cbind, lapply(std.error.group, function(x) unlist(x))))
      colnames(se) <- paste("SEM", col_num, sep = "_")
      
      res <- cbind(se, me)
      res$Group <- levels(data[, group])
      
      res[, c(5, 3, 1, 4, 2)]
    }
    
  })
  
  # pca plot fifth page
  output$pca_plot <- renderPlot({
    if (input$impute_PCA == 1) {
      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)
      
      data <- missMDA::imputePCA(data[, -c(1:8)], ncp = input$slider_pca, method = "Regularized")
      
      parallel::stopCluster(cl)
      
      data <- as.data.frame(data$completeObs)
      df_numeric <- data[ , sapply(data, is.numeric)]
      df_positives_numeric <- df_numeric[rowSums(df_numeric > 0) == ncol(df_numeric), , drop = FALSE]
      df_positives_factor <- data[rownames(df_positives_numeric), !sapply(data, is.numeric), drop = FALSE]
      df <- cbind(df_positives_factor, df_positives_numeric)
    }
    else {
      df <- data[, -c(1:8)]
    }
    
    cl <- parallel::makeCluster(2)
    doParallel::registerDoParallel(cl)
    
    res.pca <- FactoMineR::PCA(df)
    
    parallel::stopCluster(cl)
    
    if (input$radio_PCA == 1){
      plot(res.pca, choix = "ind")
    }
    else {
      plot(res.pca, choix = "var")
    }
    
  })
  
  # mca plot fifth page
  output$mca_plot <- renderPlot({
    if (input$impute_MCA == 1) {
      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)
      
      data <- missMDA::imputeMCA(data[, 4:6], ncp = input$slider_mca, method = "Regularized")
      
      parallel::stopCluster(cl)
      
      data <- as.data.frame(data$completeObs)
      df_numeric <- data[, sapply(data, is.numeric)]
      df_positives_numeric <- df_numeric[rowSums(df_numeric > 0) == ncol(df_numeric), , drop = FALSE]
      df_positives_factor <- data[rownames(df_positives_numeric), !sapply(data, is.numeric), drop = FALSE]
      df <- cbind(df_positives_factor, df_positives_numeric)
    }
    else {
      df <- data[, 4:6]
    }
    
    cl <- parallel::makeCluster(2)
    doParallel::registerDoParallel(cl)
    
    res.pca <- FactoMineR::MCA(df)
    
    parallel::stopCluster(cl)
    
    if (input$radio_MCA == 1){
      plot(res.pca, choix = "ind")
    }
    else if (input$radio_MCA == 2) {
      plot(res.pca, invisible = "ind")
    }
    else {
      plot(res.pca, choix = "var")
    }
    
  })
  
  # mfa plot fifth page
  output$mfa_plot <- renderPlot({
    if (input$impute_MFA == 1) {
      
      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)
      
      data <- missMDA::imputeMFA(data[, -c(1, 2, 3, 7, 8)], group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = input$slider_mfa, method = "Regularized")
      
      parallel::stopCluster(cl)
      
      data <- as.data.frame(data$completeObs)
      df_numeric <- data[ , sapply(data, is.numeric)]
      df_positives_numeric <- df_numeric[rowSums(df_numeric > 0) == ncol(df_numeric), , drop = FALSE]
      df_positives_factor <- data[rownames(df_positives_numeric), !sapply(data, is.numeric), drop = FALSE]
      df <- cbind(df_positives_factor, df_positives_numeric)
      colnames(df) <- c(colnames(data)[1:3], rep(composition, 7))
      res.mfa <- MFA(df, group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = 5,
                     name.group = c("Species", "Soil", "Stem_bark", "Stem_sapwood", "Stem_heartwood", "Stem_wood", "Coarse_branch_bark", "Coarse_branch_wood_without_bark", "Fine_branches"))
    }
    else {
      df <- data
      colnames(df) <- c(colnames(data)[1:8], rep(composition, 7))
      
      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)
      
      res.mfa <- FactoMineR::MFA(df[, -c(1, 2, 3, 7, 8)], group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = 5,
                                 name.group = c("Species", "Soil", "Stem_bark", "Stem_sapwood", "Stem_heartwood", "Stem_wood", "Coarse_branch_bark", "Coarse_branch_wood_without_bark", "Fine_branches"))
      
      parallel::stopCluster(cl)
      
    }
    habillage_ind <- conversion(input$habillage_ind)
    
    if (input$radio_MFA == 1){
      plot(res.mfa, choix = "ind", habillage = habillage_ind)
    }
    else if (input$radio_MFA == 2) {
      plot(res.mfa, choix = "var")
    }
    else if (input$radio_MFA == 3) {
      plot(res.mfa, choix = "group")
    }
    else if (input$radio_MFA == 4) {
      plot(res.mfa, choix = "axes")
    }
    else {
      plot(res.mfa, choix = "ind", partial = "all")
    }
    
  })
  
  # pca results fifth page
  output$results_PCA <- renderPrint({
    if (input$radio_results_PCA == 1) {
      if (input$impute_PCA == 1) {
        cl <- parallel::makeCluster(2)
        doParallel::registerDoParallel(cl)
        
        data <- missMDA::imputePCA(data[, -c(1:8)], ncp = input$slider_pca, method = "Regularized")
        
        parallel::stopCluster(cl)
        
        data <- as.data.frame(data$completeObs)
        df_numeric <- data[ , sapply(data, is.numeric)]
        df_positives_numeric <- df_numeric[rowSums(df_numeric > 0) == ncol(df_numeric), , drop = FALSE]
        df_positives_factor <- data[rownames(df_positives_numeric), !sapply(data, is.numeric), drop = FALSE]
        df <- cbind(df_positives_factor, df_positives_numeric)
      }
      else {
        df <- data[, -c(1:8)]
      }
      
      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)
      
      res.pca <- FactoMineR::PCA(df)
      
      parallel::stopCluster(cl)
      
      if (input$radio_PCA == 1) {
        if (input$results_ind_pca == "Eign values") {
          print(res.pca$eig)
        }
        else if (input$results_ind_pca == "Contribution") {
          print(res.pca$ind$contrib)
        }
        else {
          print(res.pca$ind$cos2)
        }
      }
      
      else {
        if (input$results_var_pca == "Correlation") {
          print(res.pca$var$cor)
        }
        else if (input$results_var_pca == "Contribution") {
          print(res.pca$var$contrib)
        }
        else {
          print(res.pca$var$cos2)
        }
      }
      
    }

  })
  
  # mca results fifth page
  output$results_MCA <- renderPrint({
    if (input$radio_results_MCA == 1) {
      if (input$impute_MCA == 1) {
        cl <- parallel::makeCluster(2)
        doParallel::registerDoParallel(cl)
        
        data <- missMDA::imputeMCA(data[, 4:6], ncp = input$slider_mca, method = "Regularized")
        
        parallel::stopCluster(cl)
        
        data <- as.data.frame(data$completeObs)
        df_numeric <- data[, sapply(data, is.numeric)]
        df_positives_numeric <- df_numeric[rowSums(df_numeric > 0) == ncol(df_numeric), , drop = FALSE]
        df_positives_factor <- data[rownames(df_positives_numeric), !sapply(data, is.numeric), drop = FALSE]
        df <- cbind(df_positives_factor, df_positives_numeric)
      }
      else {
        df <- data[, 4:6]
      }
      
      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)
      
      res.mca <- FactoMineR::MCA(df)
      
      parallel::stopCluster(cl)
      
      if (input$radio_MCA == 1) {
        if (input$results_ind_mca == "Eign values") {
          print(res.mca$eig)
        }
        else if (input$results_ind_mca == "Contribution") {
          print(res.mca$ind$contrib)
        }
        else {
          print(res.mca$ind$cos2)
        }
      }
      
      else if (input$radio_MCA == 2) {
        if (input$results_mod_mca == "Contribution") {
          print(res.mca$var$contrib)
        }
        else if (input$results_mod_mca == "Cos2") {
          print(res.mca$var$cos2)
        }
        else {
          print(res.mca$var$v.test)
        }
      }
    }

  })
  
  # mfa results fifth page
  output$results_MFA <- renderPrint({
    if (input$radio_results_MFA == 1) {
      if (input$impute_MFA == 1) {
        
        cl <- parallel::makeCluster(2)
        doParallel::registerDoParallel(cl)
        
        data <- missMDA::imputeMFA(data[, -c(1, 2, 3, 7, 8)], group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = input$slider_mfa, method = "Regularized")
        
        parallel::stopCluster(cl)
        
        data <- as.data.frame(data$completeObs)
        df_numeric <- data[ , sapply(data, is.numeric)]
        df_positives_numeric <- df_numeric[rowSums(df_numeric > 0) == ncol(df_numeric), , drop = FALSE]
        df_positives_factor <- data[rownames(df_positives_numeric), !sapply(data, is.numeric), drop = FALSE]
        df <- cbind(df_positives_factor, df_positives_numeric)
        colnames(df) <- c(colnames(data)[1:3], rep(composition, 7))
        
        cl <- parallel::makeCluster(2)
        doParallel::registerDoParallel(cl)
        
        res.mfa <- MFA(df, group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = 5,
                       name.group = c("Species", "Soil", "Stem_bark", "Stem_sapwood", "Stem_heartwood", "Stem_wood", "Coarse_branch_bark", "Coarse_branch_wood_without_bark", "Fine_branches"))
        
        parallel::stopCluster(cl)
        
      }
      else {
        df <- data
        colnames(df) <- c(colnames(data)[1:8], rep(composition, 7))
        
        cl <- parallel::makeCluster(2)
        doParallel::registerDoParallel(cl)
        
        res.mfa <- FactoMineR::MFA(df[, -c(1, 2, 3, 7, 8)], group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = 5,
                                   name.group = c("Species", "Soil", "Stem_bark", "Stem_sapwood", "Stem_heartwood", "Stem_wood", "Coarse_branch_bark", "Coarse_branch_wood_without_bark", "Fine_branches"))
      }
      habillage_ind <- conversion(input$habillage_ind)
      
      if (input$radio_MFA == 1) {
        if (input$results_ind == "Eign values") {
          print(res.mfa$eig)
        }
        else if (input$results_ind == "Inertia ratio") {
          print(res.mfa$inertia.ratio)
        }
        else if (input$results_ind == "Contribution") {
          print(res.mfa$ind$contrib)
        }
        else if (input$results_ind == "Cos2") {
          print(res.mfa$ind$cos2)
        }
        else {
          print(res.mfa$ind$within.inertia)
        }
      }
      
      else if (input$radio_MFA == 2) {
        if (input$results_var == 1) {
          if (input$results_var_quali == "Contribution") {
            print(res.mfa$quali.var$contrib)
          }
          else if (input$results_var_quali == "Cos2") {
            print(res.mfa$quali.var$cos2)
          }
          else if (input$results_var_quali == "Within inertia") {
            print(res.mfa$quali.var$within.inertia)
          }
          else {
            print(res.mfa$quali.var$v.test)
          }
        }
        else {
          if (input$results_var_qaunti == "Correlation") {
            print(res.mfa$quanti.var$cor)
          }
          else if (input$results_var_qaunti == "Contribution") {
            print(res.mfa$quanti.var$contrib)
          }
          else {
            print(res.mfa$quanti.var$cos2)
          }
        }
      }
      
      else if (input$radio_MFA == 3) {
        if (input$results_groups == "Lg") {
          print(res.mfa$group$Lg)
        }
        else if (input$results_groups == "RV") {
          print(res.mfa$group$RV)
        }
        else if (input$results_groups == "Correlation") {
          print(res.mfa$group$correlation)
        }
        else if (input$results_groups == "Contribution") {
          print(res.mfa$group$contrib)
        }
        else {
          print(res.mfa$group$cos2)
        }
      }
      
      else if (input$radio_MFA == 4) {
        if (input$results_partial_axes == "Correlation") {
          print(res.mfa$partial.axes$cor)
        }
        else {
          print(res.mfa$partial.axes$contrib)
        }
      }
      
    }
  })
  
  # definition of model fifth page
  mod <- reactive({
    
    if (input$model == "Linear model") {
      vaY <- paste(input$wood_lm_Y, input$composition_lm_Y, sep = "_")
      vaXfull <- full(input$quanti_X_lm, input$type_lm)
      
      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)
      
      if (input$impute_mod == 1) {
        data <- missMDA::imputeMFA(data[, -c(1, 2, 3, 7, 8)], group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = input$slider_mod, method = "Regularized")
        data <- as.data.frame(data$completeObs)
        
        if (input$neg_mod == 1) {
          df_numeric <- data[ , sapply(data, is.numeric)]
          df_positives_numeric <- df_numeric[rowSums(df_numeric > 0) == ncol(df_numeric), , drop = FALSE]
          df_positives_factor <- data[rownames(df_positives_numeric), !sapply(data, is.numeric), drop = FALSE]
          data <- cbind(df_positives_factor, df_positives_numeric)
        }
        
      }
      
      mod <- lm(formula = as.formula(paste(vaY, vaXfull, sep = "~")), data = data)
      
      parallel::stopCluster(cl)
      
      print(paste("Current model : lm(", vaY, " ~ ", vaXfull, ")", sep = ""))
      
    }
    
    else {
      
      if (input$radio_Y_type_glm == 1){
        vaY <- paste(input$wood_glm_Y, input$composition_glm_Y, sep = "_")
        family_glm <- input$family_glm1
      }
      else {
        vaY <- input$radio_Y_quali_glm
        family_glm <- input$family_glm2
      }
      
      vaX <- foreach(va_quali = input$quali_X_glm) %do% {conversion(va_quali)}
      if (input$radio_X_add_glm == 1 && length(input$quanti_X_glm) != 0) {
        vaX <- c(vaX, input$quanti_X_glm)
      }
      vaXfull <- full(vaX, input$type_glm)
      
      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)
      
      if (input$impute_mod == 1) {
        data <- missMDA::imputeMFA(data[, -c(1, 2, 3, 7, 8)], group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = input$slider_mod, method = "Regularized")
        data <- as.data.frame(data$completeObs)
        
        if (input$neg_mod == 1) {
          df_numeric <- data[ , sapply(data, is.numeric)]
          df_positives_numeric <- df_numeric[rowSums(df_numeric > 0) == ncol(df_numeric), , drop = FALSE]
          df_positives_factor <- data[rownames(df_positives_numeric), !sapply(data, is.numeric), drop = FALSE]
          data <- cbind(df_positives_factor, df_positives_numeric)
        }
        
      }
      
      mod <- glm(formula = as.formula(paste(vaY, vaXfull, sep = "~")), data = data, family = family_glm)
      
      parallel::stopCluster(cl)
      
      print(paste("Current model : glm(", vaY, " ~ ", vaXfull, ")", sep = ""))
      
    }
    
    mod
  })
  
  # summary model fifth page
  output$summary_mod <- renderPrint({
     summary(mod())
  })
  
  # plot model fifth page
  output$plot_mod <- renderPlot({
    par(mfrow = c(2,2))
    plot(mod())
  })

  # coefficients of the model fifth page
  output$coeff_mod <- renderPrint({
    coef(mod())
  })
  
  # anova (comparison with null model) fifth page
  output$anova <- renderPrint({
    if (input$model == "Linear model") {
      vaY <- paste(input$wood_lm_Y, input$composition_lm_Y, sep = "_")
      mod0 <- lm(as.formula(paste(vaY, 1, sep = "~")), data = data)
    }
    else {
      
      if (input$radio_Y_type_glm == 1){
        vaY <- paste(input$wood_glm_Y, input$composition_glm_Y, sep = "_")
        family_glm <- input$family_glm1
      }
      else {
        vaY <- input$radio_Y_quali_glm
        family_glm <- input$family_glm2
      }
      
      mod0 <- glm(as.formula(paste(vaY, 1, sep = "~")), data = data, family = family_glm)
      
    }
    
    anova(mod0, mod())
  })
  
  # confidences intervals fifth page
  output$interval_mod <- renderPrint({
    confint(mod())
  })
  
  # fitted values of the model fifth page
  output$fitted_mod <- renderPrint({
    fitted(mod())
  })
  
  # residuals values of the model fifth page
  output$residuals_mod <- renderPrint({
    resid(mod())
  })
  
  # all pairwise comparisons for quali fifth page
  output$tukey_mod <- renderPrint({
    if (input$radio_Y_type_glm == 1 && length(input$quali_X_glm) != 0 && input$radio_X_add_glm == 2 && input$model == "Generalized linear model") {
      TukeyHSD(aov(mod()))
    }
    else {
      if (input$impute_mod == 1) {
        print("The current model not work for an all pairwise comparison.")
        print("Use glm instead.")
      }
      else {
        print("Variables not match for an all pairwise comparison.")
        print("You need to set a quantitative variable for the response and one or several qualitative(s) variable(s) (only) for the explicative(s).")
      }
    }
  })
  
  # reactive data frame observed - predicted couples for quantitative variable
  obs_pred_data <- reactive({
    if (input$impute_mod == 1) {
      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)
      
      data <- missMDA::imputeMFA(data[, -c(1, 2, 3, 7, 8)], group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = 5, method = "Regularized")
      data <- as.data.frame(data$completeObs)
      
      parallel::stopCluster(cl)
      
      if (input$neg_mod == 1) {
        df_numeric <- data[ , sapply(data, is.numeric)]
        df_positives_numeric <- df_numeric[rowSums(df_numeric > 0) == ncol(df_numeric), , drop = FALSE]
        df_positives_factor <- data[rownames(df_positives_numeric), !sapply(data, is.numeric), drop = FALSE]
        data <- cbind(df_positives_factor, df_positives_numeric)
      }
      
    }
    
    # prediction
    if (length(data[, paste(input$wood_lm_Y, input$composition_lm_Y, sep = "_")]) == length(predict(mod()))) {
      obs_pred_data <- data.frame(Observed = data[, paste(input$wood_lm_Y, input$composition_lm_Y, sep = "_")], Predicted = predict(mod()))
      obs_pred_data <- as.data.frame(obs_pred_data)
      
      if (input$neg_obs_pred == 1) {
        # remove lines with negative(s) value(s)
        obs_pred_data <- subset(obs_pred_data, Observed > 0 & Predicted > 0)
      }
      
      obs_pred_data
    }
    else {
      NULL
    }
    
  })
  
  # reactive data frame observed - predicted couples for qualitative variable
  pred_quali_glm <- reactive({
    if (input$impute_mod == 1) {
      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)
      
      data <- missMDA::imputeMFA(data[, -c(1, 2, 3, 7, 8)], group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = 5, method = "Regularized")
      data <- as.data.frame(data$completeObs)
      
      parallel::stopCluster(cl)
      
      if (input$neg_mod == 1) {
        df_numeric <- data[ , sapply(data, is.numeric)]
        df_positives_numeric <- df_numeric[rowSums(df_numeric > 0) == ncol(df_numeric), , drop = FALSE]
        df_positives_factor <- data[rownames(df_positives_numeric), !sapply(data, is.numeric), drop = FALSE]
        data <- cbind(df_positives_factor, df_positives_numeric)
      }
      
    }
    
    # x use for prediction
    vaX <- foreach(va_quali = input$quali_X_glm) %do% {conversion(va_quali)}
    if (input$radio_X_add_glm == 1 && length(input$quanti_X_glm) != 0) {
      vaX <- c(vaX, input$quanti_X_glm)
    }
    x <- data[, unlist(vaX)]
    
    # y and family for glm
    vaY <- conversion(input$radio_Y_quali_glm)
    family_glm <- input$family_glm2

    # creation of target 0-1, corresponding to modality of qualitative variable to predict   
    data$Target <- ifelse(data[, vaY] == input$target, 1, 0)
    data$Target <- as.factor(data$Target)
    
    if (!all(finaldata$Target == 0)) {
      y <- data$Target
      
      # partitioning of test and train data set
      set.seed(10)
      test_index <- createDataPartition(y, times = 1, p = 0.35, list = FALSE)
      
      test_x <- x[test_index, ]
      test_y <- y[test_index]
      train_x <- x[-test_index, ]
      train_y <- y[-test_index]

      # k-fold cross-validation
      control <- trainControl(method = "cv", number = 10, p = .9, savePredictions = TRUE)
      
      # 
      unregister_dopar <- function() {
        env <- foreach:::.foreachGlobals
        rm(list = ls(name = env), pos = env)
      }
      unregister_dopar()
      
      # training of model
      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)
      
      train_glm <- train(train_x, train_y, method = "glm",
                         family = family_glm,
                         trControl = control)
      
      parallel::stopCluster(cl)
      
      # predictions
      glm_preds <- predict(train_glm, newdata =  test_x)
      
      if (length(data$Target) == length(glm_preds)) {
        
        obs_pred_data_quali <- data.frame(Observed = data$Target, Predicted = glm_preds)
        obs_pred_data_quali <- as.data.frame(obs_pred_data_quali)

        if (input$neg_obs_pred == 1) {
          # remove lines with negative(s) value(s)
          obs_pred_data_quali <- subset(obs_pred_data_quali, Observed > 0 & Predicted > 0)
        }
        
        list(obs_pred_data_quali, train_glm)
      }
      
      else {
        list(NULL, NULL)
      }
      
    }
    
    else {
      list(NULL, NULL)
    }
    
  })
  
  # predictions values of the response variable corresponding to the model
  output$prediction_mod <- renderPrint({
    
    if (input$model == "Linear model") {
      if (!is.null(obs_pred_data())) {
        print(paste("Lenght of sample use for prediction :", nrow(obs_pred_data()), sep = " "))
        rmsep <- caret::RMSE(obs_pred_data()$Predicted, obs_pred_data()$Observed)
        print(paste("RMSEP :", round(rmsep, 3), sep = " "))
      }
      else {
        print("Observed and predicted data don't have the same lenght.")
        print("Please use the NAs values imputation by an MFA model.")
      }
    }
    
    else {
      
      if (input$radio_Y_type_glm == 1) {
        if (!is.null(obs_pred_data())) {
          print(paste("Lenght of sample use for prediction :", nrow(obs_pred_data()), sep = " "))
          rmsep <- caret::RMSE(obs_pred_data()$Predicted, obs_pred_data()$Observed)
          print(paste("RMSEP :", round(rmsep, 3), sep = " "))
        }
        else {
          print("Observed and predicted data don't have the same lenght.")
          print("Please use the imputation of NAs values by an MFA model.")
        }
      }
      
      else {
        
        obs_pred_data_quali <- pred_quali_glm()[[1]]
        train_glm <- pred_quali_glm()[[2]]
        
        if (!is.null(obs_pred_data_quali)) {
          # checking proportions for training and test set
          round(mean(test_y == 0), 3) ; round(mean(train_y == 0), 3)
          round(mean(test_y == 1), 3) ; round(mean(train_y == 1), 3)
          
          # Model performance
          # confusion matrix
          confusionMatrix(obs_pred_data_quali$Predicted, obs_pred_data_quali$Observed)
          train_glm$resample
          getTrainPerf(train_glm)
          
          # prediction error, RMSEP
          rmsep <- caret::RMSE(obs_pred_data_quali$Predicted, obs_pred_data_quali$Observed)
          print(paste("RMSEP :", round(rmsep, 3), sep = " "))
        }
        
        else {
          print("The data set generated have no individuals with this filter.")
        }
        
      }
      
    }
    
  })
  
  # predictions values of the response variable corresponding to the model
  output$prediction_table <- renderDataTable({
    if (input$model == "Linear model" || input$radio_Y_type_glm == 1) {
      if (!is.null(obs_pred_data())) {
        round(obs_pred_data(), 2)
      }
    }
  })
  
  # plot predictions of the response variable corresponding to the model
  output$prediction_plot <- renderPlot({
    if (input$model == "Linear model" || input$radio_Y_type_glm == 1) {
      if (!is.null(obs_pred_data())) {
        plot(obs_pred_data()$Observed ~ obs_pred_data()$Predicted,
             xlab = "Predicted values (g.kg-1)", ylab = "Observed values (g.kg-1)",
             main = "Predicted VS. Observed values")
        if (!all(obs_pred_data()$Predicted == obs_pred_data()$Predicted[1])) {
          abline(lm(obs_pred_data()$Observed ~ obs_pred_data()$Predicted), col = "red")
        }
        else {
          abline(v = obs_pred_data()$Predicted[1], col = "red")
        }
      }
    }
  })
  
})
