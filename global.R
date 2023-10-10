# Packages importation
if(!require(knitr)) install.packages("shiny")
if(!require(tidyverse)) install.packages("shinythemes")
if(!require(dplyr)) install.packages("plotly")
if(!require(caret)) install.packages("ggplot2")
if(!require(caret)) install.packages("ggExtra")
if(!require(gam)) install.packages("graphics")
if(!require(evtree)) install.packages("FactoMineR")
if(!require(matrixStats)) install.packages("leaflet")
if(!require(evtree)) install.packages("foreach")
if(!require(gam)) install.packages("parallel")
if(!require(matrixStats)) install.packages("doParallel")
if(!require(matrixStats)) install.packages("multcomp")
if(!require(matrixStats)) install.packages("caret")
if(!require(matrixStats)) install.packages("plotrix")
if(!require(matrixStats)) install.packages("VIM")
if(!require(matrixStats)) install.packages("deltaPlotR")

library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(ggExtra)
library(graphics)
library(FactoMineR)
library(leaflet)
library(foreach)
library(parallel)
library(doParallel)
library(multcomp)
library(caret)
library(plotrix)
library(VIM)
library(deltaPlotR)

# Class definition
Environnement <- setRefClass("Loading Data",
                             fields = list(path = "character", file = "character", strVec = "character", factVect = "factor",
                                           numVec = "numeric", doubleVec = "double", intVec = "integer", dateVec = "Date"),
                             methods = list(loadingData = function(path, file, strVec, factVect, numVec, doubleVec, intVec, dateVec){
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

# Function definition

# paste all elem of 2 vectors 
assemble <- function(vec1, vec2 = composition) {
  name <- c()
  for (elem1 in vec1) {
    for (elem2 in vec2) {
      name <- c(name, paste(elem1, elem2, sep = "_"))
    }
  }
  name
}

# choosing / or * for radius intensity circle of map
intensity <- function(var, dta, para) {
  res <- 1
  if (var == "C") {
    res <- dta / para
  }
  else {
    res <- dta * para
  }
  res
}

# conversion of input qualitative variable names
conversion <- function(name) {
  group <- ""
  if (name == "Species") {
    group <- "Species_name"
  }
  else if (name == "Soil type") {
    group <- "Dutch_soil_code"
  }
  else {
    group <- "Soil_category"
  }
  group
}

# creation of formula on X side for modeling (additive + // interaction *)
full <- function(vaX, type) {
  if (length(vaX) == 0) {
    vaXfull <- 1
  }
  else if (length(vaX) == 1) {
    vaXfull <- vaX[1]
  }
  else {
    if (type == 1) {
      vaXfull <- paste(vaX, collapse = " * ")
    }
    else {
      vaXfull <- paste(vaX, collapse = " + ")
    }
  }
  vaXfull
}

# Loading data
path <- paste(getwd(), "/Data", sep = "")
suppressWarnings(data <- Environnement(path, "Wood_Nutrients.xlsx", c(), c("Sample_ID", "Plot_ID", "Tree_species", "Species_name", "Dutch_soil_code", "Soil_category"), c("Stem_bark_Ca", "Stem_bark_K", "Stem_bark_Mg", "Stem_bark_P", "Stem_bark_S", "Stem_bark_N", "Stem_bark_C", "Stem_sapwood_Ca", "Stem_sapwood_K", "Stem_sapwood_Mg", "Stem_sapwood_P", "Stem_sapwood_S", "Stem_sapwood_N", "Stem_sapwood_C", "Stem_heartwood_Ca", "Stem_heartwood_K", "Stem_heartwood_Mg", "Stem_heartwood_P", "Stem_heartwood_S", "Stem_heartwood_N", "Stem_heartwood_C", "Stem_wood_Ca", "Stem_wood_K", "Stem_wood_Mg", "Stem_wood_P", "Stem_wood_S", "Stem_wood_N", "Stem_wood_C", "Coarse_branch_bark_Ca", "Coarse_branch_bark_K", "Coarse_branch_bark_Mg", "Coarse_branch_bark_P", "Coarse_branch_bark_S", "Coarse_branch_bark_N", "Coarse_branch_bark_C", "Coarse_branch_wood_without_bark_Ca", "Coarse_branch_wood_without_bark_K", "Coarse_branch_wood_without_bark_Mg", "Coarse_branch_wood_without_bark_P", "Coarse_branch_wood_without_bark_S", "Coarse_branch_wood_without_bark_N", "Coarse_branch_wood_without_bark_C", "Fine_branches_Ca", "Fine_branches_K", "Fine_branches_Mg", "Fine_branches_P", "Fine_branches_S", "Fine_branches_N", "Fine_branches_C"), c("Lat", "Long"), c(), c())$loadingData(path, "Wood_Nutrients.xlsx", c(), c("Sample_ID", "Plot_ID", "Tree_species", "Species_name", "Dutch_soil_code", "Soil_category"), c("Stem_bark_Ca", "Stem_bark_K", "Stem_bark_Mg", "Stem_bark_P", "Stem_bark_S", "Stem_bark_N", "Stem_bark_C", "Stem_sapwood_Ca", "Stem_sapwood_K", "Stem_sapwood_Mg", "Stem_sapwood_P", "Stem_sapwood_S", "Stem_sapwood_N", "Stem_sapwood_C", "Stem_heartwood_Ca", "Stem_heartwood_K", "Stem_heartwood_Mg", "Stem_heartwood_P", "Stem_heartwood_S", "Stem_heartwood_N", "Stem_heartwood_C", "Stem_wood_Ca", "Stem_wood_K", "Stem_wood_Mg", "Stem_wood_P", "Stem_wood_S", "Stem_wood_N", "Stem_wood_C", "Coarse_branch_bark_Ca", "Coarse_branch_bark_K", "Coarse_branch_bark_Mg", "Coarse_branch_bark_P", "Coarse_branch_bark_S", "Coarse_branch_bark_N", "Coarse_branch_bark_C", "Coarse_branch_wood_without_bark_Ca", "Coarse_branch_wood_without_bark_K", "Coarse_branch_wood_without_bark_Mg", "Coarse_branch_wood_without_bark_P", "Coarse_branch_wood_without_bark_S", "Coarse_branch_wood_without_bark_N", "Coarse_branch_wood_without_bark_C", "Fine_branches_Ca", "Fine_branches_K", "Fine_branches_Mg", "Fine_branches_P", "Fine_branches_S", "Fine_branches_N", "Fine_branches_C"), c("Lat", "Long"), c(), c()))

# Vector
wood.site <- c("Stem_bark", "Stem_sapwood", "Stem_heartwood", "Stem_wood", "Coarse_branch_bark", "Coarse_branch_wood_without_bark", "Fine_branches")
composition <- c("Ca", "K", "Mg", "P", "S", "N", "C")
color <- c("orange", "red", "green", "blue", "yellow", "saddlebrown", "grey", "black", "purple", "cyan", "chocolate", "darkgreen", "darkblue", "burlywood", "pink", "darkred")

