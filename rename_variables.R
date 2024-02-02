################################################################################
#Project:       MigNet #########################################################
#Author:        Sören Schwabbauer ##############################################
#Initial_Date:  12.11.2022 #####################################################
#Situation: Different datafiles contain different names for the same variables.
#           This file renames the variables, so they all have the same name.
################################################################################
rm(list = ls())

### load libraries
library(dplyr)
library(haven)
library(tidyverse)
library(readxl)
library(stringr)


### define INPUT, OUTPUT
INPUT = "G:/Geteilte Ablagen/MigrantNetworks/04_Additional_Data_Build/FIES - RAW/OUTPUT/"


##### load files

# load codebook
codebook <- read_excel(paste0(INPUT, "01_codebook.xlsx"), sheet = "variables")

# load files (in loop)
FILE = paste0("fies_", seq(1988, 2015, by = 3) , ".rda")
for(i in FILE){load(paste0(INPUT, i))}



recode <- function(year){
  
  # da nicht jedes Jahr alle variable hat, erst einen 2spaltigen df und dann die na's entfernen
  variables_new <- codebook %>% select(relabel, {{year}}) %>% na.omit() 
  variables <- variables_new %>% pull({{year}}) %>% tolower()
  recoded <- variables_new %>% pull(relabel)
  
  # select variables
  year <- year %>% select(variables)
  
  # rename columns with values from excel sheet
  colnames(year) <- recoded
  
  year <- remove_attributes(year, "stata.info")
  
  year <- year %>% mutate(across(everything(), as.character))
  return(year)
}

# apply function
fies_1988 <- recode(fies_1988)
# ...