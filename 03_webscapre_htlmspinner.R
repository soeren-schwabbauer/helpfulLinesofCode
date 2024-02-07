################################################################################
# Date: 07.02.2024
# Project: Ceasefire Funding
# Author: Sören Schwabbauer
# Purpose: Webscrape Data, where spinner blocks read_html
# Applies RSelenium
# Uses UCDP Data as an Example
# Note: requires java jdk & java jre + os langage in english (for free_port)
#################################################################################

# load libraries
library(RSelenium)
library(netstat)
library(wdman)
library(XML)
library(rvest)
library(xml2)

library(tidyverse)


# scrape raw data --------------------------------------------------------------
i <- c(303, 500)

extracted_numbers <- list()


for(i in rebels_id){
  
  
  wp <- paste0('https://ucdp.uu.se/additionalinfo/', i, '/0#1996grouparchive')
  
  
  # --- start Rselenium server ---
  rs_driver_object <- rsDriver(
    browser = "firefox",
    chromever = NULL,
    #verbose = F,
    #version = '4.0.0-alpha-2'
    #verbose = TRUE,
    port = free_port(random = TRUE)
  )
  
  # open wp
  remDr <- rs_driver_object$client
  remDr$navigate(wp)
  Sys.sleep(3) # allow page to load
  
  
  # --- get html text ---
  text <- read_html(remDr$getPageSource()[[1]]) %>% html_text 
  
  # remove everything before Group Archive section
  text <- gsub('.*This section contains annual information no longer systematically collected by the UCDP.', "", text)
  text <- gsub(',', '', text)
  
  # get patterns for gsub
  pattern_text <- "(\\d{4}) Group Archive\\n\\n"
  pattern_year <- "\\b(\\d{4})\\b(?=.*Group Archive)"
  
  # Split the text into a list (one year = one element) based on the pattern
  text_list <- str_split(text, pattern_text)[[1]] %>% as.list()
  # first line is always empty
  text_list <- text_list[-1] 
  
  # remove spaces & lines that cause disturbance
  text_list <- lapply(text_list, function(x) gsub(" ", "", x))
  text_list <- lapply(text_list, function(x) gsub("CommentonTroopSize.*", "", x))
  
  
  # --- Get Toop sizes ---
  troop_sizes <- str_extract(text_list, "(?<=TroopSize:)[<>]?\\d+(-\\d+)?")
  years <- str_extract_all(text, pattern_year) %>% unlist()
  
  group_id <- as.character(i) #list name as character
  
  # -- troop size into df ---
  extracted_numbers[[group_id]] <- data.frame(troop_size = troop_sizes,
                                              year = years,
                                              actor_id = group_id) 
  
  Sys.sleep(60) # note: high sleep time, so we don't get too many requests
}


# bind rows from dataframe
df <- bind_rows(extracted_numbers)