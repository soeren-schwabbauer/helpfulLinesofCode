################################################################################
#Project:       RefugeesVoting #################################################
#Author:        Sören Schwabbauer ##############################################
#Initial_Date:  12.11.2022 #####################################################
#Situation: Same code for different variables, applied on a dataframe
################################################################################

library(plm)
library(htmltools)
library(modelsummary)
library(stargazer)

# create a tibble with variables
specifications <- ' "depvar",       "explvar",  "method"
                    "right_index",  "asyl_pm",  "within"
                    "variance",     "asyl_pm",  "within"
                    "right_index",  "asyl_pm",  "random"
                    "variance",     "asyl_pm",  "random"
'

specifications <- as_tibble(read.csv(textConnection(specifications),
                                     as.is = TRUE, strip.white = TRUE))


# list to store results
results <- list()

# loop regression over columns of tibble
for (i in 1:nrow(specifications)) {
  
  # Extracting variable names for each pair
  dep_variable  <- specifications$depvar[i]
  expl_variable <- specifications$explvar[i]
  lm_method     <- specifications$method[i]
  
  # Calling the function for each row in tibble
  formula <- as.formula(paste(dep_variable, "~", expl_variable))
  
  # apply (country & time) fixed-effects regression 
  lm <- try(plm(formula = formula,
                data=full_data, 
                index=c("country", "dm"), 
                model = lm_method))
  
  # Storing the results in results list
  results[[paste0(lm_method, "_", dep_variable, "~", expl_variable)]] <-  lm
  
}

results

stargazer_result <- stargazer(results, type = "html", align = T)