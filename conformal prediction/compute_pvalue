for (k in c(0, 1, 2, 3, 4))
{
  library("knitr")
  library("dplyr")
  library("ggplot2")
  library("gridExtra")
  source("functions.R")

  ## Original Data
  k <- as.character(k)
  df_test <- read.csv(file = file.path('Data', 'Demo', paste0('testset_', k, '.csv')))
  df_calib <- read.csv(file = file.path('Data', 'Demo', paste0('calibset_', k, '.csv')))
  write_output <- TRUE

  ########################################################################################
  ## ISUP
  data <- generate_vars(outcome = "ISUP", testdata = "test")
  testSet <- data$testSet
  testLabels <- data$testLabels
  calibset <- data$df_calib
  col_outcome <- data$col_outcome
  # ICP P-values: Class-conditional Inductive conformal classifier for multi-class problems
  pValues_isup <- ICPClassification(testSet = testSet, calibSet = calibset)
  # Calibration Plot
  isup <- calib_plot(pValues_isup, testSet = testSet) + ggtitle("Test: ISUP")
  # Compute conformal prediction regions at different significance levels
  df_pred05 <- df_predict_region(pValues_isup, sigfLevel = .05, outcome = 'ISUP')
  df_pred10 <- df_predict_region(pValues_isup, sigfLevel = .1, outcome = 'ISUP')
  df_pred20 <- df_predict_region(pValues_isup, sigfLevel = .2, outcome = 'ISUP')
  df_pred33 <- df_predict_region(pValues_isup, sigfLevel = 1/3, outcome = 'ISUP')
  # Cross-tabulate Outcome CX V. CP-CX Or Outcome ISUP V. CP-ISUP
  tab05 <- tab_predict_region(df_pred05)
  tab10 <- tab_predict_region(df_pred10)
  tab20 <- tab_predict_region(df_pred20)
  tab33 <- tab_predict_region(df_pred33)
  if (write_output){
    write.table(tab05, file = file.path('Output', 'Tables', paste0('test_ISUP_', k, '.csv')))
    write.table(tab10, file = file.path('Output', 'Tables', paste0('test_ISUP_', k, '.csv')), append = TRUE)
    write.table(tab20, file = file.path('Output', 'Tables', paste0('test_ISUP_', k, '.csv')), append = TRUE)
    write.table(tab33, file = file.path('Output', 'Tables', paste0('test_ISUP_', k, '.csv')), append = TRUE)
    write.table(pValues_isup, file = file.path('Output', 'Tables', paste0('pValues_isup_', k, '.csv')), sep=',', row.names = FALSE)
  }
  tab05;tab10;tab20;tab33
  grid.arrange(isup)

  rm(list=ls())
}



for (k in c(0, 1, 2, 3, 4))
{
  # external
  library("knitr")
  library("dplyr")
  library("ggplot2")
  library("gridExtra")
  source("functions.R")

  ## Original Data
  k <- as.character(k)
  df_test <- read.csv(file = file.path('Data', 'Demo', paste0('externalset_', k, '.csv')))
  df_calib <- read.csv(file = file.path('Data', 'Demo', paste0('calibset_', k, '.csv')))

  write_output <- TRUE

  ########################################################################################
  ## ISUP
  data <- generate_vars(outcome = "ISUP", testdata = "test")
  testSet <- data$testSet
  testLabels <- data$testLabels
  calibset <- data$df_calib
  col_outcome <- data$col_outcome
  # ICP P-values: Class-conditional Inductive conformal classifier for multi-class problems
  pValues_isup <- ICPClassification(testSet = testSet, calibSet = calibset)
  # Calibration Plot
  isup <- calib_plot(pValues_isup, testSet = testSet) + ggtitle("Test: ISUP")
  # Compute conformal prediction regions at different significance levels
  df_pred05 <- df_predict_region(pValues_isup, sigfLevel = .05, outcome = 'ISUP')
  df_pred10 <- df_predict_region(pValues_isup, sigfLevel = .1, outcome = 'ISUP')
  df_pred20 <- df_predict_region(pValues_isup, sigfLevel = .2, outcome = 'ISUP')
  df_pred33 <- df_predict_region(pValues_isup, sigfLevel = 1/3, outcome = 'ISUP')
  # Cross-tabulate Outcome CX V. CP-CX Or Outcome ISUP V. CP-ISUP
  tab05 <- tab_predict_region(df_pred05)
  tab10 <- tab_predict_region(df_pred10)
  tab20 <- tab_predict_region(df_pred20)
  tab33 <- tab_predict_region(df_pred33)
  if (write_output){
    write.table(tab05, file = file.path('External_output', 'Tables', paste0('test_ISUP_', k, '.csv')))
    write.table(tab10, file = file.path('External_output', 'Tables', paste0('test_ISUP_', k, '.csv')), append = TRUE)
    write.table(tab20, file = file.path('External_output', 'Tables', paste0('test_ISUP_', k, '.csv')), append = TRUE)
    write.table(tab33, file = file.path('External_output', 'Tables', paste0('test_ISUP_', k, '.csv')), append = TRUE)
    write.table(pValues_isup, file = file.path('External_output', 'Tables', paste0('pValues_isup_', k, '.csv')), sep=',', row.names = FALSE)
  }
  tab05;tab10;tab20;tab33
  grid.arrange(isup)

  rm(list=ls())
}


