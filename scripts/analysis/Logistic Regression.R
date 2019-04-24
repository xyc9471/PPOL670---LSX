#------------------------------------------------------------------------------ #
#                                                                               #
#                          PPOL 670 - Final Project                             #
#                       Analysis: Logistic Regression                           #
#                                                                               #
#                                                                               #
#------------------------------------------------------------------------------ #

# PURPOSE:    Set-up configurations and run scripts that are used to analyze data 
#             for ppol670 final project
# NOTES:      The data is from China Nutrition and Health Survey

# WRITTEN BY: Yuchen Xiang

### PART 0: Clear boiler plate --------------------------------------------------

    rm(list=ls())

### PART 1: Setup work directory ------------------------------------------------

    projectFolder  <- file.path("/Users/orangexx/Documents/Georgetown/2019 Spring/Intro to data/PPOL670---LSX")
    dataWorkFolder <- file.path(projectFolder, "data")
    rawdata        <- file.path(dataWorkFolder, "rawdata")
    output         <- file.path(dataWorkFolder, "intermediate")
    finaldata      <- file.path(dataWorkFolder, "finaldata")
    outcomes       <- file.path(projectFolder, "outputs")
    visual         <- file.path(outcomes, "visualizations")
    tables         <- file.path(outcomes, "tables")

### PART 2: Load Packages -------------------------------------------------------
    
    packages <- c("haven",
                  "tidyverse",
                  "tidyr",
                  "ggplot2",
                  "stargazer",
                  "MASS",
                  "ISLR")
    
    sapply(packages, function(x) {
      print(x)
      if (x %in% installed.packages() == FALSE) {
        install.packages(x, dependencies = TRUE) 
      }
      library(x, 
              character.only = T)
    }
    )
    
### PART 3: Load data file ----------------------------------------------------------
    
    data <- read_dta(file.path(finaldata,
                               "FinalData.dta"))

### PART 4: Preprocess the data ----------------------------------------------------
    
  # 1) Generate Gender Dummy
    data$female <- 0
    data$female[FinalData$GENDER == 2] <- 1
    
  # 2) Generate marital status dummy # reference group: single
    data$married <- 0
    data$married[data$mar_status == 2] <- 1
    
    data$divorced <- 0
    data$divorced[data$mar_status == 3] <- 1
    
    data$widowed <- 0
    data$widowed[data$mar_status == 4] <- 1
    
    data$separate <- 0
    data$separate[data$mar_status == 5] <- 1
    
  # 3) Generate urban dummy # reference group: rural
    data$urban <- 0
    data$urban[data$area == 1] <- 1

### PART 5: Logistic Regression -----------------------------------------------------
    
  # 1) Running the regression
    logit1 <- glm(working ~ female + educ_degree + urban + 
                    married + divorced + widowed + separate +
                    hhsize + kids, 
                  data = data, family = binomial)

    summary(logit1)
 
  # 2) Make a table
    indepvar <- c("Female", "Education Degree", "Urban", 
                  "Married", "Divorced", "Widowed", "Separate",
                  "Household Size", "Children")
    
    stargazer(logit1, type = "text",
              dep.var.labels = c("Currently Working?"),
              covariate.labels = indepvar,
              out = file.path(tables, "logistic.txt"))
    
  # 3) Calculate the odds ratio
    odds <- exp(coefficients(logit1))
    odds <- as.data.frame(odds)
    