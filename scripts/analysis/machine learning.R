#------------------------------------------------------------------------------ #
#                                                                               #
#                          PPOL 670 - Final Project                             #
#                   Analysis: Supervised Machine Learning                       #
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
                  "caret",
                  "recipes",
                  "skimr",
                  "rattle",
                  "pdp",
                  "e1071")
    
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
    
  # 4) Remove missing values in working status
    data1 <- data[!is.na(data$IDind),]
    data1 <- data1[!is.na(data$working),]
    data1 <- data1[data1$working != -9,]
  
  # 5) Remove irrelavant variables
    data2 <- subset(data1, select = -c(hhid, IDind, WEST_DOB_Y, wave, jobs, educ_year, inschool, province,
                                       mar_status, numkids, GENDER, indinc_cpi))

### PART 5: Split the data --------------------------------------------------------
  index = createDataPartition(data2$working, p=.75, list=F) 
  train_data = data2[index,]
  test_data = data2[-index,]

  dim(train_data)
  dim(test_data) 

  # explore the data set
  skim(train_data)

### PART 6: Impute the missing values ---------------------------------------------
  
    # 1) train data
    rec <- recipe(working~.,
                  data = train_data)
    ratio_recipe <- rec %>%
      step_knnimpute(all_predictors(), neighbors = 3)
    ratio_recipe2 <- prep(ratio_recipe, training = train_data)
    train_data1 <- bake(ratio_recipe2, train_data)

    # 2) test data
    rec1 <- recipe(working~.,
                   data = test_data)
    ratio_recipe1 <- rec1 %>%
      step_knnimpute(all_predictors(), neighbors = 3)
    ratio_recipe3 <- prep(ratio_recipe1, training = test_data)
    test_data1 <- bake(ratio_recipe3, test_data)

    # 3) explore the data to see if there are missingness
    skim(train_data1)
    skim(test_data1)

### PART 7: convert variables that have right-skewed distribution
    
    # 1) Convert household income
    convert_hhinc <- . %>% 
      mutate(hhinc_cpi = log(hhinc_cpi + 679519.07229))
    
    train_data2 <- train_data1 %>%  convert_hhinc() # Apply to both the training and test data
    test_data2 <- test_data1 %>%  convert_hhinc()
    
    # 2) Checking for results
    skim(train_data2)
    skim(test_data2)
    
    # 3) Normalize the scale
    rcp <- 
      recipe(working~.,train_data2) %>% 
      step_range(educ_degree, hhinc_cpi, hhsize, NATIONALITY) %>%  # Normalize scale
      prep()

    train_data3 <- bake(rcp,train_data2) # apply to both train data and test dat
    test_data3 <- bake(rcp,test_data2) 

    # 4) check for converted and normalized variables
    skim(train_data3)
    skim(test_data3)


### PART 7: Cross-Validation Setting --------------------------------------------------
    
    # 1) K-fold validation with 5 fold
    
    set.seed(1988) # set a seed for replication purposes 
    folds <- createFolds(train_data3$working, k = 5) # Partition the data into 5 equal folds
    sapply(folds,length)

    # 2) Set up validation conditions
    control_conditions <- 
      trainControl(method='cv', # K-fold cross validation
                   summaryFunction = twoClassSummary, # Need this b/c it's a classification problem
                   classProbs = TRUE, # Need this b/c it's a classification problem
                   index = folds # The indices for our folds (so they are always the same)
                   )


### PART 8: Supervised statistical learning algorithms ----------------------------------------
    # 1) K-nearest
    train_data3$working <- as.factor(train_data3$working)
    levels(train_data3$working) <- c("unemployed", "employed")
    
    mod_knn <-
      train(working~ ., # Equation (outcome and everything else)
            data=train_data3, # Training data 
            method = "knn", # K-Nearest Neighbors Algorithm
            metric = "ROC", # area under the curve
            trControl = control_conditions
            )
        # check the result
        mod_knn
        
        # plot the result
        plot(mod_knn)
        
    # 2) `knn` model with tuning parameters 1, 3, 5, 10
    knn_tune = expand.grid(k = c(1,3,5,10))
    knn_tune

    mod_knn2 <-
      train(working ~ ., # Equation (outcome and everything else)
            data=train_data3, # Training data 
            method = "knn", # K-Nearest Neighbors Algorithm
            metric = "ROC", # area under the curve
            tuneGrid = knn_tune, # add the tuning parameters here 
            trControl = control_conditions
            )

        # plot the result
        plot(mod_knn2)

    # 3) Classification and Regression Trees
    mod_cart <-
      train(working ~ ., # Equation (outcome and everything else)
            data=train_data3, # Training data 
            method = "rpart", # Classification Tree 
            metric = "ROC", # area under the curve 
            trControl = control_conditions
            )
        # check the result
        mod_cart
        plot(mod_cart)
  
      # Setting up complexity parameter
        tune_cart2 <- expand.grid(cp = c(0.001)) # Complexity Parameter 
        mod_cart2 <-
          train(working ~ ., # Equation (outcome and everything else) 
          data=train_data3, # Training data
          method = "rpart", # Classification Tree
          metric = "ROC", # area under the curve
          tuneGrid = tune_cart2, # Tuning parameters
          trControl = control_conditions
          )
        fancyRpartPlot(mod_cart2$finalModel)

      # Shallow Tree
        test_data3$working <- as.factor(test_data3$working)
        levels(test_data3$working) <- c("unemployed", "employed")
        
        install.packages('e1071', dependencies=TRUE) 
        pred <- predict(mod_cart,newdata = test_data3) 
        shallow_tree_table <-
          confusionMatrix(table(pred,test_data3$working))
      
      # Long Tree
        pred <- predict(mod_cart2,newdata = test_data3) 
        long_tree_table <-
          confusionMatrix(table(pred,test_data3$working))
    
    # 4) Random Forest
        mod_rf <-
          train(working ~ ., # Equation (outcome and everything else)
                data=train_data3, # Training data
                method = "ranger", # random forest (ranger is much faster than rf) 
                metric = "ROC", # area under the curve
                importance = 'impurity', # For variable importance metric (see below) 
                trControl = control_conditions
          )
        
        # Check for results
        mod_rf
        
        # Plot the results
        plot(mod_rf)
        
        # with different mtry setting
        mtry <- 10
        
        # parameters
        rf_tune <- expand.grid(mtry = 12, splitrule = "gini",
                               min.node.size = 1
        )
        
        mod_rf2 <-
          train(working ~ ., # Equation (outcome and everything else)
                data=train_data3, # Training data
                method = "ranger", # Classification Tree 
                metric = "ROC", # area under the curve 
                tuneGrid = rf_tune, # Tuning parameters 
                importance = 'impurity',
                trControl = control_conditions
          )
        
        # chech the result
        mod_rf2
        
        # Plot the result
        plot(mod_rf2)
        
        
### PART 9: Compare the output of all three models ----------------------------------
    mod_list <- list(
      knn1 = mod_knn,
      knn2 = mod_knn2,
      cart1 = mod_cart,
      cart2 = mod_cart2,
      rf = mod_rf,
      rf2 = mod_rf2
      )
    # Resamples allows us to compare model output
    resamples(mod_list)
    dotplot(resamples(mod_list),horizontal = NULL)
        
### PART 10: Variable Importance --------------------------------------------------
    pred <- predict(mod_rf,newdata = test_data3) 
    confusionMatrix(table(pred,test_data3$working))
    # 1) variance importance
    plot(varImp(mod_rf))
    
    # 2) dependency plot
    grid.arrange(
      partial(mod_rf,pred.var = "educ_degree",plot = T), 
      partial(mod_rf,pred.var = "female",plot = T), 
      partial(mod_rf,pred.var = "hhsize", plot = T)
    )
    
