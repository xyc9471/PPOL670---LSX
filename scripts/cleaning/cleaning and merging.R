#------------------------------------------------------------------------------ #
#                                                                               #
#                                    DIME                                       #
#                   Data Cleaning for PPOL670 Final Project                     #
#                                                                               #
#                                                                               #
#------------------------------------------------------------------------------ #

# PURPOSE:    Set-up configurations and run scripts that are used to clean data 
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
                "data.table",
                "dplyr")
  
  sapply(packages, function(x) {
    print(x)
    if (x %in% installed.packages() == FALSE) {
      install.packages(x, dependencies = TRUE) 
    }
    library(x, 
            character.only = T)
  }
  )

### PART 3: Load data files -----------------------------------------------------
  
  master <- read_dta(file.path(rawdata,
                               "mast_pub_12.dta")) # ID dataset
  
  educ   <- read_sas(file.path(rawdata,
                               "educ_12.sas7bdat")) # Education dataset

  income <- read_sas(file.path(rawdata,
                               "indinc_10.sas7bdat")) # Individual income dataset
  
  jobs   <- read_sas(file.path(rawdata,
                               "jobs_12.sas7bdat")) # Occupation dataset
  
  marriage <- read_sas(file.path(rawdata,
                                  "rst_12.sas7bdat")) # marriage dataset
  
  hhinfo <- read_dta(file.path(rawdata,
                               "hhinc_10.dta")) # household information dataset
  
  children <- read_sas(file.path(rawdata,
                                 "emw_12.sas7bdat")) # children information
### PART 4: Cleaning out the ID variables ----------------------------------------
  
  # 1) Check for duplicates first
  which(duplicated(master$Idind)) # integer(0), no dups in ID
  
  # 2) Drop Lunar birth date
  master <- subset(master, select = -c(MOON_DOB_Y))

  # 3) Drop observations do not fulfill requirement
  master <- master[master$WEST_DOB_Y > 1955 & master$WEST_DOB_Y < 1997, ] # In China, the retirement age is 60 and legal age for work is 18,
                                                                          # Drop obs older than 60 (2015-60 = 1955) and younger than (2015-18 = 1997)
  # 4) Calculate the total number of years a person lived
  master$lifetime <- master$DOD_Y - master$WEST_DOB_Y

  # 5) Drop people who did not live beyond 18 years old
  master <- as.data.frame(master)
  master1 <- subset(master, lifetime<0 | lifetime>18 | is.na(lifetime))
  
  # 6) Drop variables that are not needed
  master1 <- subset(master, select = c(Idind, GENDER, NATIONALITY, WEST_DOB_Y))
  
  # 7) Export the ID dataset
  write_dta(master1, file.path(output,
                               "masterID.dta"))
### PART 5: Cleaning out the education information ---------------------------
  educ[1:17] <- lapply(educ[1:17], as.numeric)
  educ <- subset(educ, select = c(IDind, WAVE, A11, A12, A13, T1, T2))
  
  # 1) Check for duplicates first
  which(duplicated(educ$IDind))
  
  # 2) Keep only the most recent record
  educ1 <-
    educ %>% 
    group_by(IDind) %>%
    slice(which.max(WAVE))
  
  which(duplicated(educ1$IDind)) # no dups anymore
  
  # 3) Rename the variables
  educ1 <-
    educ1 %>%
    rename(educ_year = A11,
           educ_degree = A12,
           inschool = A13,
           province = T1,
           area = T2)
  
  # 4) Export the education dataset
  write_dta(educ1, file.path(output,
                               "education.dta"))

### PART 6: Cleaning out the income information --------------------------------
  income[1:27] <- lapply(income[1:27], as.numeric)
  income <- subset(income, select = c(IDind, wave, indinc_cpi))  
  
  # 1) Keep only the most recent record
  income1 <-
    income %>% 
    group_by(IDind) %>%
    slice(which.max(wave))
  
  # 2) Export the income dataset
  write_dta(income1, file.path(output,
                              "income.dta"))

### PART 7: Occupation information  ----------------------------------------------------
  jobs[1:31] <- lapply(jobs[1:31], as.numeric)
  jobs <- subset(jobs, select = c(IDind, wave, B2, B4))  

  # 1) Keep only the most recent record
  jobs1 <-
    jobs %>% 
    group_by(IDind) %>%
    slice(which.max(wave))
  
  # 2) Rename the variables
  jobs1 <-
    jobs1 %>%
    rename(working = B2,
           jobs = B4)
  
  # 3) Export the jobs dataset
  write_dta(jobs1, file.path(output,
                             "occupation.dta"))

### PART 8: Cleaning out the marriage status ---------------------------------------
  marriage[1:67] <- lapply(marriage[1:67], as.numeric)
  marriage <- subset(marriage, select = c(IDind,WAVE,hhid,A8 ))
  # 1) Keep only the most recent record
  marriage1 <-
    marriage %>% 
    group_by(IDind) %>%
    slice(which.max(WAVE))
  
  # 2) Export the marriage status 
  write_dta(marriage1, file.path(output,
                                "marriage.dta"))

### PART 9: Cleaning out the household information ---------------------------------
  hhinfo[1:39] <- lapply(hhinfo[1:39], as.numeric)
  hhinfo <- subset(hhinfo, select = c(hhid, WAVE, hhinc_cpi, hhsize))  
  
  # 1) Keep only the most recent record
  hhinfo1 <-
    hhinfo %>% 
    group_by(hhid) %>%
    slice(which.max(WAVE))
  
  # 2) export the household information dataset
  write_dta(hhinfo1, file.path(output,
                               "hhinformation.dta"))

### PART 10: Cleaning out the number of children for each household ---------------
  children[1:60] <- lapply(children[1:60], as.numeric)
  children <- subset(children, select = c(IDind, hhid, wave, S47A, S47))  
  
  # 1) Keep only the most recent record 
  children1 <-
    children %>% 
    group_by(IDind) %>%
    slice(which.max(wave))
  
  # 2) Rename variables
  children1 <-
    children1 %>%
    rename(numbirth = S47A,
           numdied = S47)
  
  # 3) Generate a var indicating number of children
  children2 <- 
    children1 %>% 
    mutate_all(funs(replace_na(.,0)))
  
  children2$numkids <- children2$numbirth - children2$numdied  
  children2 <- subset(children2, numkids >= 0)
  
  # 4) Generate a dummy for whether a woman has children
  children2$kids <- 0
  children2$kids[children2$numkids > 0] <- 1
  
  # 5) Export the dataset
  children3 <- subset(children2, select = -c(numbirth, numdied))
  write_dta(children3, file.path(output,
                                 "children.dta"))  

### PART 11: Merging all info into one dataset -----------------------------
  
    # 1) Loading the intermediate datasets
    master <- read_dta(file.path(output,
                               "masterID.dta")) # ID dataset
  
    educ   <- read_dta(file.path(output,
                               "education.dta")) # Education dataset
  
    income <- read_dta(file.path(output,
                               "income.dta")) # Individual income dataset
  
    jobs   <- read_dta(file.path(output,
                               "occupation.dta")) # Occupation dataset
  
    marriage <- read_dta(file.path(output,
                                 "marriage.dta")) # marriage dataset
  
    hhinfo <- read_dta(file.path(output,
                               "hhinformation.dta")) # household information dataset
  
    children <- read_dta(file.path(output,
                                 "children.dta")) # children information
    # 2) Merging the jobs
    master <-
      master %>%
      rename(IDind = Idind)
    final <- merge(master, jobs, by = c("IDind"), all.x = TRUE)
    
    # 3) Merging the income
    income <- subset(income, select = -c(wave))
    final <- merge(final, income, by = c("IDind"), all.x = TRUE)
    
    # 4) Merging the education
    educ <- subset(educ, select = -c(WAVE))
    final <- merge(final, educ, by = c("IDind"), all.x = TRUE)    
    
    # 5) Merging the marriage
    marriage <- subset(marriage, select = -c(WAVE))
    final <- merge(final, marriage, by = c("IDind"), all.x = TRUE)    
    final <- rename(final, mar_status = A8)
    
    # 6) Merging household information
    hhinfo <- subset(hhinfo, select = -c(WAVE))
    final2 <- merge(final, hhinfo, by = c("hhid"), all.x = TRUE)    
    
    # 7) Merging children information
    children <- subset(children, select = -c(wave))
    children <- subset(children, select = -c(IDind))
    final1 <- merge(final, children, by = c("hhid"), all.x = TRUE)
    final2 <- final1[!duplicated(final1$IDind),]

### PART 12: export the final dataset
    final2 <- final2[!duplicated(final2$IDind),]
    write_dta(final2, file.path(finaldata,
                               "FinalData.dta"))    
    