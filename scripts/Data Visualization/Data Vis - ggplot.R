#------------------------------------------------------------------------------ #
#                                                                               #
#                          PPOL 670 - Final Project                             #
#                       Analysis: Data visualization                            #
#                                                                               #
#                                                                               #
#------------------------------------------------------------------------------ #
# PURPOSE:    Set-up configurations and run scripts that are used to analyze data 
#             for ppol670 final project
# NOTES:      The data is from China Nutrition and Health Survey

# WRITTEN BY: Qianying Liu

### PART 1: Setup work directory ------------------------------------------------

    projectFolder  <- file.path("/Users/apple/Documents/GitHub/PPOL670---LSX")
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
                  "reshape",
                  "ggplot2")


    sapply(packages, function(x) {
      print(x)
      if (x %in% installed.packages() == FALSE) {
        install.packages(x, dependencies = TRUE) 
        }
      library(x, 
              character.only = T)
      }
      )
    
### PART 3: Load data -------------------------------------------------------
    
    data <- read_dta(file.path(finaldata,
                           "FinalData.dta"))

### PART 4: Visualization 1. Income by gender and education level ---------------

    vis1 = subset(data, educ_degree > 0)
    vis1 = subset(vis1, educ_degree < 9)
    means <- aggregate(indinc_cpi ~ educ_degree + GENDER, vis1, mean)
    a=round(means, digits = 2)

    a$GENDER[a$GENDER == 1] <- " Male"
    a$GENDER[a$GENDER == 2] <- " Female"
    
    a$educ_degree[a$educ_degree == 1] <- "Primary School"
    a$educ_degree[a$educ_degree == 2] <- "Middle School"
    a$educ_degree[a$educ_degree == 3] <- "High School"
    a$educ_degree[a$educ_degree == 4] <- "Vocational Degree"
    a$educ_degree[a$educ_degree == 5] <- "College Degree"
    a$educ_degree[a$educ_degree == 6] <- "Master or Higher"
    
    edu_level = c("Primary School", "Middle School", "High School", "Vocational Degree", "College Degree", "Master or Higher")
    
    a$educ_degree <- factor(a$educ_degree, edu_level)
    
    levels(a$educ_degree)
    
    Vis1 <-
    ggplot(data = a, 
           mapping = aes(x = educ_degree, y = indinc_cpi, fill = GENDER)) + 
      geom_bar(stat = 'identity', position = 'dodge') + 
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + 
      scale_fill_manual(values = c("navajowhite", "lightskyblue3")) +
      theme(panel.background = element_blank()) +
      labs(title = "Average Income by Gender and Education Level",
           subtitle = "On average, male has higher income than female with the same education level from primary school degree.\nOnly for people with master degree or higher, female has higher income than male.",
           caption = "Data Source: China Nritution and Health Survey",
            x = "Education Levels",
            y = "Average Annual Income") +
      theme(plot.title = element_text(size = 16, hjust = 0))
   
### PART 5: Visualization 2. Percentage of working people by gender and education level -------------------

    vis2 = subset(data, educ_degree > 0)
    vis2 = subset(vis2, educ_degree < 9)
    vis3 = subset(vis2, working == 1)

    b<-table(vis3$GENDER,vis3$educ_degree)
    c<-table(vis2$GENDER,vis2$educ_degree)

    b<-data.frame(b/c)
    names(b)<-c("GENDER","educ_degree","Freq")
    b$GENDER<-as.character(b$GENDER)
    b$educ_degree<-as.character(b$educ_degree)

    b$GENDER[b$GENDER == 1] <- " Male"
    b$GENDER[b$GENDER == 2] <- " Female"
    
    b$educ_degree[b$educ_degree == 1] <- "Primary School"
    b$educ_degree[b$educ_degree == 2] <- "Middle School"
    b$educ_degree[b$educ_degree == 3] <- "High School"
    b$educ_degree[b$educ_degree == 4] <- "Vocational Degree"
    b$educ_degree[b$educ_degree == 5] <- "College Degree"
    b$educ_degree[b$educ_degree == 6] <- "Master or Higher"

    edu_level = c("Primary School", "Middle School", "High School", "Vocational Degree", "College Degree", "Master or Higher")
    b$educ_degree <- factor(b$educ_degree, edu_level)
    levels(b$educ_degree)

    Vis2 <-
    ggplot(data = b, mapping = aes(x = educ_degree, y = Freq * 100 , fill = GENDER)) + 
      geom_bar(stat = 'identity', position = 'dodge') + 
      theme(plot.margin = unit(c(1, 1, 1,  1), "cm")) + 
      scale_fill_manual(values = c("navajowhite", "lightskyblue3")) +
      theme(panel.background = element_blank()) +
      labs(title = "Percentage of working people by Gender and Education Level(%)",
           subtitle = "As a whole, male has higher employment rate than female with all the education level\nfrom primary to master degree.",
           caption = "Data Source: China Nritution and Health Survey",
           x = "Education Levels",
           y = "Percentage of Working People (%)") +
      theme(plot.title = element_text(size = 16, hjust = 0))

### PART 6: Visualization 3 - Average Income by Gender and Marital Status --------------------------------
    
    vis4 = subset(data, mar_status < 5)
    means2 <- aggregate(indinc_cpi ~ mar_status + GENDER, vis4, mean)
    c=round(means2, digits = 2)
    
    c$GENDER[c$GENDER == 1] <- " Male"
    c$GENDER[c$GENDER == 2] <- " Female"
    
    c$mar_status[c$mar_status == 1] <- "Never Married"
    c$mar_status[c$mar_status == 2] <- "Married"
    c$mar_status[c$mar_status == 3] <- "Divorced"
    c$mar_status[c$mar_status == 4] <- "Widowed"
    c$mar_status[c$mar_status == 5] <- "Seperated"
    
    marstatus = c("Never Married", "Married", "Divorced", "Widowed", "Seperated")
    c$mar_status <- factor(c$mar_status,marstatus)
    levels(c$mar_status)
      
    Vis3 <-
    ggplot(data = c, mapping = aes(x = GENDER, y = indinc_cpi, fill = mar_status)) + 
      geom_bar(stat = 'identity', position = 'dodge') + coord_flip()+
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + 
      scale_fill_manual(values = c("navajowhite", "lightblue1", "lightgray", "tan2")) +
      theme(panel.background = element_blank())+
      labs(title = "Average Income by Gender and Marital Status",
           subtitle = "Male has higher average income than female with all kinds of Marital Status, except\nwidowed. In general, people whoare married earn more than people who are never\nmarried. The difference between married men and never married men is bigger\nthan the difference between married women and never married women.",
           caption = "Data Source: China Nritution and Health Survey",
           x = "Gender",
           y = "Average Income",
           fill = "Marriage Status") +
      theme(plot.title = element_text(size = 16, hjust = 0))
  