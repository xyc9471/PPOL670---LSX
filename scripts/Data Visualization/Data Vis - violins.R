#------------------------------------------------------------------------------ #
#                                                                               #
#                          PPOL 670 - Final Project                             #
#                             Shiny Visualization                               #
#                                                                               #
#                                                                               #
#------------------------------------------------------------------------------ #
# PURPOSE:    Set-up configurations and run scripts that are used to analyze data 
#             for ppol670 final project
# NOTES:      The data is from China Nutrition and Health Survey

# WRITTEN BY: Terry Sang

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
                  "shiny",
                  "sf")
    
    
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
    
    dat <- read_dta(file.path(finaldata,
                               "FinalData.dta"))
    
### PART 4: Pre-processing Data ---------------------------------------------
    # 1) Subset the data
    vis1 = subset(dat, educ_degree > 0)
    vis1 = subset(vis1, educ_degree < 9)
    
    # 2) Drop NA values
    dat1 <- na.omit(dat)
    dat1
    
    # 3) Take the natural log of income
    dat2 <- dat1 %>% 
      mutate(indinc_cpi = log(indinc_cpi + 417286.33333))
    dat2
    
    # 4) Convert variables to factors
    dat2$GENDER <- as.factor(dat2$GENDER)
    dat2$educ_degree <- as.factor(dat2$educ_degree)
    dat2$area <- as.factor(dat2$area)
    dat2$kids <- as.factor(dat2$kids)
    dat2
    str(dat2)
    
    # 5) Create labels
    labels <- c("1" = "Male", "2" = "Female")
    
### PART 5: Visualizing the Data Using Shiny ---------------------------------------
    
    Boxplot4 <- 
      ggplot(dat2, aes(y=indinc_cpi, x=area, color = area)) +
      geom_violin() + 
      facet_wrap(~GENDER, labeller = labeller(GENDER = labels)) +
      labs(title = "Income by Area Faceted by Gender", 
           subtitle = "We see a drastic increase in income levels for males in Urban against Rural; however, this increase is not early as significant for females", 
           caption = "Data Source: China Nutrition and Health Survey") +
      theme(plot.title = element_text(hjust=0), 
            plot.subtitle = element_text(hjust=0)) +
      xlab("Area") +
      ylab("Logged Individual Income") +
      scale_x_discrete(labels = c('rural', 'urban')) +
      scale_color_manual(values = c("red", "blue"), 
                         labels = c('Rural','Urban'))
    print(Boxplot4)
    
    
    Boxplot5 <- 
      ggplot(dat2, aes(y=indinc_cpi, x=kids, color = kids)) + 
      geom_violin() +    
      facet_wrap(~GENDER, labeller = labeller(GENDER = labels)) +
      labs(title = "Income of People Who have Children and Who Do Not", 
           subtitle = "Plot is faceted by gender to illustrate the drastic decrease in womens' income and the overall employment rate after childbirth", 
           caption = "Data Source: China Nutrition and Health Survey") +
      theme(plot.title = element_text(hjust=0),
            plot.subtitle = element_text(hjust=0)) +
      xlab("Whether a Person Has Children") +
      ylab("Logged Individual Income") +
      labs(color = "Children") +
      scale_x_discrete(labels = c('No', 'Yes')) +
      scale_color_manual(values = c("red", "blue"),
                         labels = c('No','Yes'))
    
    print(Boxplot5)
    