#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(knitr)
library(tidyverse)
library(ggrepel)
library(vtreat)
library(corrplot)
library(tidyr)
library(psych)
library(GGally) 
library(stringr)
library(reshape2)
library(caret)
library(ROCR)
library(MASS)
library(FactoMineR)
library(tidymodels)
library(glmnet)
library(parsnip)
library(yardstick)
library(rpart)
library(rpart.plot)
library(fpc)

# Workaround for shiny dashboard not properly supporting input fields in menu items
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

# Read in data
path <- './data/youtube_UTF_8.csv'
youtube <- read.csv(path)


# Initial data cleaning
youtube <- subset(youtube, select=-c(`Urban_population`, `country_rank`, `channel_type_rank`))

youtube <- youtube %>%
  mutate(video.views = na_if(video.views, 0),
         uploads = na_if(uploads, 0)) %>%
  mutate(created_year = ifelse(created_year < 2005, NA, created_year)) %>%
  mutate_all(~ ifelse(. %in% c("nan"), NA, .)) 

youtube <- youtube[!(is.na(youtube$video.views) | is.na(youtube$uploads)), ]

youtube <- youtube %>%
  mutate(category = ifelse(is.na(category), "missing", category),
         Country = ifelse(is.na(Country), "missing", Country),
         Abbreviation = ifelse(is.na(Abbreviation), "missing", Abbreviation),
         channel_type = ifelse(is.na(channel_type), "missing", channel_type),
         created_month = ifelse(is.na(created_month), "missing", created_month))

youtube <- subset(youtube, select = -c(category, Abbreviation))

names <- subset(youtube, select = c(rank, Youtuber, Title, Country, channel_type))

youtube <- subset(youtube, select = -c(rank, Youtuber, Title))


outcome <- "average_yearly_earnings.binary"
pos <- 1

# Data Transformation functions

minmax_scale <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# This function transforms the data 
# @param df the dataframe to transform
# @param features the features to transform
# @param trans the type of transform to apply
transform_cols <- function(df, features, trans) {
  if(trans == "log"){
    # Apply log transform
    df <- df %>%
      mutate(across(all_of(features), ~log(. + 1e-6), .names = "{col}.log"))
  } else if (trans == "minmax") {
    # Applying min-max scaling
    df <- df %>%
      mutate(across(all_of(features), minmax_scale, .names = "{col}.minmax"))
  } else if (trans == "norm") {
    # Applying z-normalization
    df <- df %>%
      mutate(across(all_of(features), ~as.vector(scale(.)), .names = "{col}.norm"))
  }
  
  return(df)
}

# This function plots a histogram for each numerical value in a given dataframe
# @param df dataframe to plot
# @param ncols number of columns in output plot
# @param var.type which selection of variables to plot
plot_hist <- function(df, ncols, var.type){
  
  numeric_cols <- sapply(df, is.numeric)  
  if(var.type == "log"){
    total_numeric_cols <- sum(sapply(df, is.numeric) & grepl("\\.log$", names(df)))
    nrows <- ceiling(total_numeric_cols/ncols) 
    par(mfrow=c(nrows,ncols), mar=c(2, 2, 1, 1))  
  } else if(var.type == "default") {
    total_numeric_cols <- sum(numeric_cols) 
    nrows <- ceiling(total_numeric_cols/ncols) 
    par(mfrow=c(nrows,ncols), mar=c(2, 2, 1, 1))  
  } else if(var.type == "minmax") {
    total_numeric_cols <- sum(sapply(df, is.numeric) & grepl("\\.minmax$", names(df)))
    nrows <- ceiling(total_numeric_cols/ncols) 
    par(mfrow=c(nrows,ncols), mar=c(2, 2, 1, 1))  
    par(mfrow=c(nrows,ncols), mar=c(2, 2, 1, 1))
  } else if(var.type == "norm") {
    total_numeric_cols <- sum(sapply(df, is.numeric) & grepl("\\.norm$", names(df)))
    nrows <- ceiling(total_numeric_cols/ncols) 
    par(mfrow=c(nrows,ncols), mar=c(2, 2, 1, 1))  
  }
  
  
  for(colname in names(df)[numeric_cols]) {
    if(var.type == "log" & grepl("\\.log$", colname)) {
      hist(df[[colname]], main=colname, xlab=NULL, col="lightblue", border="black", breaks=50)
    } else if(var.type == "default") {
      hist(df[[colname]], main=colname, xlab=NULL, col="lightblue", border="black", breaks=50)
    } else if (var.type == "minmax" & grepl("\\.minmax$", colname)) {
      hist(df[[colname]], main=colname, xlab=NULL, col="lightblue", border="black", breaks=50)
    } else if(var.type == "norm" & grepl("\\.norm$", colname)) {
      hist(df[[colname]], main=colname, xlab=NULL, col="lightblue", border="black", breaks=50)
    }
  }
  
  # Reset the plotting parameters to default
  par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)
}

plot_individual_hist <- function(df, feature, line_val) {
  mean_val <- mean(df[,feature], na.rm = TRUE)
  median_val <- median(df[,feature], na.rm = TRUE)
  
  p <- ggplot(df, aes(x = df[,feature])) +
    geom_histogram(fill = "lightgreen", alpha = 0.7, color="black") +
    xlab(feature) +
    ylab("Frequency") +
    theme_minimal() +
    geom_vline(aes(xintercept = line_val), color = "red", linetype = "dashed", size = 1)
  
  return(p)
}

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      
      # Feature scaling tab
      convertMenuItem(menuItem("Feature Scaling", icon = icon("dashboard"), tabName = "scaling",
               radioButtons("fscale", "Feature Scaling Strategy:",
                            choices = c("No Scaling", "Log Transform", "Min-Max Scaling", "Z-Normalization"),
                            selected = "No Scaling")), "scaling"),
      
      # Threshold selection tab
      convertMenuItem(menuItem("Target Variable Selection", icon = icon("gear"), tabName = "selectvar",
               radioButtons("tvarthresh", "Target Variable Threshold:",
                            choices = c("Median", "Mean", "Custom"),
                            selected = "Median"),
               conditionalPanel(condition ="input.tvarthresh == 'Custom'",
                                textInput("customThresh", NULL, ""))), "selectvar"),
      
      # Single Var Model tab
      convertMenuItem(menuItem("Single Variable Models", icon = icon("cube"), tabName="singlevar",
               selectInput("singleVarSelector", "Select Variable:",
                           choices = c("Boxplot", "Density Plot", "Histogram"))), "singlevar"),
      
      # Feature Selection Strategy
      convertMenuItem(menuItem("Feature Selection", tabName = "selectfeature", icon = icon("filter"),
               radioButtons("featureSelStrat", "Feature Selection Strategy:",
                            choices = c("Forward Selection", "PCA"),
                            selected = "Forward Selection"),
               conditionalPanel(condition = "input.featureSelStrat == 'PCA'",
                                textInput("nPCs", NULL, ""))), "selectfeature"),
      
      # Classification tab
      convertMenuItem(menuItem("Classification Models", tabName = "classify", icon = icon("diagram-project"),
               radioButtons("classModel", "Classification Model:",
                            choices = c("Logistic Regression", "Decision Tree"),
                            selected = "Logistic Regression")), "classify"),
      
      # Clustering tab
      convertMenuItem(menuItem("Clustering", tabName = "cluster", icon = icon("sitemap")), "clustering")
    ),
    textOutput("res")
  ),
  dashboardBody(
    tabItems(
      # Placeholder content for each tab
      tabItem(tabName = "scaling", uiOutput("dynamicTitleScaling"), plotOutput("scalingHist")),
      tabItem(tabName = "selectvar", uiOutput("dynamicTitleVarSelection"),
              fluidRow(box(
                title = "Histogram", status="primary",solidHeader = TRUE, width=12,
                plotOutput("targetVarPlot")
              )),
              fluidRow(
                valueBoxOutput("threshValueBox"),
                valueBoxOutput("positiveclassBox"),
                valueBoxOutput("negativeclassBox")
              )),
      tabItem(tabName = "singlevar", h2("Single Var")),
      tabItem(tabName = "selectfeature", h2("Feature Selection content")),
      tabItem(tabName = "classify", h2("Classification Models content")),
      tabItem(tabName = "cluster", h2("Clustering content"))
    )
  )
)

server <- function(input, output, session) {
  # create reactive vals
  vals <- reactiveValues(
    youtube=data.frame()
  )
  vals$youtube <- youtube
  
  
  # Reactive expression for dynamic title scaling
  dynamicHeadingScaling <- reactive({
    paste("Feature Scaling -", input$fscale)
  })
  
  # Render UI for dynamic title
  output$dynamicTitleScaling <- renderUI({
    h2(dynamicHeadingScaling())
  })
  
  dynamicHeadingVarSelection <- reactive({
    paste("Variable Selection -", input$tvarthresh)
  })
  
  # Render UI for dynamic title
  output$dynamicTitleVarSelection <- renderUI({
    h2(dynamicHeadingVarSelection())
  })
  
  output$performCalcs <- reactive({})
  
  # Outputs feature scaling histograms
  output$scalingHist <- renderPlot({
    
    current_data <- vals$youtube
    print(colnames(vals$youtube))
    
    if(input$fscale == "No Scaling") {
      plot_hist(current_data, 3, "default")
    } else if (input$fscale == "Log Transform") {
      current_data <- transform_cols(current_data, colnames(current_data)[sapply(current_data, is.numeric)],"log")
      plot_hist(current_data, 3, "log")
    } else if (input$fscale == "Min-Max Scaling") {
      current_data <- transform_cols(current_data, colnames(current_data)[sapply(current_data, is.numeric)],"minmax")
      plot_hist(current_data, 3, "minmax")
    } else if (input$fscale == "Z-Normalization") {
      current_data <- transform_cols(current_data, colnames(current_data)[sapply(current_data, is.numeric)],"norm")
      plot_hist(current_data, 3, "norm")
    }
    
    vals$youtube <- current_data
  })
  

  # Outputs target variable selection histogram
  output$targetVarPlot <- renderPlot({
    
    current_data <- vals$youtube
    print(colnames(current_data))
    
    # Get the new target variable
    youtube <- youtube %>%
      mutate(average_yearly_earnings = (lowest_yearly_earnings + highest_yearly_earnings) / 2)
    # Scale the new target variable
    if (input$fscale != "No Scaling") {
      youtube <- transform_cols(youtube, "average_yearly_earnings", switch(input$fscale,
                                                                           "Log Transform" = "log",
                                                                           "Min-Max Scaling" = "minmax",
                                                                           "Z-Normalization" = "norm"
      ))
    }
    # Calculate lineval
    feature <-  switch(input$fscale,
                       "No Scaling" = "average_yearly_earnings",
                       "Log Transform" = "average_yearly_earnings.log",
                       "Min-Max Scaling" = "average_yearly_earnings.minmax",
                       "Z-Normalization" = "average_yearly_earnings.norm") 
  
    mean_val <- mean(youtube[,feature], na.rm = TRUE)
    median_val <- median(youtube[,feature], na.rm = TRUE)
    custom_val <- as.numeric(input$customThresh)
    
    vals$thresh_val <- switch(input$tvarthresh,
                         "Median" = median_val,
                         "Mean" = mean_val,
                         "Custom" = custom_val)
    
    youtube$average_yearly_earnings.binary <- ifelse(youtube[[feature]] > vals$thresh_val, 1, 0)
    vals$zeroclass <- table(ifelse(youtube[[feature]] > vals$thresh_val, 1, 0))["0"]
    vals$oneclass <- table(ifelse(youtube[[feature]] > vals$thresh_val, 1, 0))["1"]
    

    
    # Plot the histogram
    p <- plot_individual_hist(youtube, feature, vals$thresh_val)
    
    print(p)
  })
  
  # Value box for targe var selection
  output$threshValueBox <- renderValueBox({
    valueBox(
      round(vals$thresh_val, 2), "Threshold Value", color = "purple", icon = icon("grip-lines-vertical")
    )
  })
  
  output$positiveclassBox <- renderValueBox({
    valueBox(
      vals$oneclass, "Positive Class", color = "green", icon = icon("thumbs-up")
    )
  })
  
  output$negativeclassBox <- renderValueBox({
    valueBox(
      vals$zeroclass, "Negative Class", color = "red", icon = icon("thumbs-down")
    )
  })
}

shinyApp(ui, server)

















