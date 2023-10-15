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

# This function splits a dataframe into training and test sets given a particular ratio
# @param df the dataframe to operate on
# @param feature the feature to use as the vector of outcomes
# @param train_ratio the split to use
split_data <- function(df, feature, train_ratio = 0.9) {
  train_indices <- createDataPartition(df[,feature],  p= train_ratio, list = FALSE)
  
  # Split the data into training and test sets
  train_set <- df[train_indices, ]
  test_set <- df[-train_indices, ]
  
  # Return a list containing the training and test sets
  list(train = train_set, test = test_set)
}

deal_with_zeroes <- function(df){
  if(any(is.na(df))) {
    df[is.na(df)] <- 1e-6  # this is sketch af FIX THIS ASAP
  }
  cols_to_modify <- setdiff(names(df), "average_yearly_earnings.binary")
  
  df[cols_to_modify] <- lapply(df[cols_to_modify], function(x) ifelse(x == 0, 1e-6, x))
  return(df)
}

# This function seperates a dataframe into categorical and numeric variables
# @param df the dataframe to operate on
separate_vars <- function(df) {
  numeric_var_names <- names(df)[sapply(df, is.numeric)]
  factor_var_names <- names(df)[sapply(df, is.character)]
  
  numVars <- subset(df, select=numeric_var_names)
  
  catVars <- subset(df, select=factor_var_names)
  
  return(list(numVars = numVars, catVars = catVars))
}

mkPredC <- function(outCol, varCol, appCol) {
  pPos <- sum(outCol==pos)/length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol), varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  pred
}

# for numerical variables, we convert them into categorical one and
# call the `mkPredC` function above.
mkPredN <- function(outCol, varCol, appCol) {
  cuts <- unique(as.numeric(
    quantile(varCol, probs=seq(0, 1, 0.1), na.rm=T)))
  varC <- cut(varCol, cuts)
  appC <- cut(appCol, cuts)
  mkPredC(outCol, varC, appC)
}

calcAUC <- function(predcol, outcol) {
  perf <- performance(prediction(predcol, outcol==pos),'auc')
  as.numeric(perf@y.values)
}

logLikelihood <- function(ypred, ytrue) {
  sum(ifelse(ytrue, log(ypred), log(1-ypred)), na.rm=T)
}

# This function performs feature selection using forward selection
# @param df The dataframe that contains the features
# @param response The response variable
forward_selection <- function(df, response) {
  # Get names of variables minus response variable
  predictors <- setdiff(names(df), response)
  # What features we select
  included <- c()
  best_aic <- Inf
  
  # Loop to iteratively add predictors
  while(length(predictors) > 0) {
    aics <- rep(Inf, length(predictors))
    
    # Test each predictor
    for(j in seq_along(predictors)) {
      formula <- paste(response, "~", paste(c(included, predictors[j]), collapse = "+"))
      model <- lm(formula, data = df)
      aics[j] <- AIC(model)
    }
    
    # If we find a predictor that reduces the AIC, we include it
    if(min(aics) < best_aic) {
      best_aic <- min(aics)
      include <- predictors[which.min(aics)]
      included <- c(included, include)
      predictors <- setdiff(predictors, include)
    } else {
      break
    }
  }
  
  return(included)
}

clean_data <- function(df){
  df1 <- subset(df, select=-c(Country, channel_type, created_month))
  df1 <- df1 %>%
    rename(
      Country = predCountry,
      channel_type = predchannel_type,
      created_month = predcreated_month
    )
  
  cols_to_keep <- !grepl("^pred", names(df))
  df1 <- df1[, cols_to_keep]
  return(df1)
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
                           choices = NULL)), "singlevar"),
      
      # Feature Selection Strategy
      convertMenuItem(menuItem("Feature Selection", tabName = "selectfeature", icon = icon("filter"),
               radioButtons("featureSelStrat", "Feature Selection Strategy:",
                            choices = c("Forward Selection", "PCA"),
                            selected = "Forward Selection"),
               conditionalPanel(condition = "input.featureSelStrat == 'PCA'",
                                textInput("nPCs", NULL, "10"))), "selectfeature"),
      
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
      tabItem(tabName = "singlevar", uiOutput("dynamicTitleSingleVar"),
              fluidRow(box(
                title = "Double Density Plot", status="primary",solidHeader = TRUE, width=12,
                plotOutput("doubleDenPlot")
              )),
              fluidRow(
                valueBoxOutput("aucBox", width =6),
                valueBoxOutput("devianceRedBox", width=6)
              )),
      tabItem(tabName = "selectfeature", h2("Feature Selection content")),
      tabItem(tabName = "classify", h2("Classification Models content")),
      tabItem(tabName = "cluster", h2("Clustering content"))
    )
  )
)

server <- function(input, output, session) {
  # create reactive vals
  vals <- reactiveValues()
  
  # Reactive expression for dynamic title scaling
  dynamicHeadingScaling <- reactive({
    paste("Feature Scaling -", input$featurescaling)
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
  
  dynamicHeadingSingleVar <- reactive({
    paste("Single Variable Model -", input$singleVarSelector)
  })
  
  # Render UI for dynamic title
  output$dynamicTitleSingleVar <- renderUI({
    h2(dynamicHeadingSingleVar())
  })
  
  shiny::observe({
    
    if (input$fscale == "Log Transform") {
      youtube <- transform_cols(youtube, colnames(youtube)[sapply(youtube, is.numeric)],"log")
    } else if (input$fscale == "Min-Max Scaling") {
      youtube <- transform_cols(youtube, colnames(youtube)[sapply(youtube, is.numeric)],"minmax")
    } else if (input$fscale == "Z-Normalization") {
      youtube <- transform_cols(youtube, colnames(youtube)[sapply(youtube, is.numeric)],"norm")
    }
    
    if(input$tabs == "scaling") {
      print('TRUE')
      output$scalingHist <- renderPlot({
        if(input$fscale == "No Scaling") {
          plot_hist(youtube, 3, "default")
        } else if (input$fscale == "Log Transform") {
          plot_hist(youtube, 3, "log")
        } else if (input$fscale == "Min-Max Scaling") {
          plot_hist(youtube, 3, "minmax")
        } else if (input$fscale == "Z-Normalization") {
          plot_hist(youtube, 3, "norm")
        }
      })
    } else {
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
      
      if(input$tabs == "selectvar") {
        output$targetVarPlot <- renderPlot({
          p <- plot_individual_hist(youtube, feature, vals$thresh_val)
          print(p)
        })
        
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
        
      } else {
        # prepare youtube_scaled for single var models
        youtube_scaled <- youtube
        if(input$fscale == "No Scaling") {
          youtube_scaled <- bind_cols(youtube_scaled, youtube[, c("average_yearly_earnings.binary")])
          youtube_scaled <- subset(youtube_scaled, select=-c(...23, average_yearly_earnings, highest_yearly_earnings, lowest_yearly_earnings, highest_monthly_earnings, lowest_monthly_earnings, video_views_for_the_last_30_days, subscribers_for_last_30_days))
        } else if (input$fscale == "Log Transform") {
          youtube_scaled <- youtube[, grepl("\\.log$", names(youtube))]
          youtube_scaled <- bind_cols(youtube_scaled, youtube[, c("average_yearly_earnings.binary", "Country", "channel_type", "created_month")])
          youtube_scaled <- subset(youtube_scaled, select=-c(average_yearly_earnings.log, highest_yearly_earnings.log, lowest_yearly_earnings.log, highest_monthly_earnings.log, lowest_monthly_earnings.log, video_views_for_the_last_30_days.log, subscribers_for_last_30_days.log))
        } else if (input$fscale == "Min-Max Scaling") {
          youtube_scaled <- youtube[, grepl("\\.minmax$", names(youtube))]
          youtube_scaled <- bind_cols(youtube_scaled, youtube[, c("average_yearly_earnings.binary", "Country", "channel_type", "created_month")])
          youtube_scaled <- subset(youtube_scaled, select=-c(average_yearly_earnings.minmax, highest_yearly_earnings.minmax, lowest_yearly_earnings.minmax, highest_monthly_earnings.minmax, lowest_monthly_earnings.minmax, video_views_for_the_last_30_days.minmax, subscribers_for_last_30_days.minmax))
        } else if (input$fscale == "Z-Normalization") {
          youtube_scaled <- youtube[, grepl("\\.norm$", names(youtube))]
          youtube_scaled <- bind_cols(youtube_scaled, youtube[, c("average_yearly_earnings.binary", "Country", "channel_type", "created_month")])
          youtube_scaled <- subset(youtube_scaled, select=-c(average_yearly_earnings.norm, highest_yearly_earnings.norm, lowest_yearly_earnings.norm, highest_monthly_earnings.norm, lowest_monthly_earnings.norm, video_views_for_the_last_30_days.norm, subscribers_for_last_30_days.norm))
        }
        
        updateSelectInput(session, "singleVarSelector", choices = colnames(subset(youtube_scaled, select=-c(average_yearly_earnings.binary))))
        
        splits <- split_data(youtube_scaled, "average_yearly_earnings.binary", train_ratio = 0.9)
        train_data <- splits$train
        test_data <- splits$test
        
        
        youtube_scaled <- deal_with_zeroes(youtube_scaled)
        train_data <- deal_with_zeroes(train_data)
        test_data <- deal_with_zeroes(test_data)
        
        sep_train <- separate_vars(train_data)
        numVars <- sep_train$numVars
        catVars <- sep_train$catVars
        
        for (v in colnames(catVars)) {
          pi <- paste('pred', v, sep='')
          train_data[,pi] <- mkPredC(train_data[,outcome], train_data[,v], train_data[,v])
          test_data[,pi] <- mkPredC(test_data[,outcome], test_data[,v], test_data[,v])
        }
        
        # now go through all the numerical variables in the `numericVars` vector
        # and perform the predictions. Again, the outputs are stored back into
        # the data frame.
        for (v in colnames(numVars)) {
          pi <- paste('pred', v, sep='')
          train_data[,pi] <- mkPredN(train_data[,outcome], train_data[,v], train_data[,v])
          test_data[,pi] <- mkPredN(test_data[,outcome], test_data[,v], test_data[,v])
        }
        logNull <- logLikelihood(sum(train_data[,outcome]==pos)/nrow(train_data), train_data[,outcome]==pos)
        
        if(input$tabs == "singlevar") {
          output$doubleDenPlot <- renderPlot({
            p <- ggplot(data=train_data) +
              geom_density(aes(x=train_data[[input$singleVarSelector]],color=as.factor(average_yearly_earnings.binary)))
            print(p)
          })
          
          output$aucBox <- renderValueBox({
            pii <- paste('pred',input$singleVarSelector,sep='')
            aucTrain <- calcAUC(train_data[,pii],train_data[,outcome])
            valueBox(
              round(aucTrain, 3), "AUC Value", color = "purple", icon = icon("grip-lines-vertical")
            )
          })
          
          output$devianceRedBox <- renderValueBox({
            pi <- paste('pred', input$singleVarSelector, sep='')
            devDrop <- 2*(logLikelihood(train_data[,pi], train_data[,outcome]==pos) - logNull)
            valueBox(
              round(devDrop,3), "Deviance Drop", color = "green", icon = icon("thumbs-up")
            )
          })
        } else {
          train_data <- clean_data(train_data)
          test_data <- clean_data(test_data)
          
          if(input$featureSelStrat == "Forward Selection") {
            selected_features <- forward_selection(train_data, "average_yearly_earnings.binary")
            train_forward_sel <- subset(train_data, select=c(selected_features))
            test_forward_sel <- subset(test_data, select=c(selected_features))
            
            train_forward_sel$average_yearly_earnings.binary <- as.factor(train_data$average_yearly_earnings.binary)
            test_forward_sel$average_yearly_earnings.binary <- as.factor(test_data$average_yearly_earnings.binary)
            
          }
        }
      }
    }
    
  })

}

shinyApp(ui, server)














