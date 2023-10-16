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
library('grDevices')


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

# Function to apply PCA transformation on new datasets
transform_data <- function(data, pca, num_components) {
  centered_data <- scale(data, center = pca$center, scale = pca$scale)
  pc_scores <- centered_data %*% pca$rotation[, 1:num_components]
  return(as.data.frame(pc_scores))
}

# This function calculates the accuracy, precision, recall and F1 score for a model
cal_scores <- function(train_data, train_pred, calibration_data, calibration_pred){
  
  # Calculate metrics for the training data
  train_results <- tibble(
    Accuracy  = accuracy_vec(train_data$average_yearly_earnings.binary, train_pred$.pred_class),
    Precision = precision_vec(train_data$average_yearly_earnings.binary, train_pred$.pred_class),
    Recall    = recall_vec(train_data$average_yearly_earnings.binary, train_pred$.pred_class),
    F1        = f_meas_vec(train_data$average_yearly_earnings.binary, train_pred$.pred_class)
  )
  
  # Calculate metrics for the calibration data
  calibration_results <- tibble(
    Accuracy  = accuracy_vec(calibration_data$average_yearly_earnings.binary, calibration_pred$.pred_class),
    Precision = precision_vec(calibration_data$average_yearly_earnings.binary, calibration_pred$.pred_class),
    Recall    = recall_vec(calibration_data$average_yearly_earnings.binary, calibration_pred$.pred_class),
    F1        = f_meas_vec(calibration_data$average_yearly_earnings.binary, calibration_pred$.pred_class)
  )
  
  list(train = train_results, calibration = calibration_results)
}

# This function plots precision and recall vs threshold
plot_precision_recall_vs_threshold <- function(pred, data, title) {
  # Predict the probabilities
  probs <-pred$.pred_1
  
  # Create a data frame with true labels and predicted probabilities
  results <- data.frame(
    True = data$average_yearly_earnings.binary,
    Prob = probs
  )
  
  # For each threshold, compute precision and recall
  thresholds <- seq(0, 1, by = 0.01)
  metrics <- sapply(thresholds, function(thresh) {
    predictions <- ifelse(results$Prob > thresh, 1, 0)
    # Ensure predictions always have levels 0 and 1
    predictions <- factor(predictions, levels = c(0, 1))
    
    precision <- precision_vec(results$True, predictions)
    recall <- recall_vec(results$True, predictions)
    c(Precision = precision, Recall = recall)
  })
  
  # Transform results for plotting
  df <- as.data.frame(t(metrics))
  df$Threshold <- thresholds
  df$Difference <- abs(df$Precision - df$Recall)
  intersection_point <- df[which.min(df$Difference), "Threshold"]
  
  p <- ggplot(df, aes(x = Threshold)) +
    geom_line(aes(y = Precision, color = 'Precision')) +
    geom_line(aes(y = Recall, color = 'Recall')) +
    labs(title = title, 
         y = "Value", 
         x = "Threshold",
         color = "Metric") +
    geom_vline(aes(xintercept = 0.5), linetype = "dashed", color = "black", size = 0.5) +
    geom_text(aes(x = 0.5, y = 0.2, label = "Default threshold"), angle = 90, vjust = -0.5) +
    geom_vline(aes(xintercept = intersection_point), linetype = "dashed", color = "red", size = 0.5) +
    geom_text(aes(x = intersection_point, y = 0.2, 
                  label = paste0("Intersection ", round(intersection_point, 2))), angle = 90, vjust = -0.5) +
    theme_minimal()
  
  return(p)
}

# This function plots the ROC 
plot_roc <- function(predictions, data, title) {
  # Predict the probabilities
  probs <- predictions$.pred_1
  pred <- prediction(probs, data$average_yearly_earnings.binary)
  perf <- performance(pred, "tpr", "fpr")
  roc_data <- data.frame(
    FPR = unlist(perf@x.values),
    TPR = unlist(perf@y.values)
  )
  auc <- performance(pred, measure = "auc")
  auc_value <- unlist(auc@y.values) 
  ggplot(roc_data, aes(x = FPR, y = TPR)) +
    geom_line(color = "blue") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    geom_text(aes(x = 0.5, y = 0.25), 
              label = paste("AUC =", round(auc_value, 2)), 
              color = "red") +
    labs(title = title,
         x = "False Positive Rate",
         y = "True Positive Rate") +
    coord_fixed(ratio = 1) +
    theme_minimal()
}

# Plot confusion matrix
plot_con_matrix <- function(pred, data, title){
  cm <- confusionMatrix(as.factor(pred[[1]]), data$average_yearly_earnings.binary)
  plt <- as.data.frame(cm$table)
  plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
  ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
    geom_tile() + geom_text(aes(label=Freq)) +
    scale_fill_gradient(low="white", high="#009194") +
    labs(title = title, x = "Reference",y = "Prediction") 
}

# Train a logistic regression model using glm and 10 fold cross validation
train_log_reg <- function(df) {
  trControl <- trainControl(method = "cv", number = 10)
  model <- train(
    average_yearly_earnings.binary ~ ., 
    data = df, 
    method = "glm", 
    family = "binomial", 
    trControl = trControl
  )
  return(model)
}

train_dt <- function(df) {
  trControl <- trainControl(method = "cv", number = 10)
  dt_forward_cv <- train(
    average_yearly_earnings.binary ~ ., 
    data = df, 
    method = "rpart", 
    trControl = trControl
  )
  return(dt_forward_cv)
}

find_convex_hull <- function(proj2Ddf, groups) {
  do.call(rbind,
          lapply(unique(groups),
                 FUN = function(c) {
                   f <- subset(proj2Ddf, cluster==c);
                   f[chull(f),]
                 }
          )
  )
}

# Function to return the squared Euclidean distance of two given points x and y
sqr_euDist <- function(x, y) {
  sum((x - y)^2)
}

# Function to calculate WSS of a cluster, represented as a n-by-d matrix
# (where n and d are the numbers of rows and columns of the matrix)
# which contains only points of the cluster.
wss <- function(clustermat) {
  c0 <- colMeans(clustermat)
  sum(apply( clustermat, 1, FUN=function(row) {sqr_euDist(row, c0)} ))
}

# Function to calculate the total WSS. Argument `scaled_df`: data frame
# with normalised numerical columns. Argument `labels`: vector containing
# the cluster ID (starting at 1) for each row of the data frame.
wss_total <- function(scaled_df, labels) {
  wss.sum <- 0
  k <- length(unique(labels))
  for (i in 1:k)
    wss.sum <- wss.sum + wss(subset(scaled_df, labels == i))
  wss.sum
}

# Function to calculate total sum of squared (TSS) distance of data
# points about the (global) mean. This is the same as WSS when the
# number of clusters (k) is 1.
tss <- function(scaled_df) {
  wss(scaled_df)
}

# Function to return the CH indices computed using hierarchical
# clustering (function `hclust`) or k-means clustering (`kmeans`)
# for a vector of k values ranging from 1 to kmax.
CH_index <- function(scaled_df, kmax, method="kmeans") {
  if (!(method %in% c("kmeans", "hclust")))
    stop("method must be one of c('kmeans', 'hclust')")
  npts <- nrow(scaled_df)
  wss.value <- numeric(kmax) # create a vector of numeric type
  # wss.value[1] stores the WSS value for k=1 (when all the
  # data points form 1 large cluster).
  wss.value[1] <- wss(scaled_df)
  
  if (method == "kmeans") {
    # kmeans
    for (k in 2:kmax) {
      clustering <- kmeans(scaled_df, k, nstart=10, iter.max=100)
      wss.value[k] <- clustering$tot.withinss
    }
  } else {
    # hclust
    d <- dist(scaled_df, method="euclidean")
    pfit <- hclust(d, method="ward.D2")
    for (k in 2:kmax) {
      labels <- cutree(pfit, k=k)
      wss.value[k] <- wss_total(scaled_df, labels)
    }
  }
  bss.value <- tss(scaled_df) - wss.value # this is a vector
  B <- bss.value / (0:(kmax-1)) # also a vector
  W <- wss.value / (npts - 1:kmax) # also a vector
  data.frame(k = 1:kmax, CH_index = B/W, WSS = wss.value)
}


ui <- dashboardPage(
  dashboardHeader(title="Project 2 - Modelling"),
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
      convertMenuItem(menuItem("Clustering", tabName = "cluster", icon = icon("sitemap"),
                               textInput("nClu", "Number of Clusters:", "3")), "cluster")
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
      tabItem(tabName = "selectfeature", uiOutput("dynamicTitleFeatureSel"),
              fluidRow(
                conditionalPanel(condition="input.featureSelStrat == 'Forward Selection'",
                  box(title="Forward Selection Features:", status="primary", solidHeader = TRUE, width=12,
                      uiOutput("forwardSelResults")
                  )
                ),
                conditionalPanel(condition="input.featureSelStrat == 'PCA'",
                  box(title="PCA", status="primary", solidHeader=TRUE, width=12,
                      plotOutput("explainedVarPlot"))
                )),
              fluidRow(
                conditionalPanel(
                  condition="input.featureSelStrat == 'PCA'",
                  valueBoxOutput("numDimBox", width =6),
                  valueBoxOutput("explainedVarBox", width=6)
                )
              )),
      tabItem(tabName = "classify", uiOutput("dynamicTitleClass"),
              fluidRow(
                tabBox(
                  title="Metrics",
                  id="tabset1",
                  width=12,
                  tabPanel("Stats",
                           fluidRow(
                             valueBoxOutput("trainaccuracyBox", width =6),
                             valueBoxOutput("testaccuracyBox", width=6)
                           ),
                           fluidRow(
                             valueBoxOutput("trainprecisionBox", width =6),
                             valueBoxOutput("testprecisionBox", width=6)
                           ),
                           fluidRow(
                             valueBoxOutput("trainrecallBox", width =6),
                             valueBoxOutput("testrecallBox", width=6)
                           ),
                           fluidRow(
                             valueBoxOutput("trainf1Box", width =6),
                             valueBoxOutput("testf1Box", width=6)
                           )),
                  tabPanel("Precision & Recall vs Threshold", plotOutput("prvsThresh")),
                  tabPanel("ROC", plotOutput("plotROC")),
                  tabPanel("Confusion Matrix", plotOutput("plotConMat"))
                )
              ),
      ),
      tabItem(tabName = "cluster", h2("Clustering"),
              tabBox(
                title="Clustering",
                id="tabset2",
                width=12,
                tabPanel("Dendrogram", plotOutput("dendrogram")),
                tabPanel("Clusters", plotOutput("clusters")),
                tabPanel("CH Index and WSS", plotOutput("chindex"))
              ))
    )
  )
)

server <- function(input, output, session) {
  # create reactive vals
  vals <- reactiveValues()
  
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
  
  dynamicHeadingSingleVar <- reactive({
    paste("Single Variable Model -", input$singleVarSelector)
  })
  
  # Render UI for dynamic title
  output$dynamicTitleSingleVar <- renderUI({
    h2(dynamicHeadingSingleVar())
  })
  
  dynamicHeadingFeatureSel <- reactive({
    paste("Feature Selection -", input$featureSelStrat)
  })
  
  # Render UI for dynamic title
  output$dynamicTitleFeatureSel <- renderUI({
    h2(dynamicHeadingFeatureSel())
  })
  
  dynamicHeadingClass <- reactive({
    paste("Classification Model -", input$classModel)
  })
  
  # Render UI for dynamic title
  output$dynamicTitleClass <- renderUI({
    h2(dynamicHeadingClass())
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
        youtube <- deal_with_zeroes(youtube)
        youtube_cleaned <- rbind(train_data, test_data)
        
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
              round(aucTrain, 3), "AUC Value", color = "purple", icon = icon("chart-line")
            )
          })
          
          output$devianceRedBox <- renderValueBox({
            pi <- paste('pred', input$singleVarSelector, sep='')
            devDrop <- 2*(logLikelihood(train_data[,pi], train_data[,outcome]==pos) - logNull)
            valueBox(
              round(devDrop,3), "Deviance Drop", color = "orange", icon = icon("angles-down")
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
            
            output$forwardSelResults <- renderUI({
              tagList(
                lapply(selected_features, function(feature) {
                  div(
                    h3(feature)
                  )
                })
              )
            })
          } else if(input$featureSelStrat == "PCA") {
            pca <- prcomp(train_data, center = TRUE, scale. = TRUE)
            explained_variance_ratio <- pca$sdev^2 / sum(pca$sdev^2)
            cumulative_variance <- cumsum(explained_variance_ratio)
            num_components <- as.numeric(input$nPCs)
            retained_variance <- cumulative_variance[num_components]
            
            explained_variance_data <- data.frame(
              Dimension = 1:length(explained_variance_ratio),
              ExplainedVariance = explained_variance_ratio,
              CumulativeVariance = cumulative_variance
            )
            
            train_pca <- as.data.frame(pca$x[, 1:num_components])
            test_pca <- transform_data(test_data, pca, num_components)
            
            train_pca$average_yearly_earnings.binary <- as.factor(train_data$average_yearly_earnings.binary)
            test_pca$average_yearly_earnings.binary <- as.factor(test_data$average_yearly_earnings.binary)
            
            output$explainedVarPlot <- renderPlot({
              p <- ggplot(explained_variance_data, aes(x = Dimension)) +
                geom_bar(aes(y = ExplainedVariance), stat = "identity", fill = "blue", alpha = 0.7) +
                geom_line(aes(y = CumulativeVariance), color = "red", size = 1.2) +
                geom_point(aes(y = CumulativeVariance), color = "red") +
                geom_vline(aes(xintercept = num_components), color = "black", linetype = "dashed", size = 1) +
                labs(
                  title = "Explained Variance as a Function of Dimensions",
                  x = "Number of Dimensions",
                  y = "Variance Explained",
                  caption = "Blue Bars = Individual Explained Variance\nRed Line = Cumulative Explained Variance"
                ) +
                scale_y_continuous(labels = scales::percent) +
                theme_minimal()
              
              print(p)
            })
            
            output$numDimBox <- renderValueBox({
              valueBox(
                num_components, "Number of Principal Components", color = "purple", icon = icon("hashtag")
              )
            })
            
            output$explainedVarBox <- renderValueBox({
              valueBox(
                round(retained_variance,4), "Retained Variance", color = "orange", icon = icon("signal")
              )
            })
          }
          
          # Time to do classification
          if(input$featureSelStrat == "Forward Selection") {
            train_d <- train_forward_sel
            test_d <- test_forward_sel
            
          } else if(input$featureSelStrat == "PCA") {
            train_d <- train_pca
            test_d <- test_pca
          }
          
          if(input$classModel == "Logistic Regression") {
            log_reg <- train_log_reg(train_d)
            
            train_pred <- as_tibble(predict(log_reg, newdata = train_d, type = "prob")) %>%
              mutate(".pred_1" = `1`)
            
            test_pred <- as_tibble(predict(log_reg, newdata = test_d, type = "prob")) %>%
              mutate(".pred_1" = `1`)
            
            train_class <- as_tibble(ifelse(train_pred$.pred_1 > 0.5, 1, 0)) %>%
              mutate(".pred_class" = as.factor(value))
            
            test_class <- as_tibble(ifelse(test_pred$.pred_1 > 0.5, 1, 0)) %>%
              mutate(".pred_class" = as.factor(value))
          } else if(input$classModel == "Decision Tree") {
            dt <- train_dt(train_d)

            train_class <- as_tibble(predict(dt, newdata = train_d, type = "raw")) %>%
              rename(".pred_class" = value)
            test_class <- as_tibble(predict(dt, newdata = test_d, type = "raw")) %>%
              rename(".pred_class" = value)
            
            train_pred <- as_tibble(predict(dt, newdata = train_d, type = "prob")) %>%
              rename(".pred_0" = `0`, ".pred_1" = `1`)
            
            test_pred <- as_tibble(predict(dt, newdata = test_d, type = "prob")) %>%
              rename(".pred_0" = `0`, ".pred_1" = `1`)
          }
          
          performance <- cal_scores(train_d, train_class , test_d, test_class)
          
          output$trainaccuracyBox <- renderValueBox({
            valueBox(
              round(performance$train$Accuracy, 3), "Train Accuracy", color = "purple", icon = icon("dumbbell")
            )
          })
          
          output$testaccuracyBox <- renderValueBox({
            valueBox(
              round(performance$calibration$Accuracy, 3), "Test Accuracy", color = "orange", icon = icon("envelope-open-text")
            )
          })
          output$trainprecisionBox <- renderValueBox({
            valueBox(
              round(performance$train$Precision, 3), "Train Precision", color = "purple", icon = icon("dumbbell")
            )
          })
          
          output$testprecisionBox <- renderValueBox({
            valueBox(
              round(performance$calibration$Precision, 3), "Test Precision", color = "orange", icon = icon("envelope-open-text")
            )
          })
          output$trainrecallBox <- renderValueBox({
            valueBox(
              round(performance$train$Recall, 3), "Train Recall", color = "purple", icon = icon("dumbbell")
            )
          })
          
          output$testrecallBox <- renderValueBox({
            valueBox(
              round(performance$calibration$Recall, 3), "Test Recall", color = "orange", icon = icon("envelope-open-text")
            )
          })
          output$trainf1Box <- renderValueBox({
            valueBox(
              round(performance$train$F1, 3), "Train F1", color = "purple", icon = icon("dumbbell")
            )
          })
          
          output$testf1Box <- renderValueBox({
            valueBox(
              round(performance$calibration$F1, 3), "Test F1", color = "orange", icon = icon("envelope-open-text")
            )
          })
          
          output$prvsThresh <- renderPlot({
            p1 <- plot_precision_recall_vs_threshold(train_pred, train_d, "Precision and Recall vs. Threshold, Train Set")
            p2 <- plot_precision_recall_vs_threshold(test_pred, test_d, "Precision and Recall vs. Threshold, Test Set")
            grid.arrange(p1, p2, ncol=2)
          })
          
          output$plotROC <- renderPlot({
            p1 <- plot_roc(train_pred, train_d, "ROC Curve Train Set")
            p2 <- plot_roc(test_pred, test_d, "ROC Curve Test Set")
            grid.arrange(p1, p2, ncol=2)
          })
          
          output$plotConMat <- renderPlot({
            p1 <- plot_con_matrix(train_class, train_d, "Confusion Matrix Train Set")
            p2 <- plot_con_matrix(test_class, test_d, "Confusion Matrix Test Set")
            grid.arrange(p1, p2, ncol=2)
          })
          
          if(input$tabs == "cluster") {
            
            youtube_clustering <- subset(youtube, select=-c(average_yearly_earnings.binary, Country, channel_type, created_month))
            youtube_clustering <- transform_cols(youtube_clustering, colnames(youtube_clustering), "norm")
            youtube_clustering <- youtube_clustering[, grep("\\.norm$", colnames(youtube_clustering))]
            colnames(youtube_clustering) <- sub("\\.norm$", "", colnames(youtube_clustering))
            
            numClusters = as.numeric(input$nClu)
            
            d <- dist(youtube_clustering, method="manhattan")
            pfit <- hclust(d, method="ward.D2")
            groups <- cutree(pfit, k=numClusters)
            
            output$dendrogram <- renderPlot({
              plot(pfit, main="Cluster Dendrogram for Youtube Channels", labels=FALSE)
              rect.hclust(pfit, k=numClusters) 
            })

            princ <- prcomp(youtube_clustering)
            nComp <- 2
            project2D <- as.data.frame(predict(princ, newdata=youtube_clustering)[,1:nComp])
            hclust.project2D <- cbind(project2D, cluster=as.factor(groups), country=names$Country)
            
            hclust.hull <- find_convex_hull(hclust.project2D, groups)
            
            output$clusters <- renderPlot({
              ggplot(hclust.project2D, aes(x=PC1, y=PC2)) +
                geom_point(aes(shape=cluster, color=cluster)) +
                geom_text(aes(label=country, color=cluster), hjust=0, vjust=1, size=3) +
                geom_polygon(data=hclust.hull, aes(group=cluster, fill=as.factor(cluster)),
                             alpha=0.4, linetype=0) + theme(text=element_text(size=8))
            })
            
            # calculate the CH criterion
            crit.df <- CH_index(youtube_clustering, 10, method="hclust")
            
            output$chindex <- renderPlot({
              fig1 <- ggplot(crit.df, aes(x=k, y=CH_index)) +
                geom_point() + geom_line(colour="red") +
                scale_x_continuous(breaks=1:10, labels=1:10) +
                labs(y="CH index") + theme(text=element_text(size=8))
              
              fig2 <- ggplot(crit.df, aes(x=k, y=WSS), color="blue") +
                geom_point() + geom_line(colour="blue") +
                scale_x_continuous(breaks=1:10, labels=1:10) +
                theme(text=element_text(size=8))
              
              grid.arrange(fig1, fig2, nrow=1)
            })
          }
          
        }
      }
    }
    
  })

}

shinyApp(ui, server)














