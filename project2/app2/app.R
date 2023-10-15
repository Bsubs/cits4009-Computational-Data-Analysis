#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
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
      tabItem(tabName = "scaling", uiOutput("dynamicTitleScaling"), uiOutput("scalingHist")),
      tabItem(tabName = "selectvar", h2("Target Variable Selection content")),
      tabItem(tabName = "singlevar", h2("Single Variable Models content")),
      tabItem(tabName = "selectfeature", h2("Feature Selection content")),
      tabItem(tabName = "classify", h2("Classification Models content")),
      tabItem(tabName = "cluster", h2("Clustering content"))
    )
  )
)

server <- function(input, output, session) {
  # Getting variables
  
  # Reactive expression for dynamic title scaling
  dynamicHeadingScaling <- reactive({
    paste("Feature Scaling -", input$fscale)
  })
  
  # Render UI for dynamic title
  output$dynamicTitleScaling <- renderUI({
    h2(dynamicHeadingScaling())
  })
  
  output$scalingHist <- renderUI({
    if(input$fscale == "No Scaling") {
      h1("NO SCALING")
    }
  })
  
}

shinyApp(ui, server)
