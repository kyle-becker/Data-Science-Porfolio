#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
Boston <- read.csv("Data/housing.csv")

shinyUI(fluidPage(
  titlePanel("Linear Regression Application"),
  sidebarLayout(
    sidebarPanel(
      h3("Regression Output Controls"),
      textInput("Data_Location", "Enter Location of Data File:", value = "Data/housing.csv"),
      actionButton("Import_Data", "Import Data Set"),
      selectInput("Default_Data", "Use Default Data", c("Yes"="Yes", "No"="No")),
      uiOutput("Predictors"),
      uiOutput("Output_Variable"),
      h3("Correlation Comparison Controls"),
      selectInput("Transformation", "Select variable transformation", c("None"="None", "Square Root"="Square Root", "Log"="Log", "Power"="Power")),
      uiOutput("Y_Variable_Correlation"),
      h3("Predicted Test Data Controls"),
      textInput("Test_Data", "Enter Location of Testing Data:", value = "Data/housing.csv"),
      actionButton("Import_Test_Data", "Import Test Data Set"),
      sliderInput("Predicted_Values", "Enter the amount of predicted values you want to see", min = 1, max = 1000, value = c(1,10))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        
      tabPanel("Regression Output",  titlePanel("Regression Output"),
      verbatimTextOutput(outputId = "Summary_coeff"),
      verbatimTextOutput(outputId = "Read_Data")),
        tabPanel("Correlation Comparison",titlePanel("Correlation Comparison"),
        plotOutput("Scatter"),
        verbatimTextOutput(outputId = "R_Squared")),
          tabPanel("Ridge Regression Output",titlePanel("Ridge Regression Output"),
          plotOutput("PLOT_LR"),
          verbatimTextOutput(outputId = "RIDGE")),
            tabPanel("Predicted Test Data",titlePanel("Prediction Using Test Data"),
            verbatimTextOutput(outputId = "PREDICT"),
            verbatimTextOutput(outputId = "Predicted_Values"))
      )
      
      
    )
  )
))
