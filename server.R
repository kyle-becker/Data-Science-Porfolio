library(shiny)
library(ggplot2)
library(glmnet)
library(caret)


shinyServer(function(input, output) {
    Boston <- read.csv("Data/housing.csv")
    Read_Data <- eventReactive(input$Import_Data, {read.csv(input$Data_Location)})
    Read_Test_Data <- eventReactive(input$Import_Test_Data, {read.csv(input$Test_Data)})
  
  
    LM_Summary <- reactive({ 
    ifelse(input$Default_Data=="Yes", Data <- Boston, Data <- Read_Data())
    
    Linear_Model <- lm(as.formula(paste0(input$Output_Variable,"~", paste( subset(input$Predictors, !(input$Predictors==input$Output_Variable)) , collapse = "+"))), data=Data)
    summary(Linear_Model)
    
    
  })

    Scatter <- reactive({
    ifelse(input$Default_Data=="Yes", Data <- Boston, Data <- Read_Data())
    Corr <- lm(as.formula(paste0(input$Y_Variable_Correlation,"~",input$Output_Variable)), data = Data)
    Corr <- summary(Corr)
    R_Squared <- Corr$r.squared
    
    ifelse(input$Transformation=="Log", 
    P <- ggplot(data = Data, aes(log(Data[[input$Y_Variable_Correlation]]), Data[[input$Output_Variable]])),
            
            ifelse(input$Transformation=="Square Root", P <- ggplot(data = Data, aes(sqrt(Data[[input$Y_Variable_Correlation]]), Data[[input$Output_Variable]])),
                   
                   ifelse(input$Transformation=="Power", P <- ggplot(data = Data, aes(Data[[input$Y_Variable_Correlation]]^2, Data[[input$Output_Variable]])),
                          
                          P <- ggplot(data = Data, aes(Data[[input$Y_Variable_Correlation]], Data[[input$Output_Variable]])))))
    
    
    
    P + geom_point() + xlab(input$Y_Variable_Correlation) + ylab(input$Output_Variable) + ggtitle(paste(input$Y_Variable_Correlation, "Vs.", input$Output_Variable)) + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method = "lm") + geom_text(x=mean(input$Y_Variable_Correlation),y=mean(input$Output_Variable), label=format(R_Squared, digits = 3), parse = TRUE)
    
          
        
    })

    R_Squared <- reactive({
    ifelse(input$Default_Data=="Yes", Data <- Boston, Data <- Read_Data())
    
     
    Corr <- lm(as.formula(paste0(input$Y_Variable_Correlation,"~",input$Output_Variable)), data = Data)
    Corr <- summary(Corr)
    as.character(paste('R Squared:',Corr$r.squared))})
    
    PLOT_LR <- reactive({
    ifelse(input$Default_Data=="Yes", Data <- Boston, Data <- Read_Data())
    y <- as.matrix(Data[,colnames(Data)==input$Output_Variable])
    x <-  as.matrix(Data[,!colnames(Data)==input$Output_Variable])
    cv.ridge <- cv.glmnet(x, y, alpha=0, standardize=TRUE)
    plot(cv.ridge)
    
    })
    
    RIDGE <- reactive({
      ifelse(input$Default_Data=="Yes", Data <- Boston, Data <- Read_Data())
      y <- as.matrix(Data[,colnames(Data)==input$Output_Variable])
      x <-  as.matrix(Data[,!colnames(Data)==input$Output_Variable])
      cv.ridge <- cv.glmnet(x, y, alpha=0, standardize=TRUE)
      Ridge_Regression <- glmnet(x,y,family="gaussian", standardize=TRUE, alpha=0, lambda=cv.ridge$lambda.1se)
      Ridge_Regression$beta
    })
    
   PREDICT <- reactive({
     ifelse(input$Default_Data=="Yes", Data_Test <- Boston, Data_Test <- Read_Data())
     ifelse(input$Default_Data=="Yes", Data <- Boston, Data <- Read_Data())
     y <- as.matrix(Data[,colnames(Data)==input$Output_Variable])
     x <-  as.matrix(Data[,!colnames(Data)==input$Output_Variable])
     cv.ridge <- cv.glmnet(x, y, alpha=0, standardize=TRUE)
     Ridge_Regression <- glmnet(x,y,family="gaussian", standardize=TRUE, alpha=0, lambda=cv.ridge$lambda.1se)
     Data_Testx <-  as.matrix(Data_Test[,!colnames(Data_Test)==input$Output_Variable])
     
    PR <- predict.glmnet(Ridge_Regression, newx = as.matrix(Data_Testx),s="lambda min")
    DF <- data.frame(Predicted=PR, Actual=Data_Test[input$Output_Variable], Difference=PR-Data_Test[input$Output_Variable])
    names(DF) <- c("Predicted", "Actual", "Difference")
    DF[input$Predicted_Values[1]:input$Predicted_Values[2],]
    
     
   }) 
  
  
  
  output$Summary_coeff <- renderPrint({LM_Summary()})
  output$Scatter <- renderPlot({Scatter()})
  output$R_Squared <- renderPrint({R_Squared()})
  output$PLOT_LR <- renderPlot({PLOT_LR()})
  output$RIDGE <- renderPrint({RIDGE()})
  output$PREDICT <- renderPrint({PREDICT()})
  output$Predicted_Values <- renderPrint({input$Predicted_Values[1]})
  
  output$Predictors <- renderUI({
    ifelse(input$Default_Data=="Yes", Data <- Boston, Data <- Read_Data())
    checkboxGroupInput("Predictors", "Number of Predictors in Linear Regression:", choices =names(Data), selected = names(Data))})
  
  output$Output_Variable <- renderUI({
    ifelse(input$Default_Data=="Yes", Data <- Boston, Data <- Read_Data())
    selectInput("Output_Variable", "Select the variable to be Predicted", as.list(names(Data)))})
  
  output$Y_Variable_Correlation <- renderUI({
  ifelse(input$Default_Data=="Yes", Data <- Boston, Data <- Read_Data())
  selectInput("Y_Variable_Correlation", "Select Input Variable", as.list(names(Data)))})
  
})


