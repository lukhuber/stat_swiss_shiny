#set working directory
#setwd("S:/Documents/R/ShinyApp_CourseProject")

#load libraries & dataset
library(shiny)
library(mlbench) # for Pima Indian Diabetes dataset
library(caret) # for prediction building functions
library(dplyr)
library(ggplot2)
library(e1071)

#load and rename pima data frame
data(PimaIndiansDiabetes)
pima <- PimaIndiansDiabetes

#display structure 
str(pima)

#clarify meaning of the lone factor variable, and target of our analysis, diabetes
pima$diabetes <- factor(pima$diabetes, labels = c("Neg", "Pos"))

#check to see if there are any irrelevant columns (>98% NA or blank)
!apply(pima, 2, function(x) sum(is.na(x)) > .98  || sum(x=="") > .98)

#set seed and split dataset for predictions
set.seed(25)

#create 70/30 split for model training
inTrain <- createDataPartition(pima$diabetes, p = .7, list = FALSE) 
pimaTrain <- pima[inTrain,]
pimaTest <- pima[-inTrain,]

#show the split
rbind(dim(pimaTrain),dim(pimaTest))

#train the model using logistic regression
modFit_GLM <- train(diabetes ~., data = pimaTrain, method = "glm")

#display the final model
modFit_GLM$finalModel

#predict on the test set
predictions <- predict(modFit_GLM,newdata=pimaTest)

#create DF of prediction probs
predictionsProbs <- as.data.frame(predict(modFit_GLM,newdata=pimaTest, 
                                          type="prob", se=TRUE))

#combine probability column to test set
pimaWpredictions <- cbind(pimaTest, predictionsProbs$Pos)

#rename column with probability
names(pimaWpredictions)[names(pimaWpredictions) == 'predictionsProbs$Pos'] <- "Prediction_Probability"

#display accuracy
caret::confusionMatrix(pimaTest$diabetes, predictions)

# 
# #exploratory data analysis

# Compute Information Values - http://r-statistics.co/Logistic-Regression-With-R.html
# The smbinning::smbinning function converts a continuous variable into a categorical 
# variable using recursive partitioning. We will first convert them to categorical 
# variables and then, capture the information values for all variables in iv_df

# InformationValue::plotROC(pimaTest$diabetes, predictions)

#------------------------------------



#function to predict based on data on available variables
diabetes_Risk <- function(a = mean(pima$pregnant), b = mean(pima$glucose), 
                          c = mean(pima$pressure), d = mean(pima$triceps), 
                          e = mean(pima$insulin), f = mean(pima$mass), 
                          g = mean(pima$pedigree), h = mean(pima$age))
        {
        values <- as.data.frame(cbind(a,b,c,d,e,f,g,h))
        colnames(values) <- c("pregnant","glucose","pressure","triceps",
                             "insulin","mass","pedigree","age")
        
        prediction_value <- predict(modFit_GLM,newdata=values, type = "prob")[[2]]
        paste(round(100* prediction_value, 2), "%", sep="")
        }

#show most significant variables in the model
varImp_GLM <- varImp(modFit_GLM, useModel = FALSE)

#set variable & importance info as a clean DF
varImp_GLM <- data.frame(varImp_GLM$importance)
varImp_GLM$Variables <- rownames(varImp_GLM)
rownames(varImp_GLM) <- NULL
varImp_GLM <- select(varImp_GLM, Variables, Pos) %>%
                rename(Importance = Pos) %>%
                arrange(desc(Importance))

#VIP
varImp_GLM_plot <- ggplot(varImp_GLM, aes(x=Variables, y=Importance)) + 
        geom_bar(stat="identity",fill="skyblue",alpha=.8,width=.6) + 
        coord_flip() +
        labs(title = "Most Predictive Features of Diabetes", 
             subtitle = "Variable Importance Plot")


shinyServer(function(input, output) {
        
        LM <- reactive({
                brushed_data <- brushedPoints(pimaWpredictions, input$brush1,
                                              xvar = "glucose", yvar = "Prediction_Probability")
                if(nrow(brushed_data) < 2){
                        return(NULL)
                }
                lm(Prediction_Probability ~ glucose, data = brushed_data)
        })
        
        output$reactivePlot <- renderPlot({
                plot(pimaWpredictions$glucose, pimaWpredictions$Prediction_Probability, 
                     xlab = "Glucose",
                     ylab = "Diabetes Prediction Probability", 
                     main = "Glucose Levels by Diabetes Probability",
                     cex = 1.5, pch = 16, bty = "n")
                
                #if(!is.null(model())){
                        abline(LM(), col = "red", lwd = 2)
                #}
        })
        
        output$Predictions <- renderPrint({
               diabetes_Risk(input$pregnant,input$glucose,input$pressure,
                             input$triceps,input$insulin,input$mass,input$pedigree,
                             input$age)
        })
        
        output$VIP <- renderPlot({varImp_GLM_plot})
        
        output$Glucose_Plot <- renderPlot({
                        ggplot(pimaWpredictions, aes(x = glucose, y = Prediction_Probability, color = diabetes)) +
                        geom_point(alpha = .7) + 
                        scale_colour_brewer(palette = "Dark2") +
                        stat_smooth(method = loess, data = subset(pimaWpredictions, diabetes == 'Pos')) +
                        xlab("Plasma Glucose Levels") +
                        ylab("Model's Prediction Probability") +
                        ggtitle("Glucose Levels vs Model Diabetes Prediction Probability")
                
        })
})