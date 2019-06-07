## TODO: 
##  - Der Slider in der Sidebar kann auch einen Minimalwert annehmen. Dies sollte nicht moeglich sein.
##    Besser waere, wenn man keinen Slider sondern ein Textfeld verwenden wuerde. Oder keinen Minimalwert
##    mit dem Slider angeben kann.
##  - Fuer Punkt 2 fehlt noch die Moeglichkeit zur Qualitaetsueberpruefung des linearen Regressionsmodell
##  - LM Plots in selben Reiter     
##  - Transformation der Variablen einbauen
##  - Summary mit MW, SD, IQR, etc.?
##  - Plots für Verteilung der Residuen bei Regression?

library(shiny)
library(maptools)
library(car)
ui <- fluidPage(
    titlePanel("Regression Model (Dataset: Swiss)"),
    sidebarLayout(
        sidebarPanel(
            selectInput("outcome", label = h3("Outcome"),
                        choices = list( "Education" = "Education",
                                        "Fertility" = "Fertility",
                                       "Agriculture" = "Agriculture",
                                       "Examination" = "Examination",
                                       "Catholic" = "Catholic",
                                       "Infant.Mortality" = "Infant.Mortality"), selected = 1),
            
            selectInput("indepvar", label = h3("Explanatory variable"),
                        choices = list("Fertility" = "Fertility",
                                       "Agriculture" = "Agriculture",
                                       "Examination" = "Examination",
                                       "Education" = "Education",
                                       "Catholic" = "Catholic",
                                       "Infant.Mortality" = "Infant.Mortality"), selected = 1),
            sliderInput("prob", label = "Max. Wahrscheinlichkeit [%]:", min = 1, max = 100, value = c(1, 100))
            
        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("QQ-plot",  # QQPlot
                                 fluidRow(
                            column(6, plotOutput("qqplot1")),
                            column(6, plotOutput("qqplot2")))),
                        tabPanel("Boxplot", # BoxPlot
                                 fluidRow(
                                     column(6, plotOutput("boxplot1")),
                                     column(6, plotOutput("boxplot2")))), 
                        tabPanel("Scatterplot", plotOutput("scatterplot")), # Scatterplot
                        tabPanel("LM", 
                                 fluidRow(
                                   column(6,plotOutput("lmplot")),
                                   column(6, plotOutput("linreg")),
                                   column(12, plotOutput("residuals")))), # LM Plot
                        #tabPanel("ANOVA", plotOutput("anova")), # Plot
                        tabPanel("Distribution", # Plots of distributions
                                 fluidRow(
                                     column(6, plotOutput("distribution1")),
                                     column(6, plotOutput("distribution2")))
                        ),
                        tabPanel("Model summary", verbatimTextOutput("summary")), # Regression output
                        tabPanel("Data", DT::dataTableOutput('tbl')), # Data as datatable
                        tabPanel("Logistic regression model", verbatimTextOutput('logreg')), # Logistisches Regressionsmodell
                        tabPanel("Linear Regressionmodel", plotOutput("scatter")) # Lineares Regressionsmodell
            )
        )
    ))



# SERVER
server <- function(input, output) {
    
  lmResults <- reactive({
    lm(swiss[,input$outcome] ~ swiss[,input$indepvar], data = swiss)
  })
  
    # Regression output
    output$summary <- renderPrint({
        fit <- reactive({lm(swiss[,input$outcome] ~ swiss[,input$indepvar])})
        names(fit$coefficients) <- c("Intercept", input$var2)
        summary(fit)
    })
    
    # Regression plots
    output$lmplot <- renderPlot({
        fit <- lm(swiss[,input$outcome] ~ swiss[,input$indepvar])
        plot(fit)
        plot(lmResults())

    }, height=300, width=300)
    
    # Data output
    output$tbl = DT::renderDataTable({
        DT::datatable(swiss, options = list(lengthChange = FALSE))
    })
    
    # QQ-PLot output
    output$qqplot1 <- renderPlot({
        qqnorm(swiss[,input$outcome], main="Q-Q Plot", xlab=input$outcome)
        qqline(swiss[,input$outcome])
    }, height=300, width=300)
    
    output$qqplot2 <- renderPlot({
        qqnorm(swiss[,input$indepvar], main="Q-Q Plot", xlab=input$indepvar)
        qqline(swiss[,input$indepvar])
    }, height=300, width=300)
    
    # Boxplot output
    output$boxplot1 <- renderPlot({
        boxplot(swiss[,input$outcome], main="Boxplot", xlab=input$outcome)
    }, height=300, width=300)
    
    output$boxplot2 <- renderPlot({
        boxplot(swiss[,input$indepvar], main="Boxplot", xlab=input$indepvar)
    }, height=300, width=300)
    
    # Scatterplot output
    output$scatterplot <- renderPlot({
        plot(swiss[,input$indepvar], swiss[,input$outcome], main="Scatterplot",
             xlab=input$indepvar, ylab=input$outcome, pch=19)
        abline(lm(swiss[,input$outcome] ~ swiss[,input$indepvar]), col="red")
        lines(lowess(swiss[,input$indepvar],swiss[,input$outcome]), col="blue")
    }, height=400)
    
    # Logistisches Regressionsmodell
    output$logreg <- renderPrint({
      recode <- swiss[,input$outcome]
      recode[recode>input$prob[2]] <- 1
      recode[recode>1] <- 0
      logistic_model <- glm(recode ~ swiss[,input$indepvar], data = swiss, family = binomial)
      summary(logistic_model)
    })
    
    # Lineares Regressionsmodell
    output$linreg <- renderPlot({
      fit <- lm(swiss[,input$outcome] ~ swiss[,input$indepvar], data = swiss)
      plot(swiss[,input$outcome] ~ swiss[,input$indepvar] ,data=swiss, ylab = input$outcome, xlab = input$indepvar)
      abline(fit,col="red")
      crPlots(fit)
    }, height=300, width=300)
    
    # ANOVA
    output$anova <- renderText({
        ## model fitting
        Y = swiss$Education
        X2 = swiss[,input$indepvar]
        fm1 <- lm(Y ~ 1)
        summary(fm1)
        fmA <- anova(Y ~ swiss[,input$outcome])
        summary(fmA)
        fmB <- anova(Y ~ swiss[,input$indepvar])
        summary(fmB)
        fmAB <- anova(Y ~ swiss[,input$outcome] + swiss[,input$indepvar])
        summary(fmAB)
        fmAxB <- anova(Y ~ swiss[,input$outcome] * swiss[,input$indepvar])
        summary(fmAxB)
        
        ## model selection
        anova(fmAxB,fmAB)
        anova(fmAB,fmA)
        anova(fmAB,fmB)
        
    })
    
    # Histogram output var 1
    output$distribution1 <- renderPlot({
        hist(swiss[,input$outcome], main="", xlab=input$outcome)
    }, height=300, width=300)
    
    # Histogram output var 2
    output$distribution2 <- renderPlot({
        hist(swiss[,input$indepvar], main="", xlab=input$indepvar)
    }, height=300, width=300)
    
    
    output$residuals <- renderPlot({
      par(mfrow=c(1,3), cex.main=2, cex.lab=2, cex.axis=2, mar=c(4,5,2,2))
      
      residuals = summary(lmResults())$residuals
      predicted = predict(lmResults(), newdata = data.frame(x=swiss[,input$outcome]))
      plot(residuals ~ predicted, 
           main="Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals", 
           pch=19, col = COL[1,2])
      abline(h = 0, lty = 2)
      d = density(residuals)$y
      h = hist(residuals, plot = FALSE)
      hist(residuals, main="Histogram of Residuals", xlab="Residuals", 
           col=COL[1,2], prob = TRUE, ylim = c(0,max(max(d), max(h$density))))
      lines(density(residuals), col = COL[1], lwd = 2)
      qqnorm(residuals, pch=19, col = COL[1,2], main = "Normal Q-Q Plot of Residuals")
      qqline(residuals, col = COL[1], lwd = 2)
    }, height=300)
    
    # Show plot of points, regression line, residuals
    output$scatter <- renderPlot({
      
      data1 <- swiss
      x <- swiss[,input$outcome]
      y <- swiss[,input$indepvar]
      
      #used for confidence interval
      xcon <- seq(min(x)-.1, max(x)+.1, .025)
      
      predictor <- data.frame(x=xcon)
      
      yhat <- predict(lmResults())    
      yline <- predict(lmResults(), predictor)
      
      par(cex.main=1.5, cex.lab=1.5, cex.axis=1.5, mar = c(4,4,4,1))
      
      r.squared = round(summary(lmResults())$r.squared, 4)
      corr.coef = round(sqrt(r.squared), 4)
      
      plot(c(min(x),max(x)),c(min(y,yline),max(y,yline)), 
           type="n",
           xlab="x",
           ylab="y",
           main=paste0("Regression Model\n","(R = ", corr.coef,", ", "R-squared = ", r.squared,")"))
      
      
       newx <- seq(min(x), max(x), length.out=400)
       confs <- predict(lmResults(), newdata = data.frame(x=newx), 
                        interval = 'confidence')
       preds <- predict(lmResults(), newdata = data.frame(x=newx), 
                        interval = 'predict')
       
       polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = grey(.95), border = NA)
       polygon(c(rev(newx), newx), c(rev(confs[ ,3]), confs[ ,2]), col = grey(.75), border = NA)
    
       points(x,y,pch=19, col=COL[1,2])
       # hier kommt fehler
       lines(xcon, yline, lwd=2, col=COL[1])
       
       legend_pos = ifelse(lmResults()$coefficients[1] < 1, "topleft", "topright")
       legend(legend_pos, inset=.05,
              legend=c("Regression Line", "Confidence Interval", "Prediction Interval"), 
              fill=c(COL[1],grey(.75),grey(.95)))
       box()
    })
}

shinyApp(ui = ui, server = server)