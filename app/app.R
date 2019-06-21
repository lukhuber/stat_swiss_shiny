## TODO: 
##  - Logistische Regression mit Pima Indians - gibt R Vorlagen davon 
##  - Mehr Transformationen der Variablen in Berechnungscode implementieren
##  - LM Plot funktioniert noch nicht mit mehreren Variablen - liegt an der predict Funktion und der neuen lmResults() (Formel sieht anders aus)
##  - Plots ein/ausblende Funktion ("show plots") besser ausnützen und evt. noch 1, 2 Plots einfügen die zur Erklärung des Modells dienen könnten?

library(shiny)
library(maptools)
library(car)
#packages die aus dem neuen copypaste code bentigt werden - kann man denk ich noch rauscoden
library(plotrix)
library(openintro)
library(gridExtra)
library(ggplot2)
#library(shinyWidgets)

## ---
## Pima Indians libraries
## ---
library(mlbench) # for Pima Indian Diabetes dataset
library(caret) # for prediction building functions
library(dplyr)
library(ggplot2)
library(e1071)


## ---
## AUFBAU DES LAYOUTS
## ---
ui <- fluidPage(
  titlePanel("Spezielle Statistik Übung, Dataset: Swiss | HUBER, VIEHBÖCK"),
  
  ## ---
  ## Sidebar
  ## ---
  sidebarLayout(
    sidebarPanel(
      
      ## ---
      ## Dropdownauswahl für 1. Variable (Abhängig von 'navbar')
      ## ---
      conditionalPanel(condition = "input.navbar == 'Explorative Data Analysis'",
                       div(id='tab1_sidebar',
                           selectInput("outcome_exp", label = h3("1. Variable"),
                                                    choices = list("Education" = "Education",
                                                                   "Fertility" = "Fertility",
                                                                   "Agriculture" = "Agriculture",
                                                                   "Examination" = "Examination",
                                                                   "Catholic" = "Catholic",
                                                                   "Infant.Mortality" = "Infant.Mortality"), selected = 1))),
      
      conditionalPanel(condition = "input.navbar == 'Regression'",
                       div(id='tab1_sidebar',
                           checkboxGroupInput("indepvar", label = h3("Variablenauswahl"),
                                       choices = list(
                                                      "Fertility" = "Fertility",
                                                      "Agriculture" = "Agriculture",
                                                      "Examination" = "Examination",
                                                      "Catholic" = "Catholic",
                                                      "Infant.Mortality" = "Infant.Mortality"), selected = "Fertility"))),
      
      ## ---
      ## Dropdownauswahl für 2. Variable (Abhängig von 'navbar')
      ## ---
      conditionalPanel(condition = "input.navbar == 'Explorative Data Analysis'",
                       div(id='tab1_sidebar',
                           selectInput("indepvar_exp", label = h3("2. Variable"),
                                       choices = list("Education" = "Education",
                                                      "Fertility" = "Fertility",
                                                      "Agriculture" = "Agriculture",
                                                      "Examination" = "Examination",
                                                      "Catholic" = "Catholic",
                                                      "Infant.Mortality" = "Infant.Mortality"), selected = 1))),

      ## ---
      ## Slide für Wahrscheinlichkeit (wird nur bei log. RM angezeigt)
      ## ---
      conditionalPanel(condition = "input.tabs_reg == 'Logistic Regression Model'& 
                                    input.navbar == 'Regression'" ,
                       div(id='tab1_sidebar',
                           sliderInput('prob', label = 'Wahrscheinlichkeit', min = 0, max = 100, value = 70))
      ),
      
      ## ---
      ## Auswahl für Transformationstyp und anzuzeigende Plots (nur bei lin. RM angezeigt)
      ## ---
      conditionalPanel(condition = "input.navbar == 'Regression'",
                       div(id='tab1_sidebar',
                           radioButtons("type", label = h3("Transformation type:"),
                                        list("None" = "none",
                                             "Logarithmic" = "log",
                                             "Exponentially" = "exp")),
                           print(h3("Display plots:")),
                           checkboxInput("donum1", "Make #1 plot", value = T),
                           checkboxInput("donum2", "Make #2 plot", value = F),
                           checkboxInput("donum3", "Make #3 plot", value = F))
      ),
      
      ## ---
      ## Auswahl für Pima Indians
      ## ---
      conditionalPanel(condition = "input.navbar == 'Pima Indians",
                               h2("Measurements"),
                               p("Enter information into the fields below to view the probablity of a positive diagnosis."),
                               numericInput('pregnant', 'Number of times pregnant', value = 0),
                               numericInput('glucose', 'Plasma glucose concentration (2 hours in an oral glucose tolerance test)', value = 120),
                               numericInput('pressure', 'Diastolic blood pressure (mm Hg)', value = 70),
                               numericInput('triceps', 'Triceps skin fold thickness (mm)', value = 30),
                               numericInput('insulin', '2-Hour serum insulin (mu U/ml)', value = 125),
                               numericInput('mass', 'Body mass index (kg/(height in m)^2)', value = 25),
                               numericInput('age', 'Age (years)', value = 35),
                               numericInput('pedigree', 'Diabetes Pedigree Function (DPF)*', value = .5))
    ),
    

    ## ---
    ## Reiter
    ## ---
    mainPanel(
      tabsetPanel(id = "navbar", type = "pills",
                  
                  ## ---
                  ## Hauptmenü "Explorative Datenanalyse"
                  ## ---
                  tabPanel("Explorative Data Analysis",
                           br(),
                           tabsetPanel(id = "tabs_exp", type = "tabs",
                                       
                                       tabPanel("Boxplot",
                                                fluidRow(
                                                  column(6, plotOutput("boxplot1")),
                                                  column(6, plotOutput("boxplot2")),
                                                  column(6, verbatimTextOutput("summary_vars")),
                                                  column(6, verbatimTextOutput("summary_vars2")))), 
                                       
                                       tabPanel("Scatterplot", plotOutput("scatterplot")),
                                       
                                       tabPanel("Distribution",
                                                fluidRow(
                                                  column(6, plotOutput("distribution1")),
                                                  column(6, plotOutput("distribution2")))),
                                       
                                       
                                       tabPanel("QQ-plot",
                                                fluidRow(
                                                  column(6, plotOutput("qqplot1")),
                                                  column(6, plotOutput("qqplot2")))),
                                       
                                       
                                       tabPanel("Data", DT::dataTableOutput('tbl'))
                           )
                  ),
                  #br(),
                  
                  ## ---
                  ## Hauptmenü "Regression"
                  ## ---
                  tabPanel("Regression",
                           br(),
                           tabsetPanel(id = "tabs_reg", type = "tabs",
                                       tabPanel("Linear Regression Model", 
                                                fluidRow(
                                                  print(h4("LM Plots")),
                                                  column(3,plotOutput("lmplot1")),
                                                  column(3,plotOutput("lmplot2")),
                                                  column(3,plotOutput("lmplot3")),
                                                  column(3,plotOutput("lmplot4")),
                                                  
                                                  print(h4("Plotgraph")),
                                                  column(12, plotOutput("plotgraph")))),
                                       
                                       tabPanel("Residuals", 
                                                print(h4("Residual plots")),
                                                column(12, plotOutput("linreg")),
                                                column(12, plotOutput("residuals"))
                                       ),
                                       
                                       tabPanel("Model Summary", verbatimTextOutput("summary")),
                                       
                                       tabPanel("LM plot", plotOutput("scatter")), # Lineares Regressionsmodell
                                       
                                       tabPanel("Logistic Regression Model", 
                                                  verbatimTextOutput('logreg'))
                           )
                  ),
                  
                  tabPanel("Pima Indians",
                           br(),
                           h4(HTML(paste0("Model's dynamic probability of a positive diabetes diagnosis based on information in the fields below:", textOutput("Predictions")))),
                           tabsetPanel(id = "tabs_pima", type = "tabs",
                                       tabPanel("Reactive Plot",
                                                plotOutput('reactivePlot', 
                                                           brush = brushOpts(
                                                             id = "brush1"))
                                       ),
                                       tabPanel("Glucose Relationship",
                                                plotOutput('Glucose_Plot')),
                                       tabPanel("VIP",
                                                plotOutput('VIP'))
                           )
                  )
      )
    )
  ))

## ---
## Erstellen der Plots
## ---
server <- function(input, output) {
  
  set.seed(123)
  
  pt1 <- reactive({
    if (!input$donum1) return(NULL)
    qplot(rnorm(500),fill=I("red"),binwidth=0.2,main="plotgraph1")
    })
  
  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    qplot(rnorm(500),fill=I("blue"),binwidth=0.2,main="plotgraph2")
  })
  
  pt3 <- reactive({
    if (!input$donum3) return(NULL)
    qplot(rnorm(500),fill=I("green"),binwidth=0.2,main="plotgraph3")
  })
  
  output$plotgraph = renderPlot({
    ptlist <- list(pt1(),pt2(),pt3())
    wtlist <- c(input$wt1,input$wt2,input$wt3)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
  })
  
  lmResults <- reactive({
    y <- swiss$Education
    x <- swiss[,input$indepvar]
    #data <- data[data$vars %in% input$indepvar,]
    swissdata <- swiss
    
    if(input$type=="log"){
      x <- log(x)
      y <- log(y)
      swissdata <- log(swissdata)
    }
    if(input$type=="exp"){
      x <- exp(x)
      y <- exp(y)
      swissdata <- exp(swissdata)
    }
    predictors <- paste(input$indepvar,collapse="+")
    fml <- as.formula(paste("Education", " ~ ", paste(predictors, collapse="+")))
    print(fml)
    lm(fml, data=swissdata)
  })
  
  # Alte Version - dadurch funktionieren paar plots
  lmResults_old <- reactive({
    y <- swiss[,input$outcome_exp]
    x <- swiss[,input$indepvar]
    if(input$type=="log"){
      x <- log(x)
      y <- log(y)
    }
    if(input$type=="exp"){
      x <- exp(x)
      y <- exp(y)
    }
    
    lm(y ~ x, data = swiss)
  })
  
  ## ---
  ## Modellzusammenfassung
  ## ---
  output$summary <- renderPrint({
    #fit <- lm(swiss[,input$outcome] ~ swiss[,input$indepvar])
    #names(fit$coefficients) <- c("Intercept", input$var2)
    summary(lmResults())
  })
  
  ## ---
  ## Zusammenfassung der 1. Variable
  ## ---
  output$summary_vars <- renderPrint({
    x <- swiss[,input$outcome_exp]
    summary(x)
  })

  ## ---
  ## Zusammenfassung der 2. Variable
  ## ---
  output$summary_vars2 <- renderPrint({
    y <- swiss[,input$indepvar_exp]
    summary(y)
  })
  
  ## ---
  ## Regressions Plots
  ## ---
  output$lmplot1 <- renderPlot({
    plot(lmResults())
  })
  
  output$lmplot2 <- renderPlot({
    plot(lmResults(), which=2)
  })
  
  output$lmplot3 <- renderPlot({
    plot(lmResults(), which=3)
  })
  
  output$lmplot4 <- renderPlot({
    plot(lmResults(), which=4)
  })
  
  ## ---
  ## Datatable
  ## ---
  output$tbl = DT::renderDataTable({
    DT::datatable(swiss, options = list(lengthChange = FALSE))
  })
  
  ## ---
  ## QQ-Plot der 1. Variable
  ## ---
  output$qqplot1 <- renderPlot({
    qqnorm(swiss[,input$outcome_exp], main="Q-Q Plot", xlab=input$outcome_exp)
    qqline(swiss[,input$outcome_exp])
  })
  
  ## ---
  ## QQ-Plot der 2. Variable
  ## ---
  output$qqplot2 <- renderPlot({
    qqnorm(swiss[,input$indepvar_exp], main="Q-Q Plot", xlab=input$indepvar_exp)
    qqline(swiss[,input$indepvar_exp])
  })
  
  ## ---
  ## Boxplot der 1. Variable
  ## ---
  output$boxplot1 <- renderPlot({
    boxplot(swiss[,input$outcome_exp], main="Boxplot", xlab=input$outcome_exp)
  })
  
  ## ---
  ## Boxplot der 2. Variable
  ## ---
  output$boxplot2 <- renderPlot({
    boxplot(swiss[,input$indepvar_exp], main="Boxplot", xlab=input$indepvar_exp)
  })
  
  ## ---
  ## Scatterplots
  ## ---
  output$scatterplot <- renderPlot({
    plot(swiss[,input$indepvar_exp], swiss[,input$outcome_exp], main="Scatterplot",
         xlab=input$indepvar_exp, ylab=input$outcome_exp, pch=19)
    abline(lm(swiss[,input$outcome_exp] ~ swiss[,input$indepvar_exp]), col="red")
    lines(lowess(swiss[,input$indepvar_exp],swiss[,input$outcome_exp]), col="blue")
  })
  
  ## ---
  ## Logistische Regressionsmodell
  ## ---
  # mit Pima Indians machen! Bestimmen ob eine Frau Diabetes bekommen wird ja/nein -> neue app?
  output$logreg <- renderPrint({
    recode <- swiss[,input$outcome]
    recode[recode>input$prob[1]] <- 1
    recode[recode>1] <- 0
    logistic_model <- glm(recode ~ swiss[,input$indepvar], data = swiss, family = binomial)
    summary(logistic_model)
  })
  
  ## ---
  ## Lineares Regressionsmodell
  ## ---
  output$linreg <- renderPlot({
    #fit <- lm(y ~ x, data = swiss)
    #plot(lmResults(), ylab = input$outcome, xlab = input$indepvar)
    #abline(fit,col="red")
    crPlots(lmResults())
  })
  
  ## ---
  ## ANOVA
  ## ---
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
  
  ## ---
  ## Histogramm der 1. Variable
  ## ---
  output$distribution1 <- renderPlot({
    hist(swiss[,input$outcome_exp], main="", xlab=input$outcome_exp)
  })
  
  ## ---
  ## Histogramm der 2. Variable
  ## ---
  output$distribution2 <- renderPlot({
    hist(swiss[,input$indepvar_exp], main="", xlab=input$indepvar_exp)
  })
  
  ## ---
  ## Residual Plots
  ## ---
  output$residuals <- renderPlot({
    par(mfrow=c(1,3), cex.main=2, cex.lab=2, cex.axis=2, mar=c(4,5,2,2))

    residuals = summary(lmResults())$residuals
    #predicted = predict(lmResults_old(), newdata = data.frame(x=swiss[,"Education"]))
    #print(predicted)
    #plot(residuals ~ predicted, 
    #     main="Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals", 
    #     pch=19, col = COL[1,2])
    #abline(h = 0, lty = 2)
    d = density(residuals)$y
    h = hist(residuals, plot = FALSE)
    hist(residuals, main="Histogram of Residuals", xlab="Residuals", 
         col=COL[1,2], prob = TRUE, ylim = c(0,max(max(d), max(h$density))))
    lines(density(residuals), col = COL[1], lwd = 2)
    qqnorm(residuals, pch=19, col = COL[1,2], main = "Normal Q-Q Plot of Residuals")
    qqline(residuals, col = COL[1], lwd = 2)
  }, height=300)
  
  ## ---
  ## LM Plot 2 - TODO: FUNKTIONIERT NOCH NICHT RICHTIG, AUF MEHRERE VARIABLEN AUSDEHEN!
  ## ---
  output$scatter <- renderPlot({
    
    data1 <- swiss
    y <- swiss[,input$outcome_exp]
    x <- swiss[,input$indepvar_exp]
    
    #used for confidence interval
    xcon <- seq(min(x)-.1, max(x)+.1, .025)
    
    predictor <- data.frame(x=xcon)
    
    yhat <- predict(lmResults_old())    
    yline <- predict(lmResults_old(), predictor)
    
    par(cex.main=1.5, cex.lab=1.5, cex.axis=1.5, mar = c(4,4,4,1))
    
    r.squared = round(summary(lmResults_old())$r.squared, 4)
    corr.coef = round(sqrt(r.squared), 4)
    
    plot(c(min(x),max(x)),c(min(y,yline),max(y,yline)), 
         type="n",
         xlab="x",
         ylab="y",
         main=paste0("Regression Model\n","(R = ", corr.coef,", ", "R-squared = ", r.squared,")"))
    
    
    newx <- seq(min(x), max(x), length.out=400)
    confs <- predict(lmResults_old(), newdata = data.frame(x=newx), 
                     interval = 'confidence')
    preds <- predict(lmResults_old(), newdata = data.frame(x=newx), 
                     interval = 'predict')
    
    polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = grey(.95), border = NA)
    polygon(c(rev(newx), newx), c(rev(confs[ ,3]), confs[ ,2]), col = grey(.75), border = NA)
    
    points(x,y,pch=19, col=COL[1,2])
    # hier kommt fehler
    lines(xcon, yline, lwd=2, col=COL[1])
    
    legend_pos = ifelse(lmResults_old()$coefficients[1] < 1, "topleft", "topright")
    legend(legend_pos, inset=.05,
           legend=c("Regression Line", "Confidence Interval", "Prediction Interval"), 
           fill=c(COL[1],grey(.75),grey(.95)))
    box()
  })
  
  ## ---
  ## Ab hier Pima Indians
  ## ---
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
}

shinyApp(ui = ui, server = server)