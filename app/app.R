## TODO: 
##  - Transformation poly, standardisieren rein
##  - pairs plot?

library(shiny)
library(maptools)
library(car)
library(plotrix)
library(openintro)
library(gridExtra)
library(ggplot2)
library(ngram)
library(corrplot)
library(GGally)
library(broom)
library(popbio)


## ---
## Pima Indians libraries
## ---
library(MASS)
library(mlbench) # for Pima Indian Diabetes dataset
library(caret) # for prediction building functions
library(dplyr)
library(e1071)



## ---
## AUFBAU DES LAYOUTS
## ---
ui <- fluidPage(
  titlePanel("Spezielle Statistik Übung | HUBER, VIEHBÖCK"),
  
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
                           checkboxGroupInput("indepvar", label = h3("Variables"),
                                       choices = list(
                                                      "Fertility" = "Fertility",
                                                      "Agriculture" = "Agriculture",
                                                      "Examination" = "Examination",
                                                      "Catholic" = "Catholic",
                                                      "Infant.Mortality" = "Infant.Mortality"), selected = "Fertility"))),
      
      conditionalPanel(condition = "input.navbar == 'Pima Indians' & (input.tabs_pima == 'Logistic Regression Model' || input.tabs_pima == 'Prediction')",
                       div(id='tab1_sidebar',
                           checkboxGroupInput("pimavars_multi", label = h3("Variables"),
                                              choices = list(
                                                "Age" = "age",
                                                "BMI" = "bmi",
                                                "Number of pregnancies" = "npreg",
                                                "Plasma Glucose Concentration" = "glu",
                                                "Diastolic blood pressure" = "bp",
                                                "Triceps skin fold thickness" = "skin",
                                                "Diabetes Pedigree Function" = "ped"), selected = "age"))),
      
      conditionalPanel(condition = "input.navbar == 'Pima Indians' & input.tabs_pima != 'Logistic Regression Model' & input.tabs_pima != 'Prediction'",
                       div(id='tab1_sidebar',
                           radioButtons("pimavars", label = h3("Variables"),
                                              choices = list(
                                                "Age" = "age",
                                                "BMI" = "bmi",
                                                "Number of pregnancies" = "npreg",
                                                "Plasma Glucose Concentration" = "glu",
                                                "Diastolic blood pressure" = "bp",
                                                "Triceps skin fold thickness" = "skin",
                                                "Diabetes Pedigree Function" = "ped"), selected = "age"))),

      conditionalPanel(condition = "input.navbar == 'Pima Indians' & input.tabs_pima == 'Exploration'",
                       div(id='tab1_sidebar',
                           selectInput("pima_type", label = h3("Diabetes"),
                                       choices = list("Yes" = "Yes",
                                                      "No" = "No",
                                                      "All" = "All"), selected = "Yes"))
      ),
                                                      
      
      
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
                                                      "Infant.Mortality" = "Infant.Mortality"), selected = 1))
      ),

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
      conditionalPanel(condition = "input.navbar == 'Regression' || input.navbar == 'Pima Indians' & input.tabs_pima == 'Logistic Regression Model' & input.tabs_pima != 'Prediction'",
                       div(id='tab1_sidebar',
                           radioButtons("type", label = h3("Transformation type:"),
                                        list("None" = "none",
                                             "Squared" = "sqr",
                                             "Logarithmic" = "log",
                                             "Exponentially" = "exp")))
      ),
                           
      # conditionalPanel(condition = "input.tabs_reg == 'Linear Regression Model'",
      #                  div(id="tab1_sidebar",
      #                      print(h3("Display plots:")),
      #                      checkboxInput("donum1", "Make #1 plot", value = T),
      #                      checkboxInput("donum2", "Make #2 plot", value = F),
      #                      checkboxInput("donum3", "Make #3 plot", value = F))
      # ),
      
      # ---
      # Auswahl für Pima Indians
      # ## ---
      conditionalPanel(condition = "input.navbar == 'Pima Indians' & input.tabs_pima == 'Prediction'",
                               h4("Parameters"),
                               #p("Enter information into the fields below to view the probablity of a positive diagnosis."),
                               numericInput('pregnant', 'Number of pregnancies', value = 0),
                               numericInput('glucose', 'Glucose concentration', value = 120),
                               numericInput('pressure', 'Blood pressure', value = 70),
                               numericInput('triceps', 'Triceps skin fold thickness', value = 30),
                               numericInput('mass', 'Body mass index', value = 25),
                               numericInput('age', 'Age', value = 35),
                               numericInput('pedigree', 'Diabetes Pedigree Function', value = .5)),

      fluid=T, width=3
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
                                       
                                       tabPanel("Localisation",
                                                fluidRow(
                                                  column(6, plotOutput("boxplot1")),
                                                  column(6, plotOutput("boxplot2")),
                                                  column(6, verbatimTextOutput("summary_vars")),
                                                  column(6, verbatimTextOutput("summary_vars2")))), 
                                       
                                       tabPanel("Scatterplot", fluidRow(
                                                  column(12, plotOutput("scatterplot")),
                                                  column(12, plotOutput("corrplot")))),
                                        
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
                                                  column(3,plotOutput("lmplot4")))),
                                                  #column(12,plotOutput("scatter")))),
                                                  
                                                  #print(h4("Plotgraph")),
                                                  #column(12, plotOutput("plotgraph")))),
                                       
                                       tabPanel("Residuals", 
                                                print(h4("Residual plots")),
                                                column(12, plotOutput("linreg")),
                                                column(12, plotOutput("residuals"))
                                       ),
                                       
                                       tabPanel("Model Summary", 
                                                fluidRow(
                                                  print(h4("Summary")),
                                                  column(12,verbatimTextOutput("summary")),
                                                  print(h4("AIC")),
                                                  column(12,verbatimTextOutput("aic")),
                                                  print(h4("BIC")),
                                                  column(12,verbatimTextOutput("bic"))))
                                                  #column(12,verbatimTextOutput('step'))))
                                       
                           )
                  ),
                  
                  tabPanel("Pima Indians",
                           br(),
                           tabsetPanel(id = "tabs_pima", type = "tabs",
                                       tabPanel("Exploration", fluidRow(
                                         br(),
                                         print(h4("Summary")),
                                         column(6,verbatimTextOutput('pima_summary_y'),
                                                verbatimTextOutput('pima_summary_n')),
                                         #print(h4("Summary: Negative")),
                                         column(6, plotOutput('pima_barplot')),
                                         #column(6,verbatimTextOutput('pima_summary_n')),
                                         
                                         
                                         br(),
                                         column(6, plotOutput('pima_boxplot')),
                                         column(6, plotOutput('pima_hist')),
                                         column(12, plotOutput('pima_qqplot')))),
                                       
                                       tabPanel("Correlations", fluidRow(
                                         column(12, plotOutput('pima_exp')),
                                         column(12, plotOutput('pima_corr')))),
                                       
                                       tabPanel("Logistic Regression Model",
                                                verbatimTextOutput('pima_logreg'),
                                                print(h4("AIC")),
                                                column(12,verbatimTextOutput("pima_aic")),
                                                print(h4("BIC")),
                                                column(12,verbatimTextOutput("pima_bic")),
                                                #column(12,verbatimTextOutput('pima_step')),
                                                column(3, plotOutput('pima_glmplot')),
                                                column(3, plotOutput('pima_glmplot2')),
                                                column(3, plotOutput('pima_glmplot3')),
                                                column(3, plotOutput('pima_glmplot4')),
                                                h4("Residuals"),
                                                column(6, plotOutput('pima_residuals')),
                                                plotOutput('pima_modelplot')
                                                ),
                                       
                                       
                                       
                                       tabPanel("Prediction", 
                                                h4("Confusion Matrix with Pima.tr/Pima.te training & test set: "),
                                                tableOutput('pima_prediction'),
                                                h4("Accuracy: "),
                                                textOutput('pima_acc'),
                                                h4(HTML(paste0("Probability of a positive diabetes diagnosis with given parameters:",
                                                               textOutput("Predictions"))))
                                                
                        
                                       )
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
    if(input$type=="sqr"){
      x <- exp(x)
      y <- exp(y)
      swissdata <- (swissdata)^2
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
  
  output$aic <- renderPrint({
    AIC(lmResults())
  })
  
  output$bic <- renderPrint({
    BIC(lmResults())
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
    qqnorm(swiss[,input$outcome_exp], main="Q-Q Plot", xlab=input$outcome_exp, col=COL[1,2], pch=19)
    qqline(swiss[,input$outcome_exp], col=COL[1], lwd=2)
  })
  
  ## ---
  ## QQ-Plot der 2. Vari￼able
  ## ---
  output$qqplot2 <- renderPlot({
    qqnorm(swiss[,input$indepvar_exp], main="Q-Q Plot", xlab=input$indepvar_exp, col=COL[1,2], pch=19)
    qqline(swiss[,input$indepvar_exp], col=COL[1,2], lwd=2)
  })
  
  ## ---
  ## Boxplot der 1. Variable
  ## ---
  output$boxplot1 <- renderPlot({
    boxplot(swiss[,input$outcome_exp], main="Boxplot", xlab=input$outcome_exp, col=COL[1,2])
  })
  
  ## ---
  ## Boxplot der 2. Variable
  ## ---
  output$boxplot2 <- renderPlot({
    boxplot(swiss[,input$indepvar_exp], main="Boxplot", xlab=input$indepvar_exp, col=COL[1,2])
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
  
  output$corrplot <- renderPlot({
    corr <- cor(swiss)
    corrplot(corr, method="number")
  })
  
  output$step <- renderText({
    step(lmResults())
  })
  
  ## ---
  ## Logistische Regressionsmodell
  ## ---
  # output$logreg <- renderPrint({
  #   v <- swiss[,input$indepvar]
  #   v <- c(v)
  #   
  #   if (length(v) > 5 | length(v) < 2) {
  #     test <- input$indepvar
  #   } else {
  #     test <- paste(names(v), sep = "+", collapse = "+")
  #   }
  #   
  #   recode <- swiss[,"Education"]
  #   recode[recode>input$prob[1]] <- 1
  #   recode[recode>1] <- 0
  # 
  #   formula <- test
  #   
  #   mymodel <- as.formula(concatenate("recode ~ ", formula))
  #   logistic_model <- glm(mymodel, data = swiss, family = binomial)
  #   summary(logistic_model)
  # })
  
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
  ## Histogramm der 1. Variable
  ## ---
  output$distribution1 <- renderPlot({
    hist(swiss[,input$outcome_exp], main=paste("Histogram of", input$outcome_exp), xlab=input$outcome_exp, col=COL[1,2])
  })
  
  ## ---
  ## Histogramm der 2. Variable
  ## ---
  output$distribution2 <- renderPlot({
    hist(swiss[,input$indepvar_exp], main=paste("Histogram of", input$indepvar_exp), xlab=input$indepvar_exp, col=COL[1,2])
    lines(density(swiss[,input$indepvar_exp]), col=COL[1], lwd=2)
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
  })
  
  ## ---
  ## LM Plot 2 - TODO: FUNKTIONIERT NOCH NICHT RICHTIG, AUF MEHRERE VARIABLEN AUSDEHEN!
  ## ---
  output$scatter <- renderPlot({
    
    ggplot(swiss,aes(y=swiss$Education,x=swiss[,input$indepvar]))+geom_point()+geom_smooth(method="lm")+xlab(input$indepvar)
  })
  
  ## ---
  ## Ab hier Pima Indians
  ## ---
  
  #load and merge pima data frames - test and train set
  pima <- rbind(Pima.tr, Pima.te)
  
  # Exploring and visualising
  output$pima_exp <- renderPlot({
    #summary(pima)
    #library GGally
    pairs(subset(pima, select = -c(type)), col = as.factor(pima$type))
    #ggpairs(pima, columns = 1:7, title = "",  mapping=(ggplot2::aes(colour = "type")), axisLabels = "show", columnLabels = colnames(pima[, 1:7]))
  })
  
  glmResults <- reactive({
    y <- pima$type
    x <- input$pimavars_multi
    pimadata <- pima
    
    if(input$type=="log"){
      pimadata <- log(pimadata[,1:7])
    }
    if(input$type=="exp"){
      pimadata <- exp(pimadata[,1:7])
    }
    if(input$type=="sqr"){
      pimadata <- (pimadata[,1:7])^2
    }
    
    predictors <- paste(input$pimavars_multi,collapse="+")
    gfml <- as.formula(paste("pima$type", " ~ ", paste(predictors, collapse="+")))
    print(gfml)
    glm(gfml, data=pimadata, family=binomial)
  })
  
  output$pima_summary_y <- renderPrint({
    #summary(pima)
    #table(pima$type)
    summary(subset(pima, pima$type == "Yes"))
    #summary(subset(pima, pima$type == "No"))
    
  })
  
  output$pima_summary_n <- renderPrint({
    #summary(pima)
    #table(pima$type)
    #summary(subset(pima, pima$type == "Yes"))
    summary(subset(pima, pima$type == "No"))
    
  })
  
  output$pima_corr <- renderPlot({
    #corr
    layout(1)
    pima_numeric <- pima[, -c(8)]
    cor_pima <- cor(pima_numeric)
    corrplot(cor_pima, method="number")
    
  })
  
  # ggplot(pima, aes(x = age, y = type)) +
  #   geom_jitter(width = 0.5, height = 0.03, alpha = .2)
  #   geom_smooth(method = "glm", se = FALSE,
  #               method.args = list(family = "binomial")) +
  #   labs(y = expression(hat(P)(Diabetic)))
  
  output$pima_logreg <- renderPrint({
    #summary(glmResults())$coefficients[, c(1, 4)]
    summary(glmResults())
  })
  output$pima_aic <- renderPrint({
    AIC(glmResults())
  })
  
  output$pima_bic <- renderPrint({
    BIC(glmResults())
  })
  
  
  
  output$pima_boxplot <- renderPlot({
    #label<-c("Diabetic","Non-Diabetic")
    #boxplot(pima[,input$pimavars][pima$type=="Yes"],pima[,input$pimavars][pima$type=="No"],names=label,main=input$pimavars)
    if(input$pima_type == 'All') {
      boxplot(pima[,input$pimavars],main=paste("Diabetes: ", input$pima_type),xlab=input$pimavars, col=COL[1,2])
    }
    else {
      boxplot(pima[,input$pimavars][pima$type==input$pima_type],main=paste("Diabetes: ", input$pima_type),xlab=input$pimavars, col=COL[1,2])
    }
  })
  output$pima_hist <- renderPlot({
    if(input$pima_type == 'All') {
      hist(pima[,input$pimavars], breaks=seq(-0.5,max(pima[,input$pimavars])+0.5), ylim=c(0,60),xlab=input$pimavars, main=paste("Diabetes: ", input$pima_type), col=COL[1,2])
    }
    else {
      hist(pima[,input$pimavars][pima$type==input$pima_type],breaks=seq(-0.5,max(pima[,input$pimavars])+0.5),ylim=c(0,60),xlab=input$pimavars,main=paste("Diabetes: ", input$pima_type), col=COL[1,2])
    }
    
    
  })
  
  
  output$pima_barplot <- renderPlot({
    barplot(table(pima$type), main = "Diabetes in Pima Indian Women")
  })
  
  output$pima_glmplot <- renderPlot({
    plot(glmResults())
  })
  output$pima_glmplot2 <- renderPlot({
    plot(glmResults(), which=2)
  })
  output$pima_glmplot3 <- renderPlot({
    plot(glmResults(), which=3)
  })
  output$pima_glmplot4 <- renderPlot({
    plot(glmResults(), which=4)
  })
  
  ## ---
  ## QQ-Plots
  ## ---
  output$pima_qqplot <- renderPlot({
    qqnorm(pima[,input$pimavars], main="Q-Q Plot", xlab=input$pimavars, col=COL[1,2], pch=19)
    qqline(pima[,input$pimavars], col=COL[1,2], lwd=2)
  })
  
  output$pima_residuals <- renderPlot({
    #par(mfrow=c(1,3), cex.main=2, cex.lab=2, cex.axis=2, mar=c(4,5,2,2))
    residuals = glmResults()$residuals
    d = density(residuals)$y
    h = hist(residuals, plot = FALSE)
    hist(residuals, main="Histogram of Residuals", xlab="Residuals", 
         col=COL[1,2], prob = TRUE, ylim = c(0,max(max(d), max(h$density))))
    lines(density(residuals), col = COL[1], lwd = 2)
    #qqnorm(residuals, pch=19, col = COL[1,2], main = "Normal Q-Q Plot of Residuals")
    #qqline(residuals, col = COL[1], lwd = 2)
  })
  
  glmTrain <- reactive({
    predictors <- paste(input$pimavars_multi,collapse="+")
    gfml <- as.formula(paste("type", " ~ ", paste(predictors, collapse="+")))
    glm(gfml, data=Pima.tr, family=binomial)
  })

  output$pima_step <- renderText({
    step(glmResults())
  })
  
  
  output$pima_prediction <- renderTable({
    prob_pred <- predict(glmTrain(), type='response', newdata = Pima.te)
    type_pred <- ifelse(prob_pred > 0.5, 1, 0)
    cm <- table(Pima.te$type, type_pred)
    cm
    #confusionMatrix(type_pred, Pima.te$type)
    })
  
  output$pima_acc <- renderText({
     prob_pred <- predict(glmTrain(), type='response', newdata = Pima.te)
     type_pred <- ifelse(prob_pred > 0.5, 1, 0)
     cm <- table(Pima.te$type, type_pred)
     acc <- sum(diag(cm)) / sum(cm)
     
     paste(acc, "%")
    })

  output$pima_modelplot <- renderPlot ({
    # plot(input$pimavars,pima$type,xlab=input$pimavars,ylab="Probability of Diabetes") # plot with body size on x-axis and survival (0 or 1) on y-axis
    # 
    # test = pima[,input$pimavars]
    # curve(predict(glmResults(), data.frame(test=x),type="resp"),add=TRUE) # draws a curve based on prediction from logistic regression model
    # 
    # points(pima[,input$pimavars],fitted(glmResults()),pch=20) # optional: you could skip this draws an invisible set of points of body size survival based on a 'fit' to glm model. pch= changes type of dots.
    # logi.hist.plot(input$pimavars,pima$type,boxp=FALSE,type="hist",col="gray")
  })
  
   inTrain <- createDataPartition(pima$type, p = .7, list = FALSE) 
   pimaTrain <- pima[inTrain,]
   pimaTest <- pima[-inTrain,]
   modFit_GLM <- train(type ~., data = Pima.tr, method = "glm")
   predictions <- predict(modFit_GLM,newdata=Pima.te)
  
    # 
    # #function to predict based on data on available variables
   diabetes_Risk <- function(a = mean(pima$npreg), b = mean(pima$glu), 
                             c = mean(pima$bp), d = mean(pima$skin), 
                             e = mean(pima$bmi), f = mean(pima$ped),
                             g = mean(pima$age)) 
                             
   {
     values <- as.data.frame(cbind(a,b,c,d,e,f,g))
     colnames(values) <- c("npreg","glu","bp","skin",
                           "bmi","ped", "age")
     
     prediction_value <- predict(modFit_GLM,newdata=values, type = "prob")[[2]]
     paste(round(100* prediction_value, 2), "%", sep="")
   }

  output$Predictions <- renderPrint({
    diabetes_Risk(input$pregnant,input$glucose,input$pressure,
                  input$triceps,input$mass,input$pedigree,input$age)
   })
}

shinyApp(ui = ui, server = server)