## TODO: 
##  - Variablenauswahl auf alle (nicht nur 2) ausweiten - am besten einfach mit Checkbox/Radiobuttons
##  - Logistische Regression mit Pima Indians - gibt R Vorlagen davon 
##  - Fuer Punkt 2 fehlt noch die Moeglichkeit zur Qualitaetsueberpruefung des linearen Regressionsmodell
##  - LM Plots in selben Reiter     
##  - Transformation der Variablen in Berechnungscode implementieren

library(shiny)
library(maptools)
library(car)
#packages die aus dem neuen copypaste code bentigt werden - kann man denk ich noch rauscoden
library(plotrix)
library(openintro)
library(gridExtra)

## ---
## AUFBAU DES LAYOUTS
## ---
ui <- fluidPage(
  titlePanel("Regression Model (Dataset: Swiss)"),
  
  ## ---
  ## Sidebar
  ## ---
  sidebarLayout(
    sidebarPanel(
      
      ## ---
      ## Dropdownauswahl für 1. Variable
      ## ---
      selectInput("outcome", label = h3("Outcome"),
                  choices = list("Education" = "Education",
                                 "Fertility" = "Fertility",
                                 "Agriculture" = "Agriculture",
                                 "Examination" = "Examination",
                                 "Catholic" = "Catholic",
                                 "Infant.Mortality" = "Infant.Mortality"), selected = 1),
      
      ## ---
      ## Dropdownauswahl für 2. Variable
      ## ---
      selectInput("indepvar", label = h3("Explanatory variable"),
                  choices = list("Fertility" = "Fertility",
                                 "Agriculture" = "Agriculture",
                                 "Examination" = "Examination",
                                 "Education" = "Education",
                                 "Catholic" = "Catholic",
                                 "Infant.Mortality" = "Infant.Mortality"), selected = 1),
      
      ## ---
      ## Slide für Wahrscheinlichkeit (wird nur bei log. RM angezeigt)
      ## ---
      conditionalPanel(condition = "input.tabs_reg == 'Logistic Regression Model'",
                       div(id='tab1_sidebar',
                           sliderInput('prob', label = 'Wahrscheinlichkeit', min = 0, max = 100, value = 70))
      ),
      
      ## ---
      ## Auswahl für Transformationstyp und anzuzeigende Plots (nur bei lin. RM angezeigt)
      ## ---
      conditionalPanel(condition = "input.tabs_reg == 'Linear Regression Model'",
                       div(id='tab1_sidebar',
                           radioButtons("type", "Select transformation type:",
                                        list("None" = "none",
                                             "Logarithmic" = "log",
                                             "Exponentially" = "exp")),
                           checkboxInput("donum1", "Make #1 plot", value = T),
                           checkboxInput("donum2", "Make #2 plot", value = F),
                           checkboxInput("donum3", "Make #3 plot", value = F))
      )
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
                                       
                                       tabPanel("QQ-plot",
                                                fluidRow(
                                                  column(6, plotOutput("qqplot1")),
                                                  column(6, plotOutput("qqplot2")))),
                                       
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
                                       
                                       tabPanel("Model Summary", verbatimTextOutput("summary")),
                                       
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
                                                  column(6,plotOutput("lmplot")),
                                                  column(6, plotOutput("linreg")),
                                                  column(12, plotOutput("residuals")),
                                                  column(12, plotOutput("plotgraph")))),
                                       
                                       tabPanel("Logistic regression model", verbatimTextOutput('logreg'))
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
    y <- swiss[,input$outcome]
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
  
  
  # Regression output
  output$summary <- renderPrint({
    fit <- lm(swiss[,input$outcome] ~ swiss[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
  })
  
  # Summary Text
  output$summary_vars <- renderPrint({
    x <- swiss[,input$outcome]
    summary(x)
  })
  
  output$summary_vars2 <- renderPrint({
    y <- swiss[,input$indepvar]
    summary(y)
  })
  
  
  ## ---
  ## Regressions Plots
  ## ---
  output$lmplot <- renderPlot({
    plot(lmResults())
    #plot(lmResults(), which=2)
    #plot(lmResults(), which=3)
    #plot(lmResults(), which=4)
    
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
    qqnorm(swiss[,input$outcome], main="Q-Q Plot", xlab=input$outcome)
    qqline(swiss[,input$outcome])
  })
  
  ## ---
  ## QQ-Plot der 2. Variable
  ## ---
  output$qqplot2 <- renderPlot({
    qqnorm(swiss[,input$indepvar], main="Q-Q Plot", xlab=input$indepvar)
    qqline(swiss[,input$indepvar])
  })
  
  ## ---
  ## Boxplot der 1. Variable
  ## ---
  output$boxplot1 <- renderPlot({
    boxplot(swiss[,input$outcome], main="Boxplot", xlab=input$outcome)
  })
  
  ## ---
  ## Boxplot der 2. Variable
  ## ---
  output$boxplot2 <- renderPlot({
    boxplot(swiss[,input$indepvar], main="Boxplot", xlab=input$indepvar)
  })
  
  ## ---
  ## Scatterplots
  ## ---
  output$scatterplot <- renderPlot({
    plot(swiss[,input$indepvar], swiss[,input$outcome], main="Scatterplot",
         xlab=input$indepvar, ylab=input$outcome, pch=19)
    abline(lm(swiss[,input$outcome] ~ swiss[,input$indepvar]), col="red")
    lines(lowess(swiss[,input$indepvar],swiss[,input$outcome]), col="blue")
  })
  
  # Logistisches Regressionsmodell
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
    hist(swiss[,input$outcome], main="", xlab=input$outcome)
  })
  
  ## ---
  ## Histogramm der 2. Variable
  ## ---
  output$distribution2 <- renderPlot({
    hist(swiss[,input$indepvar], main="", xlab=input$indepvar)
  })
  
  
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