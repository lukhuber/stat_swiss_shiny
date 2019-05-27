library(shiny)
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
                                       "Infant.Mortality" = "Infant.Mortality"), selected = 1)
            
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
                        tabPanel("LM plot", plotOutput("lmplot")), # Plot
                        tabPanel("ANOVA", plotOutput("anova")), # Plot
                        tabPanel("Distribution", # Plots of distributions
                                 fluidRow(
                                     column(6, plotOutput("distribution1")),
                                     column(6, plotOutput("distribution2")))
                        ),
                        tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                        tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                        
            )
        )
    ))



# SERVER
server <- function(input, output) {
    
    # Regression output
    output$summary <- renderPrint({
        fit <- lm(swiss[,input$outcome] ~ swiss[,input$indepvar])
        names(fit$coefficients) <- c("Intercept", input$var2)
        summary(fit)
    })
    
    # Regression plots
    output$lmplot <- renderPlot({
        fit <- lm(swiss[,input$outcome] ~ swiss[,input$indepvar])
        plot(fit)
    })
    
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
}

shinyApp(ui = ui, server = server)