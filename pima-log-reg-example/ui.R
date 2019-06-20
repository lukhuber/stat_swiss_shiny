library(shiny)

shinyUI(fluidPage(
        fluidRow(
                column(12,
                h2("Predicting Diabetes Using the Pima Indian Diabetes Dataset"),
                   p("Predictions are based on the Pima Indian dataset from the UCI Machine Learning Repository and deveoped using logistic regression."),
                   p(HTML(paste0("<b>","The model achieves roughly 81% accuracy on the test set and should be only be viewed for educational purposes as part of the Johns Hopkins Data Science Specialization through Coursera.","</b>"))),        
                   h4(HTML(paste0("Model's dynamic probability of a positive diabetes diagnosis based on information in the fields below:", textOutput("Predictions"))))
                   )),
        fluidRow(
                column(4,
                       h2("Measurements"),
                       p("Enter information into the fields below to view the probablity of a positive diagnosis."),
                       numericInput('pregnant', 'Number of times pregnant', value = 0),
                       numericInput('glucose', 'Plasma glucose concentration (2 hours in an oral glucose tolerance test)', value = 120),
                       numericInput('pressure', 'Diastolic blood pressure (mm Hg)', value = 70),
                       numericInput('triceps', 'Triceps skin fold thickness (mm)', value = 30),
                       numericInput('insulin', '2-Hour serum insulin (mu U/ml)', value = 125),
                       numericInput('mass', 'Body mass index (kg/(height in m)^2)', value = 25),
                       numericInput('age', 'Age (years)', value = 35),
                       numericInput('pedigree', 'Diabetes Pedigree Function (DPF)*', value = .5)),
                column(8,
                       mainPanel(
                       tabsetPanel(
                               tabPanel("Reactive Plot",
                                        plotOutput('reactivePlot', 
                                        brush = brushOpts(
                                                id = "brush1"))
                                        ),
                               tabPanel("Glucose Relationship",
                                        plotOutput('Glucose_Plot')),
                               tabPanel("VIP",
                                        plotOutput('VIP'))
                       )))
                )
        )
)