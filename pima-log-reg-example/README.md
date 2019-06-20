# Shiny-App-Reproducible-Pitch
Developing Data Products Course Project - Johns Hopkins Data Science Specialization

Application Hosted on Shiny Server at: https://michaelnichols16.shinyapps.io/predicting_diabetes_coursera/

Presentation / tutorial of the application: http://rpubs.com/michaelnichols16/predicting_diabetes_coursera

# Purpose
Application features a prediction model using logistic regression to reveal a probability that an individual would receive a positive diabetes diagnosis based on the Pima Indian Diabetes Dataset from the UCI Machine Learning Dataset Repository.

### Based on the test set, the model provides 81% accuracy. 

Beyond the percentage likelihood the individual would receive a positive diagnosis, the application includes a 3-tab collection of plots. 
1. Reactive Plot
- Based on the records highlighted on the plot, a best fit line appears showing the relationship between glucose levels and the model's prediction probability of a positive diabetes diagnosis.
2. Glucose Relationship
- Spread of glucose levels compared to the model's prediction probability of a positive diabetes diagnosis. This chart is color-coded based on the actual diabetes status. A confidence interval is displayed based on the data.
3. Variable Importance Plot
- Chart to visualize the most significant features to train the logistic regression model.
