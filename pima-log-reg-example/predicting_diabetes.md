Predicting Diabetes using the Pima Indian Diabetes Dataset
========================================================
author: Michael Nichols
date: 4/6/18
autosize: true


Application Overview
========================================================
Shiny application to predict probability an individual would receive a positive diabetes diagnosis.

Highlights:
- 81% accuracy logistic regression model

```
          Reference
Prediction Neg Pos
       Neg 141   9
       Pos  35  45
```
- Dynamic probability calculation based on numeric input of the probability an individual would receive a positive diabetes diagnosis
- 3-tab collection of model visualizations

Tutorial
========================================================
## How to determine the probability an individual would receive a positive diabetes diagnosis:

- Input the following variables into the numeric input boxes listed below: Number of times pregnant, Plasma glucose concentration (2 hours in an oral glucose tolerance test), Diastolic blood pressure (mm Hg), Triceps skin fold thickness (mm), insulin (mu U/ml), Body mass index, Age (years), Diabetes Pedigree Function (DPF)

- For unknown information, default values have been provided to still enable a rough estimate prediction.

- View the percentage probability of a likelihood of a positive diabetes diagnosis. The percentage is dynamic and will update according as the information changes.

Model Visualizations
========================================================

1. Reactive Plot: dynamically assigned best fit line
2. Glucose Relationship: glucose vs prediction prob (below)
<img src="predicting_diabetes-figure/unnamed-chunk-2-1.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" height="400px" />
3. Variable Importance Plot

Where to Access
========================================================
Application hosted on Shiny server at:

https://michaelnichols16.shinyapps.io/predicting_diabetes_coursera/

Code can be found at Github here:
https://github.com/mnicho03/Shiny-App-Reproducible-Pitch
