library(ERSA)
f <- lm(Fertility ~ . , data = swiss)
exploreReg(f,swiss)