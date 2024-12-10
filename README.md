# Ocular Myasthenia Prediction Model

This repository contains the R code for an interactive Shiny application developed for

> **Multivariable Prediction Model for Suspected Ocular Myasthenia Gravis: Development and Validation**
> 
> by *Armin Handzic, MD; Marius P. Furter; Brigitte C. Messmer;
> Magdalena A. Wirth, MD; Yulia Valko, MD; Fabienne C. Fierz, MD; Edward
> A. Margolin, MD; Konrad P. Weber, MD.*

The app is hosted online at [myasthenia-prediction.app](https://myasthenia-prediction.app).

You can run the app locally by executing the following commands in R:

```
library(shiny)

runGitHub("myasthenia-prediction", "MariusFurter")
```

The repository additionally contains the [code](analysis/code) that was used to fit the prediction model, along with the raw [results](analysis/results) used for its description and validation. The original data sets are not included since they contain sensitive patient information.
