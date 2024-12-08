# Occular Myasthenia Prediction

This repository contains the Shiny app developed for

> "Multivariable Prediction Model for Suspected Ocular Myasthenia Gravis: Development and Validation"
> by
> Armin Handzic, MD, Marius P. Furter , Brigitte C. Messmer,
Magdalena A. Wirth, MD, Yulia Valko, MD, Fabienne C. Fierz, MD, Edward
A. Margolin, MD, Konrad P. Weber, MD.

The app is hosted online at [myasthenia-prediction.app](https://myasthenia-prediction.app).

You can run the app locally by executing the following commands in R:

```
library(shiny)
runGitHub("myasthenia-prediction", "MariusFurter")
```

The repository additionally contains raw versions of the results presented in the paper (analysis/results) along with the code that generated them (analysis/code). The original data sets are not included due as they contain sensitive patient information.
