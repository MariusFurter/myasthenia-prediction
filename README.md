# Occular Myasthenia Prediction

This repository contains the Shiny app developed for

**Multivariable Prediction Model for Suspected Ocular Myasthenia Gravis: Development and Validation**

by *Armin Handzic, MD, Marius P. Furter , Brigitte C. Messmer,
Magdalena A. Wirth, MD, Yulia Valko, MD, Fabienne C. Fierz, MD, Edward
A. Margolin, MD, Konrad P. Weber, MD,*

in the ***Journal of Neuro-Ophthalmology.***

The app is hosted online at [myasthenia-prediction.app](https://myasthenia-prediction.app).

You can run the app locally by executing the following commands in R:

```
library(shiny)

runGitHub("myasthenia-prediction", "MariusFurter")
```

The repository additionally contains raw versions of the [results](analysis/results) presented in the paper along with the [code](analysis/code) that generated them. The original data sets are not included since they contain sensitive patient information.
