# <span style="color:#033572">NN</span>PKPD <img src="inst/review-app/www/reactive_pkpd_logo.png" align="right" alt="" width="240" />

# Introduction

The **R**-package **NNPKPD** includes basic functions and a tool for common usage in
    PK/PD analysis and review. The PK/PD review tool, **Reactive PK/PD**, simplifies 
    the PK/PD review process.

# Getting started
The application is designed as an R package where the shiny app is included in the 'inst' subfolder. The steps to run the app are: 

1. Clone the repository
2. In Rstudio, build tab, click "Install and Restart"
3. In the R prompt, write
```
    library("NNPKPD")
    runPKPDreview()
```
4. Add a new trial in 'Setup trial' > 'Trial info' tab
5. Upload data in 'Setup trial' > 'Upload PK data' or 'Setup trial' > 'Upload PD data'
6. Setup the PD plots in 'Setup trial' > 'Setup PD plot' if needed
7. Preview plots in 'Pharmacokinetic profile' or 'Pharmacodynamic profile'