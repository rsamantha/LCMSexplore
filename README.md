Demo for the EBI metabolomics course. The package is designed to interactively explore some crucial aspects of the analysis pipeline for a metabolomics experiment. In particular, the demo will provide the feeling of the effect of different strategies for handling missing values and provide an example for discussing the usefulness of data visualization. This demo will follow the demo [pietrofranceschi/LCMSdemo](https://github.com/pietrofranceschi/LCMSdemo).

## Installation

* Depends on
```
shiny, gridExtra, tidyverse, forcats, impute, factoextra, FactoMineR
```

* If you do not have it, install devtools: in the R/R Studio shell type
```{r}
install.packages("devtools")
```
* Install LCMSexplore: In the shell type
```{r}
library(devtools)
install_github("rsamantha/LCMSexplore", dependencies = TRUE) 
```

* Load the package: In shell type
```{r}
library(LCMSdemo)
```

******

#### Dataset
The data used for the demo are the results of a UPLC-QTOF (ACQUITY UPLC + SYNAPT HDMS by Waters) analysis of rubus extracts. The raw data and the description of the samples are available @Metabolights (MTBLS333). The data are included in the package both before the `xcms fillPeaks` (as `rubusNA`) and after (as `rubusFilled`). The starting point is a matrix with samples in the rows and features (both coming form the experimental design and mz/rt coming from the measures) in the columns.


*******

### Evaluating the effect of missing values and imputation strategies

Usually, the data matrix of an untargeted metabolomics experiment contains "holes". Which is the best strategy to handle such missing values? How the different imputation strategies affect the study output?

```{r}
shinyExample('NAimputation')
```

### Exploring the data table with Principal Component Analysis (PCA)

The data analysis process of an untargeted metabolomics experiment usually is not straightforward. There is always the need to check how the data looks like in order to detect possible issues in the early stages. PCA is thus a valuable tool for the experimenter and data analyst. Here an example is provided allowing to check the effect of missing values, imputation strategies, and possible confounding factors (from the experimental design).

```{r}
shinyExample('PCAexplore')
```


