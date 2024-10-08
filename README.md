# `npiv_regression`: Nonparametric Instrumental Variables Regression

## Overview

The `npiv_regression` function performs the non-parametric regression procedure as described by Chen et al. (2023)](https://academic.oup.com/restud/advance-article-abstract/doi/10.1093/restud/rdae025/7624036?redirectedFrom=fulltext) in a Difference-in-Differences (DiD) context, as detailed in: [Callaway et al. (2024)](https://arxiv.org/abs/2107.02637)

This implementation was part of a Master's dissertation project. I have since realised that a similar implementation is available in R at [Jeffrey Racine's GitHub repository](https://github.com/JeffreyRacine/npiv).


## Installation

Before using the `npiv_regression` function, make sure you have installed and loaded the required package. You can install it from CRAN or GitHub if it's part of a custom package:

```r
# Download the package
devtools::install_github("oddish3/contdid")

# Load the package
library(contdid)
```

## Usage

The `npiv_regression` function is straightforward to use. 
You need to provide the function with the names of the treatment and outcome columns, along with the dataset that contains these columns.

### Function Signature

```r
npiv_regression(treatment_col, outcome_col, data)
```

### Parameters

- `treatment_col`: A string representing the name of the dosage variable in the dataset. This is the variable for which you want to estimate the causal effect, make sure all values are between 0 and 1. 
- `outcome_col`: A string representing the name of the outcome variable in the dataset. This is the dependent variable whose relationship with the treatment you want to study.
- `data`: A data frame containing the variables specified in `treatment_col` and `outcome_col`.

### Example

Suppose you have a dataset `final_data` that includes a column `killed_w_transformed` as your treatment variable and `change_gb_tot` as your outcome variable. You can perform the nonparametric instrumental variables regression as follows:

```r
# Perform NPIV regression
res <- npiv_regression(treatment_col = "killed_w_transformed",
                       outcome_col = "change_gb_tot", 
                       data = final_data)

# View the results
print(res)
```

## Example Analysis (to be implemented)

Here's a brief example of how you might interpret the results:

```r
# Example of interpreting the results
summary(res)
```

The output of `summary(res)` will provide you with the details of the regression, allowing you to make inferences about the causal impact of `killed_w_transformed` on `change_gb_tot`.

### Replicating replication 1 & 2.
Script 1 in the repository replicates the analysis using data from \url{https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/NXFB5R}.
Script 2 in the repository replicate the analysis using data from \url{https://www.openicpsr.org/openicpsr/project/113746/version/V1/view}.
