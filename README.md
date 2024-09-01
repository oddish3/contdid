# `npiv_regression`: Nonparametric Instrumental Variables Regression

## Overview

The `npiv_regression` function performs a nonparametric instrumental variables regression on your dataset. This method is particularly useful when you have a potentially endogenous treatment variable and want to estimate its effect on an outcome variable. The function requires specifying the treatment and outcome columns in your dataset.

## Installation

Before using the `npiv_regression` function, make sure you have installed and loaded the required package. You can install it from CRAN or GitHub if it's part of a custom package:

```r
# Download the package
devtools::install_github("oddish3/contdid")

# Load the package
library(contdid)
```

## Usage

The `npiv_regression` function is straightforward to use. You need to provide the function with the names of the treatment and outcome columns, along with the dataset that contains these columns.

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

### Output

The function will return an object containing the estimated relationship between the treatment and outcome variables. This object can include various outputs depending on the implementation, such as:

- **Coefficients**: Estimated coefficients of the regression.
- **Fitted Values**: Predicted values of the outcome variable based on the treatment variable.
- **Diagnostics**: Diagnostic statistics to assess the quality of the IV regression.

### Interpretation

The results object can be further analyzed to interpret the effect of the treatment on the outcome. Typically, you would look at the coefficients to understand the strength and direction of the relationship.

## Example Analysis

Here's a brief example of how you might interpret the results:

```r
# Example of interpreting the results
summary(res)
```

The output of `summary(res)` will provide you with the details of the regression, allowing you to make inferences about the causal impact of `killed_w_transformed` on `change_gb_tot`.

## Conclusion

The `npiv_regression` function is a powerful tool for estimating the causal effects in the presence of endogenous treatment variables. By specifying the treatment and outcome variables, you can gain insights into complex relationships within your data.
