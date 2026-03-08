# CDF of p-values at selected thresholds

CDF of p-values at selected thresholds

## Usage

``` r
cdfPerPvalue(
  subFrame,
  pvalues = c(10^(-10:-2), 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1)
)
```

## Arguments

- subFrame:

  A vibration data frame subset.

- pvalues:

  Numeric vector of p-value thresholds.

## Value

A data frame with columns `pvalue`, `cdf`, and `number`.
