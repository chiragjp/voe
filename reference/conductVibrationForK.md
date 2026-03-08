# Compute VoE for a fixed number of adjustors

Fits all combinations of `k` covariates from `adjustby` added to
`base_formula` and collects coefficient estimates, p-values, and BIC.

## Usage

``` r
conductVibrationForK(
  base_formula,
  dataFrame,
  adjustby,
  k = 1,
  family = c("gaussian", "binomial", "cox"),
  print_progress = TRUE,
  ...
)
```

## Arguments

- base_formula:

  A formula specifying the base model (exposure + forced covariates).

- dataFrame:

  A data frame containing all variables.

- adjustby:

  A formula or character vector of candidate adjustment variables.

- k:

  Number of adjustors to include in each model.

- family:

  One of `"gaussian"`, `"binomial"`, or `"cox"`.

- print_progress:

  Logical; print progress to console.

- ...:

  Additional arguments passed to
  [`run_model()`](https://chiragjp.github.io/voe/reference/run_model.md).

## Value

A list with components `vibration`, `bic`, `k`, `combinations`,
`family`, `base_formula`, and `adjust`.
