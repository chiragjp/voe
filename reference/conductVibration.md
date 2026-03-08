# Exhaustive Vibration of Effects

Fits all possible covariate adjustment subsets (from `kMin` to `kMax`
adjustors) and collects the resulting coefficient estimates, p-values,
and BIC values.

## Usage

``` r
conductVibration(
  base_formula,
  dataFrame,
  adjustby,
  family = c("gaussian", "binomial", "cox"),
  kMin = NULL,
  kMax = NULL,
  print_progress = TRUE,
  ...
)
```

## Arguments

- base_formula:

  A formula specifying the base model.

- dataFrame:

  A data frame containing all variables.

- adjustby:

  A formula or character vector of candidate adjustment variables.

- family:

  One of `"gaussian"`, `"binomial"`, or `"cox"`.

- kMin:

  Minimum number of adjustors (default 1).

- kMax:

  Maximum number of adjustors (default `length(adjustby) - 1`).

- print_progress:

  Logical; print progress to console.

- ...:

  Additional arguments passed to
  [`run_model()`](https://chiragjp.github.io/voe/reference/run_model.md).

## Value

A list with components `vibFrame`, `bicFrame`, `combinations`, `adjust`,
`family`, and `base_formula`.
