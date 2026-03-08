# Sampling-Based Vibration of Effects

Instead of fitting all 2^n - 1 covariate subsets, draws a random sample
of `n_models` adjustment combinations. Supports multi-core
parallelization via
[`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html).

## Usage

``` r
conductVibrationSample(
  base_formula,
  dataFrame,
  adjustby,
  family = c("gaussian", "binomial", "cox"),
  n_models = 1000,
  n_cores = 1,
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

- n_models:

  Number of random covariate subsets to sample (default 1000).

- n_cores:

  Number of cores for parallel execution (default 1).

- print_progress:

  Logical; print progress to console.

- ...:

  Additional arguments passed to
  [`run_model()`](https://chiragjp.github.io/voe/reference/run_model.md).

## Value

A list with components `vibFrame`, `bicFrame`, `combinations`, `adjust`,
`family`, `base_formula`, `n_models_requested`, and `n_models_fit`.

## Details

Falls back to
[`conductVibration()`](https://chiragjp.github.io/voe/reference/conductVibration.md)
automatically when `n_models` exceeds the total number of possible
subsets.
