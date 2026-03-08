# Summarize VoE results by stratum

Applies
[`summary_vibration()`](https://chiragjp.github.io/voe/reference/summary_vibration.md)
to each stratum in the vibration data frame.

## Usage

``` r
summary_vibration_stratum(vibFrame)
```

## Arguments

- vibFrame:

  A vibration data frame with a `stratum` column.

## Value

A list with components `summary`, `pvalue_cdf`, and `summary_per_k`,
each containing a `stratum` column.
