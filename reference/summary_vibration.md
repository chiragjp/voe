# Summarize VoE results

Computes RHR (relative hazard ratio), RP (relative p-value), quantiles,
CDF of p-values, and per-k summaries. Optionally identifies the best-BIC
model.

## Usage

``` r
summary_vibration(vibFrame, bicFrame = NULL)
```

## Arguments

- vibFrame:

  A vibration data frame (e.g., `vib$vibFrame`).

- bicFrame:

  Optional BIC data frame (e.g., `vib$bicFrame`).

## Value

A list with components `summary`, `pvalue_cdf`, and `summary_per_k`.
