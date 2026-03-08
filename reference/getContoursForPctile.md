# Compute contour levels for VoE density plot

Uses 2D kernel density estimation to compute contour levels at specified
percentiles.

## Usage

``` r
getContoursForPctile(vib, pctiles = seq(0.05, 0.95, by = 0.05))
```

## Arguments

- vib:

  A vibration data frame with `HR` and `pvalue` columns.

- pctiles:

  Percentile levels for contours.

## Value

A list with `levels` and `densityData`.
