# Recompute p-values from z-statistics

Some p-values are numerically zero due to large test statistics; this
recomputes them from the normal distribution.

## Usage

``` r
recomputePvalue(allData, zStatColName, pValColName)
```

## Arguments

- allData:

  A data frame with z-statistic and p-value columns.

- zStatColName:

  Name of the z-statistic column.

- pValColName:

  Name of the p-value column.

## Value

The data frame with recomputed p-values.
