# 2D hexbin volcano plot for Cox VoE

2D hexbin volcano plot for Cox VoE

## Usage

``` r
vib2d_cox(vibObj, factor_num = 1, nbins = 20)
```

## Arguments

- vibObj:

  A VoE result object (output of
  [`conductVibration()`](https://chiragjp.github.io/voe/reference/conductVibration.md)
  or
  [`conductVibrationSample()`](https://chiragjp.github.io/voe/reference/conductVibrationSample.md)).

- factor_num:

  Factor level to plot (default 1).

- nbins:

  Number of hexbin bins (default 20).

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
