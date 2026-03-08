# Plot VoE for Cox models

Creates a volcano plot of hazard ratios vs -log10(p-value) from a VoE
analysis. Two plot types are available: hexbin (`"bin"`) and contour
(`"contour"`).

## Usage

``` r
plot_vibration_cox(
  vibObj,
  type = c("bin", "contour"),
  factor_num = 1,
  adjustment_num = NA,
  ...
)
```

## Arguments

- vibObj:

  A VoE result object (output of
  [`conductVibration()`](https://chiragjp.github.io/voe/reference/conductVibration.md)
  or
  [`conductVibrationSample()`](https://chiragjp.github.io/voe/reference/conductVibrationSample.md)).

- type:

  Plot type: `"bin"` for hexbin or `"contour"` for contour plot.

- factor_num:

  Factor level to plot (default 1).

- adjustment_num:

  If not `NA`, highlights models that include this adjustment variable
  index.

- ...:

  Additional arguments passed to the plotting function.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
