# Fit a model

Dispatches to [`lm()`](https://rdrr.io/r/stats/lm.html),
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html), or
[`glm()`](https://rdrr.io/r/stats/glm.html) based on family.

## Usage

``` r
run_model(form, data, family = "gaussian", ...)
```

## Arguments

- form:

  A formula.

- data:

  A data frame.

- family:

  One of `"gaussian"`, `"cox"`, or `"binomial"`.

- ...:

  Additional arguments passed to the model fitting function.

## Value

A fitted model object.
