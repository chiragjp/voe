# Identify models containing a specific adjustment variable

Adds a `has_variable` column to `vibObj$vibFrame` indicating whether
each model includes the adjustor at position `adjustment_num`.

## Usage

``` r
find_adjustment_variable(vibObj, adjustment_num = 1)
```

## Arguments

- vibObj:

  A VoE result object.

- adjustment_num:

  Index of the adjustment variable to flag.

## Value

The modified VoE result object with `has_variable` column added.
