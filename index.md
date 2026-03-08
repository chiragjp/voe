# voe: Vibration of Effects

An R package for estimating the distribution of effect sizes and
p-values due to model specification choices. Given a base
exposure-outcome model, VoE systematically fits all (or a random sample
of) possible covariate adjustment subsets and visualizes the resulting
variability in estimates and statistical significance.

## Reference

Patel CJ, Burford B, Ioannidis JPA. Assessment of vibration of effects
due to model specification can demonstrate the instability of
observational associations. *Journal of Clinical Epidemiology*.
2015;68(9):1046-1058.

## Installation

``` r
# Install from GitHub
devtools::install_github("chiragjp/voe")
```

## Example: Is serum cadmium associated with mortality?

The association between serum cadmium and mortality depends heavily on
which confounders you adjust for. VoE quantifies this instability by
fitting every possible combination of adjustors and summarizing the
spread in effect sizes and p-values.

``` r
library(voe)
library(survival)

## 1. Load NHANES 1999-2004 mortality data ---------------------------------
load("data/nhanes9904_VoE.Rdata")

dat <- mainTab[, c("MORTSTAT", "WTMEC4YR", "PERMTH_EXM", "SES_LEVEL",
                    "RIDAGEYR", "male", "area", "LBXBCD",
                    "current_past_smoking", "any_cad", "any_ht",
                    "any_diabetes", "education", "RIDRETH1")]
dat <- dat[complete.cases(dat), ]

## 2. Define the base model and candidate adjustors ------------------------
##    The base model always includes age, sex, and the exposure (cadmium).
##    The 7 candidate adjustors will be toggled on/off in every combination.
basemodel <- Surv(PERMTH_EXM, MORTSTAT) ~ scale(I(log(LBXBCD))) +
  RIDAGEYR + male + cluster(area)

adjustors <- ~ factor(current_past_smoking) + factor(SES_LEVEL) +
  any_cad + any_ht + any_diabetes + factor(education) + factor(RIDRETH1)

## 3. Run the vibration ----------------------------------------------------
##    With 7 adjustors there are 2^7 - 1 = 127 models — runs in seconds.
vib <- conductVibration(basemodel, dat, adjustors,
                         family = "cox", weights = dat$WTMEC4YR)

## 4. Summarize ------------------------------------------------------------
##    RHR (relative hazard ratio) and RP (relative p-value) capture how
##    much the estimate and significance shift across model specifications.
summ <- summary_vibration(vib$vibFrame, vib$bicFrame)
summ$summary
#>       HR_01    HR_50    HR_99  pvalue_01  pvalue_50  pvalue_99   rHR  rPvalue
#>  1   1.03     1.12     1.18   2.8e-07    0.0003     0.19       1.15  5.83

## 5. Visualize ------------------------------------------------------------
##    Volcano plot: each point is one model specification.
##    Red line traces the median estimate at each k (number of adjustors).
plot_vibration_cox(vib, type = "contour", alpha = 0.3)

##    Color by whether smoking is in the model (adjustor #1):
plot_vibration_cox(vib, type = "contour", alpha = 0.3, adjustment_num = 1)
```

For large adjustor sets, use
[`conductVibrationSample()`](https://chiragjp.github.io/voe/reference/conductVibrationSample.md)
to randomly sample model specifications instead of exhaustive
enumeration:

``` r
## Sample 1000 of 2^13 - 1 = 8,191 possible models, using 4 cores
vib <- conductVibrationSample(basemodel, dat, adjustors,
                               family = "cox", n_models = 1000, n_cores = 4,
                               weights = dat$WTMEC4YR)
```

## Key Functions

| Function                                                                                             | Description                                                             |
|------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------|
| [`conductVibration()`](https://chiragjp.github.io/voe/reference/conductVibration.md)                 | Exhaustive VoE: fits all 2^n - 1 covariate subsets                      |
| [`conductVibrationSample()`](https://chiragjp.github.io/voe/reference/conductVibrationSample.md)     | Stochastic VoE: random sample of models with `mclapply` parallelization |
| [`conductVibrationForK()`](https://chiragjp.github.io/voe/reference/conductVibrationForK.md)         | VoE for a fixed number of adjustors k                                   |
| [`plot_vibration_cox()`](https://chiragjp.github.io/voe/reference/plot_vibration_cox.md)             | Volcano plot (hexbin or contour) of HR vs -log10(p)                     |
| [`summary_vibration()`](https://chiragjp.github.io/voe/reference/summary_vibration.md)               | RHR, RP, percentiles, Janus effect detection                            |
| [`find_adjustment_variable()`](https://chiragjp.github.io/voe/reference/find_adjustment_variable.md) | Flag models containing a specific adjustor                              |

## Supported Model Families

- **Cox proportional hazards** (`family = "cox"`) – primary use case;
  supports [`cluster()`](https://rdrr.io/pkg/survival/man/cluster.html)
  for robust SEs and survey weights
- **Linear regression** (`family = "gaussian"`)
- **Logistic regression** (`family = "binomial"`)

## Output

- **vibFrame**: data frame with estimate, SE, z-statistic, p-value,
  combination index, and factor level per model
- **bicFrame**: BIC and effective degrees of freedom per model
- **Volcano plots**: HR (x-axis) vs -log10(p-value) (y-axis), with
  median trajectory across k highlighted in red

## Repository Structure

    R/                       # Package source
      vibration.R            # Core VoE computation (exhaustive + sampling)
      plot_vibration.R       # Volcano and contour plots (ggplot2)
      post_process.R         # Summary statistics (RHR, RP, Janus effect)
    man/                     # Auto-generated documentation

    vibration/               # Original standalone scripts
    nhanes_vibration/        # Cluster-scale batch pipeline
    data/                    # NHANES dataset (nhanes9904_VoE.Rdata)
    jce_results/             # Figures and tables from the JCE paper

## Dependencies

**Required:** `survival`, `MASS`, `rlang`

**Suggested:** `ggplot2` (plotting), `RColorBrewer` (color palettes),
`parallel` (multi-core sampling)

## Contact

Chirag J. Patel - Email: <chirag@hms.harvard.edu> - Web:
[chiragjpgroup.org](http://www.chiragjpgroup.org) - Twitter:
[@chiragjp](https://twitter.com/chiragjp)

## License

MIT
