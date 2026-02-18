# Vibration of Effects (VoE)

The Vibration of Effects is an empirical approach to estimate the distribution of effect sizes and p-values due to model specification choices (i.e., which covariates to adjust for). Given a base exposure-outcome model, VoE systematically fits all (or a random sample of) possible covariate adjustment subsets and visualizes the resulting variability in estimates and statistical significance.

## Reference

Patel CJ, Burford B, Ioannidis JPA. Assessment of vibration of effects due to model specification can demonstrate the instability of observational associations. *Journal of Clinical Epidemiology*. 2015;68(9):1046-1058.

## Quick Start

```r
source('vibration/vibration.R')
source('vibration/plot_vibration.R')

## load NHANES data
load('data/nhanes9904_VoE.Rdata')

## subset to variables of interest
dat <- mainTab[, c('MORTSTAT', 'WTMEC4YR', 'PERMTH_EXM', 'SES_LEVEL', 'RIDAGEYR',
                    'male', 'area', 'LBXBCD', 'current_past_smoking', 'any_cad',
                    'any_family_cad', 'any_cancer_self_report', 'bmi', 'any_ht',
                    'any_diabetes', 'education', 'RIDRETH1', 'physical_activity',
                    'drink_five_per_day', 'LBXTC')]
dat <- subset(dat, !is.na(WTMEC4YR))
dat <- dat[complete.cases(dat), ]

## define adjustment covariates
covariates <- ~ as.factor(SES_LEVEL) + as.factor(education) + as.factor(RIDRETH1) +
  as.factor(bmi) + any_cad + any_family_cad + any_diabetes + any_cancer_self_report +
  current_past_smoking + drink_five_per_day + physical_activity + LBXTC + any_ht

## base model: serum cadmium ~ time to death, adjusted for age + sex
basemodel <- Surv(PERMTH_EXM, MORTSTAT) ~ scale(I(log(LBXBCD + .1))) + RIDAGEYR + male + cluster(area)

## exhaustive VoE (all 2^13 - 1 = 8,191 adjustment subsets)
vib <- conductVibration(basemodel, dat, covariates, family = 'cox', weights = dat$WTMEC4YR)

## sampling-based VoE (faster, with optional parallelization)
vib <- conductVibrationSample(basemodel, dat, covariates, family = 'cox',
                               n_models = 1000, n_cores = 4, weights = dat$WTMEC4YR)

## volcano plots
plot_vibration_cox(vib)
plot_vibration_cox(vib, type = 'contour', alpha = 0.3)
plot_vibration_cox(vib, type = 'contour', alpha = 0.3, adjustment_num = 1)
```

See [`vibration/vibration_start.R`](vibration/vibration_start.R) for a complete working example.

## Repository Structure

```
vibration/                # Core source code
  vibration.R             # VoE computation (exhaustive + sampling)
  plot_vibration.R        # Volcano and contour plots (ggplot2)
  post_process.R          # Summary statistics (RHR, RP, Janus effect)
  vibration_start.R       # Quick-start example

nhanes_vibration/         # Cluster-scale batch pipeline
  prepareData.R           # NHANES data assembly and weighting
  vibrationAnalysisK.R    # Per-k worker script
  writeParamFiles.R       # Parameter file generation
  gatherVibrationAnalysisK.R  # Aggregates batch results

data/                     # NHANES dataset (nhanes9904_VoE.Rdata)
jce_results/              # Figures and tables from the JCE primer
```

## Key Functions

| Function | Description |
|---|---|
| `conductVibration()` | Exhaustive VoE: fits all 2^n - 1 covariate subsets |
| `conductVibrationSample()` | Stochastic VoE: random sample of models, supports `mclapply` parallelization |
| `conductVibrationForK()` | VoE for a fixed number of adjustors k |
| `plot_vibration_cox()` | Volcano plot (2D histogram or contour) of HR vs -log10(p) |
| `summary.vibration()` | RHR, RP, percentiles, Janus effect detection |

## Supported Model Families

- **Cox proportional hazards** (`family = 'cox'`) -- primary use case; supports `cluster()` for robust SEs and survey weights
- **Linear regression** (`family = 'gaussian'`)
- **Logistic regression** (`family = 'binomial'`)

## Output

- **vibFrame**: coefficient matrix with estimate, SE, z-statistic, p-value, combination index, and factor level per model
- **bicFrame**: BIC and effective degrees of freedom per model
- **Volcano plots**: HR (x-axis) vs -log10(p-value) (y-axis), with median trajectory across k highlighted in red

## Dependencies

`survival`, `ggplot2`, `RColorBrewer`, `MASS`

## Updates

**February 18, 2026** -- Added `conductVibrationSample()`, a sampling-based alternative to the exhaustive `conductVibration()`. Instead of fitting all 2^n - 1 covariate subsets, it draws a random sample of `n_models` adjustment combinations, with multi-core support via `mclapply`. On a 13-adjustor Cox model (LBXCOT/mortality, NHANES, n=5,515), 1000 models complete in ~15s on 9 cores vs ~43s single-threaded (~3x parallel speedup). Falls back to exhaustive enumeration automatically when `n_models` exceeds the total number of possible subsets.

## Contact

Chirag J. Patel
- Email: chirag@hms.harvard.edu
- Web: [chiragjpgroup.org](http://www.chiragjpgroup.org)
- Twitter: [@chiragjp](https://twitter.com/chiragjp)

## License

MIT
