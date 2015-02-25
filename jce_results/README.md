#Vibration of Effects (VoE) Figures and Tables for primer
- The VoE is an empirical estimation of the distribution of effect sizes and p-values due to model selection.
- This directory contains the figures and tables for a manuscript entitled, "Empirical assessment of VoE due to model specification in observational associations: a primer"

##Abstract
- Objectives: Model specification -- what adjusting variables are analytically modeled –may influence results of observational associations. We present a standardized automated approach to quantify the variability of results obtained with possible choices of adjustments called the “vibration of effects” (VoE).
- Study Design and Setting: We estimated the VoE for 417 clinical, environmental, and physiological variables in association with all-cause mortality using National Health and Nutrition Examination Survey data. We selected 13 variables as possible adjustment co-variates and computed 8,192 Cox models for each of 417 variables’ associations with all-cause mortality. 
- Results: We present the magnitude of the VoE by the difference between the 99th and 1st percentile in the effect size and in the -log10(p-value) obtained by different combinations of adjustments. We also present whether there are multimodality patterns in effect sizes and p-values and the trajectory of results with increasing adjustments. For 31% of the 417 variable we observed a Janus effect, with the effect being in opposite direction in the 99th versus the 1st percentile of analyses. 
- Conclusions: Presenting VoE metrics offers empirical estimates of how unstable associations are under different model specifications. When VoE is large, claims for observational associations should be very cautious. 

## Figure and Table Legends
- Fig. 1. VoE computation schematic. (A) Data source. (B) Choose a variable of interest. (C) Construct a set of adjustment variables from a set of 13 socioeconomic, demographic, or health-related variables. Reference level is in the square brackets. (D) All-subsets Cox regression for each 8,193 models. (E) Visualization (“volcano plot”) of –log10(p-value) versus effect size (HR). The median HR and p-value the number of adjustment variables (k) in the model is in red. The 1st, median, and 99th percentile of the –log10(pvalue) and HR are depicted in the dotted line. (F) Compute VoE summary statistics, the Relative Hazard Ratio (RHR) and Relative P-value (RP).
- Fig. 2. Volcano plots visualizing the VoE for four examples, (A) Serum Vitamin D, (B) Serum Thyroxine, (C) Urinary Creatinine, (D) Serum α-Tocopherol. 2D histogram representation in upper panel and contour scatter plot is in lower panel. All effects are for a 1SD change in logged level of variable interest.
- Fig 3. Volcano plots visualizing VoE for three examples with multiple “modes”. (A) The 2D histogram for 1SD increase of the logarithm of serum cadmium, (B) Volcano scatter plot with of serum cadmium if smoking was included in the model (yellow) or smoking not included in model (black). (C) Volcano scatter plot for serum cadmium models with drink five per day (yellow) or models without drink five per day (black). (D) The 2D histogram for 1SD increase of the logarithm of serum triglycerides, (E) With total cholesterol included in the model (yellow) or total cholesterol not included in model (black). (F) With any diabetes (yellow) or models without any diabetes (black). 
- Fig. 4. Cumulative distributions of VoE for 417 variables. (A) Absolute deviation of HR from 1, (B) log10(pvalue), (C) Relative Hazard Ratio (RHR), (D) Relative P-value (RP). Examples shown in figures 1-3 are shown in the distribution.

- Table S1. Adjustment variable means of participants in NHANES.
- Table S2. Description, mean, and SE of 417 variables of interest.
- Table S3. VoE summary measures for 417 variables of interest. “HR50”=median HR. “P50”=median –log10(P-value). “P1”=1st percentile –log10(p-value). “P99”=99th percentile p-value. “RHR”=Relative Hazard Ratio. “RP”=range of –log10(p-value). “Janus”=Janus effect detected? “<0.05”=P1 and P99 < 0.05? “<0.001”=P1 and P99 are < 0.001? “<0.0001”=P1 and P99 < 0.0001. “VM”= Visual multimodality present?
- Fig. S1. VoE 2D histograms for 417 variables of interest.
- Fig. S2. Range of P-value (RP) vs Relative Hazard Ratio (RHR) for 417 associations.
- Fig. S3. VoE 2D histogram for Vitamin D levels with 19 adjusting variables.
- Fig. S4. VoE for platelet count at different levels of total cholesterol (black points: < 200 mg/dL, yellow points: 200-239 mg/dL, blue points: > 239 mg/dL).


##Contact
- Chirag J Patel
- chirag@hms.harvard.edu
- twitter: @chiragjp


