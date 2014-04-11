## gets the top interactions surveyed
load('../vibration_data/interaction_anova/interaction_all.Rdata')
colnames(anovaTables) <- c('pvalue',  'Ftest', 'df', 'ddf', 'varname', 'interacting_covariate')

load('../dictionary/dictionary.Rdata')
### sort by sample size?
interactionPvals <- merge(dictionary, anovaTables, by.x='varname', by.y='varname')
interactionPvals <- interactionPvals[order(-log10(interactionPvals$pvalue)*interactionPvals$N, decreasing=T), ]
interactionPvals.large <- subset(interactionPvals, N > 6500)

interactionPvals.large <- interactionPvals.large[!(interactionPvals.large$varname %in% c('prostate_cancer_self_report', 'breast_cancer_self_report')), ]
interactionPvals.large <- interactionPvals.large[!(interactionPvals.large$varname %in% c('LBDHD', 'LBDHBG', 'LBDHCV', 'private_water_source', 'use_water_treatment')), ]
nonSense <- which(interactionPvals.large$varname == 'BMXBMI' & interactionPvals.large$interacting_covariate == 'bmi')
interactionPvals.large <- interactionPvals.large[-nonSense, ]

interactionPvals.large <- interactionPvals.large[order(interactionPvals.large$pvalue), ]

# > interactionPvals.large[1:20, ]
#                      varname         mean           se mean_deceased se_deceased   mean_alive     se_alive            varnameModel                   variable_description
# 6     any_cancer_self_report 6.946303e-02 5.669561e-03     0.1835619  0.02376909 5.899868e-02 5.142195e-03  any_cancer_self_report              Any cancer or malignancy?
# 344                      cad 4.651885e-02 3.233336e-03     0.1194355  0.01788395 3.993646e-02 3.388743e-03                     cad                  Any CAD (self report)
# 3424                   LBXMC 3.382285e+01 3.912782e-02    33.6533060  0.05994189 3.383842e+01 3.921277e-02                   LBXMC                            MCHC (g/dL)
# 574                 DR1TCALC 7.997001e+02 1.301224e+01   624.3184886 31.31384434 8.155159e+02 1.338627e+01                DR1TCALC                           Calcium (mg)
# 3430                   LBXMC 3.382285e+01 3.912782e-02    33.6533060  0.05994189 3.383842e+01 3.921277e-02                   LBXMC                            MCHC (g/dL)
# 269            bPOTASSIUM_mg 2.668659e+03 2.554364e+01  2255.0269328 71.35363133 2.706098e+03 2.794177e+01           bPOTASSIUM_mg                          bPOTASSIUM_mg
# 2542                  LBXBPB 2.472415e+00 6.320052e-02     3.5175421  0.17749315 2.376503e+00 6.430738e-02                  LBXBPB                           Lead (ug/dL)
# 3874                  LBXRDW 1.290609e+01 3.092731e-02    13.5013031  0.10518821 1.285142e+01 2.815315e-02                  LBXRDW        Red cell distribution width (%)
# 3814                LBXPLTSI 2.571877e+02 1.930492e+00   246.3227411  5.24041382 2.581856e+02 1.953140e+00                LBXPLTSI      Platelet count SI (1000 cells/uL)
# 4542 lung_cancer_self_report 1.720222e-03 5.832987e-04     0.0000000  0.00000000 1.877989e-03 6.368834e-04 lung_cancer_self_report                lung_cancer_self_report
# 1234                DR1TSFAT 2.367871e+01 2.944430e-01    18.8472662  1.07339461 2.411441e+01 3.321903e-01                DR1TSFAT       Total saturated fatty acids (gm)
# 1199                DR1TS180 6.122047e+00 7.596630e-02     4.8930098  0.25094716 6.232881e+00 8.565127e-02                DR1TS180           SFA 18:0 (Octadecanoic) (gm)
# 1192                DR1TS160 1.311990e+01 1.458991e-01    10.5540093  0.55285807 1.335129e+01 1.679844e-01                DR1TS160           SFA 16:0 (Hexadecanoic) (gm)
# 3933                LBXSAPSI 8.142397e+01 8.948024e-01    90.5539250  2.78257180 8.060022e+01 8.938282e-01                LBXSAPSI             Alkaline phosphotase (U/L)
# 1054                DR1TPOTA 2.656556e+03 2.616666e+01  2235.1294244 71.08599937 2.694560e+03 2.853205e+01                DR1TPOTA                         Potassium (mg)
# 1283                DR1TTFAT 7.263065e+01 7.015412e-01    57.7258414  2.76707386 7.397476e+01 8.362523e-01                DR1TTFAT                         Total fat (gm)
# 824                 DR1TM201 1.746039e-01 3.963338e-03     0.1190509  0.00818723 1.796137e-01 3.990447e-03                DR1TM201             MFA 20:1 (Eicosenoic) (gm)
# 868                 DR1TMFAT 2.724091e+01 2.812954e-01    21.6412127  1.02430665 2.774588e+01 3.313869e-01                DR1TMFAT Total monounsaturated fatty acids (gm)
# 818                 DR1TM181 2.537898e+01 2.633981e-01    20.1126771  0.93017024 2.585389e+01 3.087077e-01                DR1TM181           MFA 18:1 (Octadecenoic) (gm)
# 1176                DR1TS140 1.937607e+00 3.550939e-02     1.5187833  0.14210670 1.975377e+00 3.703179e-02                DR1TS140          SFA 14:0 (Tetradecanoic) (gm)
#      series    N ncases time_followup       pvalue      Ftest df ddf interacting_covariate
# 6     1;2;3 8607    496            56 6.414451e-21 146.278997  4  34              RIDRETH1
# 344   1;2;3 8568    489            56 3.833721e-18  97.606395  4  34              RIDRETH1
# 3424  1;2;3 8602    496            56 1.212111e-08  20.381020  4  34              RIDRETH1
# 574   1;2;3 8431    476            56 2.850840e-07  15.366314  4  34              RIDRETH1
# 3430  1;2;3 8602    496            56 3.902562e-07  36.713402  1  40                 LBXTC
# 269   1;2;3 8387    475            56 2.247402e-06  12.549588  4  34              RIDRETH1
# 2542  1;2;3 8600    496            56 3.104456e-06  12.138332  4  34              RIDRETH1
# 3874  1;2;3 8602    496            56 5.160670e-06  27.663004  1  40        any_family_cad
# 3814  1;2;3 8602    496            56 1.454555e-05  24.357728  1  40                 LBXTC
# 4542  1;2;3 8607    496            56 1.563697e-05  23.975927  1  41              RIDAGEYR
# 1234  1;2;3 8431    476            56 2.478985e-05  22.725757  1  40                 LBXTC
# 1199  1;2;3 8431    476            56 3.515994e-05  21.680665  1  40                 LBXTC
# 1192  1;2;3 8431    476            56 3.624970e-05  21.590301  1  40                 LBXTC
# 3933  1;2;3 8590    493            56 3.676871e-05  21.548264  1  40        any_family_cad
# 1054  1;2;3 8431    476            56 5.279219e-05   8.837512  4  34              RIDRETH1
# 1283  1;2;3 8431    476            56 5.421277e-05  20.412369  1  40                 LBXTC
# 824   1;2;3 8431    476            56 5.495391e-05  20.373069  1  40                 LBXTC
# 868   1;2;3 8431    476            56 6.599021e-05  19.846152  1  40                 LBXTC
# 818   1;2;3 8431    476            56 7.587302e-05  19.447811  1  40                 LBXTC
# 1176  1;2;3 8431    476            56 7.813001e-05  19.364520  1  40                 LBXTC

load('../bigTable_ultrasens_all.Rdata')

mod <- coxph(Surv(PERMTH_EXM, MORTSTAT) ~ RIDAGEYR + male + cluster(area) + I(factor(RIDRETH1, levels=c(3, 1,2, 4,5)))*any_cancer_self_report, mainTab, weights=mainTab$WTMEC2YR)