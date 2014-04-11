### loads in the vibration metrics and analyzes them
library(ggplot2)
load('../../vibration_metrics/vibration_metrics.Rdata')
vib <- subset(vibration.summaries, rHR < 20 & rPvalue < 30)
vib[, 'effect_deviation'] <- NA
vib[vib$HR_50 >= 1, 'effect_deviation'] <- vib$HR_50[vib$HR_50 >= 1] - 1
vib[vib$HR_50 < 1, 'effect_deviation'] <- (1/vib$HR_50[vib$HR_50 < 1]) - 1

vib[, 'effect_deviation_percentile'] <- ecdf(vib$effect_deviation)(vib$effect_deviation)
p <- ggplot(vib, aes(x=(effect_deviation)))
p <- p + stat_ecdf() + theme_bw() + xlab('Hazard Ratio Deviation from 1.0') + ylab('Cumulative Frequency Distribution')
fig.1 <- vib[vib$varname %in% c('LBXT4', 'URXUCR', 'LBXVID', 'LBXVIE'), ]
fig.1 <- fig.1[order(fig.1$varname), ]
fig.1$label <- c('Thyroxine (Figure 2B)', 'Serum Vitamin D (Figure 2A)', 'Serum alpha-Tocopherol (Figure 2D)', 'Urinary Creatinine (Figure 2C)')
p <- p + geom_point(aes((effect_deviation), effect_deviation_percentile), data=fig.1, size=4)
p <- p + geom_text(aes(label=label, x=(effect_deviation), y=effect_deviation_percentile), data=fig.1, size=5, hjust=-.05)
fig.2 <- vib[vib$varname %in% c('LBXBCD', 'LBXTR'), ]
fig.2 <- fig.2[order(fig.2$varname), ]
fig.2$label <- c('Serum Cadmium (Figure 3ABC)', 'Serum Triglycerides (Figure 3DEF)')
p <- p + geom_point(aes(effect_deviation, effect_deviation_percentile), data=fig.2, size=4)
p <- p + geom_text(aes(label=label, x=effect_deviation, y=effect_deviation_percentile), data=fig.2, size=5, hjust=-.05, vjust=.1)
p <- p + scale_x_continuous(limits=c(0,3.5), breaks=seq(0,3.5,by=.25))
ggsave(file='cdf_median_HR.pdf', plot=p, h=9, width=9)

### 
vib[, 'pval50_percentile'] <- ecdf(-log10(vib$pvalue_50))(-log10(vib$pvalue_50))
p <- ggplot(vib, aes(x=I(-log10(pvalue_50))))
p <- p + stat_ecdf() + theme_bw() + xlab('median(-log10(pvalue))') + ylab('Cumulative Frequency Distribution')
fig.1 <- vib[vib$varname %in% c('LBXT4', 'URXUCR', 'LBXVID', 'LBXVIE'), ]
fig.1 <- fig.1[order(fig.1$varname), ]
fig.1$label <- c('Thyroxine (Figure 2B)', 'Serum Vitamin D (Figure 2A)', 'Serum alpha-Tocopherol (Figure 2D)', 'Urinary Creatinine (Figure 2C)')
p <- p + geom_point(aes(x=I(-log10(pvalue_50)), y=pval50_percentile), data=fig.1, size=4)
p <- p + geom_text(aes(label=label, x=I(-log10(pvalue_50)), y=pval50_percentile), data=fig.1, size=5, hjust=-.05)
fig.2 <- vib[vib$varname %in% c('LBXBCD', 'LBXTR'), ]
fig.2 <- fig.2[order(fig.2$varname), ]
fig.2$label <- c('Serum Cadmium (Figure 3ABC)', 'Serum Triglycerides (Figure 3DEF)')
p <- p + geom_point(aes(I(-log10(pvalue_50)), pval50_percentile), data=fig.2, size=4)
p <- p + geom_text(aes(label=label, x=I(-log10(pvalue_50)), y=pval50_percentile), data=fig.2, size=5, hjust=-.05) 
p <- p + scale_x_continuous(limits=c(0,15), breaks=seq(0,15,by=1))
ggsave(file='cdf_median_pval.pdf', plot=p, h=9, width=9)


###
vib[,'rhr_percentile'] <- ecdf(vib$rHR)(vib$rHR)
p <- ggplot(vib, aes(x=(rHR)))
p <- p + stat_ecdf() + theme_bw() + xlab('Relative Hazard Ratio') + ylab('Cumulative Frequency Distribution')
fig.1 <- vib[vib$varname %in% c('LBXT4', 'URXUCR', 'LBXVID', 'LBXVIE'), ]
fig.1 <- fig.1[order(fig.1$varname), ]
fig.1$label <- c('Thyroxine (Figure 2B)', 'Serum Vitamin D (Figure 2A)', 'Serum alpha-Tocopherol (Figure 2D)', 'Urinary Creatinine (Figure 2C)')
p <- p + geom_point(aes((rHR), rhr_percentile), data=fig.1, size=4)
p <- p + geom_text(aes(label=label, x=(rHR), y=rhr_percentile), data=fig.1, size=5, hjust=-.1)

fig.2 <- vib[vib$varname %in% c('LBXBCD', 'LBXTR'), ]
fig.2 <- fig.2[order(fig.2$varname), ]
fig.2$label <- c('Serum Cadmium (Figure 3ABC)', 'Serum Triglycerides (Figure 3DEF)')
p <- p + geom_point(aes((rHR), rhr_percentile), data=fig.2, size=4)
p <- p + geom_text(aes(label=label, x=(rHR), y=rhr_percentile), data=fig.2, size=5, hjust=-.1) 
p <- p + scale_x_continuous(limits=c(1,2), breaks=seq(1,2,by=.1)) ## narrow the range between 1 and 2
p <- p + scale_y_continuous(limits =c(0,1), breaks=seq(0,1, by=.1))
ggsave(file='cdf_rhr.pdf', plot=p, h=9, width=9)


vib[, 'rp_percentile'] <- ecdf(vib$rPvalue)(vib$rPvalue)
p <- ggplot(vib, aes(x=(rPvalue)))
p <- p + stat_ecdf() + theme_bw() + xlab('Range of P-value (RP)') + ylab('Cumulative Frequency Distribution')
fig.1 <- vib[vib$varname %in% c('LBXT4', 'URXUCR', 'LBXVID', 'LBXVIE'), ]
fig.1 <- fig.1[order(fig.1$varname), ]
fig.1$label <- c('Thyroxine (Figure 2B)', 'Serum Vitamin D (Figure 2A)', 'Serum alpha-Tocopherol (Figure 2D)', 'Urinary Creatinine (Figure 2C)')
p <- p + geom_point(aes((rPvalue), rp_percentile), data=fig.1, size=4)
p <- p + geom_text(aes(label=label, x=(rPvalue), y=rp_percentile), data=fig.1, size=5, hjust=-.1)

fig.2 <- vib[vib$varname %in% c('LBXBCD', 'LBXTR'), ]
fig.2 <- fig.2[order(fig.2$varname), ]
fig.2.cad <- vib[vib$varname %in% c('LBXBCD'), ]
fig.2.cad$label <- 'Serum Cadmium (Figure 3ABC)'
fig.2.tg <- vib[vib$varname %in% c('LBXTR'), ] 
fig.2.tg$label <- 'Serum Triglycerides (Figure 3DEF)'
p <- p + geom_point(aes(x=rPvalue, y=rp_percentile), data=fig.2, size=4)
p <- p + geom_text(aes(label=label, x=rPvalue, y=rp_percentile), data=fig.2.cad, size=5, vjust=-1) 
p <- p + geom_text(aes(label=label, x=rPvalue, y=rp_percentile), data=fig.2.tg, size=5, hjust=-.1) 
p <- p + scale_x_continuous(limits=c(0,11), breaks=seq(1,11,by=.5)) ## narrow the range between 1 and 10
p <- p + scale_y_continuous(limits =c(0,1), breaks=seq(0,1, by=.1))
ggsave(file='cdf_rpval.pdf', plot=p, h=9, width=9)

cor(vib$rPvalue, -log10(vib$pvalue_99))
mod <- lm(rPvalue ~ I(-log10(pvalue_99)), vib)
p <- ggplot(vib, aes(y=rPvalue, x=-log10(pvalue_99)))
p <- p + geom_point() + geom_smooth(method='lm', formula=y ~ x, color='red') + theme_bw()
p <- p + xlab('-log10(99th percentile p-value)') + ylab('Relative P-value')
ggsave(file='rpval_vs_99pvalue.pdf', plot=p, h=9, width=9)
# 
# mod <- lm(rHR ~ I(-log10(pvalue_99)), vib2)
# p <- ggplot(vib2, aes(y=rHR, x=-log10(pvalue_99)))
# p <- p + geom_point() + geom_smooth(method='lm', formula=y ~ x, color='red') + theme_bw()
# p <- p + xlab('-log10(99th percentile p-value)') + ylab('Relative Hazard Ratio') 
# ggsave(file='rhr_vs_99pvalue.pdf', plot=p, h=9, width=9)
# 

cor(vib$rHR, vib$rPvalue)
mod2 <- lm(rPvalue ~ rHR,vib)
p <- ggplot(vib, aes(y=rPvalue, x=(rHR)))
p <- p + geom_point() + geom_smooth(method='lm', formula=y ~ x, color='red') + theme_bw()
p <- p + xlab('Relative Hazard Ratio') + ylab('Range P-value (RP)')
p <- p + annotate("text", x = 3.5, y = 14, label = "cor=.09\nslope=0.71\np=0.06")
ggsave(file='rpval_vs_rhr.pdf', plot=p, h=9, width=9)

