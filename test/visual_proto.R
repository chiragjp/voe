library(ggplot2)

load('../vibration_data/all/LBXGTC_vibration_gather.Rdata')
varname <- 'LBXGTC'
vibFrame <- as.data.frame(vibFrame)
colnames(vibFrame) <- c('estimate', 'HR', 'se', 'robust_se', 'z', 'pvalue', 'combination_index', 'k')
vib1 <- as.data.frame(t(colMeans(subset(vibFrame, k == 0))))
vib2 <- as.data.frame(t(colMeans(subset(vibFrame, k == 10))))
p <- ggplot(vibFrame, aes(HR, -log10(pvalue)))
#p <- p + stat_density2d(geom='tile',aes(fill= ..density..), contour=FALSE)
p <- p + geom_hex() 
#p <- p + stat_density2d(aes(fill = ..level..), geom='polygon')
#p <- p + geom_tile(aes(fill = z)) + stat_contour()
p <- p + geom_point(data=vib1, color='blue') + geom_errorbarh(aes(xmax=exp(estimate+2*robust_se), xmin=exp(estimate-2*robust_se), height=.05), data=vib1, color='blue') 
p <- p + geom_point(data=vib2, color='red') + geom_errorbarh(aes(xmax=exp(estimate+2*robust_se), xmin=exp(estimate-2*robust_se), height=.05), data=vib2, color='red')
p <- p + ggtitle(varname) + xlab('Hazard Ratio')
filename <- sprintf('%s.pdf', varname)
ggsave(file=filename, plot=p, h=7, width=7)

