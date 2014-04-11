
source('post_process.R')
library(ggplot2)
library(MASS)
vibFrame <- load_vibration_data('../vibration_data/all_v4/LBXMPAH_vibration_gather.Rdata')
vib <- vibFrame$vibFrame

dens <- kde2d(vib$HR, -log10(vib$pvalue), n = 200)
prob <- seq(0.05, .95, by=.05)
dx <- diff(dens$x[1:2])
dy <- diff(dens$y[1:2])
sz <- sort(dens$z)
c1 <- cumsum(sz) * dx * dy
levels <- sapply(prob, function(x) {
        approx(c1, sz, xout = 1 - x)$y
})


df <- data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))
p <- ggplot(vib, aes(x=HR, y=-log10(pvalue))) 
p <- p + geom_point(alpha=1/10)
p <- p + geom_contour(data=df, aes(x=x,y=y,z=z), breaks=levels, size=.5)
p <- p + theme_bw() 
p <- p + ggtitle(sprintf('%s\nRHR = %.02f\nRPvalue = %.02f', 'varnameTitle', .02, .02)) + xlab('Hazard Ratio')
ggsave(file='LBXMPAH_contour.pdf', plot=p, h=7, width=7)


##### test doing two panels in one
p1 <- ggplot(vib, aes(HR, -log10(pvalue))) 
p1 <- p1 + geom_hex(bins=75) + scale_fill_gradientn(colours=c('blue','yellow'), name='') + theme_bw() + theme(legend.position="bottom", legend.justification='left')
p1 <- p1 + ggtitle('A\nB\nC')
p2 <- ggplot(data=df, aes(x=x,y=y,z=z)) + geom_contour(breaks=levels, size=.5) + theme_bw()

grid.arrange(p1, p2)

# g_legend<-function(a.gplot){
# 	tmp <- ggplot_gtable(ggplot_build(a.gplot))
# 	leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
# 	legend <- tmp$grobs[[leg]]
# 	return(legend)
# }
# 
# legend <- g_legend(p1)
# lwidth <- sum(legend$width)
# 
# grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
# 						p2 + theme(legend.position="none"),
# 						main ="this is a title",
# 						left = "-log10(pvalue)"),
# 						widths=unit.c(unit(1, "npc") - lwidth, lwidth), legend, 
# 						nrow=1, ncol=2)


#pushViewport(viewport(layout=grid.layout(2,2)))

