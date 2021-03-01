# final graphs
library(ggplot2)
sca<-read.csv("output_data/combined_scaling_data.csv")
sca$site.met.area<-paste(sca$site, sca$metric, sca$plot_area)

table(sca$metric, sca$site)

st.err <- function(x) {  sd(x, na.rm=T)/sqrt(length(x))}
cv<-function(x) { mean(x, na.rm=T)/sd(x, na.rm=T) }
q5<-function(x) {quantile(x, 0.25, na.rm=T)}
q95<-function(x) {quantile(x, 0.75, na.rm=T)}

head(sca)
AV <- aggregate( list(value=sca$value), by=list(site=sca$site, metric=sca$metric, plot_area=sca$plot_area),FUN= "mean", na.rm=T)
SE <- aggregate( list(se=sca$value), by=list(site=sca$site, metric=sca$metric, plot_area=sca$plot_area),FUN=  st.err)

SD <- aggregate( list(sd=sca$value), by=list(site=sca$site, metric=sca$metric, plot_area=sca$plot_area),FUN=  "sd", na.rm=T)
CV <- aggregate( list(cv=sca$value), by=list(site=sca$site, metric=sca$metric, plot_area=sca$plot_area),FUN=  cv)
VAR <- aggregate( list(var=sca$value), by=list(site=sca$site, metric=sca$metric, plot_area=sca$plot_area),FUN="var")

V5 <- aggregate( list(q5=sca$value), by=list(site=sca$site, metric=sca$metric, plot_area=sca$plot_area),FUN=q5)
V95 <- aggregate( list(q95=sca$value), by=list(site=sca$site, metric=sca$metric, plot_area=sca$plot_area),FUN=q95)



AV$sd<-SD$sd
AV$se<-SE$se
AV$cv<-CV$cv
AV$var<-VAR$var
AV$q5<-V5$q5
AV$q95<-V95$q95

head(AV)

sca$plot_area<-as.numeric(sub(" .*", "", sca$plot_area))
# create a site-metric-area column to match means to individual
AV$site.met.area<-paste(AV$site, AV$metric, AV$plot_area)

head(sca)




#1.
s.h<-sca[sca$metric=="mean.max.canopy.ht.aop",]
height<-AV[AV$metric=="mean.max.canopy.ht.aop",]

s.h$AVG<-height$value[match(s.h$site.met.area, height$site.met.area)]
s.h$SD<-height$sd[match(s.h$site.met.area, height$site.met.area)]
s.h$type<-"Height"

# subtract each obs from the mean, divide by standard deviation
s.h$subtract<- (s.h$value- s.h$AVG)/s.h$SD
head(s.h)
ggplot(s.h, aes(x=plot_area, y=subtract, col=site))+geom_point(position = position_dodge(width=10))+ylab("subtract from mean, divide by SD")


### try 2

height



#2. open-ness
s.g<-sca[sca$metric=="GFP.AOP.aop",]
gfp<-AV[AV$metric=="GFP.AOP.aop",]

s.g$AVG<-gfp$value[match(s.g$site.met.area, height$site.met.area)]
s.g$SD<-gfp$sd[match(s.g$site.met.area, height$site.met.area)]
s.g$type<-"Open-ness"

gfp$type<-"Open-ness"


#3	Internal Heterogeneity/complexity:
#   o	Rugosity
# o	Foliar height diversity (FHD)
vert<-AV[AV$metric=="VCI.AOP.aop",]
vert$type<-"Internal heterogeneity"
# o	Entropy
entropy<-AV[AV$metric=="entropy.aop",]
entropy$type<-"Internal heterogeneity"

# •	External Heterogeneity /complexity:
#   o	Rumple
rumple<-AV[AV$metric=="rumple.aop",]
rumple$type<-"External heterogeneity"
# rug
top.rugosity<-AV[AV$metric=="top.rugosity.aop",]
top.rugosity$type<-"External heterogeneity"

log.model <-lm(log(value) ~ plot_area, height)
log.model.g1 <- data.frame(x = height$plot_area,y = exp(fitted(log.model)))
log.model.g1

height
g1<-  ggplot(height, aes(x=plot_area, y=value, col=site, shape=type))+geom_point(size=4)+
  geom_errorbar(aes(ymin=q5, ymax=q95),size=1.5, width=15)+
  geom_line(data = log.model.g1, aes(x, y), size = 1, linetype = 2) +
  theme_classic()+theme(text=element_text(size=18))+
  labs(x = bquote('Analysis area'~(m^2)), y="Mean max height (m)")+
  scale_shape_manual(values=15)+scale_y_log10()
g1

g2<-  ggplot(gfp, aes(x=plot_area, y=value, col=site, shape=type))+geom_point(size=4)+
  geom_errorbar(aes(ymin=q5, ymax=q95),size=1.5, width=15)+
  geom_line()+theme_classic()+theme(text=element_text(size=18))+
  labs(x = bquote('Analysis area'~(m^2)), y="Gap fraction")+
  scale_shape_manual(values=16)
g2

g3<-  ggplot(vert, aes(x=plot_area, y=value, col=site, shape=type))+geom_point(size=4)+
  geom_errorbar(aes(ymin=q5, ymax=q95),size=1.5, width=15)+
  geom_line()+theme_classic()+theme(text=element_text(size=18))+
  labs(x = bquote('Analysis area'~(m^2)), y="Vertical complexity index")+
  scale_shape_manual(values=17)
g3

g4<-  ggplot(entropy, aes(x=plot_area, y=value, col=site, shape=type))+geom_point(size=4)+
  geom_errorbar(aes(ymin=q5, ymax=q95),size=1.5, width=15)+
  geom_line()+theme_classic()+theme(text=element_text(size=18))+
  labs(x = bquote('Analysis area'~(m^2)), y="Entropy")+
  scale_shape_manual(values=17)
g4

g5<-  ggplot(rumple, aes(x=plot_area, y=value, col=site, shape=type))+geom_point(size=4)+
  geom_errorbar(aes(ymin=q5, ymax=q95),size=1.5, width=15)+
  geom_line()+theme_classic()+theme(text=element_text(size=18))+
  labs(x = bquote('Analysis area'~(m^2)), y="Rumple")+
  scale_shape_manual(values=18)
g5

g6<-  ggplot(top.rugosity, aes(x=plot_area, y=value, col=site, shape=type))+geom_point(size=4)+
  geom_errorbar(aes(ymin=q5, ymax=q95),size=1.5, width=15)+
  geom_line()+theme_classic()+theme(text=element_text(size=18))+
  labs(x = bquote('Analysis area'~(m^2)), y="Top rugosity")+
  scale_shape_manual(values=18)
g6

library(patchwork)

g1+g2+g3+g4+g5+g6

library(ggpubr)
ggarrange(g1, g2, g3, g4, g5, g6, common.legend=T, legend="bottom", nrow=2, ncol=3)


ggarrange( g3, g4, common.legend=T, legend="none")
ggarrange( g5, g6, common.legend=T, legend="none")

####################################


a<-rbind( gfp, vert, entropy, top.rugosity, rumple)
head(a)

a$pa<-paste(a$site, a$plot_area)
height$pa<-paste(height$site, height$plot_area)

a$height<-height$value[match(a$pa, height$pa)]

head(a)

ggplot(a, aes(x=plot_area, y=height))+geom_point(size=4)+
theme_classic()+theme(text=element_text(size=18))+
  labs(x = bquote('Analysis area'~(m^2)), y="Top rugosity")+
  scale_shape_manual(values=c(15, 16, 17,18))+theme(legend.position = "right")



ggplot(a, aes(x=plot_area, y=))




table(AV$metric)
AV$metric<-factor(AV$metric, levels=c("mean.max.canopy.ht.aop","GFP.AOP.aop","VCI.AOP.aop","entropy.aop","rumple.aop","top.rugosity.aop"))


ggplot(AV, aes(x=plot_area, y=value, col=site))+geom_point()+
  geom_errorbar(aes(ymin=q5, ymax=q95, width=10))+facet_wrap(~metric, scales="free_y", nrow=3)+
  geom_line()+theme_classic()+theme(text=element_text(size=18))+
  labs(x = bquote('Analysis area'~(m^2)), y="metric value")


ggplot(AV, aes(x=plot_area, y=cv, col=site))+geom_point()+
facet_wrap(~metric, scales="free_y")

ggplot(AV, aes(x=plot_area, y=se, col=site))+geom_point()+
  facet_wrap(~metric, scales="free_y")


dev.off()
library(ggplot2)
