# final graphs




### below I calculate a .csv called "output_data_figure_2.csv".  You can make it again, or just use these values.
obs<-read.csv('output_data/figure_2.csv')

head(obs)

st.err <- function(x) {  sd(x, na.rm=T)}


#obs<-obs[!is.na(obs$subtract),]

dim(obs)

table(obs$type)
use.AV<- aggregate( list(value=obs$value), by=list(site=obs$site, metric=obs$type, plot_area=obs$plot_area, sma=obs$site.met.area),FUN= "mean", na.rm=T)
use.se <- aggregate( list(value=abs(obs$value)), by=list(site=obs$site, metric=obs$type, plot_area=obs$plot_area,sma=obs$site.met.area),FUN= st.err)


head(use.se)

head(use.AV)  # bring in se into AV dataframe
use.AV$se<-use.se$value[match(use.AV$sma, use.se$sma)]


head(use.AV)
ggplot(use.AV, aes(x=plot_area, y=value, shape=site, col = site ))+
  geom_point(position=position_dodge(20),size=2) +geom_line(size=1)+
  geom_errorbar(position=position_dodge(20),data=use.AV, mapping=aes(x=plot_area, ymin=value-se, ymax=value+se), width=0.2, size=1)+
  facet_wrap(~metric, scales="free")+  theme_bw()+ylab("Native value")+scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9"))+
  xlab("plot size (m)")


ggplot(use.AV, aes(x=plot_area, y=se, shape=site, col = site ))+
  geom_point(position=position_dodge(20),size=2) +geom_line(size=1)+
  # geom_errorbar(position=position_dodge(20),data=use.AV, mapping=aes(x=plot_area, ymin=value-se, ymax=value+se), width=0.2, size=1)+
  facet_wrap(~metric)+  theme_bw()+ylab("Z score")+scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9"))+
  xlab("plot size (m)")



obs$type<-factor(obs$type, levels=c("mean maximum canopy height","gap fraction","vertical complexity index","entropy","rumple","top rugosity"))
## the figure that is in the paper
ggplot(obs, aes(x=plot_area, y=subtract, shape=site, col = site ))+
  geom_point(position=position_dodge(25)) +facet_wrap(~type)+
  theme_bw()+ylab("Z score")+scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9"))+
  geom_smooth(method="lm", se=F)


# 
# ggplot(obs, aes(x=plot_area, group=plot_area, y=subtract, shape=site, col = site ))+
#   geom_violin(aes(group=plot_area,position=position_dodge(25)) +facet_wrap(site~type, scales="free_x")+
#   theme_bw()+ylab("Z score")+scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9"))




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


table(sca$metric)

#1.  s.h holds the values, height has averages
s.h<-sca[sca$metric=="mean.max.canopy.ht.aop",]
height<-AV[AV$metric=="mean.max.canopy.ht.aop",]

s.h$AVG<-height$value[match(s.h$site.met.area, height$site.met.area)]
s.h$SD<-height$sd[match(s.h$site.met.area, height$site.met.area)]
s.h$type<-"mean maximum canopy height"

# subtract each obs from the mean, divide by standard deviation
s.h$subtract<- (s.h$value- s.h$AVG)/s.h$SD
g.height<-ggplot(s.h, aes(x=plot_area, y=AVG, col=site))+geom_point(position = position_dodge(width=10))+ylab("")+
  ggtitle("mean maximum canopy height")+scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9"))
g.height



#2. open-ness
s.g<-sca[sca$metric=="GFP.AOP.aop",]

gfp<-AV[AV$metric=="GFP.AOP.aop",]

s.g$AVG<-gfp$value[match(s.g$site.met.area, gfp$site.met.area)]
s.g$SD<-gfp$sd[match(s.g$site.met.area, gfp$site.met.area)]
s.g$type<-"gap fraction"

gfp$type<-"gap fraction"

# subtract each obs from the mean, divide by standard deviation
s.g$subtract<- (s.g$value- s.g$AVG)/s.g$SD
head(s.g)
g.gfp<-ggplot(s.g, aes(x=plot_area, y=subtract, col=site))+geom_point(position = position_dodge(width=10))+ylab("")+
  ggtitle("gap fraction")
g.gfp



#3	Internal Heterogeneity/complexity:
#   o	Rugosity
s.r<-sca[sca$metric=="top.rugosity.aop",]
table(sca$metric)
rug<-AV[AV$metric=="top.rugosity.aop",]

s.r$AVG<-rug$value[match(s.r$site.met.area, rug$site.met.area)]
s.r$SD<-rug$sd[match(s.r$site.met.area, rug$site.met.area)]
s.r$type<-"top rugosity"
rug$type<-"top rugosity"

# subtract each obs from the mean, divide by standard deviation
s.r$subtract<- (s.r$value- s.r$AVG)/s.r$SD
head(s.r)
g.rug<-ggplot(s.r, aes(x=plot_area, y=subtract, col=site))+geom_point(position = position_dodge(width=10))+ylab("Relativized Rugosity")+
  ggtitle("top rugosity")
g.rug


# 
# # o	Foliar height diversity (FHD)
# s.r<-sca[sca$metric=="top.rugosity.aop",]
# table(sca$metric)
# rug<-AV[AV$metric=="top.rugosity.aop",]
# 
# s.r$AVG<-rug$value[match(s.r$site.met.area, rug$site.met.area)]
# s.r$SD<-rug$sd[match(s.r$site.met.area, rug$site.met.area)]
# s.r$type<-"Internal Heterogeneity"
# rug$type<-"Internal Heterogeneity"
# 
# # subtract each obs from the mean, divide by standard deviation
# s.r$subtract<- (s.r$value- s.r$AVG)/s.r$SD
# head(s.r)
# ggplot(s.r, aes(x=plot_area, y=subtract, col=site))+geom_point(position = position_dodge(width=10))+ylab("subtract from mean, divide by SD")
# 

# VCI
s.v<-sca[sca$metric=="VCI.AOP.aop",]
table(sca$metric)
vert<-AV[AV$metric=="VCI.AOP.aop",]

s.v$AVG<-vert$value[match(s.v$site.met.area, vert$site.met.area)]
s.v$SD<-vert$sd[match(s.v$site.met.area, vert$site.met.area)]
s.v$type<-"vertical complexity index"
vert$type<-"vertical complexity index"

# subtract each obs from the mean, divide by standard deviation
s.v$subtract<- (s.v$value- s.v$AVG)/s.v$SD
head(s.v)
g.VCI<-ggplot(s.v, aes(x=plot_area, y=subtract, col=site))+geom_point(position = position_dodge(width=10))+ylab("Relativized VCI")+
  ggtitle("vertical complexity index")
g.VCI


#######################
# o	Entropy
table(sca$metric)
s.e<-sca[sca$metric=="entropy.aop",]
entropy<-AV[AV$metric=="entropy.aop",]
entropy$type<-"entropy"


s.e$AVG<-entropy$value[match(s.e$site.met.area, entropy$site.met.area)]
s.e$SD<-entropy$sd[match(s.e$site.met.area, entropy$site.met.area)]
s.e$type<-"entropy"


# subtract each obs from the mean, divide by standard deviation
s.e$subtract<- (s.e$value- s.e$AVG)/s.e$SD
head(s.e)
g.entropy<-ggplot(s.e, aes(x=plot_area, y=subtract, col=site))+geom_point(position = position_dodge(width=10))+ylab("Relativized Entropy")+
  ggtitle("entropy")
g.entropy

###

s.ru<-sca[sca$metric=="rumple.aop",]
rumple<-AV[AV$metric=="rumple.aop",]
rumple$type<-"rumple"


s.ru$AVG<-rumple$value[match(s.ru$site.met.area, rumple$site.met.area)]
s.ru$SD<-rumple$sd[match(s.ru$site.met.area, rumple$site.met.area)]
s.ru$type<-"rumple"


# subtract each obs from the mean, divide by standard deviation
s.ru$subtract<- (s.ru$value- s.ru$AVG)/s.ru$SD
g.rumple<-ggplot(s.ru, aes(x=plot_area, y=subtract, col=site))+geom_point(position = position_dodge(width=10))+ylab("Relativized Entropy")+
  ggtitle("rumple")
g.rumple




############################

library(patchwork)

g.height + g.gfp +  g.VCI  +g.entropy+  g.rumple+g.rug

table(s.g$type)

all<-rbind(s.h, s.g, s.e, s.v, s.r, s.ru)
head(all)

summary(all$subtract)
write.csv(all, file = "output_data/figure_2.csv")

    

####### Alex stopped here 3/13/2021. Did not go below this.

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
