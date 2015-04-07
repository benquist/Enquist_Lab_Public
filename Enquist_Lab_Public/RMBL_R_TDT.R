################################################
###   Brian J. Enquist
###   analysis of RMBL flux and trait data for AECR 2015 paper
###   used in Enquist et al. 2015
###   RMBL NPP and flux data
################################################
library(smatr)
library(car)
library(lmSupport)
library(ggplot2)
library(scales)
library(gridExtra)
library(reshape)
library(gtable)
library(AICcmodavg)

####  Importing RMBL 2010 flux data from ProdDivAll_transformed.csv
##### Note, except where noted, these values, including the trait data are already log10 transformed. 
####

########################
### RMBL NPP - Full model
########################
my.data <- read.csv(file="/Users/benquist/Desktop/ProdDivAll_transformed.csv",header=T)
m1 <- lm(log10(NPP.BioOverTime.)~ CWM.SLA+ CWV.SLA + logBio, data=my.data)
summary(m1) 
AIC(m1)
modelEffectSizes(m1)  #lm.sunSquares is depreciated
avPlots(m1)
crPlots(m1)
confint(m1)
vif(m1) 

#  Assess relationship between elevation and total biomass (there is no relationship)
# theme_set(theme_gray(base_size = 20)) # 
myplot <- ggplot(data=my.data, aes(x = Elevation, y = logBio))
myplot <- myplot + geom_point(size = 4)

### will tive a simple black and white graph 
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_line(colour = "black"))
summary(myplot)
myplot + geom_point()

myplot <- ggplot(data=my.data, aes(x = CWM.SLA, y = logBio))
summary(myplot)
myplot + geom_point() 

########################
### RMBL NEP - Full model  
########################

m2<-lm(log10(posNEP)~ CWM.SLA+ CWV.SLA + logBio, data=my.data)
summary(m2) #R2= 0.4877, p = 0.0001284
AIC(m2)
modelEffectSizes(m2)
avPlots(m2)
crPlots(m2)
confint(m2)
vif(m2) 

########################
### RMBL NPP - Full model - RMBL NPP with site as a factor 
########################

my.data <- read.csv(file="/Users/benquist/Desktop/ProdDivAll_transformed.csv",header=T)
m1 <- lm(log10(NPP.BioOverTime.)~ CWM.SLA+ CWV.SLA + logBio +as.factor(Site_name), data=my.data)
summary(m1) #R2= 0.9567, p <0.0001
AIC(m1)
modelEffectSizes(m1)  #lm.sunSquares is depreciated
avPlots(m1)
crPlots(m1)
confint(m1)
vif(m1) 

#################################
###RMBL NEP with site as a factor
#################################
m2<-lm(log10(posNEP)~ CWM.SLA + CWV.SLA + logBio +as.factor(Site_name), data=my.data)
summary(m2) 
AIC(m2)
modelEffectSizes(m2)
avPlots(m2)
crPlots(m2)
confint(m2)
vif(m2) 

#############################################
###RMBL NEP with site as a factor no biomass
############################################

m2<-lm(log10(posNEP)~ CWM.SLA + CWV.SLA +as.factor(Site_name), data=my.data)
summary(m2) 
AIC(m2)
modelEffectSizes(m2)
avPlots(m2)
crPlots(m2)
confint(m2)
vif(m2) 

#############################
###RMBL NEP diversity########
#############################

m2<-lm(log10(posNEP)~ nbsp , data=my.data)
# m2<-lm(log10(posNEP)~ nbsp +as.factor(Site_name), data=my.data) # using site as a factor
summary(m2) 
AIC(m2)
modelEffectSizes(m2)
avPlots(m2)
crPlots(m2)
confint(m2)
vif(m2) 

###RMBL NEP diversity with site as a factor

m2<-lm(log10(posNEP)~ nbsp +as.factor(Site_name), data=my.data)
summary(m2) 
AIC(m2)
modelEffectSizes(m2)
avPlots(m2)
crPlots(m2)
confint(m2)
vif(m2) 


### RMBL Reco with site as a factor
my.data <- read.csv(file="/Users/benquist/Desktop/ProdDivAll_transformed.csv",header=T)
m1 <- lm(log10(Reco)~ CWM.SLA+ CWV.SLA + logBio +as.factor(Site_name), data=my.data)
summary(m1) #R2= 0.9567, p <0.0001
AIC(m1)
modelEffectSizes(m1)  #lm.sunSquares is depreciated
avPlots(m1)
crPlots(m1)
confint(m1)
vif(m1) 
