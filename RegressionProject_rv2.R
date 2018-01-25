require(datasets); 
data(mtcars); 
require(GGally); 
#library(lazyeval)
require(ggplot2)
# If error in GGally, need to also install.packages("lazyeval")
library(dplyr); 
##################
#Data Exploration
names(mtcars)
head(mtcars)
dim(mtcars)
boxplot(mtcars)
summary(mtcars)
hist(mtcars$am)
par(mfrow=c(2,2))
hist(mtcars$mpg)
plot(density(mtcars$mpg))
plot(density(mtcars$mpg[mtcars$am==1]))
plot(density(mtcars$mpg[mtcars$am==0]))
###
par(mfrow=c(2,2))
hist(mtcars$cyl)
plot(density(mtcars$cyl))
plot(density(mtcars$cyl[mtcars$am==1]))
plot(density(mtcars$cyl[mtcars$am==0]))
mtcars1<-mtcars[,-9]
#Might need to run "graphics.off()" in order to plot graph below
g <- ggpairs(mtcars, lower = list(continuous = "smooth"),params = NULL, upper = list(
  continuous = wrap("cor", size = 4.75, alignPercent = 1)
))
g
##############
#Creating a categorical variable based on Automatic Manual
mtcars$amBin = ifelse(mtcars$am >0.2, "Man", "Auto")
mtcars1<-mtcars[,c(-7,-8,-9,-10,-11)]
g <- ggpairs(mtcars1, lower = list(continuous = "smooth"), params = NULL, mapping = ggplot2::aes(colour=amBin), upper = list(
  continuous = wrap("cor", size = 3, alignPercent = 1)
))
g
# From ggpairs plot, highest correlation variables with mpg are: wt, Cyl, disp, hp, drat, vs
#Lowest correlation qsec, gear, am, carb
##############
#Creating a binary variable
#library(dplyr); 
#mtcars = mutate(mtcars, am = 1 * (am > 0.21)) #<0.21 is automatic, else it is manual
#hist(mtcars$am)
############################
#Plot the data coloring Automatic=0 (red) and Manual=1 (blue)
g = ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(am)))
g = g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g = g + xlab("weight in 1000lbs") + ylab("mpg")
g
####
g1 = ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(am)))
g1 = g1 + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g1 = g1 + xlab("weight in 1000lbs") + ylab("mpg")
fitwt = lm(mpg ~ wt, data = mtcars)
fitMwt <-lm(mpg[mtcars$am==1]~wt[mtcars$am==1], mtcars)
fitAwt<-lm(mpg[mtcars$am==0]~wt[mtcars$am==0],mtcars)
g2 = g1
g2 = g2 + geom_abline(intercept = coef(fitMwt)[1], slope = coef(fitMwt)[2], size = 1, colour = "blue") # Manual
g2 = g2 + geom_abline(intercept = coef(fitAwt)[1], slope = coef(fitAwt)[2], size = 1, colour = "red") #Automatic
g2 = g2 + geom_abline(intercept = coef(fitwt)[1], slope = coef(fitwt)[2], size = 1) #Comparing to the first fit
g2
#Weight Not considering automatic & manual
#Associated fitted line
#fitwthpam <-lm(mpg~wt+hp+am, mtcars)
#yhat<-coef(fitwthpam)[1]+coef(fitwthpam)[2]*mtcars$wt+coef(fitwthpam)[3]*mtcars$hp
#g1=g
#g1=g1 + geom_line(aes(mtcars$wt, yhat)
#g1
fitwt = lm(mpg ~ wt, data = mtcars)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fitwt)[1], slope = coef(fitwt)[2], size = 2)
g1
#Reading coeffcients
summary(fitwt)$coef
#residuals
plot(fitwt,which=1)
#Fitted lines separating automatic and manual
fitwtam = lm(mpg ~ wt + factor(am), data = mtcars)
#Parallel Lines
summary(fitwtam)$coef
g2 = g
g2 = g2 + geom_abline(intercept = coef(fitwtam)[1], slope = coef(fitwtam)[2], size = 1) # Automatic
g2 = g2 + geom_abline(intercept = coef(fitwtam)[1] + coef(fitwtam)[3], slope = coef(fitwtam)[2], size = 1) #Manual
g2 = g2 + geom_abline(intercept = coef(fitwt)[1], slope = coef(fitwt)[2], size = 1) #Comparing to the first fit
g2
#Lines with different slopes and intercepts
plot(fitwtam,which=1)
#residuals
####
####
#Displacement Not considering automatic & manual
#Associated fitted line
#Plot the data coloring Automatic=0 (red) and Manual=1 (blue)
gdisp = ggplot(mtcars, aes(x = disp, y = mpg, colour = factor(am)))
gdisp = gdisp + geom_point(size = 6, colour = "black") + geom_point(size = 4)
gdisp = gdisp + xlab("Displacement (cu.in)") + ylab("mpg")
gdisp

fitdisp = lm(mpg ~ disp, data = mtcars)
g3 = gdisp
g3 = g3 + geom_abline(intercept = coef(fitdisp)[1], slope = coef(fitdisp)[2], size = 1)
g3
#Reading coeffcients
summary(fitdisp)$coef
#residuals
plot(fitdisp,which=1)
#Fitted lines separating automatic and manual
fitdispam = lm(mpg ~ disp + factor(am), data = mtcars)
#Parallel Lines
summary(fitdispam)$coef
g4 = gdisp
g4 = g4 + geom_abline(intercept = coef(fitdispam)[1], slope = coef(fitdispam)[2], size = 1) # Automatic
g4 = g4 + geom_abline(intercept = coef(fitdispam)[1] + coef(fitdispam)[3], slope = coef(fitdispam)[2], size = 1) #Manual
g4 = g4 + geom_abline(intercept = coef(fitdisp)[1], slope = coef(fitdisp)[2], size = 1) #Comparing to the first fit
g4
#Lines with different slopes and intercepts
plot(fitdispam,which=1) #residuals
###
#Back to Weight
#####
fitwtcylam = lm(mpg ~ wt + factor(cyl) + factor(am), data = mtcars)
summary(fitwtcyl)$coef
g5 = g
g5 = g5 + geom_abline(intercept = coef(fitwtcylam)[1], slope = coef(fitwtcylam)[2], size = 1) # Automatic
g5 = g5 + geom_abline(intercept = coef(fitwtcylam)[1] + coef(fitwtcylam)[3], slope = coef(fitwtcylam)[2], size = 1) #Manual
g5 = g5 + geom_abline(intercept = coef(fitwt)[1], slope = coef(fitwt)[2], size = 1) #Comparing to the first fit
g5
####
#Adding disp
fitwtcyldispam = lm(mpg ~ wt + factor(cyl) + factor(am) + disp, data = mtcars)
summary(fitwtcyldispam)$coef
#Adding disp + hp
fitwtcyldisphpam = lm(mpg ~ wt + factor(cyl) + factor(am) + disp + hp, data = mtcars)
summary(fitwtcyldisphpam)$coef
#Adding disp + hp + drat
fitwtcyldisphpdratam = lm(mpg ~ wt + factor(cyl) + factor(am) + disp + hp +drat, data = mtcars)
summary(fitwtcyldisphpdratam)$coef
#Adding disp + hp + drat
fitwtcyldisphpdratam = lm(mpg ~ wt + factor(cyl) + factor(am) + disp + hp +drat, data = mtcars)
summary(fitwtcyldisphpdratam)$coef
#Adding disp + hp + drat +vs
fitwtcyldisphpdratvsam = lm(mpg ~ wt + factor(cyl) + factor(am) + disp + hp +drat + factor(vs), data = mtcars)
summary(fitwtcyldisphpdratvsam)$coef
#Cascading models
anova(fitwt,fitwtam,fitwtcylam,fitwtcyldispam,fitwtcyldisphpam,fitwtcyldisphpdratam, fitwtcyldisphpdratvsam)
#Final model wt + cyl + hp 
fitwtcylhpam = lm(mpg ~ wt + factor(cyl) + factor(am) +  hp,  data = mtcars)
summary(fitwtcylhpam)$coef
#Final model wt + cyl + hp 
fitwthpam = lm(mpg ~ wt + hp + factor(am),  data = mtcars)
summary(fitwthpam)$coef
plot(fitwthpam,which=1)
#######
#removing am to the final model wt + cyl + hp 
fitwthp = lm(mpg ~ wt + hp,  data = mtcars)
summary(fitwthp)$coef
#Notice increase on standard error when am was introduced as well as the p values
plot(fitwthp,which=1) 
#################################
fitM <-lm(mpg[mtcars$am==1]~wt[mtcars$am==1]+hp[mtcars$am==1], mtcars)
fitA<-lm(mpg[mtcars$am==0]~wt[mtcars$am==0]+hp[mtcars$am==0],mtcars)
summary(fitA)$coef
summary(fitM)$coef
#Plotting results
fitMhp <-lm(mpg[mtcars$am==1]~hp[mtcars$am==1], mtcars)
fitAhp<-lm(mpg[mtcars$am==0]~hp[mtcars$am==0],mtcars)
summary(fitAhp)$coef
summary(fitMhp)$coef
#################################
fitMwt <-lm(mpg[mtcars$am==1]~wt[mtcars$am==1], mtcars)
fitAwt<-lm(mpg[mtcars$am==0]~wt[mtcars$am==0],mtcars)
summary(fitAwt)$coef
summary(fitMwt)$coef
##
#Plotting based on wt 
g8 = g
g8 = g8 + geom_abline(intercept = coef(fitMwt)[1], slope = coef(fitMwt)[2], size = 1, colour = "blue") # Manual
g8 = g8 + geom_abline(intercept = coef(fitAwt)[1], slope = coef(fitAwt)[2], size = 1, colour = "red") #Automatic
g8 = g8 + geom_abline(intercept = coef(fitwt)[1], slope = coef(fitwt)[2], size = 1) #Comparing to the first fit
g8
#####
fithp = lm(mpg ~ hp, data = mtcars)
summary(fithp)$coef
ghp = ggplot(mtcars, aes(x = hp, y = mpg, ymin=0, ymax=50, colour = factor(am)))
ghp = ghp + geom_point(size = 6, colour = "black") + geom_point(size = 4)
ghp = ghp + xlab("hp (1hp=736 Watts)") + ylab("mpg")
ghp
g9 = ghp
g9 = g9 + geom_abline(intercept = coef(fitAhp)[1], slope = coef(fitAhp)[2], size = 1, colour = "red") #Automatic
g9 = g9 + geom_abline(intercept = coef(fitMhp)[1], slope = coef(fitMhp)[2], size = 1, colour = "blue") # Manual
g9 = g9 + geom_abline(intercept = coef(fithp)[1], slope = coef(fithp)[2], size = 1) #Comparing to the first fit
g9
library(rgl)
#plot3d(mtcars$wtmt[cars$am==1],mtcars$hp[cars$am==1],mtcars$mpg[cars$am==1], col="red", pch = 21, size =8)
#plot3d(mtcars$wtmt[cars$am==0],mtcars$hp[cars$am==0],mtcars$mpg[cars$am==0], col="black", pch = 21, size =8)
plot3d(mtcars$wt,mtcars$hp,mtcars$mpg, col="blue", pch = 21, size =8)
yhat<-coef(fitwthpam)[1]+coef(fitwthpam)[2]*mtcars$wt+coef(fitwthpam)[3]*mtcars$hp
#lines3d(mtcars$wt,mtcars$hp,yhat)
points3d(mtcars$wt,mtcars$hp,yhat, col="red", pch = 21, size =8)
title3d('main', 'sub', 'xlab', 'ylab', 'zlab')
#residuals
plot(fitM,which=1)
plot(fitA,which=1)
round(hatvalues(fitM),4)
round(dfbeta(fitM),4)
round(dfbetas(fitM),4)
round(hatvalues(fitA),4)
round(dfbeta(fitA),4)
round(dfbetas(fitA),4)
round(hatvalues(fitwthp),4)
round(dfbeta(fitwthp),4)
round(dfbetas(fitwthp),4)
round(hatvalues(fitwthpam),4)
round(dfbeta(fitwthpam),4)
round(dfbetas(fitwthpam),4)
########
library(rgl)
######################
#Plot the data coloring Automatic=0 (red) and Manual=1 (blue)
gears = ggplot(mtcars, aes(x = gear, y = mpg, colour = factor(am)))
gears = gears + geom_point(size = 6, colour = "black") + geom_point(size = 4)
gears = gears + xlab("gears") + ylab("mpg")
gears

######
gears = ggplot(mtcars, aes(x = gear, y = mpg, colour = factor(am)))
gears = gears + geom_point(size = 6, colour = "black") + geom_point(size = 4)
gears = gears + xlab("gears") + ylab("mpg")
gears
#################################
fitMgear <-lm(mpg[mtcars$am==1]~gear[mtcars$am==1], mtcars)
fitAgear<-lm(mpg[mtcars$am==0]~gear[mtcars$am==0],mtcars)
summary(fitAgear)$coef
summary(fitMgear)$coef
fitgear <-lm(mpg ~ gear, mtcars)
fitgearam <-lm(mpg ~ gear+am, mtcars)
fitgearxam <-lm(mpg ~ gear*am, mtcars)
fitgearwt <-lm(mpg~gear+wt, mtcars)
fitgearwthp <-lm(mpg~gear+wt+hp, mtcars)
fitgearwtdisp <-lm(mpg~gear+wt+disp, mtcars)
fitgearwtam <-lm(mpg~gear+wt+am, mtcars)
fitgearwthpam <-lm(mpg~gear+wt+hp+am, mtcars)
fitgearwtdispam <-lm(mpg~gear+wt+disp+am, mtcars)
fitgearwtxam <-lm(mpg~gear*am+wt, mtcars)
fitgearwthpxam <-lm(mpg~gear*am+wt+hp, mtcars)
fitgearwtdispxam <-lm(mpg~gear*am+wt+disp, mtcars)
summary(fitgear)$coef
summary(fitgearam)$coef
summary(fitgearxam)$coef
summary(fitgearwt)$coef
summary(fitgearwthp)$coef
summary(fitgearwtdisp)$coef
summary(fitgearwtam)$coef
summary(fitgearwthpam)$coef
summary(fitgearwtdispam)$coef
summary(fitgearwtxam)$coef
summary(fitgearwthpxam)$coef
summary(fitgearwtdispxam)$coef

fitMgearwthp <-lm(mpg[mtcars$am==1]~wt[mtcars$am==1]+hp[mtcars$am==1]+gear[mtcars$am==1], mtcars)
fitAgearwthp<-lm(mpg[mtcars$am==0]~wt[mtcars$am==0]+hp[mtcars$am==0]+gear[mtcars$am==0],mtcars)
#####
#No Gear
#################################
#Starting with highest correlation weight
fitwt <-lm(mpg ~ wt, mtcars) #Good
fitwtam <-lm(mpg ~ wt+am, mtcars) #am decreased accuracy
#fitwtxam<-lm(mpg ~ wt*am, mtcars) #Bad
fitwtdisp <-lm(mpg~wt+disp, mtcars) #displacement made it worse
fitwtdispam <-lm(mpg~wt+disp+am, mtcars)#displacement+am made it worser
fitwthp <-lm(mpg~wt+hp, mtcars) #Good
fitwthpam <-lm(mpg~wt+hp+am, mtcars) #ok on hp, worse on wt
fitwthpamdrat <-lm(mpg~wt+hp+am+drat, mtcars) #ok on hp, worse on wt
fitwthpdrat <-lm(mpg~wt+hp+drat, mtcars) #ok on hp, worse on wt
fitwthpdratcyl <-lm(mpg~wt+hp+drat+cyl, mtcars) #Not good
###
#Assuming cylinders
fitwtcyl <-lm(mpg~wt+cyl, mtcars) #Good
fitwtcylhp<-lm(mpg~wt+cyl+hp, mtcars)
fitwtcylam <-lm(mpg~wt+cyl+am, mtcars) #ok on hp, worse on wt
fitwtcylamdrat <-lm(mpg~wt+cyl+am+drat, mtcars) #ok on hp, worse on wt
fitwtcyldrat <-lm(mpg~wt+cyl+drat, mtcars) #ok on hp, worse on wt

summary(fitwt)$coef
summary(fitwtam)$coef
summary(fitwtxam)$coef
summary(fitwtdisp)$coef
summary(fitwtdispam)$coef
summary(fitwtdispamhp)$coef
summary(fitwthp)$coef
summary(fitwthpam)$coef
summary(fitwthpamdrat)$coef
summary(fitwthpdrat)$coef
summary(fitwthpdratcyl)$coef
summary(fitwtcyl)$coef
summary(fitwtcylhp)$coef
summary(fitwtcylam)$coef
summary(fitwtcylamdrat)$coef
summary(fitwtcyldrat)$coef
##Model Comparison
anova(fitwt,fitwthp,fitwthpam,fitwthpamdrat)
anova(fitwt,fitwthp,fitwthpdrat)
anova(fitwt,fitwtcyl,fitwtcylam)
anova(fitwt,fitwtcyl,fitwtcyldrat)
anova(fitwt,fitwtcyl,fitwtcylhp)
#####################
fitMdrat <-lm(mpg[mtcars$am==1]~drat[mtcars$am==1], mtcars)
fitAdrat<-lm(mpg[mtcars$am==0]~drat[mtcars$am==0],mtcars)
summary(fitAdrat)$coef
summary(fitMdrat)$coef
fitdrat <-lm(mpg ~ drat, mtcars)
fitdratam <-lm(mpg ~ drat+am, mtcars)
fitdratxam <-lm(mpg ~ drat*am, mtcars)
fitdratwt <-lm(mpg~drat+wt, mtcars)
fitdratwthp <-lm(mpg~drat+wt+hp, mtcars)
fitdratwtdisp <-lm(mpg~drat+wt+disp, mtcars)
fitdratwtam <-lm(mpg~drat+wt+am, mtcars)
fitdratwthpam <-lm(mpg~drat+wt+hp+am, mtcars)
fitdratwtdispam <-lm(mpg~drat+wt+disp+am, mtcars)
fitdratwtxam <-lm(mpg~drat*am+wt, mtcars)
fitdratwthpxam <-lm(mpg~drat*am+wt+hp, mtcars)
fitdratwtdispxam <-lm(mpg~drat*am+wt+disp, mtcars)
summary(fitdrat)$coef
summary(fitdratam)$coef
summary(fitdratxam)$coef
summary(fitdratwt)$coef
summary(fitdratwthp)$coef
summary(fitdratwtdisp)$coef
summary(fitdratwtam)$coef
summary(fitdratwthpam)$coef
summary(fitdratwtdispam)$coef
summary(fitdratwtxam)$coef
summary(fitdratwthpxam)$coef
summary(fitdratwtdispxam)$coef
###
#####################
fitMdrat <-lm(mpg[mtcars$am==1]~drat[mtcars$am==1], mtcars)
fitAdrat<-lm(mpg[mtcars$am==0]~drat[mtcars$am==0],mtcars)
summary(fitAdrat)$coef
summary(fitMdrat)$coef
fitdrat <-lm(mpg ~ drat, mtcars)
fitdratam <-lm(mpg ~ drat+am, mtcars)
fitdratxam <-lm(mpg ~ drat*am, mtcars)
fitdrat <-lm(mpg~drat, mtcars)
fitdrathp <-lm(mpg~drat+hp, mtcars)
fitdratdisp <-lm(mpg~drat+disp, mtcars)
fitdratam <-lm(mpg~drat+am, mtcars)
fitdrathpam <-lm(mpg~drat+hp+am, mtcars)
fitdratdispam <-lm(mpg~drat+disp+am, mtcars)
fitdratxam <-lm(mpg~drat*am, mtcars)
fitdrathpxam <-lm(mpg~drat*am+hp, mtcars)
fitdratdispxam <-lm(mpg~drat*am+disp, mtcars)
summary(fitdrat)$coef
summary(fitdratam)$coef
summary(fitdratxam)$coef
summary(fitdrat)$coef
summary(fitdrathp)$coef
summary(fitdratdisp)$coef
summary(fitdratam)$coef
summary(fitdrathpam)$coef
summary(fitdratdispam)$coef
summary(fitdratxam)$coef
summary(fitdrathpxam)$coef
summary(fitdratdispxam)$coef
anova(fitwt,fitwtdisp,fitwtdispam)
anova(fitwt,fitwthp,fitwthpam,fitwthpamdrat)
anova(fitwt,fitwthp,fitwthpdrat)
anova(fitwt,fitwtcyl,fitwtcylam)
anova(fitwt,fitwtcyl,fitwtcyldrat)
anova(fitwt,fitwtcyl,fitwtcylhp)






