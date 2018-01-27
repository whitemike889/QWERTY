####working directory####
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/QWERTY/9 15 stuff/exp analyses")

####import files####
exp1data = read.csv("exp1_final.csv")
exp2data = read.csv("exp2_final.csv")

####data screening####
##accuracy checks
summary(exp1data)
table(exp1data$partno)
table(exp1data$speed)
hist(exp1data$speed)
exp1data$real_fake = factor(exp1data$real_fake,
                            levels = c("fake", "real"))

##missing data
##no missing data to replace

##outliers
with(exp1data, plot(speed, rating))
mahal = mahalanobis(exp1data[ , c(5,7)],
                    colMeans(exp1data[ , c(5,7)], na.rm = T),
                    cov(exp1data[ , c(5,7)], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(exp1data[ , c(5,7)]))
table(mahal < cutoff)
subset(exp1data, mahal > cutoff)

##assumptions
correlations = cor(exp1data[, c(5,7, 13, 14, 15, 17)], use="pairwise.complete.obs")
correlations
symnum(correlations)

##make the random stuff
random = rchisq(nrow(exp1data), 7)
#fake = lm(random~., data=exp1data[ , c(5, 7, 12, 13, 14, 15, 17)])
fake = lm(random~., data=exp1data[ , c(5, 7)])

##get the linearity plot
##create the standardized residuals
standardized = rstudent(fake)
qqnorm(standardized)
abline(0,1)

##multivariate normality
hist(standardized, breaks=15)

##homogeneity and homoscedaticity
fitvalues = scale(fake$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)

####MLM stuff####
library(nlme)

####separate real fake####
real = subset(exp1data,real_fake == "real")
fake = subset(exp1data,real_fake == "fake")

####centering the variables####
real$speed = scale(real$speed, scale = F)
real$lrswitch = scale(real$lrswitch, scale = F)
real$fingerswitch = scale(real$fingerswitch, scale = F)
real$rha = scale(real$rha, scale = F)

fake$speed = scale(fake$speed, scale = F)
fake$lrswitch = scale(fake$lrswitch, scale = F)
fake$fingerswitch = scale(fake$fingerswitch, scale = F)
fake$rha = scale(fake$rha, scale = F)

####intercept only model####
model1f = gls(rating ~ 1, 
             data = fake, 
             method = "ML", 
             na.action = "na.omit")
summary(model1f)

model1r = gls(rating ~ 1, 
             data = real, 
             method = "ML", 
             na.action = "na.omit")
summary(model1r)

####random intercept only model####
model2f = lme(rating ~ 1, 
             data = fake, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model2f)

model2r = lme(rating ~ 1, 
             data = real, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model2r)


####controls predictor models####
model3f = lme(rating ~ letterfreq, 
              data = fake, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|partno)
summary(model3f)

model3r = lme(rating ~ letterfreq, 
             data = real, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model3r)

####main effects models####
model4f = lme(rating ~ letterfreq + speed + lrswitch + fingerswitch + rha, 
             data = fake, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model4f)

model4r = lme(rating ~ letterfreq + speed + lrswitch + fingerswitch + rha, 
             data = real, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model4r)

####interaction models####
model5f = lme(rating ~ letterfreq + speed*lrswitch*fingerswitch*rha, 
             data = fake, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model5f)

anova(model1f, model2f, model3f, model4f, model5f)
anova(model1r, model2r, model3r, model4r, model5r)

####break up by speed fake####
##speed by RSA, speed by fingerswitch, fingerswitch by RSA
fake$lowspeed = fake$speed + sd(fake$speed)
fake$highspeed = fake$speed - sd(fake$speed)

model5fl = lme(rating ~ letterfreq + lowspeed*lrswitch*fingerswitch*rha, 
              data = fake, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|partno)
summary(model5fl)

model5fh = lme(rating ~ letterfreq + highspeed*lrswitch*fingerswitch*rha, 
               data = fake, 
               method = "ML", 
               na.action = "na.omit",
               random = ~1|partno)
summary(model5fh)

model5r = lme(rating ~ letterfreq + speed*lrswitch*fingerswitch*rha, 
             data = real, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model5r)

####break up speed REAL###
####speed:lrswitch:fingerswitch####
real$lowspeed = real$speed + sd(real$speed)
real$highspeed = real$speed - sd(real$speed)

model5rl = lme(rating ~ letterfreq + lowspeed*lrswitch*fingerswitch*rha, 
              data = real, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|partno)
summary(model5rl)

####double moderation####
##break down the low and average speed for hand switches
real$lowlrswitch = real$lrswitch + sd(real$lrswitch)
real$highlrswitch = real$lrswitch - sd(real$lrswitch)

##low speed low hand switch
model5rll = lme(rating ~ letterfreq + lowspeed*lowlrswitch*fingerswitch*rha, 
               data = real, 
               method = "ML", 
               na.action = "na.omit",
               random = ~1|partno)
summary(model5rll)

model5rlh = lme(rating ~ letterfreq + lowspeed*highlrswitch*fingerswitch*rha, 
                data = real, 
                method = "ML", 
                na.action = "na.omit",
                random = ~1|partno)
summary(model5rlh)

model5ral = lme(rating ~ letterfreq + speed*lowlrswitch*fingerswitch*rha, 
                data = real, 
                method = "ML", 
                na.action = "na.omit",
                random = ~1|partno)
summary(model5ral)

model5rah = lme(rating ~ letterfreq + speed*highlrswitch*fingerswitch*rha, 
                data = real, 
                method = "ML", 
                na.action = "na.omit",
                random = ~1|partno)
summary(model5rah)

model5rh = lme(rating ~ letterfreq + highspeed*lrswitch*fingerswitch*rha, 
               data = real, 
               method = "ML", 
               na.action = "na.omit",
               random = ~1|partno)
summary(model5rh)

####lrswitch:fingerswitch:rha ####
##break down by hand switching
##then by finger switching
real$lowfingerswitch = real$fingerswitch + sd(real$fingerswitch)
real$highfingerswitch = real$fingerswitch - sd(real$fingerswitch)

model5rl2 = lme(rating ~ letterfreq + speed*lowlrswitch*fingerswitch*rha, 
              data = real, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|partno)
summary(model5rl2)

model5rll2 = lme(rating ~ letterfreq + speed*lowlrswitch*lowfingerswitch*rha, 
                data = real, 
                method = "ML", 
                na.action = "na.omit",
                random = ~1|partno)
summary(model5rll2)

model5rlh2 = lme(rating ~ letterfreq + speed*lowlrswitch*highfingerswitch*rha, 
                data = real, 
                method = "ML", 
                na.action = "na.omit",
                random = ~1|partno)
summary(model5rlh2)

model5rh2 = lme(rating ~ letterfreq + speed*highlrswitch*fingerswitch*rha, 
               data = real, 
               method = "ML", 
               na.action = "na.omit",
               random = ~1|partno)
summary(model5rh2)

model5rhl2 = lme(rating ~ letterfreq + speed*highlrswitch*lowfingerswitch*rha, 
                data = real, 
                method = "ML", 
                na.action = "na.omit",
                random = ~1|partno)
summary(model5rhl2)

model5rhh2 = lme(rating ~ letterfreq + speed*highlrswitch*highfingerswitch*rha, 
                 data = real, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5rhh2)

####how they type####
model5howr = lme(rating ~ letterfreq + speed + lrswitch + fingerswitch + rha + howtype, 
                 data = real, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5howr)

model5howf = lme(rating ~ letterfreq + speed + lrswitch + fingerswitch + rha + howtype, 
                data = fake, 
                method = "ML", 
                na.action = "na.omit",
                random = ~1|partno)
summary(model5howf)


####exp 2####

####data screening####
##accuracy checks
summary(exp2data)
table(exp2data$partno)
table(exp2data$speed)
hist(exp2data$speed)
exp2data$realfake = factor(exp2data$realfake,
                            levels = c("fake", "real"))

##missing data
##no missing data to replace

##outliers
with(exp2data, plot(speed, rating))
mahal = mahalanobis(exp2data[ , c(5,7)],
                    colMeans(exp2data[ , c(5,7)], na.rm = T),
                    cov(exp2data[ , c(5,7)], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(exp2data[ , c(5,7)]))
table(mahal < cutoff)
subset(exp2data, mahal > cutoff)

##assumptions
correlations = cor(exp1data[, c(5,7, 13, 14, 15, 17)], use="pairwise.complete.obs")
correlations
symnum(correlations)

##make the random stuff
random = rchisq(nrow(exp2data), 7)
#fake = lm(random~., data=exp1data[ , c(5, 7, 12, 13, 14, 15, 17)])
fake = lm(random~., data=exp2data[ , c(5, 7)])

##get the linearity plot
##create the standardized residuals
standardized = rstudent(fake)
qqnorm(standardized)
abline(0,1)

##multivariate normality
hist(standardized, breaks=15)

##homogeneity and homoscedaticity
fitvalues = scale(fake$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)

####MLM stuff####
library(nlme)

####separate real fake####
real = subset(exp2data,realfake == "real")
fake = subset(exp2data,realfake == "fake")

####centering the variables####
real$speed = scale(real$speed, scale = F)
real$lrswitch = scale(real$lrswitch, scale = F)
real$fingerswitch = scale(real$fingerswitch, scale = F)
real$rha = scale(real$rha, scale = F)

fake$speed = scale(fake$speed, scale = F)
fake$lrswitch = scale(fake$lrswitch, scale = F)
fake$fingerswitch = scale(fake$fingerswitch, scale = F)
fake$rha = scale(fake$rha, scale = F)

####intercept only model####
model1f = gls(rating ~ 1, 
             data = fake, 
             method = "ML", 
             na.action = "na.omit")
summary(model1f)

model1r = gls(rating ~ 1, 
             data = real, 
             method = "ML", 
             na.action = "na.omit")
summary(model1r)

####random intercept only model####
model2f = lme(rating ~ 1, 
             data = fake, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model2f)

model2r = lme(rating ~ 1, 
             data = real, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|partno)
summary(model2r)

####controls predictor models####
model3f = lme(rating ~ letterfreq, 
              data = fake, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|partno)
summary(model3f)

model3r = lme(rating ~ letterfreq, 
              data = real, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|partno)
summary(model3r)

####main effects models####
model4f = lme(rating ~ letterfreq + speed + lrswitch + fingerswitch + rha, 
              data = fake, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|partno)
summary(model4f)

model4r = lme(rating ~ letterfreq + speed + lrswitch + fingerswitch + rha, 
              data = real, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|partno)
summary(model4r)

####interaction models####
model5f = lme(rating ~ letterfreq + speed*lrswitch*fingerswitch*rha, 
              data = fake, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|partno)
summary(model5f)

model5r = lme(rating ~ letterfreq + speed*lrswitch*fingerswitch*rha, 
              data = real,
              method = "ML", 
              na.action = "na.omit",
              random = ~1|partno)
summary(model5r)

anova(model1f, model2f, model3f, model4f, model5f)
anova(model1r, model2r, model3r, model4r, model5r)

####fake speed:lrswitch:fingerswitch:rha####
##speed break down first
####break up by speed fake####
##speed by RSA, speed by fingerswitch, fingerswitch by RSA
fake$lowspeed = fake$speed + sd(fake$speed)
fake$highspeed = fake$speed - sd(fake$speed)

model5fl = lme(rating ~ letterfreq + lowspeed*lrswitch*fingerswitch*rha, 
               data = fake, 
               method = "ML", 
               na.action = "na.omit",
               random = ~1|partno)
summary(model5fl)

model5fh = lme(rating ~ letterfreq + highspeed*lrswitch*fingerswitch*rha, 
               data = fake, 
               method = "ML", 
               na.action = "na.omit",
               random = ~1|partno)
summary(model5fh)

##break down average and high speeds
##left right switching
fake$lowlrswitch = fake$lrswitch + sd(fake$lrswitch)
fake$highlrswitch = fake$lrswitch - sd(fake$lrswitch)

model5fal = lme(rating ~ letterfreq + speed*lowlrswitch*fingerswitch*rha, 
              data = fake, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|partno)
summary(model5fal)

summary(model5f)

model5fah = lme(rating ~ letterfreq + speed*highlrswitch*fingerswitch*rha, 
              data = fake, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|partno)
summary(model5fah)

model5fhl = lme(rating ~ letterfreq + highspeed*lowlrswitch*fingerswitch*rha, 
                data = fake, 
                method = "ML", 
                na.action = "na.omit",
                random = ~1|partno)
summary(model5fhl)

summary(model5fh)

model5fhh = lme(rating ~ letterfreq + highspeed*highlrswitch*fingerswitch*rha, 
                data = fake, 
                method = "ML", 
                na.action = "na.omit",
                random = ~1|partno)
summary(model5fhh)

##average speed, low LR break down by finger switch
fake$lowfingerswitch = fake$fingerswitch + sd(fake$fingerswitch)
fake$highfingerswitch  = fake$fingerswitch - sd(fake$fingerswitch)

##avg speed low LR low FS
model5fall = lme(rating ~ letterfreq + speed*lowlrswitch*lowfingerswitch*rha, 
                data = fake, 
                method = "ML", 
                na.action = "na.omit",
                random = ~1|partno)
summary(model5fall)

##avg speed low LR avg FS
model5fala = lme(rating ~ letterfreq + speed*lowlrswitch*fingerswitch*rha, 
                 data = fake, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5fala)

##avg speed low LR high FS
model5falh = lme(rating ~ letterfreq + speed*lowlrswitch*highfingerswitch*rha, 
                 data = fake, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5falh)

##avg speed avg LR low FS
model5faal = lme(rating ~ letterfreq + speed*lrswitch*lowfingerswitch*rha, 
                 data = fake, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5faal)

##avg speed avg LR avg FS
summary(model5f)

##avg speed avg LR high FS
model5faah = lme(rating ~ letterfreq + speed*lrswitch*highfingerswitch*rha, 
                 data = fake, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5faah)

##avg speed high LR low FS
model5fahl = lme(rating ~ letterfreq + speed*highlrswitch*lowfingerswitch*rha, 
                 data = fake, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5fahl)

##avg speed high LR avg FS
model5faha = lme(rating ~ letterfreq + speed*highlrswitch*fingerswitch*rha, 
                 data = fake, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5faha)

##avg speed high LR high FS
model5fahh = lme(rating ~ letterfreq + speed*highlrswitch*highfingerswitch*rha, 
                 data = fake, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5fahh)

##high speed avg LR low FS
model5fhal = lme(rating ~ letterfreq + highspeed*lrswitch*lowfingerswitch*rha, 
                 data = fake, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5fhal)

##high speed avg LR avg FS
model5fhaa = lme(rating ~ letterfreq + highspeed*lrswitch*fingerswitch*rha, 
                 data = fake, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5fhaa)

##high speed avg LR high FS
model5fhah = lme(rating ~ letterfreq + highspeed*lrswitch*highfingerswitch*rha, 
                 data = fake, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5fhah)

##high speed high LR low FS
model5fhhl = lme(rating ~ letterfreq + highspeed*highlrswitch*lowfingerswitch*rha, 
                 data = fake, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5fhhl)

##high speed high LR avg FS
model5fhha = lme(rating ~ letterfreq + highspeed*highlrswitch*fingerswitch*rha, 
                 data = fake, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5fhha)

##high speed high LR high FS
model5fhhh = lme(rating ~ letterfreq + highspeed*highlrswitch*highfingerswitch*rha, 
                 data = fake, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5fhhh)

####real: lrswitch:fingerswitch:rha####
##break down by left right
real$lowlrswitch = real$lrswitch + sd(real$lrswitch)
real$highlrswitch = real$lrswitch - sd(real$lrswitch)

##low LR 
model5rl = lme(rating ~ letterfreq + speed*lowlrswitch*fingerswitch*rha, 
                 data = real, 
                 method = "ML", 
                 na.action = "na.omit",
                 random = ~1|partno)
summary(model5rl)

##avg LR
summary(model5r)

##high LR
model5rh = lme(rating ~ letterfreq + speed*highlrswitch*fingerswitch*rha, 
               data = real, 
               method = "ML", 
               na.action = "na.omit",
               random = ~1|partno)
summary(model5rh)

##break down FS
real$lowfingerswitch = real$fingerswitch + sd(real$fingerswitch)
real$highfingerswitch = real$fingerswitch - sd(real$fingerswitch)

##avg LR low FS
model5al = lme(rating ~ letterfreq + speed*lrswitch*lowfingerswitch*rha, 
               data = real, 
               method = "ML", 
               na.action = "na.omit",
               random = ~1|partno)
summary(model5al)

##avg LR avg FS
summary(model5r)

##avg LR high FS
model5ah = lme(rating ~ letterfreq + speed*lrswitch*highfingerswitch*rha, 
               data = real, 
               method = "ML", 
               na.action = "na.omit",
               random = ~1|partno)
summary(model5ah)

##high LR low FS
model5hl = lme(rating ~ letterfreq + speed*highlrswitch*lowfingerswitch*rha, 
               data = real, 
               method = "ML", 
               na.action = "na.omit",
               random = ~1|partno)
summary(model5hl)

##high LR avg FS
model5ha = lme(rating ~ letterfreq + speed*highlrswitch*fingerswitch*rha, 
               data = real, 
               method = "ML", 
               na.action = "na.omit",
               random = ~1|partno)
summary(model5ha)

##high LR high FS
model5hh = lme(rating ~ letterfreq + speed*highlrswitch*highfingerswitch*rha, 
               data = real, 
               method = "ML", 
               na.action = "na.omit",
               random = ~1|partno)
summary(model5hh)

