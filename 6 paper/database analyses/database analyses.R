####working directory####
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/QWERTY/9 15 stuff/database analyses")

##Comparison data:
##- Regression
##- control: letter frequency
##- main effects: rha, finger switch, LR switch
##- interactions: all above
##- DV: rating


####afinn####
##import file
afinndata = read.csv("afinn.csv")
afinnmodel1 = lm(Valence ~ letter_freq, 
                 data = afinndata)
afinnmodel2 = lm(Valence ~ letter_freq + rha + LR_switch + finger_switch, 
                 data = afinndata)
afinnmodel3 = lm(Valence ~ letter_freq + rha * LR_switch * finger_switch, 
                 data = afinndata)

summary(afinnmodel1)
summary(afinnmodel2)
summary(afinnmodel3)
anova(afinnmodel1, afinnmodel2, afinnmodel3)
##no sig interactions

####anew####
##import file
anewdata = read.csv("anew.csv")
anewmodel1 = lm(Valence.Mean ~ letter_freq, 
                 data = anewdata)
anewmodel2 = lm(Valence.Mean ~ letter_freq + rha + LR_switch + finger_switch, 
                 data = anewdata)
anewmodel3 = lm(Valence.Mean ~ letter_freq + rha * LR_switch * finger_switch, 
                 data = anewdata)

summary(anewmodel1)
summary(anewmodel2)
summary(anewmodel3)
anova(anewmodel1, anewmodel2, anewmodel3)
##no interactions

####dodds####
##import file
doddsdata = read.csv("dodds.csv")
doddsmodel1 = lm(happiness_average ~ letter_freq, 
                data = doddsdata)
doddsmodel2 = lm(happiness_average ~ letter_freq + rha + LR_switch + finger_switch, 
                data = doddsdata)
doddsmodel3 = lm(happiness_average ~ letter_freq + rha * LR_switch * finger_switch, 
                data = doddsdata)

summary(doddsmodel1)
summary(doddsmodel2)
summary(doddsmodel3)
anova(doddsmodel1, doddsmodel2, doddsmodel3)
##marginal LR_switch:finger_switch interaction

####ferstl####
##import file
ferstldata = read.csv("ferstl.csv")
ferstlmodel1 = lm(valence ~ letter_freq, 
                data = ferstldata)
ferstlmodel2 = lm(valence ~ letter_freq + rha + LR_switch + finger_switch, 
                data = ferstldata)
ferstlmodel3 = lm(valence ~ letter_freq + rha * LR_switch * finger_switch, 
                data = ferstldata)

summary(ferstlmodel1)
summary(ferstlmodel2)
summary(ferstlmodel3)
anova(ferstlmodel1, ferstlmodel2, ferstlmodel3)
##no interactions and no RHA ... might be too small, which is what he said

####warriner####
##import file
warrinerdata = read.csv("warriner.csv")
warrinermodel1 = lm(V.Mean.Sum ~ letter_freq, 
                data = warrinerdata)
warrinermodel2 = lm(V.Mean.Sum ~ letter_freq + rha + LR_switch + finger_switch, 
                data = warrinerdata)
warrinermodel3 = lm(V.Mean.Sum ~ letter_freq + rha * LR_switch * finger_switch, 
                data = warrinerdata)

summary(warrinermodel1)
summary(warrinermodel2)
summary(warrinermodel3)
anova(warrinermodel1, warrinermodel2, warrinermodel3)
##no interactions


