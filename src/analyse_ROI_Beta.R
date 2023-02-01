rm(list=ls()) #clean console

library(Rmisc)
library(stringr)

library(ez)
library(schoRsch)

# run anova assumption checks
library(dplyr)
library(tidyverse)
library(rstatix)

library(lsmeans)
library(afex)
#######################################################

# pathResults <- paste0('/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/',
#                       'Nonmetric/derivatives/cpp_spm-stats/group/',
#                       'task-Nonmetric_space-MNI_FWHM-6_MarsBar_roi/')
pathResults <- paste0('/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/',
                      'RhythmBlock/derivatives/cpp_spm-stats/group/',
                      'task-RhythmBlock_space-MNI_FWHM-6_MarsBar_roi/')
########

betas <- NA

dataNames = "^(.*Sphere5mm.*).csv$" # Sphere10mm Sphere5mm 
temp = list.files(path = pathResults,pattern=dataNames)
csvFileNb <- length(temp)     

resultFiles = list()
for (i in 1:csvFileNb) {
  fileToRead = paste(pathResults, temp[i], sep ='/')
  x  = read.csv(fileToRead, sep =',') # sep = ; or , or /
  resultFiles[i] = list(x)
}

# bind txt files using rbind comment
betas = do.call(rbind, resultFiles)

#### order things a bit for easy manipulation for plotting

#make sure subjects are factor
betas$sub <-as.factor(betas$sub)
betas$subLabel <-as.factor(betas$subLabel)
betas$expType <-as.factor(betas$expType)
betas$roi <-as.factor(betas$roi)

#shoten the label name for better figure x-axis naming
betas$expType<-ifelse(betas$expType == 'pitch_change', 'P4', 'P1')

#make roi order to call accordingly
betas$roi_order <- ifelse(betas$roi == 'lSTG', 1,
                          ifelse(betas$roi == 'rSTG', 2, 
                                 ifelse(betas$roi == 'lSMA', 3, 
                                        ifelse(betas$roi == 'rSMA', 4, 
                                               ifelse(betas$roi == 'lpreM', 5,
                                                      ifelse(betas$roi == 'rpreM', 6,
                                                             ifelse(betas$roi == 'lputa', 7,
                                                                    ifelse(betas$roi == 'rputa', 8,
                                                                           ifelse(betas$roi == 'lcereb', 9, 10)))))))))

betas$hemis <- ifelse((betas$roi_order %% 2) == 1, 'left', 'right')


# split and combine data so we have 1 column only for betas
simple <- betas
condition2<- betas

simple[,4] <- NULL
condition2[,3] <- NULL
simple$condition<-'simple'
condition2$condition<-'complex' 
# condition2$condition<-'nonmetric' 
names(condition2)[3] <- 'beta'
names(simple)[3] <- 'beta'
betas<-rbind(simple,condition2)

##### control point

# # check is nonmetric and simple are correctly labelled
# simpleSTG <- subset(simple, roi_order<3)
# 
# df2 <- summarySE(data = simpleSTG, 
#                 groupvars=c('expLabel','expType', 'roi_order','roi'),
#                 measurevar='beta')
# df2
# 
# nonmetricSTG <- subset(nonmetric, roi_order<3)
# 
# df3 <- summarySE(data = nonmetricSTG, 
#                  groupvars=c('expLabel','expType', 'roi_order','roi'),
#                  measurevar='beta')
# df3


# make a condition x roi column for plotting
betas$condRoi <- paste(betas[,'condition'], betas[,'roi'])
betas$condRoiExp <- paste(betas[,'condRoi'], betas[,'expType'])


# factor the levels
betas$expType<- as.factor(betas$expType)
betas$subType<- as.factor(betas$subType)
betas$hemis<- as.factor(betas$hemis)
betas$condition<-as.factor(betas$condition)
betas$condRoi<-as.factor(betas$condRoi)
betas$condRoiExp<-as.factor(betas$condRoiExp)

# redo subject labelling to show repeated subjects 
betas$subLabel<- as.character(betas$subLabel)

betas$subLabel[betas$subLabel == "sub-033"] <- "sub-001"
betas$subLabel[betas$subLabel == "sub-030"] <- "sub-002"
betas$subLabel[betas$subLabel == "sub-026"] <- "sub-003"
betas$subLabel[betas$subLabel == "sub-024"] <- "sub-004"
betas$subLabel[betas$subLabel == "sub-032"] <- "sub-007"
betas$subLabel[betas$subLabel == "sub-025"] <- "sub-012"
betas$subLabel[betas$subLabel == "sub-029"] <- "sub-011"
betas$subLabel[betas$subLabel == "sub-027"] <- "sub-015"
betas$subLabel[betas$subLabel == "sub-028"] <- "sub-021"
betas$subLabel[betas$subLabel == "sub-031"] <- "sub-023"

betas$subLabel<- as.factor(betas$subLabel)

#check data levels
summary(betas)
str(betas)


##############################################################
# STATS #
##############################################################


# let's separate the roi and hemisphere
betas$onlyroi <- sub('.', '', betas$roi)
betas$onlyroi <- as.factor(betas$onlyroi)

#### let's try anova
# we need to split P1 and P4 for anova
betasP4 <- subset(betas, expType =='P4')
betasP1 <- subset(betas, expType =='P1')



# separate the ROIs for 2 separate ANOVAs 
# - since we do not compare the ROIs, why should we put them into same anova anyways?!
betasP4.prem <- subset(betasP4, onlyroi=="preM")
betasP4.sma <- subset(betasP4, onlyroi=='SMA')
betasP4.stg <- subset(betasP4, onlyroi=='STG')
betasP4.puta <- subset(betasP4, onlyroi=='puta')
betasP4.cereb <- subset(betasP4, onlyroi=='cereb')

#########################################

# ANOVA

#########################################
# check anova assumptions
# test homogenity - conditions/hemisphere and rois separately
betasP4 %>%
  group_by(hemis, onlyroi) %>%
  levene_test(beta ~ condition)

# test normality - does not pass - not sure working right
betasP4 %>%
  group_by(hemis,roi, condition) %>%
  shapiro_test(beta)


# anova - not sure we are passing ok for assumptions
preM <- ezANOVA(data=betasP4.prem, 
                dv=.(beta), 
                wid=.(subLabel), 
                within =.(condition, hemis), 
                detailed=TRUE, 
                type=3) #

preM
anova_out(preM)

# hemisphere sig.


sma <- ezANOVA(data=betasP4.sma, 
               dv=.(beta), 
               wid=.(subLabel), 
               within =.(condition, hemis), 
               detailed=TRUE, 
               type=3) #

sma
anova_out(sma)

# hemisphere sig.


stg <- ezANOVA(data=betasP4.stg, 
               dv=.(beta), 
               wid=.(subLabel), 
               within =.(condition, hemis), 
               detailed=TRUE, 
               type=3) #

stg
anova_out(stg)

# hemisphere and hemis x condition sig.

# think about plotting the betas (by separating bad/good tappers?) for main effect and interaction?
#Plot the interaction.
# cue_by_flank_plot = ezPlot(
#   data = betasP4
#   , dv = .(beta)
#   , wid = .(subLabel)
#   , within = .(condition,onlyroi)
#   , between=.(subType)
#   , x = .(condition)
#   , split = .(onlyroi)
#   , x_lab = 'Hemisphere'
#   , y_lab = 'Betas'
#   , split_lab = 'Roi'
# )
# #Show the plot.
# print(cue_by_flank_plot)


##########

#### RUN STATS ON EACH ROI SEPARATELY with LMM - maybe P1 and P4 can be modeled together 
# checking NONMETRIC !== SIMPLE? 

#########################################

# LMM

#########################################
# afex package LMM

m1 <- mixed(beta ~ expType * condition * hemis * onlyroi * subType + (1|subLabel), data = betas)
m1

# pitch vs. fixed pitch : no difference
# condition : nonmetric vs. simple : no diff
# tappers: good vs. bad no diff

# let's split the data into pitch & rois

# stg
m1 <- mixed(beta ~  condition * hemis * subType + (1|subLabel), data = betasP4.stg)
m1
# hemisphere sig. 

# sma
m2 <- mixed(beta ~  condition * hemis * subType + (1|sub), data = betasP4.sma)
m2

# premotor
m3 <- mixed(beta ~  condition * hemis * subType + (1|sub), data = betasP4.prem)
m3

# putamen
m4 <- mixed(beta ~  condition * hemis * subType + (1|sub), data = betasP4.puta)
m4

# cerebellum
m5 <- mixed(beta ~  condition * hemis * subType + (1|sub), data = betasP4.cereb)
m5

# aggregate the hemispheres:
betas.expLabel <- as.factor(betas$expLabel)
a <- betas
head(a)
a[,13] <- NULL
a[,12] <- NULL
a[,9] <- NULL
a[,8] <- NULL
a[,5] <- NULL
head(a)
agg <- aggregate(a$beta, FUN = mean,
                    by = list(sub = a$sub, 
                              subLabel = a$subLabel,
                              expLabel = a$expLabel,
                              subType = a$subType, 
                              condition = a$condition, 
                              onlyroi = a$onlyroi))
names(agg)[7] <- 'beta'

m1 <- mixed(beta ~ expLabel * condition * onlyroi * subType + (1|subLabel), data = agg)
m1

# only rois sig.  --> not meaningful since we do not compare the rois 

# split the rois
# agg.roi<-subset(agg, onlyroi=='STG')
# agg.roi<-subset(agg, onlyroi=='SMA')
agg.roi<-subset(agg, onlyroi=='preM')
# agg.roi<-subset(agg, onlyroi=='puta')
# agg.roi<-subset(agg, onlyroi=='cereb')

m1 <- mixed(beta ~ expLabel * condition * subType + (1|subLabel), data = agg.roi)
m1


inter <- emmeans(m1, ~ expLabel,  lmer.df = "satterthwaite")
pairs(inter, adjust="bonferroni")
## pitch changes >> fixed pitch

inter <- emmeans(m1, ~ subType,  lmer.df = "satterthwaite")
pairs(inter, adjust="bonferroni")


#### we face with modeling issues - I think due to expLabel
## omit the expLabel


a <- betasP4 # or betasP1
head(a)
a[,13] <- NULL
a[,12] <- NULL
a[,9] <- NULL
a[,8] <- NULL
a[,5] <- NULL
head(a)
agg <- aggregate(a$beta, FUN = mean,
                 by = list(sub = a$sub, 
                           subLabel = a$subLabel,
                           subType = a$subType, 
                           condition = a$condition, 
                           onlyroi = a$onlyroi))
names(agg)[6] <- 'beta'

m1 <- mixed(beta ~ condition * onlyroi * subType + (1|subLabel), data = agg)
m1

# only roi sig.

# let's look at specific roi
# agg.roi<-subset(agg, onlyroi=='STG')
# agg.roi<-subset(agg, onlyroi=='SMA')
# agg.roi<-subset(agg, onlyroi=='preM')
# agg.roi<-subset(agg, onlyroi=='puta')
agg.roi<-subset(agg, onlyroi=='cereb')

m1 <- mixed(beta ~  condition * subType + (1|subLabel), data = agg.roi)
m1

# nothing sig. beside cereb ROI, condition is sig in complex vs. simple 



