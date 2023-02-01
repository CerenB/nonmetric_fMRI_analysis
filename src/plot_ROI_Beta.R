rm(list=ls()) #clean console

library(ggplot2)
library(doBy)
library(cowplot)
library(Rmisc)
library(stringr)

#######################################################

# pathResults <- paste0('/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/',
#                       'Nonmetric/nonmetric_derivatives_cpp_spm-stats/group/',
#                       'task-Nonmetric_space-MNI_FWHM-6_MarsBar_roi/')
pathResults <- paste0('/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/',
                      'RhythmBlock/derivatives/cpp_spm-stats/group/',
                      'task-RhythmBlock_space-MNI_FWHM-6_MarsBar_roi/')

# RhythmBlock_AllRhythmvsSilenceCoord_Beta_GroupROIs_Smoothing6_Sphere5mm_202301201800
# RhythmBlock_AllRhythmvsSilenceCoord_Beta_GroupROIs_Smoothing6_Sphere10mm_202301201823
# Nonmetric_AllRhythmvsSilenceCoord_Beta_GroupROIs_Smoothing6_Sphere5mm_202301201939
# Nonmetric_AllRhythmvsSilenceCoord_Beta_GroupROIs_Smoothing6_Sphere10mm_202301231130

########

roiSize = '10' # 5 or 10mm roi sphere size 

betas <- NA

dataNames = "^(.*202301201823.*).csv$" # Sphere10mm Sphere5mm 
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
# betas$roi_order <- ifelse(betas$roi == 'lSTG', 1,
#                                  ifelse(betas$roi == 'rSTG', 2, 
#                                         ifelse(betas$roi == 'lSMA', 3, 
#                                                ifelse(betas$roi == 'rSMA', 4, 
#                                                       ifelse(betas$roi == 'lpreM', 5,
#                                                              ifelse(betas$roi == 'rpreM', 6,
#                                                                     ifelse(betas$roi == 'lputa', 7,
#                                                                           ifelse(betas$roi == 'rputa', 8,
#                                                                                  ifelse(betas$roi == 'lcereb', 9, 10)))))))))

betas$roi_order <- ifelse(betas$roi == 'lSTG', 1,
                          ifelse(betas$roi == 'rSTG', 2, 
                                 ifelse(betas$roi == 'SMA', 3, 
                                               ifelse(betas$roi == 'lpreM', 4,
                                                      ifelse(betas$roi == 'rpreM', 5,6)))))
betas$hemis <- ifelse((betas$roi_order %% 2) == 1, 'left', 'right')


# split and combine data so we have 1 column only for betas
simple <- betas
condition2<- betas

simple[,4] <- NULL
condition2[,3] <- NULL
simple$condition<-'simple'
condition2$condition<-'complex'
#condition2$condition<-'nonmetric'
names(condition2)[3] <- 'beta'
names(simple)[3] <- 'beta'
betas<-rbind(simple,condition2)

##### control point

# check is nonmetric and simple are correctly labelled
simpleSTG <- subset(simple, roi_order<3)

df2 <- summarySE(data = simpleSTG,
                groupvars=c('expLabel','expType', 'roi_order','roi'),
                measurevar='beta')
df2

condition2STG <- subset(condition2, roi_order<3)

df3 <- summarySE(data = condition2STG,
                 groupvars=c('expLabel','expType', 'roi_order','roi'),
                 measurevar='beta')
df3


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

betas$condExp <- paste(betas[,'condition'], betas[,'expType'])
betas$RoiExp <- paste(betas[,'roi'], betas[,'expType'])

# make df to be used in summary stats for plots' error bars
df <- summarySE(data = betas, 
                groupvars=c('expLabel','expType', 'roi_order', 'condRoi', 'condition','hemis'),
                measurevar='beta')
df


shapesize = 2
shapetype = 21
shapestroke = 1
transparent = 1 #0.6
jitter  = position_jitterdodge(0.2) # position_jitter(width=0.3)

# color code:
# trials: yellow: #fbb03b", purple: "#954af0", gray: "#8c8c8f"
nonmetricGray = "#3d8c55ff" # complex green= 3d8c55ff, nonmetricGrap = 6B6B6B
simplePurple = "#8a4a95"
shadedGray = '#c7c5c5'
sharedPurple = '#938096'

simpleOrange = '#ec5800'
nonmetricBlack= '#1b131c'
# https://encycolorpedia.com/8a4a95

# cond2 = 'Nonmetric'
cond2 = 'Complex'
######################### SEPARATE ROIs to plot FIGURES

########################

# 1. STG - varying pitch vs. fixed pitch

########################

subsetbetas = subset(betas,roi_order <3)
rois = 'STG'

df <- summarySE(data = subsetbetas, 
                groupvars=c('expLabel','expType', 'roi_order', 'condition','condRoi', 'hemis'),
                measurevar='beta')
df

min(subsetbetas$beta)
max(subsetbetas$beta)
setlimit = c(-0.3,3.5) 
setbreak = c(-0.5,0,0.5,1,1.5,2, 2.5, 3)


# Grouping according to pitch vs. no-pitch
fig <- ggplot(data = subsetbetas, 
              aes(x = reorder(condRoi, roi_order),
                  y = beta, 
                  color = reorder(expType,expLabel),
                  group = reorder(expType,expLabel))) +
  geom_point(data=subsetbetas,aes(x = reorder(condRoi, roi_order), y = beta), size = shapesize,
             position = jitter, shape = shapetype, stroke = shapestroke) + 
  
  stat_summary(aes(color=reorder(expType,expLabel)), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.6,position = position_dodge(width=.75)) +
  
  theme_classic() +
  geom_errorbar(data = df, 
                aes(ymin = beta-se, ymax = beta+se, group = reorder(expType,expLabel)), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
  
  ggtitle("") +
  ylab("Beta Parameter Estimates (a.u.)") +
  xlab("") +
  theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=8, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=11, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  scale_x_discrete(labels = c(paste0(cond2,"\nL STG"),"Simple\nL STG",paste0(cond2,"\nR STG"),"Simple\nR STG"))+ #"Nonmetric\nL STG"
  scale_color_manual(name = 'Pitch', labels = c("Varying", "Fixed"), values=c(simpleOrange,nonmetricBlack)) + 
  theme(legend.position= c(.9, .95)) 
  #theme(legend.title = element_blank()) 
fig


##### SAVE
filename <- paste0(pathResults, 'BetaValues_pitch_5mm_', rois, '.png')
ggsave(filename, fig, dpi=300, width=5, height=2.5) # 1024 x 512 
#ggsave(filename, fig, dpi=300, width=6, height=2.4)



########################

# 2. SMA preMotor - varying pitch vs. fixed pitch

########################
setlimit = c(-0.5,2) 
setbreak = c(-0.5,0,0.5, 1, 1.5)

subsetbetas = subset(betas,roi_order >= 3 & roi_order < 7)
rois = 'SMA_preMotor'

df <- summarySE(data = subsetbetas, 
                groupvars=c('expLabel','expType', 'roi_order', 'condition','condRoi', 'hemis'),
                measurevar='beta')
df

min(subsetbetas$beta)
max(subsetbetas$beta)



# Grouping according to pitch vs. no-pitch
fig <- ggplot(data = subsetbetas, 
              aes(x = reorder(condRoi, roi_order),
                  y = beta, 
                  color = reorder(expType,expLabel),
                  group = reorder(expType,expLabel))) +
  geom_point(data=subsetbetas,aes(x = reorder(condRoi, roi_order), y = beta), size = shapesize,
             position = jitter, shape = shapetype, stroke = shapestroke) + 
  stat_summary(aes(color=reorder(expType,expLabel)), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.6,position = position_dodge(width=.75)) +
  theme_classic() +
  geom_errorbar(data = df, 
                aes(ymin = beta-se, ymax = beta+se, group = reorder(expType,expLabel)), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
  ggtitle("") +
  ylab("Beta Parameter Estimates (a.u.)") +
  xlab("") +
  theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + 
  theme(axis.text.y=element_text(size=8, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=11, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  scale_x_discrete(labels = c(paste0(cond2,"\nL SMA"),"Simple\nL SMA",paste0(cond2,"\nR SMA"),"Simple\nR SMA",paste0(cond2,"\nL preMotor"),"Simple\nL preMotor",paste0(cond2,"\nR preMotor"),"Simple\nR preMotor"))+ 
  scale_color_manual(name = 'Pitch', labels = c("Varying", "Fixed"), values=c(simpleOrange,nonmetricBlack)) + 
  theme(legend.position= c(.9, .95)) 
  # theme(legend.title = element_blank())
fig

###### save
filename <- paste0(pathResults, 'BetaValues_pitch_5mm_', rois, '.png')
ggsave(filename, fig, dpi=300, width=10, height=2.5) # 1024 x 512 



################################################

# 3. STG - Nonmetric vs. simple

################################################
# grouping according to the exp conditions

subsetbetas = subset(betas,roi_order <3)
rois = 'STG'

df <- summarySE(data = subsetbetas, 
                groupvars=c('expLabel','expType', 'roi_order','roi', 'condition','condExp', 'condRoi', 'RoiExp'),
                measurevar='beta')
df

setlimit = c(-0.3,3.5) 
setbreak = c(-0.5,0,0.5,1,1.5,2, 2.5, 3)

fig <- ggplot(data = subsetbetas, 
              aes(x = reorder(RoiExp, expLabel),
                  y = beta, 
                  color = reorder(condExp,expLabel),
                  group = condition)) +
  geom_point(data=subsetbetas, 
             aes(x = reorder(RoiExp, expLabel), 
                 y = beta), 
             size = shapesize,
             position = jitter, shape = shapetype, stroke = shapestroke) + 
  stat_summary(aes(color=reorder(condExp,expLabel)), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.6,position = position_dodge(width=.75)) +
  theme_classic() +
  geom_errorbar(data = df, 
                aes(ymin = beta-se, ymax = beta+se, group = condition), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
  ggtitle("") +
  ylab("Beta Parameter Estimates (a.u.)") +
  xlab("") +
  theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=8, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=11, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  scale_x_discrete(labels = c("L STG\nVarying Pitch","R STG\nVarying Pitch","L STG\nFixed Pitch","R STG\nFixed Pitch"))+ 
  scale_color_manual(name = 'Conditions with Pitch', labels = c(paste0(cond2,"Varying"), "Simple Varying", paste0(cond2,"Fixed"), "Simple Fixed"),values=c(nonmetricGray, simplePurple, shadedGray, sharedPurple)) + 
  theme(legend.position= c(.85, .85)) +
  theme(legend.text=element_text(size=7)) +
  theme(legend.title=element_text(size=8))
  # theme(legend.title = element_blank())
fig

filename <- paste0(pathResults, 'BetaValues_NonmetricSimple_5mm_', rois, '.png')
ggsave(filename, fig, dpi=300, width=5, height=2.75) # 1024 x 5xx 
#ggsave(filename, fig, dpi=300, width=6, height=2.4)



################################################

# 4. SMA premotor - Nonmetric vs. simple

################################################
# grouping according to the exp conditions

setlimit = c(-0.5,2) 
setbreak = c(-0.5,0,0.5, 1, 1.5)

subsetbetas = subset(betas,roi_order >= 3 & roi_order < 7)
rois = 'SMA_preMotor'

df <- summarySE(data = subsetbetas, 
                groupvars=c('expLabel','expType', 'roi_order','roi', 'condition','condExp', 'condRoi', 'RoiExp'),
                measurevar='beta')
df


fig <- ggplot(data = subsetbetas, 
              aes(x = reorder(RoiExp, expLabel),
                  y = beta, 
                  color = reorder(condExp,expLabel),
                  group = condition)) +
  geom_point(data=subsetbetas, 
             aes(x = reorder(RoiExp, expLabel), 
                 y = beta), 
             size = shapesize,
             position = jitter, shape = shapetype, stroke = shapestroke) + 
  stat_summary(aes(color=reorder(condExp,expLabel)), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.6,position = position_dodge(width=.75)) +
  theme_classic() +
  geom_errorbar(data = df, 
                aes(ymin = beta-se, ymax = beta+se, group = condition), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
  ggtitle("") +
  ylab("Beta Parameter Estimates (a.u.)") +
  xlab("") +
  theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=8, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=11, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  scale_x_discrete(labels = c("L preMotor\nVarying Pitch","L SMA\nVarying Pitch","R preMotor\nVarying Pitch","R SMA\nVarying Pitch", "L preMotor\nFixed Pitch","L SMA\nFixed Pitch","R preMotor\nFixed Pitch","R SMA\nFixed Pitch"))+
  scale_color_manual(values=c(nonmetricGray, simplePurple, shadedGray, sharedPurple)) +
  theme(legend.position= "none")
fig

filename <- paste0(pathResults, 'BetaValues_NonmetricSimple_5mm_', rois, '.png')
ggsave(filename, fig, dpi=300, width=10, height=2.5) # 1024 x 512 




########################

# 5. putamen & cerebellum - varying pitch vs. fixed pitch

########################
setlimit = c(-0.5,0.5) 
setbreak = c(-0.5,0,0.5)

subsetbetas = subset(betas,roi_order >= 7 & roi_order < 11)
rois = 'puta_cerebellum'

df <- summarySE(data = subsetbetas, 
                groupvars=c('expLabel','expType', 'roi_order', 'condition','condRoi', 'hemis'),
                measurevar='beta')
df

min(subsetbetas$beta)
max(subsetbetas$beta)

# Grouping according to pitch vs. no-pitch
fig <- ggplot(data = subsetbetas, 
              aes(x = reorder(condRoi, roi_order),
                  y = beta, 
                  color = reorder(expType,expLabel),
                  group = reorder(expType,expLabel))) +
  geom_point(data=subsetbetas,aes(x = reorder(condRoi, roi_order), y = beta), size = shapesize,
             position = jitter, shape = shapetype, stroke = shapestroke) + 
  stat_summary(aes(color=reorder(expType,expLabel)), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.6,position = position_dodge(width=.75)) +
  theme_classic() +
  geom_errorbar(data = df, 
                aes(ymin = beta-se, ymax = beta+se, group = reorder(expType,expLabel)), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
  ggtitle("") +
  ylab("Beta Parameter Estimates (a.u.)") +
  xlab("") +
  theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + 
  theme(axis.text.y=element_text(size=8, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=11, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  scale_x_discrete(labels = c(paste0(cond2,"\nL Putamen"),"Simple\nL Putamen",paste0(cond2,"\nR Putamen"),"Simple\nR Putamen",paste0(cond2,"\nL Cereb"),"Simple\nL Cereb",paste0(cond2,"\nR Cereb"),"Simple\nR Cereb"))+
  scale_color_manual(name = 'Pitch', labels = c("Varying", "Fixed"), values=c(simpleOrange,nonmetricBlack)) + 
  # theme(legend.position= c(.9, .95))
# theme(legend.title = element_blank())
  theme(legend.position= "none")
fig

###### save
filename <- paste0(pathResults, 'BetaValues_pitch_', rois, '.png')
ggsave(filename, fig, dpi=300, width=10, height=2.5) # 1024 x 512 





################################################

# 6. putamen & cerebellum - Nonmetric vs. simple

################################################
# grouping according to the exp conditions

setlimit = c(-0.5,0.5) 
setbreak = c(-0.5,0,0.5)

subsetbetas = subset(betas,roi_order >= 7 & roi_order < 11)
rois = 'puta_cerebellum'

df <- summarySE(data = subsetbetas, 
                groupvars=c('expLabel','expType', 'roi_order','roi', 'condition','condExp', 'condRoi', 'RoiExp'),
                measurevar='beta')
df

min(subsetbetas$beta)
max(subsetbetas$beta)

fig <- ggplot(data = subsetbetas, 
              aes(x = reorder(RoiExp, expLabel),
                  y = beta, 
                  color = reorder(condExp,expLabel),
                  group = condition)) +
  geom_point(data=subsetbetas, 
             aes(x = reorder(RoiExp, expLabel), 
                 y = beta), 
             size = shapesize,
             position = jitter, shape = shapetype, stroke = shapestroke) + 
  stat_summary(aes(color=reorder(condExp,expLabel)), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.6,position = position_dodge(width=.75)) +
  theme_classic() +
  geom_errorbar(data = df, 
                aes(ymin = beta-se, ymax = beta+se, group = condition), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
  ggtitle("") +
  ylab("Beta Parameter Estimates (a.u.)") +
  xlab("") +
  theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=8, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=11, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  scale_x_discrete(labels = c("L Cereb\nVarying Pitch","L Putamen\nVarying Pitch","R Cereb\nVarying Pitch","R Putamen\nVarying Pitch", "L Cereb\nFixed Pitch","L Putamen\nFixed Pitch","R Cereb\nFixed Pitch","R Putamen\nFixed Pitch"))+
  scale_color_manual(values=c(nonmetricGray, simplePurple, shadedGray, sharedPurple)) +
  theme(legend.position= "none")
fig

filename <- paste0(pathResults, 'BetaValues_NonmetricSimple_', rois, '.png')
ggsave(filename, fig, dpi=300, width=10, height=2.5) # 1024 x 512 


################################################

# 7. ALL ROIs Together - Nonmetric vs. simple

################################################

setlimit = c(-0.5,3.2) 
setbreak = c(-0.5,0,0.5, 1 , 1.5, 2, 2.5, 3)

subsetbetas = subset(betas,expType == 'P4')
rois = 'allROI'

df <- summarySE(data = subsetbetas, 
                groupvars=c('expLabel','expType', 'roi_order','roi', 'condition','condExp', 'condRoi', 'RoiExp'),
                measurevar='beta')
df

min(subsetbetas$beta)
max(subsetbetas$beta)

fig <- ggplot(data = subsetbetas, 
              aes(x = reorder(RoiExp, roi_order),
                  y = beta, 
                  color = reorder(condExp,expLabel),
                  group = condition)) +
  geom_point(data=subsetbetas, 
             aes(x = reorder(RoiExp, roi_order), 
                 y = beta), 
             size = shapesize,
             position = jitter, shape = shapetype, stroke = shapestroke) + 
  stat_summary(aes(color=reorder(condExp,expLabel)), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.6,position = position_dodge(width=.75)) +
  theme_classic() +
  geom_errorbar(data = df, 
                aes(ymin = beta-se, ymax = beta+se, group = condition), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
  ggtitle("") +
  ylab("Beta Parameter Estimates (a.u.)") +
  xlab("") +
  theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=8, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=11, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  scale_x_discrete(labels = c("L STG","R STG","L SMA","R SMA", "L Premotor","R Premotor","L Putamen","R Putamen", "L Cereb","R Cereb"))+
  scale_color_manual(name = 'Conditions', labels = c(cond2, "Simple"),values=c(nonmetricGray, simplePurple)) + 
  theme(legend.position= c(.85, .85)) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.title=element_text(size=9))
fig


filename <- paste0(pathResults, 'BetaValues_NonmetricSimple_5mm_', rois, '.png')
ggsave(filename, fig, dpi=300, width=10, height=4) # 1024 x 512 






################################################

# ALL ROIs Together - Nonmetric vs. simple -- poster ICAC

################################################

# let's separate the roi and hemisphere
betas$onlyroi <- ifelse(betas$roi == 'SMA' , 'SMA',
                        ifelse(betas$roi == 'cereb', 'cereb',(sub('.', '', betas$roi))))
betas$onlyroi <- as.factor(betas$onlyroi)

subsetbetas = subset(betas,expType == 'P4')

head(subsetbetas)
# WHAT TO DO NEXT:
# add the conditions you use in the plotting in this aggregation 
a <- subsetbetas
#
agg <- aggregate(a$beta, FUN = mean,
                   by = list(sub = a$sub,
                             subLabel = a$subLabel,
                             expLabel = a$expLabel,
                             subType = a$subType,
                             condition = a$condition,
                             onlyroi = a$onlyroi))
names(agg)[7] <- 'beta'
names(agg)[6] <- 'roi'

# add roi order for plotting order
agg$roi_order <-  ifelse(agg$roi == 'STG', 1,
                            ifelse(agg$roi == 'preM', 2, 
                                   ifelse(agg$roi == 'SMA', 3,
                                          ifelse(agg$roi == 'cereb', 4,5))))

  
# reassign back 

# color code:
nonmetricGray = "#3d8c55ff" # complex green= 3d8c55ff, nonmetricGrap = 6B6B6B
simplePurple = "#8a4a95"


# cond2 = 'Nonmetric'
cond2 = 'Complex'

# shapesize = 1.5
# shapetype = 21
# shapestroke = 1
# transparent = 1 #0.6
# jitter  = position_jitterdodge(0.2) # position_jitter(width=0.3)
# 
# # to find the limits of the plot
# min(agg$beta)
# max(agg$beta)

# setlimit = c(-0.5,3) 
# setbreak = c(-0.5,0, 1 , 2, 3)
# 
# 
# rois = 'allROI'
# 
# df <- summarySE(data = agg, 
#                 groupvars=c('roi_order','roi', 'condition', 'subType'),
#                 measurevar='beta')
# df
# 
# 
# 
# fig <- ggplot(data = agg, 
#               aes(x = reorder(roi, roi_order),
#                   y = beta, 
#                   color = condition,
#                   group = condition)) +
#   geom_point(data=agg, 
#              aes(x = reorder(roi, roi_order), 
#                  y = beta), 
#              size = shapesize,
#              position = jitter, shape = shapetype, stroke = shapestroke) + 
#   stat_summary(aes(color = condition), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.6,position = position_dodge(width=.75)) +
#   theme_classic() +
#   geom_errorbar(data = df, 
#                 aes(ymin = beta-se, ymax = beta+se, group = condition), 
#                 color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
#   ggtitle("") +
#   ylab("") +
#   xlab("") +
#   theme(axis.text.x=element_text(size=12, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
#   theme(axis.text.y=element_text(size=12, angle=0, colour='black')) +
#   theme(axis.title.y=element_text(size=10, angle=90, colour='black')) +
#   scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
#   scale_x_discrete(labels = NULL )+
#   scale_color_manual(name = '', labels = c(cond2, "Simple"),values=c(nonmetricGray, simplePurple)) + 
#   theme(legend.position= "none")
#   # theme(legend.position= c(.85, .85)) +
#   # theme(legend.text=element_text(size=8)) +
#   # theme(legend.title=element_text(size=9))
# fig
# 
# 
# filename <- paste0(pathResults, 'BetaValues_NonmetricSimple_5mm_poster_noHemis_', rois, '.png')
# # ggsave(filename, fig, dpi=300, width=15, height=6, units='cm') 
# ggsave(filename, fig, dpi=300, width=6, height=3) # 1024 x 512
# 
# 




#### ADD A SEPARATION COLOR OR TEXTURE/shape  FOR GOOD VS. BAD TAPPERS

shapesize = 1
shapetype = 21
shapestroke = 1
transparent = 1 #0.6
jitter  = position_jitterdodge(0.1) # position_jitter(width=0.3)

# to find the limits of the plot
min(agg$beta)
max(agg$beta)

setlimit = c(-0.3,2.5) 
setbreak = c(-0.3,0, 1 , 2, 3)

rois = '5ROIs'


agg$condSubType = paste(agg[,'condition'], agg[,'subType'])

df <- summarySE(data = agg, 
                groupvars=c('roi_order','roi', 'condition', 'subType', 'condSubType'),
                measurevar='beta')
df

df2 <- summarySE(data = agg, 
                groupvars=c('roi_order','roi', 'condSubType'),
                measurevar='beta')
df2

# colors 
nonmetricGrayBad = "#9ec5aa" # complex green= 3d8c55ff, nonmetricGrap = 6B6B6B
nonmetricGrayGood = "#3d8c55ff" 
simplePurpleBad = "#c4a4c9"
simplePurpleGood = "#8a4a95"
# conditions
category2 = 'Complex'
category1 = 'Simple'

cond1= paste0(category2," Bad Tapper")
cond2 = paste0(category2,' Good Tapper')
cond3 = paste0(category1,' Bad Tapper')
cond4 = paste0(category1,' Good Tapper')

fig <- ggplot(data = agg, 
              aes(x = reorder(roi, roi_order),
                  y = beta, 
                  color = condSubType,
                  group = condSubType)) +
  geom_point(data=agg, 
             aes(x = reorder(roi, roi_order), 
                 y = beta), 
             size = shapesize,
             position = jitter, shape = shapetype, stroke = shapestroke) + 
  stat_summary(aes(color = condSubType), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.6, position = position_dodge(width=.75)) +
  theme_classic() +
  geom_errorbar(data = df, 
                aes(ymin = beta-se, ymax = beta+se, group = condSubType), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
  ggtitle("") +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=12, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=12, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=10, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  # scale_x_discrete(labels = NULL )+
  scale_color_manual(name = '', labels = c(cond1, cond2, cond3, cond4), values=c(nonmetricGrayBad, nonmetricGrayGood, simplePurpleBad, simplePurpleGood)) + 
  # theme(legend.position= "none")
  theme(legend.position= c(.85, .85)) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.title=element_text(size=9))
fig


filename <- paste0(pathResults, 'BetaValues_AllRhythmvsSilence_',category2, 'Simple_', roiSize, 'mm_noHemis_', rois, '.png')
# ggsave(filename, fig, dpi=300, width=15, height=6, units='cm') 
ggsave(filename, fig, dpi=300, width=6, height=3) # 1024 x 512


