rm(list=ls()) #clean console

library(ggplot2)
library(doBy)
library(cowplot)
library(Rmisc)
library(stringr)

library(ez)


#######################################################

pathResults <- paste0('/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/',
                      'Nonmetric/derivatives/cpp_spm-stats/group/',
                      'task-Nonmetric_space-MNI_FWHM-6_MarsBar_roi/')

########

betas <- NA

dataNames = "^(.*Sphere10mm.*).csv$" # Sphere10mm Sphere5mm 
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
nonmetric<- betas

simple[,4] <- NULL
nonmetric[,3] <- NULL
simple$condition<-'simple'
nonmetric$condition<-'nonmetric'
names(nonmetric)[3] <- 'beta'
names(simple)[3] <- 'beta'
betas<-rbind(simple,nonmetric)

# make a condition x roi column for plotting
betas$condRoi <- paste(betas[,'condition'], betas[,'roi'])
betas$condRoiExp <- paste(betas[,'condRoi'], betas[,'expType'])


# factor the levels
betas$expType<- as.factor(betas$expType)
# betas$expLabel<- as.factor(betas$expLabel)
betas$subType<- as.factor(betas$subType)
# betas$subTypeLabel<- as.factor(betas$subTypeLabel)
betas$hemis<- as.factor(betas$hemis)
betas$condition<-as.factor(betas$condition)
betas$condRoi<-as.factor(betas$condRoi)
betas$condRoiExp<-as.factor(betas$condRoiExp)

#check data levels
summary(betas)
str(betas)



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


######################### SEPARATE ROIs to plot FIGURES
# only some rois to plot - STG
subsetbetas = subset(betas,roi_order <3)
df <- summarySE(data = subsetbetas, 
                groupvars=c('expLabel','expType', 'roi_order', 'condition','condRoi', 'hemis'),
                measurevar='beta')
df

min(subsetbetas$beta)
max(subsetbetas$beta)
setlimit = c(-0.021,2.5) 
setbreak = c(-0.1,0,1,2)


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
  ylab("Beta Parameter Estimates (%)") +
  xlab("") +
  theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=8, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=11, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
 # scale_x_discrete(labels = c("Moving","Static","Moving","Static","Moving","Static","Moving","Static"))+
  # theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("#8a4a95","#6B6B6B")) + # yellow: #fbb03b", purple: "#954af0", gray: "#8c8c8f"
  theme(legend.position= c(.95, .95)) +
  theme(legend.title = element_blank())
fig

########################

# ADD HERE THE REST OF THE ROIS AT THE END - 11.08.2022

########################




################################################
# grouping according to the exp conditions
################################################
subsetbetas$condExp <- paste(subsetbetas[,'condition'], subsetbetas[,'expType'])
subsetbetas$RoiExp <- paste(subsetbetas[,'roi'], subsetbetas[,'expType'])


df <- summarySE(data = subsetbetas, 
                groupvars=c('expLabel','expType', 'roi_order','roi', 'condition','condExp', 'condRoi', 'RoiExp'),
                measurevar='beta')
df


# check is nonmetric and simple are correctly labelled
simpleSTG <- subset(simple, roi_order<3)

df2 <- summarySE(data = simpleSTG, 
                groupvars=c('expLabel','expType', 'roi_order','roi'),
                measurevar='beta')
df2

nonmetricSTG <- subset(nonmetric, roi_order<3)

df3 <- summarySE(data = nonmetricSTG, 
                 groupvars=c('expLabel','expType', 'roi_order','roi'),
                 measurevar='beta')
df3

##############################################################################
fig <- ggplot(data = subsetbetas, 
              aes(x = reorder(RoiExp, expLabel),
                  y = beta, 
                  color = condExp,
                  group = condition)) +
  geom_point(data=subsetbetas, 
             aes(x = reorder(RoiExp, expLabel), 
                 y = beta), 
             size = shapesize,
             position = jitter, shape = shapetype, stroke = shapestroke) + 
  
  stat_summary(aes(color=condExp), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.6,position = position_dodge(width=.75)) +
  
  theme_classic() +
  geom_errorbar(data = df, 
                aes(ymin = beta-se, ymax = beta+se, group = condition), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
  
  ggtitle("") +
  ylab("Beta Parameter Estimates (%)") +
  xlab("") +
  theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=8, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=11, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  # scale_x_discrete(labels = c("Moving","Static","Moving","Static","Moving","Static","Moving","Static"))+
  # theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("#8a4a95","#6B6B6B", '#e37df5', '#c7c5c5')) + # yellow: #fbb03b", purple: "#954af0", gray: "#8c8c8f"
  theme(legend.position= c(.9, .9)) +
  theme(legend.title = element_blank())
fig






### plot only motor (SMA +preM)
setlimit = c(-0.5,1.5) 
setbreak = c(-0.5,0,0.5, 0.1, 1.5)

subsetbetas = subset(betas,roi_order >= 3 & roi_order < 7)
df <- summarySE(data = subsetbetas, 
                groupvars=c('exp_order','expType', 'roi_order', 'condition','condRoi', 'hemis'),
                measurevar='beta')
df

fig <- ggplot(data = subsetbetas, 
              aes(x = reorder(condRoi, roi_order),
                  y = beta, 
                  color = reorder(expType,exp_order),
                  group = expType)) +
  geom_point(data=subsetbetas,aes(x = reorder(condRoi, roi_order), y = beta), size = shapesize,
             position = jitter, shape = shapetype, stroke = shapestroke) + 
  
  stat_summary(aes(color=reorder(expType,exp_order)), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.6,position = position_dodge(width=.75)) +
  
  theme_classic() +
  geom_errorbar(data = df, 
                aes(ymin = beta-se, ymax = beta+se, group = expType), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +
  
  ggtitle("") +
  ylab("Beta Parameter Estimates (%)") +
  xlab("") +
  theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=8, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=11, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  # scale_x_discrete(labels = c("Moving","Static","Moving","Static","Moving","Static","Moving","Static"))+
  # theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("#954af0","#8c8c8f")) + # yellow: #fbb03b"
  theme(legend.position="top")
fig



###### 05.08.2022
# ADD HERE A SEPARATION COLOR OR TEXTURE  FOR GOOD VS. BAD TAPPERS
#####

####################### ALL PLOTS IN ONE FIGURE
setlimit = c(-2,3) 
setbreak = c(-2,0,1,2,3)


df <- summarySE(data = betas, 
                groupvars=c('expType', 'exp_order','roi_order', 'condRoi', 'condition','hemis'),
                measurevar='beta')
df

fig <- ggplot(data = betas, 
              aes(x = reorder(condRoi,roi_order), 
                  y = beta, 
                  color = reorder(expType,exp_order),
                  group = group)) +
  geom_point(data=betas,aes(x = reorder(condRoi,roi_order), y = beta), size = shapesize,
             position = jitter, shape = shapetype, stroke = shapestroke) + 
  
  stat_summary(aes(color=reorder(expType,exp_order)), fun=mean, fun.min = mean, fun.max = mean, geom="crossbar", size=0.6, width=0.6,position = position_dodge(width=.75)) +
  
  theme_classic() +
  geom_errorbar(data = df, 
                aes(ymin = beta-se, ymax = beta+se, group = reorder(expType,exp_order)), 
                color = 'black',size=0.5, width=0.15, alpha = transparent, position = position_dodge(width=.75)) +

  ggtitle("") +
  ylab("Beta Parameter Estimates (%)") +
  xlab("") +
  theme(axis.text.x=element_text(size=8, face = 'bold', angle=0, colour='black')) + # face = 'bold', 
  theme(axis.text.y=element_text(size=8, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=11, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="left") +
  # scale_x_discrete(labels = c("Moving","Static","Moving","Static","Moving","Static","Moving","Static"))+
  # theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("#954af0","#fbb03b")) +
  theme(legend.position="none")
fig

filename <- paste(pathResults, 'BetaValues_MotionStatic_EBSC.png', sep = '')

# ggsave(filename, fig, dpi=300, width=8, height=2.4)

ggsave(filename, fig, dpi=300, width=4.5, height=3)

#### add fonts for ggpplot. 
filename <- paste(pathCosmoResults, 'BetaValues_MotionStatic_EBSC.pdf', sep = '')
ggsave(filename, fig, dpi=300, width=8, height=2.4)



##############################################################
# STATS #
##############################################################

#### let's try anova
summary(betas)

###### prepare for anova -- factorise the columns #####
betas$sub <- as.factor(betas$sub)
betas$roi <- as.factor(betas$roi)
betas$condition <- as.factor(betas$condition)
betas$group <- as.factor(betas$group)
betas$hemis <- as.factor(betas$hemis)


# let's separate the roi and hemisphere
betas$onlyroi <- ifelse(betas$roi == 'lV5', 'V5', 
                        ifelse(betas$roi == 'rV5', 'V5', 
                               ifelse(betas$roi == 'lPT', 'PT','PT')))

betas$onlyroi <- as.factor(betas$onlyroi)

## after separating lV5 into left and V5, it worked! 
my.anova <- ezANOVA(data=betas, 
                    dv=.(beta), 
                    wid=.(sub), 
                    within =.(condition, onlyroi, hemis), 
                    between=.(group), 
                    detailed=TRUE, 
                    type=3) #
#anova_out(my.anova)
my.anova



# separate the ROIs for 2 separate ANOVAs - since we do not compare the ROIs, why should we put them into same anova?!
betas.V5 <- subset(betas, onlyroi=="V5")
betas.PT <- subset(betas, onlyroi=='PT')


# now let's have separated anovas for each region
v5.anova <- ezANOVA(data=betas.V5, 
                    dv=.(beta), 
                    wid=.(sub), 
                    within =.(condition, hemis), 
                    between=.(group), 
                    detailed=TRUE, 
                    type=3) #

v5.anova
anova_out(v5.anova)

pt.anova <- ezANOVA(data=betas.PT, 
                    dv=.(beta), 
                    wid=.(sub), 
                    within =.(condition, hemis), 
                    between=.(group), 
                    detailed=TRUE, 
                    type=3) # return_aov = TRUE

pt.anova
anova_out(pt.anova)


######### HERE WE TIRED PAIRWISE T-test ##### be careful 

betas.V5.eb <- subset(betas.V5, group =='EB')
# pairwise.t.test(betas.V5.eb$beta, betas.V5.eb$condition, pool.sd = F, p.adjust.method="bonferroni")


##### pairwise PT Anova - group x condition
betas.PT.eb <- subset(betas.PT, group =='EB')
# pairwise.t.test(betas.PT$beta, betas.PT.eb$condition, pool.sd = F, p.adjust.method="bonferroni")

# pairwise.t.test(betas.PT$beta, betas.PT$group, pool.sd = F, p.adjust.method="bonferroni")

# pairwise.t.test(betas.PT$beta, betas.PT$condition, pool.sd = F, p.adjust.method="bonferroni")

### PT - group x hemisphere
df <- summarySE(data = betas.PT, 
                groupvars=c('group', 'group_order','roi_order', 'hemis'),
                measurevar='beta')
df
#Plot the interaction.
cue_by_flank_plot = ezPlot(
  data = betas.PT
  , dv = .(beta)
  , wid = .(sub)
  , within = .(condition,hemis)
  , between=.(group)
  , x = .(hemis)
  , split = .(group)
  , col = .(condition)
  , x_lab = 'Hemisphere'
  , y_lab = 'Betas'
  , split_lab = 'Group'
)
#Show the plot.
print(cue_by_flank_plot)



eb <- subset(betas.PT, group =='EB')
sc <- subset(betas.PT, group =='SC')

eb.left <- subset(eb, hemis =='left')
eb.right <- subset(eb, hemis =='right')

sc.left <- subset(sc, hemis =='left')
sc.right <- subset(sc, hemis =='right')

# hemisphere across groups
t.test(eb.left$beta,eb.right$beta)
# t = -1.1068, df = 58.101, p-value = 0.273
t.test(sc.left$beta,sc.right$beta)
# t = 1.5522, df = 56.105, p-value = 0.1262

# groups
t.test(eb.left$beta,sc.left$beta)
# t = -0.56813, df = 60.479, p-value = 0.572
t.test(eb.right$beta,sc.right$beta) # sig
# t = 2.4266, df = 61.518, p-value = 0.01819

# condition of no-interest
t.test(eb.left$beta,sc.right$beta)
# t = 1.0442, df = 60.167, p-value = 0.3006
t.test(eb.right$beta,sc.left$beta)
# t = 0.3702, df = 53.406, p-value = 0.7127

# FDR correction
p.values <- c(0.273, 0.1262, 0.572 , 0.01819)
p.adjust(p.values, method = "bonferroni", n = length(p.values))



### PT hemisphere x condition
m <- subset(betas.PT, condition =='motion')
s <- subset(betas.PT, condition =='static')

m.left <- subset(m, hemis =='left')
m.right <- subset(m, hemis =='right')

s.left <- subset(s, hemis =='left')
s.right <- subset(s, hemis =='right')

# hemisphere across groups
t.test(m.left$beta,m.right$beta)
# t = -1.1068, df = 58.101, p-value = 0.273
t.test(s.left$beta,s.right$beta)
# t = 1.5522, df = 56.105, p-value = 0.1262

# groups
t.test(eb.left$beta,sc.left$beta)
# t = -0.56813, df = 60.479, p-value = 0.572
t.test(eb.right$beta,sc.right$beta) # sig
# t = 2.4266, df = 61.518, p-value = 0.01819

t.test(eb.right$beta,sc.right$beta, paired = TRUE, alternative = "two.sided")


# condition of no-interest
t.test(eb.left$beta,sc.right$beta)
# t = 1.0442, df = 60.167, p-value = 0.3006
t.test(eb.right$beta,sc.left$beta)
# t = 0.3702, df = 53.406, p-value = 0.7127






#### tried using aov output from ezANOVA and lsmeans/emmeans --> does not work
library(lsmeans)
# note: lsmeans resides in emmeans now
# custom contrasting info:
# https://aosmith.rbind.io/2019/04/15/custom-contrasts-emmeans/

### let's try another anova package for calculating the pairwise t-tests
# https://www.psychologie.uni-heidelberg.de/ae/meth/team/mertens/blog/anova_in_r_made_easy.nb.html
library(afex)

# aov_ez(id = "id", dv = "log_rt", fhch, between = "task", within = c("length", "stimulus"))
ez.pt <- aov_ez(id = "sub",
       dv = "beta", 
       data = betas.PT, 
       between = "group",
       within = c("condition", "hemis")) # ,return = "nice"

ez.pt
# to compare with previous anova package
pt.anova


# hemisphere x condition interaction
interaction1 <- emmeans(ez.pt,specs = c("hemis","condition"))

contrast(interaction1, method="pairwise", adjust="bonferroni")

# group x condition interaction
interaction2 <- emmeans(ez.pt, ~ group * condition)
interaction2

eb.m = c(1,0,0,0)
sc.m = c(0,1,0,0)
eb.s = c(0,0,1,0)
sc.s = c(0,0,0,1)

# choose the contrast of interest
## only test specified tests
con <- list(
  "eb.m - sc.m" = eb.m - sc.m,
  "eb.s - sc.s" = eb.s - sc.s,
  "eb.m - eb.s" = eb.m - eb.s,
  "sc.m - sc.s" = sc.m - sc.s
)
contrast(interaction2, con, adjust = "bonferroni")




# hemisphere x group 
interaction3 <- emmeans(ez.pt, ~ group * hemis)
interaction3

contrast(interaction3, method="pairwise",adjust="bonferroni")

eb.l = c(1,0,0,0)
sc.l = c(0,1,0,0)
eb.r = c(0,0,1,0)
sc.r = c(0,0,0,1)

# choose the contrast of interest
## only test specified tests
con <- list(
  "eb.l - sc.l" = eb.l - sc.l,
  "eb.r - sc.r" = eb.r - sc.r,
  "eb.l - eb.r" = eb.l - eb.r,
  "sc.l - sc.r" = sc.l - sc.r
)
contrast(interaction3, con, adjust = "bonferroni")

#Plot the interaction.
cue_by_flank_plot = ezPlot(
  data = betas.PT
  , dv = .(beta)
  , wid = .(sub)
  , within = .(condition,hemis)
  , between=.(group)
  , x = .(hemis)
  , split = .(group)
  , x_lab = 'Hemisphere'
  , y_lab = 'Betas'
  , split_lab = 'Group'
)
#Show the plot.
print(cue_by_flank_plot)



# now v5
ez.v5 <- aov_ez(id = "sub",
                dv = "beta", 
                data = betas.V5, 
                between = "group",
                within = c("condition", "hemis")) # ,return = "nice"

ez.v5
v5.anova

# group x condition 
interaction1 <- emmeans(ez.v5, specs = c("group","condition"))
contrast(interaction1, method="pairwise")

# condition x hemisphere
interaction2 <- emmeans(ez.v5, specs = c("hemis","condition"))
contrast(interaction2, method="pairwise")

l.m = c(1,0,0,0)
r.m = c(0,1,0,0)
l.s = c(0,0,1,0)
r.s = c(0,0,0,1)

# choose the contrast of interest
## only test specified tests
con <- list(
  "l.m - l.s" = l.m - l.s,
  "r.m - r.s" = r.m - r.s
)
contrast(interaction2, con, adjust = "bonferroni")



#Plot the interaction.
cue_by_flank_plot = ezPlot(
  data = betas.V5
  , dv = .(beta)
  , wid = .(sub)
  , within = .(condition,hemis)
  , between=.(group)
  , x = .(condition)
  , split = .(hemis)
  , x_lab = 'Hemisphere'
  , y_lab = 'Betas'
  , split_lab = 'Group'
)
#Show the plot.
print(cue_by_flank_plot)



########## let's try behav - brain correlation #################################################
# read _jasp formatted .csv file - easier to correlate (already with behav data)

# for more formatting options:
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software#compute-correlation-matrix-in-r

data <- NA
dataNames = "*1728_jasp.csv"
temp = list.files(path = pathResults,pattern=dataNames)
fileToRead = paste(pathResults, temp, sep ='/')

data<-read.csv(fileToRead, sep =';') 


# clean the data
data[,11] <- NULL
data[,13] <- NULL


#divide across groups
sc.data <- subset(data,group =='SC')
eb.data <- subset(data,group =='EB')


# correlation matrix
library(Hmisc)
install.packages("corrplot")
library(corrplot)


#omit also first two columns for easing correlation matrix
sc.data[,1] <- NULL
sc.data[,1] <- NULL

sc.res <- rcorr(as.matrix(sc.data))
sc.res 


eb.data[,1] <- NULL
eb.data[,1] <- NULL

eb.res <- rcorr(as.matrix(eb.data))
eb.res 

# Extract the correlation coefficients & p-values
sc.res$r
sc.res$P

eb.res$r
eb.res$P

# vector-by-vector comparison - as an example to get one value
x = eb.data$Behav_Motion
y = eb.data$lV5Motion
res=cor.test(x, y, method="pearson" ); res[["p.value"]]


# alternatively, we can visualise it
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html#reorder-a-correlation-matrix
sc.fig = corrplot(sc.res$r, type = "upper",
         tl.col = "black", tl.srt = 45, diag = FALSE) # order = 'AOE' or 'hclust or 'alphabet', order = "alphabet",

eb.fig = corrplot(eb.res$r, type = "upper", 
         tl.col = "black", tl.srt = 45, diag = FALSE) # order = "alphabet"

# an idea could be separating motion and static columns for visualisation 
# but I do not think it would drastically improve the vis. 

#### try to add p-values - did not work ####
# FDR adjust for p-values -- not sure how to make it work...
fdr.p = p.adjust(sc.res$P, method ='fdr', n = length(sc.res$P))
eb.fdr.p = p.adjust(sc.res$P, method ='fdr', n = length(eb.res$P))


corrplot(sc.res$r, type="upper", order="AOE", 
         p.mat = sc.res$P, sig.level = 0.01, insig = "blank")

# save some of the output into table and figure
filename <- paste(pathResults, 'BetavsBehavior_SC_correlation_15112021.pdf', sep = '')
# save as table the output
df.sc.p =data.frame(sc.res$P)
df.sc.r =data.frame(sc.res$r)

# or actually put together EB and SC p and EB SC r into two different csv?
all.sc = rbind(df.sc.p, df.sc.r)

##### let's first organise the p-values and save them as .excel sheet
# take only the last two columns and correct the p-values with fdr
sc.fdr.p <- p.adjust(sc.res$P[0:8,9:10],  method ='fdr')

# only motion correction
sc.fdr.p <- p.adjust(sc.res$P[0:8,9],  method ='fdr')
sc.fdr.p 
sc.res$P[0:8,9]
sc.res$r[0:8,9]
# only static correction
sc.fdr.p <- p.adjust(sc.res$P[0:8,10],  method ='fdr')
sc.fdr.p 
sc.res$P[0:8,10]
sc.res$r[0:8,10]




