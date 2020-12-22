setwd('C:/Users/Beefgallo/Dropbox/UIUC/ASD-SI/Experiment 2 results/EXP2RData') # folder where data is located

library(readxl)
library(tidyr)
library(plyr)
library(ggplot2)
library(visreg)
library(dplyr)
library(purrr)
library(lme4)
library(corrr)
library(corrplot)
library(Matrix)
library(Rcpp)
library(rJava)
library(xlsx)
library(psy)
library(lme4)

DemoData = read_excel("dataforR3.xlsx", sheet=3) #demographic data (gender, age, languages, English proficiency)
#for some unknown reason, DemoData has 4 extra rows
#this removes them
DemoData = DemoData %>% drop_na()

EC = read_excel("dataforR3.xlsx", sheet=1, col_names =T) #read sheet with EC results
EC2 = gather(EC, item, accuracy) #make it long format
EC2$Participant <- rep(1:173, times=33) #add participant column and # for all questions
EC3 = merge(EC2, DemoData, by="Participant") #add demographic data columns
EC3$Task <- rep("EC",length(EC3$Participant)) #add task column and say EC for all EC questions


IC = read_excel("dataforR3.xlsx", sheet=6, col_names =T)
IC2 = gather(IC, item, accuracy)
IC2$Participant <- rep(1:173, times=14)
IC3 = merge(IC2, DemoData, by="Participant")
IC3$Task <- rep("IC",length(IC3$Participant))

ECxIC <- rbind(EC3,IC3)

ECxIC$Participant <- as.factor(ECxIC$Participant)
ECxIC$AQLevel <- as.factor(ECxIC$AQLevel)
colnames(ECxIC) = c("Participant", "Item", "SI.Resp","Gender", "Age", 
                    "Native Language", "Proficiency", "Academic Degree",
                    "AQ", "AQLevel", "SI.Task") #Change column names so that the model can be clear to
                                                #people who haven't written this code

#interaction plot - 
interaction.plot(ECxIC$AQ, ECxIC$SI.Task, ECxIC$SI.Resp,
                 main = "SI response x AQ interaction", ylab="SI response", xlab="AQ score",
                 trace.label = "Task Type", col= 'darkgreen')

#Correlation on averaged data
Averaged = read_excel("dataforR3.xlsx", sheet=8, col_names =T)  #import the averaged data


corr.mat <- cbind(Averaged$AVG.IC,Averaged$AVG.EC, Averaged$AQ, Averaged$AVG.ART) 
corr.mat2 <- cor(corr.mat, method="kendall", use="pairwise") #kendall's tau ICxECxAQ
colnames(corr.mat2) <- c('AVG.IC', 'AVG.EC', 'AQ', 'AVG.ART')
row.names(corr.mat2) <- c('AVG.IC', 'AVG.EC', 'AQ', 'AVG.ART')
corrplot(corr.mat2, method="number") #plot the correlations (methods= "circle", "square", "ellipse", "number", "shade", "color", "pie" )


##correlation with p values:
cor.test(Averaged$AVG.EC, Averaged$AQ, method = "kendall", use = "pairwise")
cor.test(Averaged$AVG.IC, Averaged$AQ, method = "kendall", use = "pairwise")
cor.test(Averaged$AVG.ART, Averaged$AQ, method = "kendall", use = "pairwise")
cor.test(Averaged$AVG.EC, Averaged$AVG.ART, method = "kendall", use = "pairwise")
cor.test(Averaged$AVG.IC, Averaged$AVG.ART, method = "kendall", use = "pairwise")
cor.test(Averaged$AVG.EC, Averaged$AVG.IC, method = "kendall", use = "pairwise")



#AQ distribution histogram
qplot(DemoData$AQ, geom="histogram", binwidth = 5,main = "AQ Distribution", xlab = "AQ", fill=I("green"), col=I("blue"), alpha=I(.2), xlim=c(40,140))

#Plotting ECxIC, ECxAQ and ICxAQ
plot(Averaged$AVG.EC, Averaged$AVG.IC, main = 
       "Statement Avg. Logical x Gumball Avg. Logical", 
     xlab= "Statement logical", ylab = "Gumball logical",frame.plot=FALSE , 
     col=I("coral4")) #plot EC logical x IC logical

#plot IC logical x AQ
plot(Averaged$AVG.IC, Averaged$AQ, main = "IC Avg. Logical x AQ", 
     xlab= "IC logical", ylab = "AQ score",frame.plot=FALSE , col=I("violetred")) 

#plot EC logical x AQ
plot(Averaged$AVG.EC, Averaged$AQ, main = "EC Avg. Logical x AQ", 
     xlab= "EC logical", ylab = "AQ score",frame.plot=FALSE , col=I("Indianred")) 

#plot those who have EC logical response rate over .8
#create a df with only those who have more than .8 logical response to EC
AveragedEC80 = Averaged[Averaged$AVG.EC>.8,] 
#create a df with only those who have more than .8 logical response to IC
AveragedIC80 = Averaged[Averaged$AVG.IC>.8,] 

#EC >.8, IC distribution histogram
qplot(AveragedEC80$AVG.IC, geom="histogram", binwidth = .1,
      main = "EC>.8, IC Distribution", xlab = "AVG IC", fill=I("darkmagenta"), 
      col=I("brown4"), alpha=I(.2), xlim=c(0,1.0), ylim=c(0,20))
#IC >.8, EC distribution histogram
qplot(AveragedIC80$AVG.EC, geom="histogram", binwidth = .1,
      main = "IC>.8, EC Distribution", xlab = "AVG EC", fill=I("darkolivegreen4"), 
      col=I("darkolivegreen1"), alpha=I(.2), xlim=c(0,1.0), ylim=c(0,20))

#Plot ECxIC for those who got >.8 In EC
plot(AveragedEC80$AVG.EC, AveragedEC80$AVG.IC, main = "EC>.8 x IC", 
     xlab = "EC logical Response", ylab = "IC logical Response",
     frame.plot=FALSE , col=I("darkcyan")) #plot EC logical>.8 x IC logical

#plot ECxIC for those who got >.8 in IC
plot(AveragedIC80$AVG.IC, AveragedIC80$AVG.EC, main = "IC>.8 x EC", 
     xlab= "IC Logical Response", ylab = "EC logical Response",frame.plot=FALSE , 
     col=I("aquamarine4")) #plot IC logical>.8 X EC logical

corECxAge <- cor(DemoData$Age, Averaged$AVG.EC, method= "kendall", use="pairwise")
corICxAge <- cor(DemoData$Age, Averaged$AVG.IC, method= "kendall", use="pairwise")

table (ECxIC$SI.Resp, ECxIC$AQLevel) #looking into 1/0 Si response by AQLeve
#Logistic regression mode
#first, setting M as the AQ reference level
ECxIC$AQLevel <- relevel(ECxIC$AQLevel, ref="M") #make M the reference level
Exp2LogReg<- glmer(SI.Resp ~ AQLevel+SI.Task + (1+SI.Task|Participant),
              data=ECxIC, family=binomial)
summary(Exp2LogReg)
Anova(Exp2LogReg, type="III") #ombinus test (shows only intercept being significant, SI.task being mildly significant)
lsmeans(Exp2LogReg, "AQLevel") #posthoc test (not suitable for glm, do I need glht?)

#correlations for the different levels of AQ
AQlow = read_excel("dataforR3.xlsx", sheet=9, col_names =T)
AQmid = read_excel("dataforR3.xlsx", sheet=10, col_names =T)
AQhigh = read_excel("dataforR3.xlsx", sheet=11, col_names =T)

corrlowAQ = cor(AQlow, method="kendall", use="pairwise") 
corrmidAQ = cor(AQmid, method="kendall", use="pairwise")
corrhighAQ = cor(AQhigh, method="kendall", use="pairwise")

#different high and low, median split. over 85 high (using Lau's report)
AQhigh2 = Averaged[Averaged$AQ>84,]
AQlow2 = Averaged[Averaged$AQ<85,]

cor(AQhigh2, method="kendall", use="pairwise")
cor(AQlow2, method="kendall", use="pairwise") #this (both cors) gives weird results
                                              #both negative



#removing the low AQ scores and correlating the rest with 
#the other tasks

AQmidhigh = Averaged[Averaged$AQ>99,]


#calculating Cronach's alpha for SI tasks
#read sheet with full EC results
ECfull = read_excel("dataforR3.xlsx", sheet=2, col_names =T) 
#read sheet with full IC results
ICfull = read_excel("dataforR3.xlsx", sheet=5, col_names =T)

#cronbach's alpha
cronbach(ECfull)
cronbach(ICfull)

#z-scores for EC and IC tasks
#then exported to excel sheet

ECz <- scale(EC, scale=TRUE, center=TRUE) #EC zscores
ICz <- scale(IC, scale=TRUE, center=TRUE) #IC zscores
zscores <- cbind(ECz, ICz) #combine all
write.xlsx(zscores,"C:/Users/Beefgallo/Dropbox/UIUC/ASD-SI/zscores.xlsx",sheetName = "Exp2")

#import comp.z-scores for the two SI tasks
#z-scores for each question, avg. for each task and then avg. of both tasks
CompZscore = read_excel("dataforR.xlsx", sheet=12) 
corZscore <- cbind(CompZscore, DemoData$AQ, Averaged$AVG.ART)
corZscore2 <- cor(corZscore, method="kendall", use="pairwise")

#AQ comm, import and correlations
AQComm = read_excel("dataforR3.xlsx", sheet=13, col_names =T)
AQCommHigh = AQComm[AQComm$AQComm>12,]
AQCommLow = AQComm[AQComm$AQComm<13,]

#this does not replicate nieuwland's results, and still we see most correlation
#not that this is the goal!!!, when dividing AQ to three sub-groups
cor(AQCommHigh, method="kendalECl", use="pairwise")
cor(AQCommLow, method="kendall", use="pairwise")

ggplot(ECxIC, 
       aes(SI.Resp, color=factor(AQLevel)))+ 
  geom_point()

#alphas for EC and IC tasks

fullEC = read_excel("dataforR3.xlsx", sheet=2, col_names =T) #read sheet with full EC results
fullIC = read_excel("dataforR3.xlsx", sheet=5, col_names =T) #read sheet with full IC results

cronbach(fullEC) #cronbach's alpha, reliability for EC task 
cronbach(fullIC) #cronbach's alpha, reliability for IC task


#**Qualitative data analysis**

AQ = read.xlsx("dataforR3.xlsx", sheetIndex=16)
AQ = AQ %>% drop_na() #Removing some empty rows from data.
Judge = read.xlsx("dataforR3.xlsx", sheetIndex=17)
Judge2 = gather(Judge,"Item", "Judgment",2:9)
Reason = read.xlsx("dataforR3.xlsx", sheetIndex=18)
Reason2 = gather(Reason,"Item", "Reasoning",2:9) 
JudgReas = merge(Judge2, Reason2, by=c("Item","RespondentID"))
AQJR = merge(AQ, JudgReas, by="RespondentID")

#reviewing the data and making sure variables are defined as factors
summary(AQJR)
AQJR$Judgment = as.factor(AQJR$Judgment)
AQJR$RespondentID = as.factor(AQJR$RespondentID)
AQJR$Reasoning = as.factor(AQJR$Reasoning)  

#maximal mixed effects model
#mixed effects models, from maximal to ...minimal..(most did not converge)
glmmeMax<- glmer(Judgment ~ AQ+Reasoning + (1+AQ|RespondentID)+(1+Reasoning|Item),
                 data=AQJR, family="binomial")

#the above model gave some errors, trying another version
glmmeMax2<- glmer(Judgment ~ AQ+Reasoning + (1|AQ)+(1+Reasoning|Item),
                 data=AQJR, family="binomial")

#removing slopes and interactions gives errors. Trying a Bayesian random effects model
#with default priors


library(rstanarm)
library(iterators)

Bglmme = stan_glmer(Judgment ~ AQ+Reasoning + 
                         (1+Reasoning|RespondentID)+
                         (1+AQ*Reasoning|Item), data=AQJR, family="binomial", prior = normal(), prior_intercept = normal(), prior_aux = cauchy(0,5), prior_covariance = decov(), prior_PD = FALSE)

summaryBM <- summary(Bglmme)
write.xlsx(summaryBM, "C:/Users/Beefgallo/Desktop/AQJRBayesianSummary.xlsx")
launch_shinystan(Bglmme)
