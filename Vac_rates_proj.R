library(tidyverse)
library(BEST)
library(BayesFactor)
library(magrittr)
library(BaylorEdPsych)
library(car)
library(MCMCpack) 
library(ez)
library(tseries)
library(changepoint)
library(bcp)
library(nlme)
library(psych)
library(dplyr)
library(modeest)
library(effsize)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(psych)
library(lmtest)
library(MASS)
library(caret)

########## 

# inspect WHO reported vaccination rates dataset
str(usVaccines)
summary(usVaccines)
boxplot(usVaccines)

# check for NAs
sapply(usVaccines, 
       function(x) sum(is.na(x)))

# plot time series of vaccination rates (1980-2017)
plot.ts(usVaccines)

# cpt variance

par(mfrow=c(3,2))
dtp1VAR <- cpt.var(diff(usVaccines[,1])) # DTP1
plot(dtp1VAR,
     cpt.col="grey",
     cpt.width=5,
     main='DTP1')
dtp1VAR

hepbVAR <- cpt.var(diff(usVaccines[,2])) # HepB_BD
plot(hepbVAR,
     cpt.col="grey",
     cpt.width=5,
     main='HepB_BD')
hepbVAR

polVAR <- cpt.var(diff(usVaccines[,3])) # Pol3
plot(polVAR,
     cpt.col="grey",
     cpt.width=5,
     main='Pol3')
polVAR

hibVAR <- cpt.var(diff(usVaccines[,4])) # Hib3
plot(hibVAR,
     cpt.col="grey",
     cpt.width=5,
     main='Hib3')
hibVAR

mcvVAR <- cpt.var(diff(usVaccines[,5])) # MCV1
plot(mcvVAR,
     cpt.col="grey",
     cpt.width=5,
     main='MCV1')
mcvVAR

# clear
dev.off()

#cpt mean
par(mfrow=c(3,2))
plot(cpt.mean(diff(usVaccines[,1])))
plot(cpt.mean(diff(usVaccines[,2])))
plot(cpt.mean(diff(usVaccines[,3])))
plot(cpt.mean(diff(usVaccines[,4])))
plot(cpt.mean(diff(usVaccines[,5])))


# decompose
par(mfrow=c(3,2))
decDTP <- decompose(ts(usVaccines[,1], frequency = 12))
plot(decDTP)
acf(decDTP$random,na.action=na.pass)
adf.test(usVaccines[,1])

decHepB <- decompose(ts(usVaccines[,2], frequency = 12))
plot(decHepB)
acf(decHepB$random,na.action=na.pass)
adf.test(usVaccines[,2])

decPol <- decompose(ts(usVaccines[,3], frequency = 12))
plot(decPol)
acf(decPol$random,na.action=na.pass)
adf.test(usVaccines[,3])

decHib3 <- decompose(ts(usVaccines[,4], frequency = 12))
plot(decHib3)
acf(decHib3$random,na.action=na.pass)
adf.test(usVaccines[,4])

decMCV <- decompose(ts(usVaccines[,5], frequency = 12))
plot(decMCV)
acf(decMCV$random,na.action=na.pass)
adf.test(usVaccines[,5])
acf(diff(usVaccines[,5]))

dev.off()

########## 

# inspect Cali kindergarten vaccination reporting dataset
str(allSchoolsReportStatus)
summary(allSchoolsReportStatus)

# proportion of schools reporting
allSchoolsReportStatus %>%
  group_by(pubpriv, reported) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))

# create separate dfs for public/private
pubSch <- allSchoolsReportStatus %>%
  filter(pubpriv=='PUBLIC')
privSch <- allSchoolsReportStatus %>%
  filter(pubpriv=='PRIVATE')


# graph
pubPlot <- ggplot(pubSch, aes(reported)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales:::percent) +
  labs(title="Public",
       x="", y="") 

privPlot <- ggplot(privSch, aes(reported)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales:::percent,limit = c(0, 1)) +
  labs(title="Private",
       x="", y="") 

ggarrange(pubPlot, privPlot)

# prop test for credible difference
propTable <- table(allSchoolsReportStatus[,2:3])
propMatrix <- propTable

prop.test(x=c(5584,1397), 
          n=c(5584+148, 1397+252))


########## 

# inspect district dataset
str(districts)
summary(districts)

# only analyze districts that have complete data
distComplete <- districts[districts$DistrictComplete==TRUE,]
boxplot(distComplete[,2:6])

# mean vaccination rates
100 - mean(distComplete$WithoutDTP)
100 - mean(distComplete$WithoutPolio)
100 - mean(distComplete$WithoutMMR)
100 - mean(distComplete$WithoutHepB)

# 2013 WHO vaccination rates
usVaccines[34,]

##########

dist.corr <- cor(distComplete[,2:4])
corrplot(dist.corr, addCoef.col = 'azure1')

# regression analysis
dist.model <- lm(
  formula=PctUpToDate~WithoutDTP+WithoutPolio+WithoutMMR+WithoutHepB,
  data=distComplete)
summary(dist.model)

##########

# create new df variable 
myDistricts <- districts

# check for missing data
sapply(myDistricts, 
       function(x) sum(is.na(x)))

# factor outcome variable
myDistricts$DistrictComplete <- factor(myDistricts$DistrictComplete)
str(myDistricts)

# distributions of predictor variables
hist(myDistricts$PctChildPoverty)
hist(myDistricts$PctFreeMeal)
hist(myDistricts$PctFamilyPoverty)
hist(myDistricts$Enrolled)
hist(myDistricts$TotalSchools)

par(mfrow=c(3,2))
boxplot(myDistricts$PctChildPoverty,
        main='Child Poverty')
boxplot.stats(myDistricts$PctChildPoverty)$out
boxplot(myDistricts$PctFreeMeal,
        main='Free Meal')
boxplot.stats(myDistricts$PctFreeMeal)$out # none
boxplot(myDistricts$PctFamilyPoverty,
        main='Family Poverty')
boxplot.stats(myDistricts$PctFamilyPoverty)$out
boxplot(myDistricts$Enrolled,
        main='Enrolled')
boxplot.stats(myDistricts$Enrolled)$out
boxplot(myDistricts$TotalSchools,
        main='Total Schools')
boxplot.stats(myDistricts$TotalSchools)$out
dev.off()

# reviewing outliers by variable
out <- boxplot.stats(myDistricts$PctFamilyPoverty)$out
out_enroll <- which(myDistricts$PctFamilyPoverty %in% out)
length(out_enroll)
myDistricts[out_enroll, ]



## stats: a vector of length 5, containing the extreme of the lower whisker, 
# the lower ‘hinge’, the median, the upper ‘hinge’ and the extreme of the 
# upper whisker. (extreme should not to be confused with outliers which are 
#outside the whiskers)
boxplot.stats(myDistricts$Enrolled)


# correlation analysis on predictors to find any relationships
pred.corr <- cor(myDistricts[,9:13])
corrplot(pred.corr, addCoef.col = 'azure1') #method='number'

### factor analysis

# The Kaiser criterion (Kaiser 1960) recommends that the number of 
# principal components or factors is the number of dots above the 1.0 line.
scree(myDistricts[,9:13])

## Bartlett’s test was statistically significant, suggesting 
# that the observed correlation matrix among the items is not an identity matrix.
# This really isn’t a particularly powerful indication that you have a factorable 
# dataset, though - all it really tells you is that at least some of the 
# variables are correlated with each other.
cortest.bartlett(myDistricts[,9:13]) 


## The Kaiser-Meyer-Olkin (KMO) measure of sampling adequacy is a better measure
# of factorability. The KMO tests to see if the partial correlations within your
# data are close enough to zero to suggest that there is at least one latent 
# factor underlying your variables. The min acceptable value is 0.50, but most 
# authors recommend a value of at 0.60 before undertaking a factor analysis.
KMO(myDistricts[,9:13]) 


# factor analysis - factoring
dist.fa <- factanal(myDistricts[,9:13], factors=2)
dist.fa

# principal component analysis - factoring
dist.pca <- principal(myDistricts[,9:13], 
                      nfactors = 2,
                      rotate = "oblimin")
dist.pca
# plot diagram
fa.diagram(dist.pca)

# % of variance that can be explained by the retained factors for each variable
dist.pca$communality
# computes a vector of the eigenvalues the five principal components
dist.pca$values[1:5]
# uses those eigenvalues to compute the percentage of variance associated with 
# each of these factors:
dist.pca$values[1:5]/length(dist.pca$values)

# combine the dependent variable and the factor scores into a dataset 
regdata <- cbind(myDistricts[which(names(myDistricts) == "DistrictComplete")], 
                 dist.pca$scores)

# TC1: % of children/families in poverty and children eligible for free meals
# TC2: enrolled students and number of different schools in the district
# change column names for new dataset 
colnames(regdata) <- c("DistrictComplete", "Poverty", "Students")

poverty <- regdata$Poverty
students <- regdata$Students

myDistricts %<>%
  mutate(Poverty_fact = poverty,
         Students_fact = students)

## logistic regression
# myDistricts$pct_child <- myDistricts$PctChildPoverty/100
# myDistricts$pct_fam <- myDistricts$PctFamilyPoverty/100
# myDistricts$pct_meal<- myDistricts$PctFreeMeal/100

# create logit model
dist.log.model <- glm(formula = DistrictComplete~Poverty_fact+Students_fact, 
                      data=myDistricts,
                      family=binomial())
summary(dist.log.model)

# testlog <- glm(formula = DistrictComplete~PctChildPoverty+PctFreeMeal+
#                         PctFamilyPoverty+Enrolled+TotalSchools, 
#                       data=myDistricts,
#                       family=binomial())
# summary(testlog)

# test accuracy of model
confMatrix5 <- table(round(predict(dist.log.model, type='response')), 
                    myDistricts$DistrictComplete)
acc5 <- (sum(diag(confMatrix5))) / sum(confMatrix5) 
acc5 # 94.3%


# convert log-odds to odds
exp(coef(dist.log.model))
# CI of odds
exp(confint(dist.log.model))
# compare null to predictor model
anova(dist.log.model, test='Chisq')
# psuedo-r2
PseudoR2(dist.log.model) # Nagelkerke 0.07610023

# VIF analysis for multi-collinearity
vif(dist.log.model)


# ---Bayesian analysis---

# convert DistrictComplete(outcome) to a 0 or 1 binary output
dCompBin <- as.numeric(myDistricts$DistrictComplete) - 1 
myDistricts %<>%
  mutate(DistComp_bin = dCompBin)
myDistricts <- relocate(myDistricts, DistComp_bin,
                        .after=DistrictComplete)

# model Bayes
bayesDist <- MCMClogit(formula = DistComp_bin~Poverty_fact+Students_fact, 
                       data=myDistricts)
summary(bayesDist)
plot(bayesDist)

# odds function
oddsHDI <- function(bayesOut, var) {
  # create a matrix fo apply() function
  LogOdds <- as.matrix(bayesOut[,var]) 
  # apply() iterates the exp() function over each log-odd value
  Odds <- apply(LogOdds,1,exp) 
  # plot histogram
  hist(Odds) 
  # add lower HDI limit
  abline(v=quantile(Odds,c(0.025)),col='black') 
  # add upper HDI limit
  abline(v=quantile(Odds,c(0.975)),col='black')
}

# convert log-odds to odds
oddsHDI(bayesDist, 'Poverty_fact')
oddsHDI(bayesDist, 'Students_fact')
LogOdds <- as.matrix(bayesDist[,'Students_fact']) 
Odds <- apply(LogOdds,1,exp) 
mean(Odds)

dev.off()


##########

# predicting percentages, which technically speaking is discrete or bounded (0-1)
# we are wanting to predict fully vaccinated (=100 or 1)
# logistic regression should be used

# check number of full vac
myDistricts %>%
  filter(PctUpToDate==100) %>%
  count()

# convert response to a binary output
pctBin <- ifelse(myDistricts$PctUpToDate<100, 0, 1)
pctBin <- factor(pctBin)
# create binary col for binary response
myDistricts %<>%
  mutate(PctUpToDate_bin = pctBin)
# relocate col 
myDistricts <- relocate(myDistricts, PctUpToDate_bin,
                    .after=PctUpToDate)

# interpretation of log-log would be for a 1% increase in Enrolled, the probability that 
# a student is fully vaccinated decreases by .73 on a [0,1] scale
# myDist2$Enrolled <- log(myDist2$Enrolled)
# myDist2$TotalSchools <- log(myDist2$TotalSchools)
# myDist2$PctFamilyPoverty <- log(myDist2$PctFamilyPoverty)


# logit model 
fullyVac.log.model <- glm(formula = PctUpToDate_bin~Poverty_fact+Students_fact, 
                      data=myDistricts,
                    family=binomial())
summary(fullyVac.log.model)

# testlog2 <- glm(formula = PctUpToDate_bin~PctChildPoverty+PctFreeMeal+
#                  PctFamilyPoverty+Enrolled+TotalSchools, 
#                data=myDistricts,
#                family=binomial())
# summary(testlog2)

# test accuracy of model
confMatrix6 <- table(round(predict(fullyVac.log.model, type='response')), 
                     myDistricts$PctUpToDate_bin)
acc6 <- (sum(diag(confMatrix6))) / sum(confMatrix6) 
acc6 # 93.7%

# convert log-odds to odds
exp(coef(fullyVac.log.model))
# CI of odds
exp(confint(fullyVac.log.model))
# compare null to predictor model
anova(fullyVac.log.model, test='Chisq')
# psuedo-r2
PseudoR2(fullyVac.log.model) # Nagelkerke 0.24497914

## outlier testing

# calculate influence of outliers on model
cooksd <- cooks.distance(fullyVac.log.model)
plot(cooksd, pch="*", 
     cex=1, 
     main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
# In general use, observations that have a cook’s distance greater than 4 times 
# the mean may be classified as influential. This is not a hard boundary.
text(x=1:length(cooksd)+1, y=cooksd, 
     labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), 
     col="red") 

# gives most extreme outlier
outlierTest(pct.log.model)

# model Bayes
bayesFullVac <- MCMClogit(formula = PctUpToDate_bin~Poverty_fact+Students_fact, 
                       data=myDistricts)
summary(bayesFullVac)
plot(bayesFullVac)

# convert log-odds to odds
oddsHDI(bayesFullVac, 'Poverty_fact')
LogOddsP <- as.matrix(bayesFullVac[,'Poverty_fact']) 
OddsP <- apply(LogOddsP,1,exp) 
mean(OddsP)

oddsHDI(bayesFullVac, 'Students_fact')
LogOddsS <- as.matrix(bayesFullVac[,'Students_fact']) 
OddsS <- apply(LogOddsS,1,exp) 
mean(OddsS)

cor(myDistricts$PctFreeMeal,myDistricts$PctUpToDate)

##########

# predicting percentages, which technically speaking is discrete or bounded (0-1)
# we are wanting to predict percentage of all enrolled students with belief
# exceptions. 

# convert response to a binary output
belBin <- ifelse(myDistricts$PctBeliefExempt>0, 1, 0)
belBin <- factor(belBin)
# create binary col for binary response
myDistricts %<>%
  mutate(PctBeliefExempt_bin = belBin)
# relocate col 
myDistricts <- relocate(myDistricts, PctBeliefExempt_bin,
                        .after=PctBeliefExempt)

# logit model - binomial needs to be 0 to 1
belExc.log.model <- glm(formula = PctBeliefExempt_bin~Poverty_fact+Students_fact, 
                          data=myDistricts,
                          family=binomial)
summary(belExc.log.model)

# testlog3 <- glm(formula = PctBeliefExempt~PctChildPoverty+PctFreeMeal+
#                   PctFamilyPoverty+Enrolled+TotalSchools, 
#                 data=myDistricts,
#                 family=binomial())
# summary(testlog3)

# test accuracy of model
confMatrix7 <- table(round(predict(belExc.log.model, type='response')), 
                     myDistricts$PctBeliefExempt_bin)
acc7 <- (sum(diag(confMatrix7))) / sum(confMatrix7) 
acc7 # 79.0%


# convert log-odds to odds
exp(coef(belExc.log.model))
# CI of odds
exp(confint(belExc.log.model))
# compare null to predictor model
anova(belExc.log.model, test='Chisq')
# psuedo-r2
PseudoR2(belExc.log.model) # Nagelkerke 0.2736118


# model Bayes
bayesBelExc <- MCMClogit(formula = PctBeliefExempt_bin~Poverty_fact+Students_fact, 
                          data=myDistricts)
summary(bayesBelExc)
plot(bayesBelExc)

# convert log-odds to odds
oddsHDI(bayesBelExc, 'Poverty_fact')
LogOddsP <- as.matrix(bayesBelExc[,'Poverty_fact']) 
OddsP <- apply(LogOddsP,1,exp) 
mean(OddsP)

oddsHDI(bayesBelExc, 'Students_fact')
LogOddsS <- as.matrix(bayesBelExc[,'Students_fact']) 
OddsS <- apply(LogOddsS,1,exp) 
mean(OddsS)

hist(myDistricts$Students_fact,
     main='Enrolled/Total Schools Distribution')


dev.off()

# average headcount for districts with higher rates of poverty
myDistricts %>%
  filter(PctChildPoverty>=29) %>%
  summarise(avg = mean(Enrolled))

# average headcount for districts with lower rates of poverty
myDistricts %>%
  filter(PctChildPoverty<=13) %>%
  summarise(avg = mean(Enrolled))



