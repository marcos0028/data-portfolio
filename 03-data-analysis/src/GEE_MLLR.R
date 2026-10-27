####
#GEE and Multilevel logistic regression

load("acvddata_2022_2024.Rda")

source("methods_lab_functions.R")

library(gmodels)
require(ggplot2)
library(gridExtra)
library(plyr)
library(grid)
library(geepack)
library(caret)
library(geepack)
library(forestplot)
library(Epi)
library(MatchIt)
library(epitools)
library(ROCR)
library(ROCit)
library(pROC)
library(cutpointr)
library(bootLR)

# number of patients per hospital
table_counts <- table(surgdata$ny_hosp_id)
# selection of hospitals with ≥100 patients only
hospitals_100 <- names(table_counts[table_counts >= 100])
#filter of dataset
surgdata_filtered <- surgdata[surgdata$ny_hosp_id %in% hospitals_100, ]
#######################################GEE MODEL

# Run gee model from same covariates
colSums(is.na(surgdata_filtered))
surgdata_filtered<-na.omit(surgdata_filtered)

##We do the model with GEE and all the variables of interest
GEE_full <- geeglm(long_stay~recl_age + males +race +ethnicity +year +dead +adm_type +procedure +risky +severe,
  id = ny_hosp_id, family = binomial("logit"),corstr = "exchangeable",data = surgdata_filtered)


confint.geeglm <- function(object, parm, level = 0.95, ...) {
  cc <- coef(summary(object))
  mult <- qnorm((1+level)/2)
  citab <- with(as.data.frame(cc),
                cbind(Lower=Estimate-mult*Std.err,
                      Upper=Estimate+mult*Std.err))
  rownames(citab) <- rownames(cc)
  citab[parm,]
}

cbind(coef(summary(GEE_full)),confint.geeglm(GEE_full))

#As we see, the intercept is no significant, as the p-value is 0.974 and the error is very high.
#This is because there are variables that are generating problems. If we take a look to the p-values, 
#we can see that males, with a p-value of 0.0564, and year, with a p-value of 0.968, are not significant.
#Lets reduced the model by erasing those variables (males and year).


##We do the reduced GEE model
GEE_reduced <- geeglm(long_stay~recl_age +race +ethnicity +dead +adm_type +procedure +risky +severe,
                   id = ny_hosp_id, family = binomial("logit"),corstr = "exchangeable",data = surgdata_filtered)

cbind(coef(summary(GEE_reduced)),confint.geeglm(GEE_reduced))


#Every variable is now significant, at level 0.05.

#The odd of having a long stay is of: exp(-3.16)=0.0424 and the probability of 
#having a long stay is of 0.0848

int<-GEE_reduced$coefficients[1]

exp(int)
pr_longstay<- as.numeric((exp(int))/1+(exp(int)))

#Among all covariates included, "severe" shows the
#strongest association with "long_stay" (OR = 3.36), much
#more than the effect of other variables such as risky (OR = 1.40)
#or "adm_type" (OR = 1.11)


### Subgroup analysis

# Primary surgical=0
model_surgical0<-geeglm(long_stay~recl_age +race +ethnicity +dead +adm_type +procedure +risky,data=surgdata_filtered[surgdata_filtered$surgical==0,],id = ny_hosp_id, family = binomial("logit"),corstr = "exchangeable")

# Primary surgical=1
model_surgical1<-geeglm(long_stay~recl_age +race +ethnicity +dead +adm_type +procedure +risky,data=surgdata_filtered[surgdata_filtered$surgical==1,],id = ny_hosp_id, family = binomial("logit"),corstr = "exchangeable")

cbind(coef(summary(model_surgical0)),confint.geeglm(model_surgical0))
cbind(coef(summary(model_surgical1)),confint.geeglm(model_surgical1))

int1<-model_surgical0$coefficients[1]
int2<-model_surgical1$coefficients[1]
pr_longstay_surgical0<- as.numeric((exp(int1))/1+(exp(int1)))#0.298
pr_longstay_surgical1<- as.numeric((exp(int2))/1+(exp(int2)))#0.978

#The probability of having a long stay when the patient has not passed by a surgical 
#operation (0.298), is much lower that the probability of having a long stay when the
#patient has passed a surgical operation (0.978)

#######################################MLLR MODEL


library(tidyverse)
library(lme4)


## Hospitals volume (nr of patients)

hosp_vol<-count(surgdata_filtered$ny_hosp_id)
names(hosp_vol) <- c('ny_hosp_id', 'hospvol')

surgdata_filtered1<-merge(surgdata_filtered,hosp_vol, by ='ny_hosp_id', all.x=TRUE)
surgdata_filtered1$hospvol <-(surgdata_filtered1$hospvol/100)                          # Hospital volume is in units of 100 patients 

## Hospital surgical volume (nr of surgical interventions)

hosp_surgical_vol <-aggregate(surgdata_filtered1$surgical,by=list(surgdata_filtered1$ny_hosp_id),FUN="sum")  
names(hosp_surgical_vol) <-c('ny_hosp_id', 'hosp_surgical_vol')

surgdata_filtered1<-merge(surgdata_filtered1,hosp_surgical_vol, by ='ny_hosp_id', all.x=TRUE)
surgdata_filtered1$hosp_surgical_vol <-(surgdata_filtered1$hosp_surgical_vol/100)   


rm(hosp_vol, hosp_surgical_vol)


### FIRST MODEL: NULL MLLR

model.0 <- glmer( long_stay ~ (1|ny_hosp_id), 
                  family = binomial("logit"), nAGQ = 10,data= surgdata_filtered1)

summary(model.0)

## B0 of null model
int_null<-fixef(model.0)


##The estimated variance of the random effects was (tau2):

tau2_null<-as.numeric(c(as.data.frame(VarCorr(model.0))[4]))
tau2_null

## The 95% probability interval for the hospital-specific intercepts is (i.e., 95% of hospitals will have a random intercept that 
## lies within this interval):

se<-sqrt(tau2_null)
ci_int<- c((int_null-se),(int_null+se))
names(ci_int)<- c('95_lower_int','95_upper_int')
ci_int

## At an average hospital (i.e., a hospital whose random effect was equal to zero on the logit scale), the probability of death was: 

pr_death<- as.numeric((exp(int_null))/1+(exp(int_null)))
pr_death

#Thus, for 95% of hospitals, the hospital-specific probability of death  would lie in the interval:

pr_death_ci <- (exp(ci_int)/1+exp(ci_int))
names(pr_death_ci )<- c('95_lower_pr','95_upper_pr')
pr_death_ci


###SECOND MODEL: 








#######################################PROPENSITY SCORE

library(survival)

surgdata_filtered$endar <- 0
surgdata_filtered$endar[
  (surgdata_filtered$year == 2015 & surgdata_filtered$procedure == 66) | 
    ((surgdata_filtered$year == 2016 | surgdata_filtered$year == 2017) & surgdata_filtered$procedure == 63)
] <- 1


PropLogit<-glm(endar~recl_age+ males+risky + severe+ cost +year+race+procedure+ dead,family = binomial("logit"),data=surgdata_filtered)

# Predicted probability of endarteroctomy intervention 

surgdata_filtered$p1<-predict(PropLogit,newdata=surgdata_filtered,type="response")    # propensity score=predicted probabilities from the model on endarteroctomy

## Techniques of use of PS ----

surgdata_filtered$p_group<-cut(surgdata_filtered$p1,quantile(surgdata_filtered$p1,probs=seq(0,1,0.20),na.rm=TRUE),labels=FALSE,include.lowest=TRUE,right=TRUE) # labels avoid writing intervals and using only numbers

quantile(surgdata_filtered$p1,probs=seq(0,1,0.20),na.rm=TRUE)

CrossTable(surgdata_filtered$p_group,surgdata_filtered$endar)


############## Conditional logistic regression (MATCHED BY P_GROUP) ----


# We match every treated patient with an untreated patient having similar propensity scores. We balance treated and untreated patients by the baseline characteristics.   
# Greedy: a treated subject is selected at random. The untreated subject whose propensity score is closest to that of this randomly selected subject is chosen for matching.

p.match<-matchit(formula = endar~recl_age+males+risky+severe,
                 data    = surgdata_filtered,                                # Cannot have missing values. Use complete dataset
                 method  = "nearest",                                   # greedy match
                 distance= "logit",                                     # Distance defined by usual propensity score from logistic model
                 ratio   = 1)                                           # 1:1 match is the default

#Summary of differences in the baseline characteristics among treated and untreated patients before matching, after matching and sample size of matched patients.

summary(p.match)

# Plot of propensity score of matched ptca and not ptca patients and of not matched patients that not admitted to ptca.  

plot(p.match,type = "jitter",interactive=F)

# Plot by PS the unmatched proportions of treated and control (not the unbalancy) and matched proportions (not the balance). 

plot(p.match,type = "hist",interactive=F)

## Output Matched Data Sets for further analysis

dat.match <- match.data(p.match)                      # This dataset contains only matched observations, balanced by PS. Lower number of obs than amidata. Subclass= matched pair membership

CrossTable(dat.match$p_group,dat.match$endar)          # Balanced number of patients in treated and control groups for every category of propensity score (p_group).  

# Index number of rows of every the couple of matched observations. Treated group in the first column and untreated in the other.   

p.match$match.matrix

# Use the matched data for the model.Unconditional logistic regression (Conditional is not required). The matched data are balanced.

DMLogit<-glm(dead~endar,data=dat.match,family=binomial(link = "logit"))
round(cbind(Beta=coef(DMLogit),confint(DMLogit),P=coef(summary(DMLogit))[,4],exp(cbind(OR=coef(DMLogit),confint(DMLogit)))),4)

################# GEE for binary outcomes ----

# Run gee model from same covariates

modgee<-geeglm(dead~endar,data=surgdata_filtered[!is.na(surgdata_filtered$p_group),],id=p_group,family=binomial(link="logit"),corstr="exchangeable")

summary(modgee)
cc <- coef(summary(modgee))
citab <- with(as.data.frame(cc),cbind(OR=round(exp(Estimate),3),LCL=round(exp(Estimate-1.96*Std.err),3),UCL=round(exp(Estimate+1.96*Std.err),3)))
rownames(citab) <- rownames(cc)
print(citab)

modgee2<-geeglm(long_stay~endar,data=surgdata_filtered[!is.na(surgdata_filtered$p_group),],id=p_group,family=binomial(link="logit"),corstr="exchangeable")

summary(modgee2)
cc2 <- coef(summary(modgee2))
citab2 <- with(as.data.frame(cc2),cbind(OR=round(exp(Estimate),3),LCL=round(exp(Estimate-1.96*Std.err),3),UCL=round(exp(Estimate+1.96*Std.err),3)))
rownames(citab2) <- rownames(cc2)
print(citab2)

#After adjustment using propensity score–based GEE, endarterectomy was not significantly 
#associated with mortality (OR = 1.05, 95% CI: 0.92–1.20).
#On the other hand, endarterectomy is significantly 
#associated with the length of stay (OR = 2.09, 95% CI: 1.931-2.256)


######### Inverse Probability of Treatment Weighting (IPTW) ----

# Weights= wi= (Zi/ei) + ((1-Zi)/(1-ei)) where zi= 1 or 0

surgdata_filtered$weight<-ifelse(surgdata_filtered$endar==1,1/surgdata_filtered$p1,1/(1-surgdata_filtered$p1))    # create the variable with the weights, for treatment group: 1/p for control group: 1/(1-p)
tapply(surgdata_filtered$weight,surgdata_filtered$endar,summary)     




# Map distribution

# Dissimilar propensity - proportions of observations by propensity score of endar and not endar patients.Unbalanced distribution.   

ggplot(data = surgdata_filtered,
       mapping = aes(x=p,fill=factor(endar),weight=NULL)) +
  layer(geom="density",stat="density",position="identity") +
  theme_bw() +
  theme(legend.key = element_blank()) + 
  labs(title = "Original Cohort")



# IPTW logistic regression- Weighted logistic regression model. Estimates a logistic regression model by maximizing the weighted likelihood.
# The ML estimator includes also the matrix with weights. 

WLogit<-glm(dead~endar,weights=weight,data=surgdata_filtered,family=binomial(link="logit"))
round(cbind(Beta=coef(WLogit),confint(WLogit),P=coef(summary(WLogit))[,4],exp(cbind(OR=coef(WLogit),confint(WLogit)))),4)

## ATT Inverse Probability of Treatment Weighting (IPTW) ----

surgdata_filtered$weight_att<-ifelse(surgdata_filtered$endar==1,1,surgdata_filtered$p1/(1-surgdata_filtered$p1))  # Create the variable with the weights, for treatment group: 1 for control group: p/(1-p)
tapply(surgdata_filtered$weight_att,surgdata_filtered$endar,summary)                        # Show summarizing statistics of weights for each level of endar


## IPTW logistic regression- Weighted logistic regression model. 

WLogit2<-glm(dead~endar,weights=weight_att,data=surgdata_filtered,family=binomial(link="logit"))
round(cbind(Beta=coef(WLogit2),confint(WLogit2),P=coef(summary(WLogit2))[,4],exp(cbind(OR=coef(WLogit2),confint(WLogit2)))),4)

