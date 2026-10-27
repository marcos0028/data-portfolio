#Standardization
load("acvddata_2015_2017.Rda")

library(gmodels)
library(epitools)
library(Hmisc)

table(acvddata$cl_age)
#We need to merge class 1,2 and 3 since they have few observations 

acvddata$recl_age<-ifelse(acvddata$cl_age %in% c(1,2,3),3,acvddata$cl_age)

# Create a common structure for the denominator/nominator
n_terms<-length(levels(as.factor(acvddata$recl_age)))*length(levels(as.factor(acvddata$males)))
std_m<-matrix(c(rep(0,1,n_terms)),length(levels(as.factor(acvddata$recl_age))))
rownames(std_m) <- levels(as.factor(acvddata$recl_age))
colnames(std_m) <- levels(as.factor(acvddata$males))

# Population table for the whole population

pop_ij<-table(acvddata[,c("recl_age")],acvddata[,c("males")])
totpop<-sum(pop_ij)
# Standard population denominators
std_den <- table(
  acvddata$recl_age,
  acvddata$males
)

# Standard population numerators
std_num <- table(
  acvddata$recl_age[acvddata$long_stay == 1],
  acvddata$males[acvddata$long_stay == 1]
)

std_rate_ij <- std_num / std_den

# Clean up
std_rate_ij[is.nan(std_rate_ij)] <- 0
std_rate_ij[is.na(std_rate_ij)]  <- 0


freq <- table(acvddata$ny_hosp_id)

# order by frequency (descending)
freq[order(freq, decreasing = TRUE)]

#Ricordati di farlo prima
quantile(acvddata$los)
acvddata$long_stay <- ifelse(acvddata$los>8, 1, 0)
# CALCULATE CRUDE AND DIRECT STANDARDIZED RATES FOR HOSPITAL 19
target_group<-subset(acvddata,acvddata$ny_hosp_id=="19")          # Select from acvddata only patients of hosp_id 19
outcome_intarget<-subset(target_group,target_group$long_stay==1)     # Select from patients of hosp_id 19 only the expired ones 

label_target_group<-"Hospital 19"                               # Store the label name

num_data<-table(outcome_intarget[,c("recl_age")],outcome_intarget[,c("males")])   # Age - gender table of dead patients of hosp_id 19. Cells contain dij.
den_data<-table(target_group[,c("recl_age")],target_group[,c("males")])           # Age - gender table of patients of hosp_id 19. Cells contain nij.


asr_ijt<-as.matrix(num_data/den_data)                                        # Age - gender table with dij/nij cells   
#remove NaNs
asr_ijt[is.nan(asr_ijt)] <- 0
asr_ijt[is.na(asr_ijt)] <- 0


cr_t<-((sum(num_data)/sum(den_data) ))*100                              # Crude rate=sum(dij)/Sum(nij) *100
sr_t<-sum(unclass(asr_ijt)*unclass(pop_ij))/totpop*100                  # Standardized rate=sum(dij/nij * Nij)/N *100
sr_t.se<- sqrt((sr_t/100*(1-sr_t/100))/(sum(den_data)))                 # Standard error  
sr_t.ll95  <- sr_t - 1.96 * sr_t.se                                     # Lower 95% C.I. 
sr_t.ul95  <- sr_t + 1.96 * sr_t.se                                     # Upper 95% C.I.

output<-data.frame(label_target_group,round(cr_t,2),round(sr_t,2),round(sum(den_data),2),round(sr_t.ll95,2),round(sr_t.ul95,2))
names(output)<-c("Unit","Crude_Rate","Stand_Rate","N","Lower95","Upper95")
output
#Repeat for the other hospitals 

## Indirect Standardization ----
# Adopts AHRQ formulas for indirect standardization
# Regression Standardization estimate the model using quality score as an outcome of a different logistic regression model


# Fit a logistic model with death outcome and class age and males control variables 

DLogit<-glm(long_stay~cl_age+males,family = binomial("logit"),data=acvddata)
  
#Show selected results: Coefficients, C.I. of the coefficients, Exp(Coefficiets)=Odd Ratios, C.I. of the OR and P-value of wald test 
  
round(cbind(Beta=coef(DLogit),confint(DLogit),P=coef(summary(DLogit))[,4],exp(cbind(OR=coef(DLogit),confint(DLogit)))),4)
  
# Linear combination of the predictors
  
acvddata$p<-predict(DLogit,newdata=acvddata,type="response")           # Create a variable p with predicted probabilities from fitted logistic model              

# Total deaths from ami patients divided by the number of observations.Average outcome in the entire sample:reference population rate

Ybar<-sum(acvddata$long_stay,na.rm=TRUE)/dim(acvddata)[1]                   # Overall death rate (alpha)


# Average predicted for ALL dataset
mean(acvddata$p)                                                      # Error, with missing values.         
mean(acvddata$p,na.rm=TRUE)                                           # Without missing values 
sum(acvddata$p,na.rm=TRUE)/dim(acvddata)[1]                            # Divided by the number of observations
sum(acvddata$p,na.rm=TRUE)/sum(!is.na(acvddata$p))                     # Divided by the number of observations WITHOUT missing values

# Calculate Standardized Rates for hosp_id = 1
O1<-mean(acvddata[acvddata$ny_hosp_id=="1",c("long_stay")],na.rm=TRUE)     # Oi=(1/ni )sum(Yij ) OBSERVED DEATH RATE at hosp_id 1
E1<-mean(acvddata[acvddata$ny_hosp_id=="1",c("p")],na.rm=TRUE)        # Ei = (1/ni )sum(Pij) EXPECTED DESTH RATE at hosp_id 1O17
N1<-dim(acvddata[acvddata$ny_hosp_id=="1",])[1]                       # Number of patients at hosp_id 1.
P1j<-acvddata[acvddata$ny_hosp_id=="1",c("p")]                        # Predicted probability for the patients of hospital 1, for every j-th patint


rar1<-Ybar*(O1/E1)                                                  # Risk adjusted rate (indirect standardized rate)
rar1.se<-sqrt( (Ybar/E1)^2 *(1/N1)^2 * sum(P1j*(1-P1j),na.rm=TRUE)) # Standard error of risk adjusted rate
rar1.ll95  <- rar1 - 1.96 * rar1.se                                 # Lowwer 95% C.I. 
rar1.ul95  <- rar1 + 1.96 * rar1.se                                 # Upper 95% C.I.

# output label, crude death rate, standardized rate, nr of observations in hosp_id 1 and C.I. 
output2<-data.frame("HOSPITAL 1",round(O1*100,2),round(rar1*100,2),N1,round(rar1.ll95*100,2),round(rar1.ul95*100,2))
names(output2)<-c("Unit","CR","SR","N","Lower95","Upper95")   # Rename the variables

# View output
output2
#We do for all hospital
calc_rates_for_hospital <- function(hosp_id,
                                    acvddata,
                                    pop_ij,
                                    totpop,
                                    std_rate_ij,
                                    Ybar,
                                    p_var = "p") {
  
  ## ---------------------------
  ## Subset data
  ## ---------------------------
  target_group <- subset(acvddata, ny_hosp_id == hosp_id)
  outcome_intarget <- subset(target_group, long_stay == 1)
  
  label_target_group <- paste("Hospital", hosp_id)
  
  ## ---------------------------
  ## Direct standardization
  ## ---------------------------
  num_data <- table(outcome_intarget$recl_age,
                    outcome_intarget$males)
  
  den_data <- table(target_group$recl_age,
                    target_group$males)
  
  asr_ijt <- as.matrix(num_data / den_data)
  asr_ijt[is.nan(asr_ijt) | is.na(asr_ijt)] <- 0
  
  cr_t <- (sum(num_data) / sum(den_data)) * 100
  
  sr_t <- sum(unclass(asr_ijt) * unclass(pop_ij)) / totpop * 100
  
  sr_t.se <- sqrt((sr_t / 100 * (1 - sr_t / 100)) / sum(den_data))
  sr_t.ll95 <- sr_t - 1.96 * sr_t.se
  sr_t.ul95 <- sr_t + 1.96 * sr_t.se
  
  ## ---------------------------
  ## Expected rate (indirect, age–sex)
  ## ---------------------------
  exp_counts <- den_data * std_rate_ij
  exp_rate <- (sum(exp_counts) / sum(den_data)) * 100
  
  ## ---------------------------
  ## MODEL-BASED RISK-ADJUSTED RATE
  ## ---------------------------
  Oi <- mean(target_group$long_stay, na.rm = TRUE)
  Ei <- mean(target_group[[p_var]], na.rm = TRUE)
  Ni <- nrow(target_group)
  Pij <- target_group[[p_var]]
  
  rar <- Ybar * (Oi / Ei)
  
  rar.se <- sqrt(
    (Ybar / Ei)^2 * (1 / Ni)^2 *
      sum(Pij * (1 - Pij), na.rm = TRUE)
  )
  
  rar.ll95 <- rar - 1.96 * rar.se
  rar.ul95 <- rar + 1.96 * rar.se
  
  ## ---------------------------
  ## Output
  ## ---------------------------
  output <- data.frame(
    Unit = label_target_group,
    Crude_Rate = round(cr_t, 2),
    Expected_Rate = round(exp_rate, 2),
    Stand_Rate = round(sr_t, 2),
    Stand_CI = paste0("(", round(sr_t.ll95, 2), " – ", round(sr_t.ul95, 2),")"),   # Combine CI
    N = Ni,
    Risk_Adjusted_Rate = round(rar * 100, 2),
    Risk_Adjusted_CI = paste0("(",round(rar.ll95 * 100, 2), " – ", round(rar.ul95 * 100, 2),")") # Combine CI
  )
  
  return(output)
}

#Run it for other hospitals 
hospital_ids <- c("19", "3","96", "124", "184", "100")

results <- do.call(
  rbind,
  lapply(hospital_ids,
         calc_rates_for_hospital,
         acvddata = acvddata,
         pop_ij = pop_ij,
         totpop = totpop,
         std_rate_ij = std_rate_ij,
         Ybar = Ybar)
)


results

#The distance between Crude_rate and Standard_rate are close with each other so case-mix (age-sex)
#doesn't change much the rate of longer staying in hospital
#Direct standardization: we compare stand_rate with each other, Hospital 19 is the best, while 96 is the worst
#Indirect standardization: we computed the expected rate on the standard population (overall)
#And we find out that only hospital 19 perform better than the standard 

#Rar is the ratio between the observed events over the expected number of events multiplied 
#by the reference population rate (p, that we obtain from the linear model)

####NOTE! We should use different covariates to improve the prediction, in this case
#Rar is calculated with only age and sex 
## Funnel Plots using the NY data ----
library(ggplot2)
m_acvd<-aggregate(acvddata$long_stay,by=list(acvddata$ny_hosp_id),FUN="mean")             
m_acvd_nrow<-aggregate(acvddata$long_stay,by=list(acvddata$ny_hosp_id),FUN=NROW)          
names(m_acvd)<-c("ny_hosp_id","cr")                                                
names(m_acvd_nrow)<-c("ny_hosp_id","n")                                            
m_acvd<-merge(m_acvd,m_acvd_nrow,by=c("ny_hosp_id"))                                 
m_acvd$cr<-m_acvd$cr*100                                                            
hosp_county <- aggregate(
  ny_county ~ ny_hosp_id,
  acvddata,
  FUN = function(x) unique(x)[1]
)              
names(hosp_county)<-c("ny_hosp_id","ny_county")
z_acvd<-merge(m_acvd,hosp_county,by=c("ny_hosp_id"),all.x=TRUE)                    
z_acvd$county_up<-ifelse(z_acvd$ny_county>20,2,1)                                  


funnel_plot(title="Acute Cerebrovascular Diseases Longer stay Rates\n in the State of New York - Year 2015-2017", # Graph Title
            names=z_acvd$ny_hosp_id,                # variable for group names
            names_outliers=1,                      # differently plot outliers by names (0=No;1=Yes)
            plot_names=1,                          # plot names instead of dots
            in_colour=1,                           # colour points within boundaries (0=No;1=Yes)
            colour_var=z_acvd$county_up,            # variable for colouring groups (numeric)
            colour_values=c("indianred2","blue"),  # list of colours for all levels of colour_var
            rate=z_acvd$cr,                         # value of standardized rates
            unit=100,                              # denominator of rates
            population=z_acvd$n,                    # total number of subjects for each rate
            binary=1,                              # binary variable (0=No;1=Yes)
            p_mean="weighted",                     # Unweighted, Weighted or specified value
            filename="",                           # output graph filename 
            graph="acvd_long_stay",                 # name of graph object
            pdf_height=3.5,                        # height pdf output
            pdf_width=6,                           # width pdf output
            ylab="Indicator per 100",              # y axis label
            xlab="Total N",                        # x axis label
            selectlab="County",                    # Label for group legend
            selectlev=c("1","2"),                  # Values of group legend
            dot_size=1.5,                          # Scaling factor for dots in the funnel plot
            dfout="dfout")                         # funnel dataset saved in global output object

print(acvd_long_stay)

save(acvddata,file="acvddata_2015_2017.Rda")

