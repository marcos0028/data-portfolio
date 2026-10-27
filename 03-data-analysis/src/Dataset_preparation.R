#-------------------------
# CEREBROVASCULAR DISEASE
#-------------------------

# dataset preparation ####
# we want to merge the 3 datasets corresponding to the years from 2015 to 2017 

## 1. apply the "create_hospdata" function to the years ####

# create a function to process the hospital data

rm(list=ls())

create_hospdata <- function(input,output,value_labels="value_labels.txt") { #crea una funzione con 3 parametri
  hospdata <- read.csv(file=input,stringsAsFactors = F,header=T,sep=",")
  print(names(hospdata))
  
  # Creates a dummy variable for gender (come abbiamo visto prima)
  hospdata$males<-0                                                                         
  hospdata$males<-ifelse(hospdata$Gender=="U",NA,hospdata$males)                            
  hospdata$males<-ifelse(hospdata$Gender=="M" & !is.na((hospdata$Gender)),1,hospdata$males) 
  
  ## Extract the levels of the variables
  
  # Assign the unique levels of the variable considering it as factor variable. 
  
  ny_counties<-levels(factor(hospdata$Hospital.County))       # Save the levels of the variable Hospital.County to ny_counties
  ny_areas <- if("Hospital.Service.Area" %in% names(hospdata)){
    levels(factor(hospdata$Hospital.Service.Area))
  } else if("Health.Service.Area" %in% names(hospdata)){
    levels(factor(hospdata$Health.Service.Area))
  } else {
    stop("Service area column not found!")
  }
  ny_age<-levels(factor(hospdata$Age.Group))  
  ny_adm_types<-levels(factor(hospdata$Type.of.Admission))
  ny_races<-levels(factor(hospdata$Race))
  ny_ethnicities<-levels(factor(hospdata$Ethnicity))
  if("CCS.Diagnosis.Description" %in% names(hospdata)) {
    ny_diags <- levels(factor(hospdata$CCS.Diagnosis.Description))
    hospdata$diagnosis <- as.numeric(as.factor(hospdata$CCS.Diagnosis.Description))
  } else if("CCSR.Diagnosis.Description" %in% names(hospdata)) {
    ny_diags <- levels(factor(hospdata$CCSR.Diagnosis.Description))
    hospdata$diagnosis <- as.numeric(as.factor(hospdata$CCSR.Diagnosis.Description))
  } else {
    stop("Diagnosis column not found!")
  }
  if("CCS.Procedure.Description" %in% names(hospdata)) {
    ny_procs <- levels(factor(hospdata$CCS.Procedure.Description))
    hospdata$procedure <- as.numeric(as.factor(hospdata$CCS.Procedure.Description))
  } else if("CCSR.Procedure.Description" %in% names(hospdata)) {
    ny_procs <- levels(factor(hospdata$CCSR.Procedure.Description))
    hospdata$procedure <- as.numeric(as.factor(hospdata$CCSR.Procedure.Description))
  } else {
    stop("Procedure column not found!")
  }
  ny_dispositions<-levels(factor(hospdata$Patient.Disposition))
  ny_severities<-levels(factor(hospdata$APR.Severity.of.Illness.Description))
  ny_risks<-levels(factor(hospdata$APR.Risk.of.Mortality)) 
  
  
  # Store the levels into the working space. (salva i livelli in ambiente globale)
  
  assign("ny_counties",ny_counties,envir=.GlobalEnv)       # assign ny_counties values to ny_counties variable and store in global envoirment
  assign("ny_areas",ny_areas,envir=.GlobalEnv)
  assign("ny_age",ny_age,envir=.GlobalEnv)
  assign("ny_adm_types",ny_adm_types,envir=.GlobalEnv)
  assign("ny_races",ny_races,envir=.GlobalEnv)
  assign("ny_ethnicities",ny_ethnicities,envir=.GlobalEnv)
  assign("ny_diags",ny_diags,envir=.GlobalEnv)
  assign("ny_procs",ny_procs,envir=.GlobalEnv)
  assign("ny_dispositions",ny_dispositions,envir=.GlobalEnv)
  assign("ny_severities",ny_severities,envir=.GlobalEnv)
  assign("ny_risks",ny_risks,envir=.GlobalEnv)
  
  ## Transformation into numerical variables (creazione di colonne numeriche)
  
  # ny_hosp_id
  if("Facility.Name" %in% names(hospdata)){
    hospdata$ny_hosp_id <- as.numeric(as.factor(hospdata$Facility.Name))
  } else if("Hospital.Name" %in% names(hospdata)){
    hospdata$ny_hosp_id <- as.numeric(as.factor(hospdata$Hospital.Name))
  } else {
    stop("Facility/Hospital name column not found!")
  }
  
  # ny_area (already handled above)
  hospdata$ny_area <- if("Hospital.Service.Area" %in% names(hospdata)){
    as.numeric(as.factor(hospdata$Hospital.Service.Area))
  } else {
    as.numeric(as.factor(hospdata$Health.Service.Area))
  }
  
  # Turn the levels of a variable transformed in a factor variable in numeric entries, assigning the index number.
  
  hospdata$ny_hosp_id<-as.numeric(as.factor(hospdata$Facility.Name))  # generates ny_hosp_id variable in hospdata data.frame with the transformed values of Facility_Name 
  hospdata$ny_county<-as.numeric(as.factor(hospdata$Hospital.County))
  hospdata$ny_area <- if("Hospital.Service.Area" %in% names(hospdata)){
    as.numeric(as.factor(hospdata$Hospital.Service.Area))
  } else if("Health.Service.Area" %in% names(hospdata)){
    as.numeric(as.factor(hospdata$Health.Service.Area))
  } else {
    stop("Service area column not found!")
  }
  hospdata$cl_age<-as.numeric(as.factor(hospdata$Age.Group))
  hospdata$zipcode<-as.numeric(as.factor(hospdata$Zip.Code...3.digits))
  hospdata$adm_type<-as.numeric(as.factor(hospdata$Type.of.Admission))
  hospdata$race<-as.numeric(as.factor(hospdata$Race))
  hospdata$ethnicity<-as.numeric(as.factor(hospdata$Ethnicity))
  hospdata$los<-hospdata$Length.of.Stay                                                  
  
  if("Patient.Disposition" %in% names(hospdata)) {
    ny_dispositions <- levels(factor(hospdata$Patient.Disposition))
    hospdata$disposition <- as.numeric(factor(hospdata$Patient.Disposition, levels = ny_dispositions))
    expired_index <- grep("Expired", ny_dispositions)
    hospdata$dead <- 0
    hospdata$dead <- ifelse(hospdata$disposition %in% expired_index, 1, 0)
  } else {
    stop("Patient.Disposition column not found!")
  }
  
  if("CCS.Diagnosis.Description" %in% names(hospdata)) {
    hospdata$diagnosis <- as.numeric(as.factor(hospdata$CCS.Diagnosis.Description))
    ny_diags <- levels(factor(hospdata$CCS.Diagnosis.Description))
  } else if("CCSR.Diagnosis.Description" %in% names(hospdata)) {
    hospdata$diagnosis <- as.numeric(as.factor(hospdata$CCSR.Diagnosis.Description))
    ny_diags <- levels(factor(hospdata$CCSR.Diagnosis.Description))
  } else {
    stop("Diagnosis column not found!")
  }  
  if("CCS.Procedure.Description" %in% names(hospdata)) {
    hospdata$procedure <- as.numeric(as.factor(hospdata$CCS.Procedure.Description))
    ny_procs <- levels(factor(hospdata$CCS.Procedure.Description))
  } else if("CCSR.Procedure.Description" %in% names(hospdata)) {
    hospdata$procedure <- as.numeric(as.factor(hospdata$CCSR.Procedure.Description))
    ny_procs <- levels(factor(hospdata$CCSR.Procedure.Description))
  } else {
    stop("Procedure column not found!")
  }
  hospdata$drg<-as.numeric(as.factor(hospdata$APR.DRG.Code))
  hospdata$mdc<-as.numeric(as.factor(hospdata$APR.MDC.Code))
  hospdata$severity<-as.numeric(as.factor(hospdata$APR.Severity.of.Illness.Description))
  hospdata$risk<-as.numeric(as.factor(hospdata$APR.Risk.of.Mortality))
  hospdata$payment_type<-as.numeric(as.factor(hospdata$Payment.Typology.1))
  hospdata$cost<-hospdata$Total.Charges
  
  # Dummy variable for surgical. The not available entries were recorded as Not Applicable in the original variable.
  # come abbiamo fatto per il genere, anche per la variabile chirurgia creiamo una dummy
  hospdata$surgical<-0
  hospdata$surgical<-ifelse(hospdata$APR.Medical.Surgical.Description=="Not Applicable",NA,hospdata$surgical)
  hospdata$surgical<-ifelse(hospdata$APR.Medical.Surgical.Description=="Surgical" & !is.na((hospdata$surgical)),1,hospdata$surgical)
  
  # Ami- Acute myocardial infarction, grep function search for matches. Output the index number of Acute myocardial infarction from the ny_diags stored before. 
  # cerca nella lista dei livelli "ny_diags" la parola "infarction", restituisce l'indice del livello corrispondente ad AMI (cioè quante categorie ha all'interno)
  grep("infarction",ny_diags) 
  ny_diags[8] # scrivendo 8 sto prendendo quel particolare livello dal vettore, in questo caso 8 corrisponde ad AMI
  
  # Create a dummy variable for AMI. Note: the index for ami equal to 8 was checked before ( with grep("infarction",ny_diags)) from ny_diags variable. 
  hospdata$ami<-0
  hospdata$ami<-ifelse(hospdata$diagnosis==8,1,hospdata$ami)
  # Se la colonna diagnosis corrisponde all’indice di AMI (8), assegna 1 a ami
  
  # Remove comma from cost variable and different entries exept 0-9. digits.
  hospdata$cost<-as.numeric(gsub("[^0-9.]","",hospdata$cost))
  hospdata$los <-as.numeric(gsub("[^0-9.]","",hospdata$los))
  
  # Create a dataframe with the selected variables. Include all the rows and the specified selected columns.   
  ny_hospdata<-hospdata[,c("ny_hosp_id","ny_county","ny_area","cl_age",
                           "males","zipcode","adm_type","race","ethnicity","los",
                           "disposition","diagnosis","procedure","drg","mdc","severity",
                           "risk","payment_type","cost","surgical",
                           "dead","ami")]
  
  # Save the re-encoded and reduced data frame in .csv format in the working directory. Row names setted as false because first column does not contains the names of the observations.      
  write.csv(ny_hospdata,output,row.names=FALSE)
  
  ##write.csv(ny_hospdata,"ny_hospdata_2017.csv",row.names=FALSE)  # Manual save
  
  ##  Save the levels in an external file 
  
  # Save the levels of factor variables in a text file (.txt).
  
  sink(value_labels)
  # sink()  # Manual run 
  cat ("\n")
  cat ("Counties\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_counties)
  cat ("\n")
  cat ("Areas\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_areas)
  cat ("\n")
  cat ("Age\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_age)
  cat ("\n")
  cat ("Adm Types\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_adm_types)
  cat ("\n")
  cat ("Races\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_races)
  cat ("\n")
  cat ("Ethnicities\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_ethnicities)
  cat ("\n")
  cat ("Diagnoses\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_diags)
  cat ("\n")
  cat ("Procedures\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_procs)
  cat ("\n")
  cat ("Dispositions\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_dispositions)
  cat ("\n")
  cat ("Risks\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_risks)
  sink()
  
  # Remove hospdata (the complete dataset) from Global environment to clear the memory.
  rm(hospdata)
}

create_hospdata2024<- function(input,output,value_labels="value_labels.txt") { #crea una funzione con 3 parametri
  hospdata <- read.csv(file=input,stringsAsFactors = F,header=T,sep=",")
  print(names(hospdata))
  
  # Creates a dummy variable for gender (come abbiamo visto prima)
  hospdata$males<-0                                                                         
  hospdata$males<-ifelse(hospdata$Gender=="U",NA,hospdata$males)                            
  hospdata$males<-ifelse(hospdata$Gender=="M" & !is.na((hospdata$Gender)),1,hospdata$males) 
  
  ## Extract the levels of the variables
  
  # Assign the unique levels of the variable considering it as factor variable. 
  
  ny_counties<-levels(factor(hospdata$Hospital.County))       # Save the levels of the variable Hospital.County to ny_counties
  ny_areas <- if("Hospital.Service.Area" %in% names(hospdata)){
    levels(factor(hospdata$Hospital.Service.Area))
  } else if("Health.Service.Area" %in% names(hospdata)){
    levels(factor(hospdata$Health.Service.Area))
  } else {
    stop("Service area column not found!")
  }
  ny_age<-levels(factor(hospdata$Age.Group))  
  ny_adm_types<-levels(factor(hospdata$Type.of.Admission))
  ny_races<-levels(factor(hospdata$Race))
  ny_ethnicities<-levels(factor(hospdata$Ethnicity))
  if("CCS.Diagnosis.Description" %in% names(hospdata)) {
    ny_diags <- levels(factor(hospdata$CCS.Diagnosis.Description))
    hospdata$diagnosis <- as.numeric(as.factor(hospdata$CCS.Diagnosis.Description))
  } else if("CCSR.Diagnosis.Description" %in% names(hospdata)) {
    ny_diags <- levels(factor(hospdata$CCSR.Diagnosis.Description))
    hospdata$diagnosis <- as.numeric(as.factor(hospdata$CCSR.Diagnosis.Description))
  } else {
    stop("Diagnosis column not found!")
  }
  if("CCS.Procedure.Description" %in% names(hospdata)) {
    ny_procs <- levels(factor(hospdata$CCS.Procedure.Description))
    hospdata$procedure <- as.numeric(as.factor(hospdata$CCS.Procedure.Description))
  } else if("CCSR.Procedure.Description" %in% names(hospdata)) {
    ny_procs <- levels(factor(hospdata$CCSR.Procedure.Description))
    hospdata$procedure <- as.numeric(as.factor(hospdata$CCSR.Procedure.Description))
  } else {
    stop("Procedure column not found!")
  }
  ny_dispositions<-levels(factor(hospdata$Patient.Disposition))
  ny_severities<-levels(factor(hospdata$APR.Severity.of.Illness.Description))
  ny_risks<-levels(factor(hospdata$APR.Risk.of.Mortality)) 
  
  
  # Store the levels into the working space. (salva i livelli in ambiente globale)
  
  assign("ny_counties",ny_counties,envir=.GlobalEnv)       # assign ny_counties values to ny_counties variable and store in global envoirment
  assign("ny_areas",ny_areas,envir=.GlobalEnv)
  assign("ny_age",ny_age,envir=.GlobalEnv)
  assign("ny_adm_types",ny_adm_types,envir=.GlobalEnv)
  assign("ny_races",ny_races,envir=.GlobalEnv)
  assign("ny_ethnicities",ny_ethnicities,envir=.GlobalEnv)
  assign("ny_diags",ny_diags,envir=.GlobalEnv)
  assign("ny_procs",ny_procs,envir=.GlobalEnv)
  assign("ny_dispositions",ny_dispositions,envir=.GlobalEnv)
  assign("ny_severities",ny_severities,envir=.GlobalEnv)
  assign("ny_risks",ny_risks,envir=.GlobalEnv)
  
  ## Transformation into numerical variables (creazione di colonne numeriche)
  
  # ny_hosp_id
  if("Facility.Name" %in% names(hospdata)){
    hospdata$ny_hosp_id <- as.numeric(as.factor(hospdata$Facility.Name))
  } else if("Hospital.Name" %in% names(hospdata)){
    hospdata$ny_hosp_id <- as.numeric(as.factor(hospdata$Hospital.Name))
  } else {
    stop("Facility/Hospital name column not found!")
  }
  
  # ny_area (already handled above)
  hospdata$ny_area <- if("Hospital.Service.Area" %in% names(hospdata)){
    as.numeric(as.factor(hospdata$Hospital.Service.Area))
  } else {
    as.numeric(as.factor(hospdata$Health.Service.Area))
  }
  
  # Turn the levels of a variable transformed in a factor variable in numeric entries, assigning the index number.
  
  hospdata$ny_hosp_id<-as.numeric(as.factor(hospdata$Facility.Name))  # generates ny_hosp_id variable in hospdata data.frame with the transformed values of Facility_Name 
  hospdata$ny_county<-as.numeric(as.factor(hospdata$Hospital.County))
  hospdata$ny_area <- if("Hospital.Service.Area" %in% names(hospdata)){
    as.numeric(as.factor(hospdata$Hospital.Service.Area))
  } else if("Health.Service.Area" %in% names(hospdata)){
    as.numeric(as.factor(hospdata$Health.Service.Area))
  } else {
    stop("Service area column not found!")
  }
  hospdata$cl_age<-as.numeric(as.factor(hospdata$Age.Group))
  hospdata$zipcode<-as.numeric(as.factor(hospdata$Zip.Code))
  hospdata$adm_type<-as.numeric(as.factor(hospdata$Type.of.Admission))
  hospdata$race<-as.numeric(as.factor(hospdata$Race))
  hospdata$ethnicity<-as.numeric(as.factor(hospdata$Ethnicity))
  hospdata$los<-hospdata$Length.of.Stay                                                  
  
  if("Patient.Disposition" %in% names(hospdata)) {
    ny_dispositions <- levels(factor(hospdata$Patient.Disposition))
    hospdata$disposition <- as.numeric(factor(hospdata$Patient.Disposition, levels = ny_dispositions))
    expired_index <- grep("Expired", ny_dispositions)
    hospdata$dead <- 0
    hospdata$dead <- ifelse(hospdata$disposition %in% expired_index, 1, 0)
  } else {
    stop("Patient.Disposition column not found!")
  }
  
  if("CCS.Diagnosis.Description" %in% names(hospdata)) {
    hospdata$diagnosis <- as.numeric(as.factor(hospdata$CCS.Diagnosis.Description))
    ny_diags <- levels(factor(hospdata$CCS.Diagnosis.Description))
  } else if("CCSR.Diagnosis.Description" %in% names(hospdata)) {
    hospdata$diagnosis <- as.numeric(as.factor(hospdata$CCSR.Diagnosis.Description))
    ny_diags <- levels(factor(hospdata$CCSR.Diagnosis.Description))
  } else {
    stop("Diagnosis column not found!")
  }  
  if("CCS.Procedure.Description" %in% names(hospdata)) {
    hospdata$procedure <- as.numeric(as.factor(hospdata$CCS.Procedure.Description))
    ny_procs <- levels(factor(hospdata$CCS.Procedure.Description))
  } else if("CCSR.Procedure.Description" %in% names(hospdata)) {
    hospdata$procedure <- as.numeric(as.factor(hospdata$CCSR.Procedure.Description))
    ny_procs <- levels(factor(hospdata$CCSR.Procedure.Description))
  } else {
    stop("Procedure column not found!")
  }
  hospdata$drg<-as.numeric(as.factor(hospdata$APR.DRG.Code))
  hospdata$mdc<-as.numeric(as.factor(hospdata$APR.MDC.Code))
  hospdata$severity<-as.numeric(as.factor(hospdata$APR.Severity.of.Illness.Description))
  hospdata$risk<-as.numeric(as.factor(hospdata$APR.Risk.of.Mortality))
  hospdata$payment_type<-as.numeric(as.factor(hospdata$Payment.Typology.1))
  hospdata$cost<-hospdata$Total.Charges
  
  # Dummy variable for surgical. The not available entries were recorded as Not Applicable in the original variable.
  # come abbiamo fatto per il genere, anche per la variabile chirurgia creiamo una dummy
  hospdata$surgical<-0
  hospdata$surgical<-ifelse(hospdata$APR.Medical.Surgical.Description=="Not Applicable",NA,hospdata$surgical)
  hospdata$surgical<-ifelse(hospdata$APR.Medical.Surgical.Description=="Surgical" & !is.na((hospdata$surgical)),1,hospdata$surgical)
  
  # Ami- Acute myocardial infarction, grep function search for matches. Output the index number of Acute myocardial infarction from the ny_diags stored before. 
  # cerca nella lista dei livelli "ny_diags" la parola "infarction", restituisce l'indice del livello corrispondente ad AMI (cioè quante categorie ha all'interno)
  grep("infarction",ny_diags) 
  ny_diags[8] # scrivendo 8 sto prendendo quel particolare livello dal vettore, in questo caso 8 corrisponde ad AMI
  
  # Create a dummy variable for AMI. Note: the index for ami equal to 8 was checked before ( with grep("infarction",ny_diags)) from ny_diags variable. 
  hospdata$ami<-0
  hospdata$ami<-ifelse(hospdata$diagnosis==8,1,hospdata$ami)
  # Se la colonna diagnosis corrisponde all’indice di AMI (8), assegna 1 a ami
  
  # Remove comma from cost variable and different entries exept 0-9. digits.
  hospdata$cost<-as.numeric(gsub("[^0-9.]","",hospdata$cost))
  hospdata$los <-as.numeric(gsub("[^0-9.]","",hospdata$los))
  
  # Create a dataframe with the selected variables. Include all the rows and the specified selected columns.   
  ny_hospdata<-hospdata[,c("ny_hosp_id","ny_county","ny_area","cl_age",
                           "males","zipcode","adm_type","race","ethnicity","los",
                           "disposition","diagnosis","procedure","drg","mdc","severity",
                           "risk","payment_type","cost","surgical",
                           "dead","ami")]
  
  # Save the re-encoded and reduced data frame in .csv format in the working directory. Row names setted as false because first column does not contains the names of the observations.      
  write.csv(ny_hospdata,output,row.names=FALSE)
  
  ##write.csv(ny_hospdata,"ny_hospdata_2017.csv",row.names=FALSE)  # Manual save
  
  ##  Save the levels in an external file 
  
  # Save the levels of factor variables in a text file (.txt).
  
  sink(value_labels)
  # sink()  # Manual run 
  cat ("\n")
  cat ("Counties\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_counties)
  cat ("\n")
  cat ("Areas\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_areas)
  cat ("\n")
  cat ("Age\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_age)
  cat ("\n")
  cat ("Adm Types\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_adm_types)
  cat ("\n")
  cat ("Races\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_races)
  cat ("\n")
  cat ("Ethnicities\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_ethnicities)
  cat ("\n")
  cat ("Diagnoses\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_diags)
  cat ("\n")
  cat ("Procedures\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_procs)
  cat ("\n")
  cat ("Dispositions\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_dispositions)
  cat ("\n")
  cat ("Risks\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_risks)
  sink()
  
  # Remove hospdata (the complete dataset) from Global environment to clear the memory.
  rm(hospdata)
}


#-------------------------------------------------------------------------------

# Use the function created. Parameters: input equal to the NYSDOH data and value_labels.txt and  output reduced dataset in .csv format saved 
# in the working directory and text file with the levels of the  factor variables.

### 2022 ####
create_hospdata(input="Data/Hospital_Inpatient_Discharges_(SPARCS_De-Identified)__2022.csv",
                output="ny_hospdata_2022.csv", value_labels="value_labels_2022.txt")

### 2023 ####
create_hospdata(input="Data/Hospital_Inpatient_Discharges_(SPARCS_De-Identified)__2023.csv",
                output="Data/ny_hospdata_2023.csv", value_labels="Data/value_labels_2023.txt")

### 2024 ####
#!change  hospdata$zipcode<-as.numeric(as.factor(hospdata$Zip.Code...3.digits)) into 
# hospdata$zipcode<-as.numeric(as.factor(hospdata$Zip.Code))
create_hospdata2024(input="Hospital_Inpatient_Discharges_(SPARCS_De-Identified)__2024.csv",
                output="ny_hospdata_2024.csv", value_labels="value_labels_2024.txt")

## Load and save the reduced datasets

## 2. load all the reduced datasets ####
hosp2022 <- read.csv("ny_hospdata_2022.csv")
hosp2023 <- read.csv("ny_hospdata_2023.csv")
hosp2024 <- read.csv("ny_hospdata_2024.csv")

## 3. add a year column ####
hosp2022$year <- 2022
hosp2023$year <- 2023
hosp2024$year <- 2024
text <- readLines("value_labels_2022.txt")

grep("cerebr", text, value = T)

#diagnosis 10 in 2022, 2023, 2024 
grep("ENDARTERECTOMY", text, value = T)
#procedure 48 in 2022. 2023, 2024
#
## 4. merge the datasets ####
hospdata <- rbind(hosp2022, hosp2023, hosp2024)

## 5. save the combined dataset ####
save(hospdata,file="ny_hospdata_2022_2024.Rda")

# load dataset
rm(list=ls())
load("ny_hospdata_2022_2024.Rda")

# initial analysis of hospital data ####

source("methods_lab_functions.R")

#install.packages("gmodels")
#install.packages("epitools")
#install.packages("Hmisc")

library(gmodels)
library(epitools)
library(Hmisc)
library(dplyr)
library(tidyr)
hospdata %>% 
  filter(procedure == 48) %>% 
  count(diagnosis, sort = T) %>% 
  slice_head(n =15)
#[313] "Occlusion or stenosis of precerebral or cerebral arteries without infarction"
#[316] "Open wounds of head and neck, subsequent encounter"  
#[58] "Cataract and other lens disorders"    
#[60] "Cerebral palsy"
#[59] "Cerebral infarction" 
#[326] "Other aftercare encounter" 
#[329] "Other general signs and symptoms" 
#[327] "Other and ill-defined cerebrovascular disease"
####list of diagnosis linked to cerebrovascular diseases

#[10] Acute hemorrhagic cerebrovascular disease
#[59] Cerebral infarction
#[313] Occlusion or stenosis of precerebral or cerebral arteries without infarction
#[327] Other and ill-defined cerebrovascular disease
#[423] Sequela of cerebral infarction and other cerebrovascular disease
#[424] Sequela of hemorrhagic cerebrovascular disease
#[462] Transient cerebral ischemia
acvddiseases <- c(10,59,313,327,423,424,462)
acvddata <- hospdata %>% 
  filter(diagnosis %in% acvddiseases,
         complete.cases(.))
# Re-encode risk and severity of illness.
#ny_risks:  # Extreme=2, Major=3, Minor=4, Moderate=5 (the natural order: 0 -> Minor, 1 -> Moderate, 2 -> Major and 3 -> Extreme)
acvddata$risky<-NA                
acvddata$risky<-ifelse(acvddata$risk=="4",0,acvddata$risky) # <= the order is inverted
acvddata$risky<-ifelse(acvddata$risk=="5",1,acvddata$risky)    
acvddata$risky<-ifelse(acvddata$risk=="3",2,acvddata$risky)
acvddata$risky<-ifelse(acvddata$risk=="2",3,acvddata$risky)

#ny_severities:  # Extreme=2, Major=3, Minor=4, Moderate=5 
acvddata$severe<-NA
acvddata$severe<-ifelse(acvddata$severity=="4",0,acvddata$severe)# <= the order is inverted
acvddata$severe<-ifelse(acvddata$severity=="5",1,acvddata$severe)  
acvddata$severe<-ifelse(acvddata$severity=="3",2,acvddata$severe)
acvddata$severe<-ifelse(acvddata$severity=="2",3,acvddata$severe)


acvddata$recl_age<-ifelse(acvddata$cl_age %in% c(1,2,3),3,acvddata$cl_age)

quantile(acvddata$los)
acvddata$long_stay <- ifelse(acvddata$los>11, 1, 0)

# Save the subsets
save(acvddata,file="acvddata_2022_2024.Rda")

#-------------------------------------------------------------------------------
load("acvddata_2022_2024.Rda")
surgdata <- acvddata %>% 
  filter(surgical ==1,
         dead ==0) %>% 
  mutate(endar = ifelse(procedure == 48,1L,0L))
sum(surgdata$endar) #8820

surgdata %>% 
  count(ny_hosp_id, sort = T) %>% 
  slice_head(n = 100)

# Split the data into subsets and return the result in a convenient form. FUN parameter computes summary statistics which can be applied to all data subsets.
m_acvd <- aggregate(surgdata$los,
                    by = list(surgdata$ny_hosp_id),
                    FUN = median,
                    na.rm = TRUE)

m_acvd_nrow <- aggregate(surgdata$los,
                         by = list(surgdata$ny_hosp_id),
                         FUN = "NROW")

names(m_acvd) <- c("ny_hosp_id", "median_los")
names(m_acvd_nrow) <- c("ny_hosp_id", "n")

m_acvd <- merge(m_acvd, m_acvd_nrow, by = "ny_hosp_id")
#List of hospitals with median los, patient who has undergone surgical procedure

quantile(surgdata$los,0.75)
wilcox.test(los~surgical, acvddata)

surgdata <- surgdata %>% 
  mutate(great_los =ifelse(los>quantile(surgdata$los,0.75),1,0))

chisq.test(table(surgdata$great_los, surgdata$endar))
tab <- table(surgdata$great_los, surgdata$endar)
odds_ratio <- (tab[2,2] / tab[2,1]) / (tab[1,2] / tab[1,1])

model <- glm(great_los~endar, family=binomial("logit"), surgdata)
summary(model)

endardata <- surgdata %>% 
  filter(endar ==1)
noendardata <- surgdata %>% 
  filter(endar ==0)

surgdata %>%
  group_by(endar, cl_age, males) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(endar) %>%
  mutate(prop = n / sum(n))

surgdata%>%
  pivot_longer(
    cols = c(cl_age, males, race),
    names_to = "covariate",
    values_to = "category"
  ) %>%
  group_by(endar, covariate, category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(endar, covariate) %>%
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(x = category, y = prop, fill = factor(endar))) +
  geom_col(position = "dodge") +
  facet_wrap(~ covariate, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "",
    y = "Proportion",
    fill = "endar"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

