# The script loads the available data, 
# - filters out patients over 18 years with T2DM, 
# - adds/corrects & filters the BMI and calculates the ratio TRIG/HDL to create the dataset for clustering
# - preprocesses conditions_dataset to add the ICD_groups


# load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# INPUT
patients <- FHIR_Resource_patient
encounters <- FHIR_Resource_encounter
conditions <- FHIR_Resource_condition
data_bmi <- BMI_dataset
observations <- ILM_dataset(laboratory data)

#-------------------------------------------------------------------------------
# calculate age of patients, delete birthdate
#-------------------------------------------------------------------------------

temp<-encounters %>%                      # look for date of first occurrence of PID
  group_by(PID) %>%
  arrange(E_period_start) %>%
  slice(1L) %>% select (PID, E_period_start)

temp$E_period_start<-gsub("T..*","",temp$E_period_start)
temp$PID<-gsub("\\Patient/","",temp$PID)    
patients<-merge(patients, temp, by="PID")
patients$birthDate <- ifelse(nchar(patients$birthDate) == 4, paste0(patients$birthDate, "-07-01"), patients$birthDate)  # add month-day if missing
patients$birthDate <- parse_date_time(patients$birthDate, c('%m-%Y', '%y', '%Y-%m-%d', '%m%%Y', '%m.%Y'))
patients$age<-trunc((patients$birthDate %--% patients$E_period_start) / years(1))

patients<-patients[, !names(patients) %in% c("birthDate","E_period_start")]  

#-------------------------------------------------------------------------------
# filter patients with T2DM
#-------------------------------------------------------------------------------
conditions$PID<-gsub("\\Patient/","",conditions$PID)
conditions$EID<-gsub("\\Encounter/","",conditions$EID)

conditions_filtered <- conditions %>%
  filter(grepl("^E11",C_code_coding_code, ignore.case = TRUE))
# typo in C_code_coding code
conditions[conditions$C_code_coding_code=="E11.90#","C_code_coding_code"]<-"E11.90"
conditions[conditions$C_code_coding_code=="E11.41#","C_code_coding_code"]<-"E11.41"

unique_PID <- unique(conditions_filtered$PID)

# take all available diagnosis of these T2DM_patients
conditions_filtered_all <- conditions %>%
  filter(PID %in% unique_PID)

# merge filtered and patients dataset based on PID
patients_conditions <- inner_join(patients,conditions_filtered_all, by = "PID")

#-------------------------------------------------------------------------------
# calculate/add (with the help of adipositas condition) BMI 
#-------------------------------------------------------------------------------

dim(data_bmi[which(data_bmi$Groesse!=0 | data_bmi$Gewicht!=0.0),])
data_bmi<-data_bmi[which(data_bmi$Groesse!=0 | data_bmi$Gewicht!=0.0),]
for (i in 1:nrow(data_bmi)){ # rows where weight/height and units are swapped
	if(data_bmi[i,"Groesse"] < data_bmi[i,"Gewicht"] & data_bmi[i,"Eh."]=="KG" & data_bmi[i,"Eh..1"]=="CM"){
		original_height<-data_bmi[i,"Groesse"]
		data_bmi[i,"Groesse"]<-data_bmi[i,"Gewicht"]
		data_bmi[i,"Gewicht"]<-original_height
		data_bmi[i,"Eh."]<-"CM"
		data_bmi[i,"Eh..1"]<-"KG"
	}
}
data_bmi$BMI<-NA
# different units
# transform some meters in cm, if it's not higher than 2.2m
for (i in 1:nrow(data_bmi)){
	if (data_bmi[i,"Eh."]=="M" & data_bmi[i,"Groesse"] <2.2){
		data_bmi[i,"Groesse"]<-data_bmi[i,"Groesse"]*100
		data_bmi[i,"Eh."]<-"CM"
	}
	if (data_bmi[i,"Eh."]=="M" & data_bmi[i,"Groesse"] >100){   # change unit to CM
		data_bmi[i,"Eh."]<-"CM"
	}
	if (data_bmi[i,"Eh."]=="" & data_bmi[i,"Groesse"] >100){  # add unit CM
		data_bmi[i,"Eh."]<-"CM"
	}
	if (data_bmi[i,"Eh..1"]=="CM" & data_bmi[i,"Gewicht"] >35){ # transform CM --> KG for the weight
		data_bmi[i,"Eh..1"]<-"KG"
	}
	if (data_bmi[i,"Eh..1"]=="K" & data_bmi[i,"Gewicht"]){ # transform K --> KG 
		data_bmi[i,"Eh..1"]<-"KG"
	}
	if (data_bmi[i,"Eh..1"]=="" & data_bmi[i,"Gewicht"] >35){  # add unit
		data_bmi[i,"Eh..1"]<-"KG"
	}
}

for(i in 1:nrow(data_bmi)){
	if(data_bmi[i,"Eh."]=="CM" & data_bmi[i,"Eh..1"]=="KG"){  # need possible ranges here
		data_bmi[i,"BMI"] <- data_bmi[i,"Gewicht"] / ((data_bmi[i,"Groesse"]/100) ^ 2)
	}
}


# # replace missing BMI with the help of adipositas-diagnosis 
# search for IDs of T2DM patients that are not occuring in data_bmi --> add them to missing_ids

adipositas <- conditions[conditions[,Patient-ID] %in% missing_ids,] %>%
		filter(grepl("^E66",C_code_coding_code, ignore.case = TRUE))

k<-nrow(bmi)+1
for (i in 1:nrow(adipositas)){
	if (adipositas$C_code_coding_version != "2022"){
		if(grepl("0$",adipositas[i,"C_code_coding_code"])){
			bmi[k, "Fall"]<- adipositas[i,"FallNr"]
			bmi[k, "Patient"]<- adipositas[i,"PatVID"]
			bmi[k, "BMI"]<- 30
		}
		if(grepl("1$",adipositas[i,"C_code_coding_code"])){
			bmi[k, "Fall"]<- adipositas[i,"FallNr"]
			bmi[k, "Patient"]<- adipositas[i,"PatVID"]
			bmi[k, "BMI"]<- 35
		}
		if(grepl("2$",adipositas[i,"C_code_coding_code"])){
			bmi[k, "Fall"]<- adipositas[i,"FallNr"]
			bmi[k, "Patient"]<- adipositas[i,"PatVID"]
			bmi[k, "BMI"]<- 40
		}
		else{
			bmi[k, "Fall"]<- adipositas[i,"FallNr"]
			bmi[k, "Patient"]<- adipositas[i,"PatVID"]
			if(grepl("0$",adipositas[i,"C_code_coding_code"])){
				bmi[k, "BMI"]<- 30
			}
			if(grepl("1$",adipositas[i,"C_code_coding_code"])){
				bmi[k, "BMI"]<- 35
			}
			if(grepl("6$",adipositas[i,"C_code_coding_code"])){
				bmi[k, "BMI"]<- 40
			}
			if(grepl("7$",adipositas[i,"C_code_coding_code"])){
				bmi[k, "BMI"]<- 50
			}
			if(grepl("8$",adipositas[i,"C_code_coding_code"])){
				bmi[k, "BMI"]<- 60
			}
		}
		k<-k+1
	}
}

# set range for meaningful BMI
filtered_bmi <- data_bmi[data_bmi$BMI >= 12 & data_bmi$BMI <= 80, ]
filtered_bmi<-unique(filtered_bmi)


#-------------------------------------------------------------------------------
# calculate TRIG/HDL-ratio
#-------------------------------------------------------------------------------
## choose T2DM patients out of lab data
unique_PID<-unique(patients_conditions$PatVID)
df <- observations %>%
  filter(PatVID %in% unique_PID)

# calculate ratio with a maximum of 14 days between the two measurements
df <- df %>%
  group_by(PatVID) %>%
  arrange(PatVID,ErgeDatZeit) %>%  
  mutate(
    time_diff = abs(difftime(ErgeDatZeit, lag(ErgeDatZeit), units = "days")),
    ratio = if_else(
      EID == lag(EID) & Loinc.Code == "14646-4"   & lag(Loinc.Code) == "14927-8" & time_diff <= 14 & all(MethEinheit == first(MethEinheit)), # units are the same  
      lag(as.numeric(Ergebnis)) / as.numeric(Ergebnis),
      NA_real_
    )
  ) %>%
  ungroup()


# KEEP ONLY ROWS WITH A TRIG/HDL-RATIO
rows_to_keep<-df[!is.na(df$ratio),] 
# enter a fantasy-loinc
rows_to_keep$Loinc.Code<-"11111"
# replace columnname valueQuantity
rows_to_keep$Ergebnis<-rows_to_keep$ratio
# delete unit and reference range
rows_to_keep$MethEinheit<-NA
rows_to_keep$Referenzwertausgaben<-NA

# ADD LOINC+VALUES OF HBA1c
hba1c<-observations[which(observations$Loinc.Code %in% c("17856-6")),]
df<-rbind(hba1c,rows_to_keep)



#-------------------------------------------------------------------------------
# preprocess conditions --> add icd-groups
#-------------------------------------------------------------------------------
conditions$character<-substr(conditions$C_code_coding_code,1,1)
conditions$number <- substr(conditions$C_code_coding_code,2,4)
conditions$number <-gsub("\\.","",conditions$number)

conditions[which(conditions$character == "A" | conditions$character == "B"),"group"] <- "A00-B99"
conditions[which(conditions$character == "C"),"group"] <- "C00-D48"
conditions[which(conditions$character == "D" & conditions$number < 49),"group"] <- "C00-D48"
conditions[which(conditions$character == "D" & conditions$number > 48),"group"] <- "D50-D89"
conditions[which(conditions$character == "E"),"group"] <- "E00-E90"
conditions[which(conditions$character == "F"),"group"] <- "F00-F99"
conditions[which(conditions$character == "G"),"group"] <- "G00-G99"
conditions[which(conditions$character == "H" & conditions$number < 60),"group"] <- "H00-H59"
conditions[which(conditions$character == "H" & conditions$number > 59),"group"] <- "H60-H95"
conditions[which(conditions$character == "I"),"group"] <- "I00-I99"
conditions[which(conditions$character == "J"),"group"] <- "J00-J99"
conditions[which(conditions$character == "K"),"group"] <- "K00-K93"
conditions[which(conditions$character == "L"),"group"] <- "L00-L99"
conditions[which(conditions$character == "M"),"group"] <- "M00-M99"
conditions[which(conditions$character == "N"),"group"] <- "N00-N99"
conditions[which(conditions$character == "O"),"group"] <- "O00-O99"
conditions[which(conditions$character == "Q"),"group"] <- "Q00-Q99"
conditions[which(conditions$character == "R"),"group"] <- "R00-R99"
conditions[which(conditions$character == "S" | conditions$character == "T"),"group"] <- "S00-T98"
conditions[which(conditions$character == "U"),"group"] <- "U00-U99"
conditions[which(conditions$character == "V" | conditions$character == "W"| conditions$character == "X"| conditions$character == "Y"),"group"] <- "V01-Y84"
conditions[which(conditions$character == "Z"),"group"] <- "Z00-Z99"

#-------------------------------------------------------------------------------
# OUTPUT: data_preprocessed, conditions
