#the script examines the renal function of patients in the various clusters on the basis of
#- the diagnoses (+plot)
#- the egfr values
#- the values for serum creatinine

# libraries
library('dplyr')
library('tidyr')
library('ggplot')

# INPUT

data_clustering
conditions<- FHIR_Resource_condition
observations<-ILM_dataset(laboratory data)

#-------------------------------------------------------------------------------  
# Diseases of the urogenital system
#-------------------------------------------------------------------------------  
cond_df <- merge(conditions, data_clustering, by=c("PatVID","CASE_ID"))
subset_N<-cond_df %>%
  filter(grepl("^N", C_code_coding_code))

subset_N[which(subset_N$number <= 8),"subgroupN"] <- "N00-N08"
subset_N[which(subset_N$number >= 10 & subset_N$number <= 16),"subgroupN"] <- "N10-N16"
subset_N[which(subset_N$number >= 17 & subset_N$number <= 19),"subgroupN"] <- "N17-N19"
subset_N[which(subset_N$number >= 20 & subset_N$number <= 23),"subgroupN"] <- "N20-N23"
subset_N[which(subset_N$number >= 25 & subset_N$number <= 29),"subgroupN"] <- "N25-N29"
subset_N[which(subset_N$number >= 30 & subset_N$number <= 39),"subgroupN"] <- "N30-N39"
subset_N[which(subset_N$number >= 40 & subset_N$number <= 51),"subgroupN"] <- "N40-N51"
subset_N[which(subset_N$number >= 60 & subset_N$number <= 64),"subgroupN"] <- "N60-N64"
subset_N[which(subset_N$number >= 70 & subset_N$number <= 77),"subgroupN"] <- "N70-N77"
subset_N[which(subset_N$number >= 80 & subset_N$number <= 98),"subgroupN"] <- "N80-N98"
subset_N[which(subset_N$number == 99),"subgroupN"] <- "N99-N99"
# total count of patients per cluster, calculate percentages, use ggplot for representation (see DiaClusT_conditions.R)

# N18 in detail

subset_N18<-subset_N[which(subset_N$number==18),]
subset_N18$suppl<-substr(subset_N18$C_code_coding_code,5,6)
subset_N18[which(subset_N18$suppl == 1),"label"] <- "stage 1"
subset_N18[which(subset_N18$suppl == 2),"label"] <- "stage 2"
subset_N18[which(subset_N18$suppl == 3),"label"] <- "stage 3"
subset_N18[which(subset_N18$suppl == 4),"label"] <- "stage 4"
subset_N18[which(subset_N18$suppl == 5),"label"] <- "stage 5"
subset_N18[which(subset_N18$suppl == 80),"label"] <- "unilateral"
subset_N18[which(subset_N18$suppl == 89),"label"] <- "other, stage unspecified"
subset_N18[which(subset_N18$suppl == 9),"label"] <- "chronic kidney disease, unspecified"

# total count of patients per cluster, calculate percentages, use ggplot for representation (see DiaClusT_conditions.R)


#-------------------------------------------------------------------------------
# egfr
#-------------------------------------------------------------------------------
observations <- observations %>%
arrange(PatVID, ErgeDatZeit) %>%
group_by(PatVID) %>%
mutate(visit = match(ErgeDatZeit, unique(ErgeDatZeit)))

  
data_egfr<-observations[which(observations$Loinc.Code %in% c("88294-4")),]
  
meets_45_threshold <- function(data) {
  data_below_45 <- data %>%
    filter(Ergebnis < 45) %>%
    arrange(ErgeDatZeit)
  
  if (nrow(data_below_45) == 0) {
    return(FALSE)
  }
  min_date <- min(data_below_45$ErgeDatZeit)
  max_date <- max(data_below_45$ErgeDatZeit)
  date_range <- as.numeric(difftime(max_date, min_date, units = "days"))
  
  return(date_range >= 90)
}

# check if patient meets a threshold (without time range)
meets_threshold <- function(data, threshold) {
  return(any(data$Ergebnis < threshold))
}

results <- data.frame(cluster = integer(), threshold = integer(), PatVID = character())

for (cluster in unique(data_egfr$cluster)) {
  for (threshold in c(15, 45, 60)) {
    if (threshold == 45) {
      # threshold 45 (with time range)
      patients_meeting_threshold <- data_egfr %>%
        filter(cluster == !!cluster) %>%
        group_by(PatVID) %>%
        summarise(meets_threshold = meets_45_threshold(pick(everything()))) %>%
        filter(meets_threshold) %>%
        select(PatVID)
    } else {
      # thresholds 15 and 60 (no time range)
      patients_meeting_threshold <- data_egfr %>%
        filter(cluster == !!cluster) %>%
        group_by(PatVID) %>%
        summarise(meets_threshold = meets_threshold(pick(everything()), threshold)) %>%
        filter(meets_threshold) %>%
        select(PatVID)
    }
    
    results <- rbind(results, data.frame(cluster = cluster, threshold = threshold, PatVID = patients_meeting_threshold$PatVID))
  }
}

# some patients are in more than one cluster, we have to remove them
duplicated_patvid <- results[duplicated(results$PatVID) | duplicated(results$PatVID, fromLast = TRUE), ]

# filter out rows where PatVID is duplicated with different cluster assignment
filtered_results <- results[!(duplicated(results$PatVID) & duplicated(results$PatVID, fromLast = TRUE) & duplicated_patvid$PatVID %in% results$PatVID & duplicated_patvid$cluster != results$cluster), ]

# calculate how many rows were excluded
exclusion_count <- nrow(results) - nrow(filtered_results)

# filter results --> patient is only counted for the lowest threshold
final_results <- filtered_results %>%
  group_by(PatVID, cluster) %>%
  arrange(threshold) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(cluster,threshold) %>%
  summarise(count = n())


# mean per cluster
summary_parameters <- data_egfr %>%
  group_by(cluster, Loinc.Code) %>%
  summarise(mean_value = mean(Ergebnis, na.rm = TRUE),
            sd_value = sd(Ergebnis, na.rm=TRUE),
            min_value = min(Ergebnis, na.rm = TRUE),
            max_value = max(Ergebnis, na.rm = TRUE),
            med_value= median(Ergebnis,na.rm=TRUE))

# add missing patients (threshold >60 to dataset)


#-------------------------------------------------------------------------------
# serum_creatinine/ c-reactive protein necessary?  (prepare_lab_DiaClusT.R) 
#-------------------------------------------------------------------------------
# choose creatinine + removal of data points that lie outside the measurable range
data_subset<-observations[which(observations$Loinc.Code %in% c("14682-9")& observations$spec_char=="0"),]	
data<-merge(data_subset,data_clustering[,c("PatVID","gender")], by="PatVID")
data<-unique(data)
# split into data_male, data_female by gender 

# remove outliers (>5sd from mean)
remove_outliers <- function(x) {
  mean_val <- mean(x)
  sd_value <- sd(x)
  lower_bound <- mean_val - 5 * sd_value
  upper_bound <- mean_val + 5 * sd_value
  x <- if_else(x >= lower_bound & x <= upper_bound, x, NA_real_)
}

data <- data_male %>%	# female respectively
  mutate(across([column containing measured values], remove_outliers)) %>%
  drop_na()

# limitation to first visit
data <- data %>%
  arrange(PatVID, ErgeDatZeit) %>%
  group_by(PatVID) %>%
  mutate(visit = match(ErgeDatZeit, unique(ErgeDatZeit)))

data<-data[which(data$visit==1),]

# print mean, median, range, sd
# graphical overview using ggplot2
