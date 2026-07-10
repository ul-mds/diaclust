# the script examines the various diagnoses of the cohort,
# - examines the combination of diagnoses that occur most frequently
# - calculates the Elixhauser comorbidity scores,
# - examines especially the occurrence of coronary events


# libraries
library(UpSetR)
library(tidyr)


# INPUT
data_clustering
conditions <- FHIR_Resource_condition

# add visit count to dataset

data_clustering <- data_clustering %>%
  arrange(PatVID, ErgeDatZeit) %>%
  group_by(PatVID) %>%
  mutate(visit = match(ErgeDatZeit, unique(ErgeDatZeit)))

#-------------------------------------------------------------------------------
# check conditions per cluster
#-------------------------------------------------------------------------------
cond_df<-merge(data_clustering,conditions,by=c("PatVID","FallNr"))
#cond_df<-cond_df[which(cond_df$visit==1),]                                    # limitation to first visit of each patient --> if wanted
cond_df<-unique(cond_df)
table(cond_df$group, cond_df$cluster)

# example graphical overview
cluster_total_counts <- cond_df %>%
  distinct(PatVID, group, cluster) %>%
  group_by(cluster) %>%
  summarise(total = n_distinct(PatVID))

# calculate the percentages
cond_group <- cond_df %>%
  group_by(cluster,group) %>%
  distinct(PatVID, group) %>%
  summarise(count = n()) %>%
  left_join(cluster_total_counts, by = "cluster") %>%
  mutate(percentage = count / total * 100)

ggplot(data = na.omit(cond_group), aes(x = group, y = percentage, fill = factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Percentage of ICD-Groups by Cluster Visit 1") +
  scale_y_continuous(name = "Percentage") +
  scale_x_discrete(name = "ICD-10-Code", guide = guide_axis(angle = 90)) +
  scale_fill_discrete(name = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        legend.position = "bottom", plot.title=element_text(size=10))

#-------------------------------------------------------------------------------
# check combinations of conditions using UpSetR (example)
#-------------------------------------------------------------------------------
subset<-cond_df[which(cond_df$visit=="1" & cond_df$cluster=="1-MARD"),]
# remove E11 itself
subset <- subset[which(!(subset$character == "E" & subset$number == "11")), ]

profile_df <- subset %>% 
  group_by(PatVID, group) %>%
  summarise(count=n()) %>%
  spread(key = group, value = count)

pid_vec <- as.data.frame(profile_df$PatVID)

profile_df<-profile_df[,WANTED DIAGNOSIS_GROUPS]               

profile_df[!is.na(profile_df)] <- 1
profile_df[is.na(profile_df)] <- 0

groups<-colnames(profile_df)
groups <- groups[-1]

profile_df<-cbind(pid_vec,profile_df)

upset(profile_df[,2:16],sets=groups, nintersects=15, set_size.show=TRUE,set_size.numbers_size=0,order.by = "freq", mainbar.y.label = "Anzahl Schnittpunkte", line.size=0.2, text.scale =1.5)

#-------------------------------------------------------------------------------
# coronary events
#-------------------------------------------------------------------------------
subset_I<-cond_df %>% filter(grepl("^I",C_code_coding_code))

# check for specific conditions of ischemic heart disease (see Ahlqvist reference)
coronary_events<-c("I21", "I20", "I24","I251", "I253", "I254", "I255", "I256", "I257", "I258", "I259")

ce_data<- subset_I %>% 
  rowwise() %>%
  filter(any(sapply(coronary_events, function(pattern) grepl(pattern, group))))
ce_table<-table(ce_data$cluster,ce_data$group)

ce<-as.data.frame(ce_table)
# extract actual patients only once

# repeat for 'stroke pattern' (see Ahlqvist reference)
stroke<-c("I60", "I61", "I63", "I64")

#-------------------------------------------------------------------------------
# search for specific codes (examples)
#-------------------------------------------------------------------------------
count_cond_patients <- cond_df %>%
                      filter(grepl("\\b(N18|Z99\\.2|E11\\.2|N08\\.3|D63\\.8|Z94\\.0|T86\\.1|N25\\.8|T85\\.6|Z49\\.2|Z49\\.0)\\b", C_code_coding_code)) %>%
                      distinct(PatVID, cluster) %>%
                      count(cluster, name = "n_patients")

                    
#-------------------------------------------------------------------------------
# Elixhauser comorbidity scores
#-------------------------------------------------------------------------------
# need to remove dot in ICD code
cond_df$C_code_coding_code<-gsub("\\.","",cond_df$C_code_coding_code)

# subset dataframe on cluster assignment and (if wanted) first visit
subset1<-cond_df[which(cond_df$visit=="1" & cond_df$cluster=="1-MARD"),] 
# [...]
subset4<-cond_df[which(cond_df$visit=="1" & cond_df$cluster=="4-SIRD"),]

elixhauser1 <-comorbidity(x = subset1, id = "PatVID", code = "C_code_coding_code", map = "elixhauser_icd10_quan", assign0 = FALSE)

# assign different weights to each comorbidity based on its impact on mortality or other outcomes.
vw_eci1 <- score(elixhauser1, weights = "vw", assign0 = FALSE)

# extract, mean, sd, summary ...
