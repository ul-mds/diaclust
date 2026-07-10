library(survival)
library(survminer)
library(dplyr)

df<-data_clustering

entlass<-file containing discharge type data for the case numbers
deaths<-entlass[which(entlass$BA=="T"),]      # T==code for death

deaths %>%
  count(Fall) %>%
  filter(n > 1)
# --> no duplicates
deaths$Fall<-as.integer(deaths$Fall)

# keep every patient once
km_data <- df %>%
  group_by(PatVID) %>%
  arrange(ErgeDatZeit) %>%   # sort: earliest first
  slice(1) %>%               # keep first row per patient
  ungroup()

# check if patient died at any time

id_check<-read.csv(file to match patients and case numbers)
check_up<-merge(deaths,id_check[,2:3],by.x="Fall", by.y="id_fallnummer")

km_data$event <- ifelse(km_data$PatVID %in% check_up$PatVID, 1, 0)
km_data<- km_data %>% left_join(check_up %>% select(PatVID, Datum), by = "PatVID")
km_data<-unique(km_data)

# if not dead, we need the most late date --> set to max ErgeDatZeit
km_data$Datum[is.na(km_data$Datum)] <- as.Date(max(ErgeDatZeit))

# what we need: patient; cluster; time_point(first visit)--> "ErgeDatZeit"; dead (yes/no) --> "event"; time_point_dead_or_last_follow_up -->"Datum"
km_data$ErgeDatZeit <- as.Date(km_data$ErgeDatZeit)
km_data$Datum <- as.Date(km_data$Datum)
km_data$survival_time <- as.numeric(km_data$Datum - km_data$ErgeDatZeit)
km_data$cluster<-factor(km_data$cluster, levels=c("1","3","2","4"), labels=c("SIDD","SIRD","MOD","MARD"))
fit <- survfit(Surv(survival_time, event) ~ cluster, data = km_data)


km_plot<-ggsurvplot(fit,  size = 1,  # change line size
           break.time.by=365.25 *1,
           xscale = "d_y",
           palette = c("#A6CEE3","#B2DF8A","#FBB4AE", "#CAB2D6"), # color 
           legend.title="",
           legend.labs = c("SIDD", "SIRD", "MOD", "MARD"),
           conf.int = TRUE, 
           pval = TRUE, 
           pval.coord = c(3050, 0.98),
           pval.size= 4.5,
           ylim=c(0.8,1),
           xlab="Time (years)"
)

# add legend to bottom left
km_plot$plot <- km_plot$plot +
  theme(
    legend.position = c(0.10, 0.10),
    legend.justification = c(0, 0)
  )

ggsave(
  "KM-curve.png",
  km_plot$plot,
  dpi = 300,
  width = 8,
  height = 6
)

# pairwise comparisons using log-rank test 

pairwise_survdiff(
  Surv(survival_time, event) ~ cluster,
  data = km_data,
  p.adjust.method = "BH"
)


# Cox-regression
km_data$cluster <- relevel(factor(km_data$cluster), ref = "MOD")
# categorical variables need to be factors
km_data$gender <- factor(km_data$gender)
km_data$age <- as.numeric(km_data$age)

# estimate effects with Cox proportional hazards model
coxph(Surv(survival_time, event) ~ cluster, data = km_data)
#2 coxph(Surv(survival_time, event) ~ cluster + gender + eGFR, data = km_data)
#3 coxph(Surv(survival_time, event) ~ cluster + gender + eGFR + elix_score, data = km_data)
