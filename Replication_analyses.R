# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)
library(janitor)

set.seed(21)

# Replication data prep ---------------------
rep_data <- read_csv("Volleyball_Replication_Data.csv") %>%
  clean_names()
head(rep_data)


# Split datasets to make it easier 

tilt_data <- rep_data %>%
  filter(condition == "tilt")

straight_data <- rep_data %>%
  filter(condition == "straight")

# Convert to wide column to add difference column

tilt_wide <- tilt_data %>%
  pivot_wider(id_cols = c(condition, subject), # identifying column(s)
              names_from = trial, # the new column names
              values_from = force_fc # the new column values
  ) 

# Create a mean for each participant across the 7 trials they completed

tilt_wide <- tilt_wide %>%
  rowwise() %>%
  mutate(tilt_force = mean(c(trial_1, trial_2, trial_3, trial_4, trial_5,
                                   trial_6, trial_7, trial_8, trial_9, trial_10,
                                   trial_11, trial_12, trial_13, trial_14), na.rm = TRUE)) %>%
  as.data.frame()

straight_wide <- straight_data %>%
  pivot_wider(id_cols = c(condition, subject), # identifying column(s)
              names_from = trial, # the new column names
              values_from = force_fc # the new column values
  ) 


straight_wide <- straight_wide %>%
  rowwise() %>%
  mutate(straight_force = mean(c(trial_1, trial_2, trial_3, trial_4, trial_5,
                           trial_6, trial_7, trial_8, trial_9, trial_10,
                           trial_11, trial_12, trial_13, trial_14), na.rm = TRUE)) %>%
  as.data.frame()

# Join datasets

paired_data <- inner_join(tilt_wide, straight_wide, by = c("subject")) 
paired_data <- paired_data %>%
  select(subject, tilt_force, straight_force)

# add differences column to wide dataset

paired_data <- paired_data %>% 
  mutate(differences =  tilt_force - straight_force)

# Convert to long dataset

rep_data_long <- paired_data %>%
  gather(key = "condition", value = "force_fc", straight_force, tilt_force)
head(rep_data_long, 3)

## Replication descriptives  ---------------

rep_desc <- rep_data_long %>%
  group_by(condition) %>%
  summarise(count = n(),
            mean = mean(force_fc),
            sd = sd(force_fc)) %>%
  mutate(mean_diff = mean(paired_data$differences), 
         sd_diff = sd(paired_data$differences))
rep_desc 

## Resolving assumptions  ---------------------------------------

## Distribution check 

ggplot(rep_data_long, aes(force_fc)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10)

ggplot(rep_data_long, aes(condition, force_fc, color = condition)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

### Outliers check 

paired_data %>%
  identify_outliers(differences)

### Normality check  

paired_data %>% shapiro_test(differences) 

## Paired t-test  -----------------------------

rep_data_long$condition <- as.factor(rep_data_long$condition)

# R compares conditions alphabetically, I am reordering here to match the original study

rep_data_long$condition <- forcats::fct_relevel(rep_data_long$condition, "tilt_force", "straight_force")


#replication_ttest <- t.test(force_fc ~ condition, rep_data_long, 
#                  alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
#  tidy()
#replication_ttest

replication_ttest <- t.test(paired_data$tilt_force, paired_data$straight_force, paired = TRUE)
replication_ttest


### Replication effect size calculation ------

rep_dz <- d.dep.t.diff(mdiff = rep_desc$mean_diff[1], 
                        sddiff = rep_desc$sd_diff[1], 
                        n = rep_desc$count[1], a = 0.05)
rep_dz

rep_dav <- d.dep.t.avg(m1 = rep_desc$mean[2], m2 = rep_desc$mean[1], 
                       sd1 = rep_desc$sd[2], sd2 = rep_desc$sd[1],
                       n = rep_desc$count[1], a = 0.05)
rep_dav

## Calculate Original ES --------

#Original descriptives

orig_values <- data_frame(
  ori_pval = 0.046,
  N = 12,
  m1 = 0.13,
  sd1 = 0.04,
  m2 = 0.12,
  sd2 = 0.04)

# Estimating the t-value

quantile = 1 - orig_values$ori_pval/2 # for two-tailed

ori_tval <- qt(quantile, df = 11)


# Estimating the original effect size

ori_dz <- d.dep.t.diff.t(t = ori_tval, n = orig_values$N, a = 0.05)
ori_dz

ori_dav <- d.dep.t.avg(m1 = orig_values$m1, m2 = orig_values$m2, 
                       sd1 = orig_values$sd1, sd2 = orig_values$sd2,
                       n = orig_values$N, a = 0.05)
ori_dav

# Looks like dav is the closest to reported ES of 0.3

# Replication analyses - z-test (dz) --------

rep_test <- compare_smd(
  smd1 = ori_dz$d,
  n1 = orig_values$N,
  smd2 = rep_dz$d,
  n2 = rep_desc$count[1],
  paired = TRUE,
  alternative = "greater")
rep_test

# Replication analyses - z-test (reported original es) --------

rep_test <- compare_smd(
  smd1 = 0.3,
  n1 = orig_values$N,
  smd2 = rep_dav$d,
  n2 = rep_desc$count[1],
  paired = TRUE,
  alternative = "greater")
rep_test

