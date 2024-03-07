# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)
library(janitor)

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
                                   trial_11, trial_12, trial_12, trial_14), na.rm = TRUE)) %>%
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
                           trial_11, trial_12, trial_12, trial_14), na.rm = TRUE)) %>%
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

replication_ttest <- t.test(force_fc ~ condition, rep_data_long, 
                  alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
replication_ttest

### Replication effect size calculation ------

rep_dz <- smd_calc(
  x = paired_data$differences,
  paired = TRUE,
  var.equal = FALSE,
  alpha = 0.05,
  mu = 0,
  bias_correction = FALSE,
  rm_correction = FALSE,
  glass = NULL,
  smd_ci = "nct")
rep_dz

# Replication analyses - z-test --------

rep_test <- compare_smd(
  smd1 = 0.3,
  n1 = 12,
  smd2 = rep_dz$estimate,
  n2 = rep_desc$count[1],
  paired = TRUE,
  alternative = "greater")
rep_test

