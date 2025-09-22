# Library & Notes
## ─────────────────────────────
## 📦 Data Import & Manipulation
## ─────────────────────────────
library(haven)       # Read SPSS, SAS, Stata files
library(readr)       # Read CSVs
library(readxl)      # Read Excel files
library(data.table)  # Fast data manipulation
library(dplyr)       # Data manipulation
library(tidyr)       # Data tidying
library(stringr)     # String operations
library(purrr)       # Functional programming
library(zoo)         # Time series & rolling functions
library(lubridate)   # Dates & times
library(fuzzyjoin)   # Fuzzy matching joins

## ─────────────────────────────
## 📊 Modeling & Statistics
## ─────────────────────────────
library(lme4)        # Mixed-effects models
library(lmerTest)    # p-values for lmer
library(coxme)       # Cox mixed-effects models
library(survival)    # Survival analysis
library(MuMIn)       # Model selection & R²
library(lsmeans)     # Least-squares means (now emmeans)
library(mgcv)        # Generalized Additive Models
library(gratia)      # GAM diagnostics & visualization
library(gamm4)       # GAMM fitting

## ─────────────────────────────
## 📑 Tables & Reporting
## ─────────────────────────────
library(broom)       # Tidy model output
library(pander)      # Pretty markdown tables
library(apaTables)   # APA-style tables
library(flextable)   # Formatted tables
library(officer)     # Export to Word/PowerPoint
library(webshot2)    # Save HTML tables/images

## ─────────────────────────────
## 🎨 Plotting & Visualization
## ─────────────────────────────
library(ggplot2)     # General plotting


#Load Data
# setwd("C:/Users/hubshmz/Documents/Code/git/data")  # FORBOW computer
setwd("C:/Users/zachh/Documents/Code/git/Data")  # Laptop
# setwd("C:/Users/zacha/Documents/RStudio/forbow/git/Data") # Tupper Windows Comp
# setwd("C:/Users/zacha/Documents/forbow/code/git") # Home Computer

FOR <- readr::read_csv("FOR.csv", col_types = cols(.default = col_character())) 
Night <- readr::read_csv("part4_nightsummary_sleep_cleaned.csv", col_types = cols(.default = col_character())) 
NightM <- read_excel("motionloggerdata.xlsx")


#Merge Datasets
FOR <- FOR %>%
  mutate(oheightinch  = as.numeric(as.character(oheightinch)), oweightlb  = as.numeric(as.character(oweightlb)), oheightcm  = as.numeric(as.character(oheightcm)), oweightkg  = as.numeric(as.character(oweightkg)))


FOR <- FOR %>% mutate(oheightcm = ifelse(is.na(oheightcm), oheightinch * 2.54, oheightcm))
FOR <- FOR %>% mutate(oweightkg = ifelse(is.na(oweightkg), oweightlb * 0.453592, oweightkg))
FOR <- FOR %>% mutate(BMI = round(oweightkg / ((oheightcm / 100)^2), 1))

FOR <- FOR %>% group_by(subject_id) %>% arrange(time_point) %>%
  mutate(
    BMI_filled = BMI, 
    BMI_filled = if_else(
      is.na(BMI_filled),
      rollapplyr(BMI_filled, width = 5, FUN = function(x) {
        median(x[!is.na(x)], na.rm = TRUE)
      }, partial = TRUE, fill = NA),
      BMI_filled
    )
  ) %>% ungroup()

Night$Subject_ID <- substr(Night$filename, 1, 12)
Night$Assessment_Date <- str_extract(Night$filename, "\\d{4}-\\d{2}-\\d{2}")
Night <- Night %>% filter(!str_detect(filename, "F[1-3]|f[1-3]|M[1-3]|m[1-3]|\\$R"))


bin_files_to_remove <- c("8000103174_left wrist_068158_2025-06-03 10-40-36.bin", "8000156273_left wrist_060381_2024-11-05 12-04-08_partial.bin", "8000156273_left wrist_060381_2024-11-05 12-27-40.bin", "800304581_right wrist_060843_2024-11-05 13-16-09.bin", "800304582__060365_2024-11-05 12-03-44.bin",  "800304582__060365_2024-11-05 12-28-01.bin")

Night <- Night %>% filter(!filename %in% bin_files_to_remove)
Night <- Night %>% mutate(Subject_ID = gsub("^800-(\\d{3})-(\\d+)_$", "800-0\\1-\\2", Subject_ID))

Night$Subject_ID <- gsub("-", "", Night$Subject_ID)
Night$Subject_ID <- gsub("[a-zA-Z]", "", Night$Subject_ID)
Night$Subject_ID <- gsub("_", "", Night$Subject_ID)
FOR$Subject_ID <- gsub("-", "", as.character(FOR$subject_id))

setDT(Night)
setDT(FOR)
Night$geneactiv <- 1
FOR[, Assessment_Date := as.Date(assessment_date)]
FOR$Assessment_Date_FOR <- FOR$Assessment_Date
Night[, Assessment_Date := as.Date(Assessment_Date)]  
Night$Assessment_Date_Night <- Night$Assessment_Date

# Perform the join with date window logic
merged1 <- Night[FOR, on = .(Subject_ID), allow.cartesian = TRUE]
merged1[, diffdays := abs(as.numeric(difftime(Assessment_Date_Night, Assessment_Date_FOR, units = "days")))]
merged1 <- merged1 %>% filter(diffdays <= 179)

NightM <- NightM %>%
  mutate(filename = paste0(ID, "motionlogg"))
NightM$weekday <- NightM$eday
NightM <- NightM %>% 
  filter(!str_detect(Subject_ID, "F[1-3]|f[1-3]|M[1-3]|m[1-3]|\\$R"))
setDT(NightM)
NightM[, Assessment_Date := as.Date(sdate)]
NightM$Assessment_date_nightM <- NightM$Assessment_Date
NightM$SptDuration <- NightM$dur/60

merged2 <- NightM[FOR, on = .(Subject_ID), allow.cartesian = TRUE]
merged2[, diffdays := abs(as.numeric(difftime(Assessment_date_nightM, Assessment_Date_FOR, units = "days")))]
merged2 <- merged2 %>% filter(diffdays <= 179)
merged2$motionlogger <- 1

merged <- rbindlist(list(merged1, merged2), fill = TRUE)



#Midpoint
merged$sleeponset <- as.numeric(merged$sleeponset)
merged$wakeup <- as.numeric(merged$wakeup)

base_date <- as.Date("2000-01-01")
merged$sleeponset_time <- as.POSIXct(base_date) + merged$sleeponset * 3600
merged$wakeup_time     <- as.POSIXct(base_date) + merged$wakeup * 3600

merged$sleep_duration <- difftime(merged$wakeup_time, merged$sleeponset_time, units = "secs")
merged$sleep_duration[merged$sleep_duration < 0] <- merged$sleep_duration[merged$sleep_duration < 0] + 86400  
merged$sleep_midpoint <- merged$sleeponset_time + (merged$sleep_duration / 2)
merged$sleep_midpoint_time <- format(merged$sleep_midpoint, "%H:%M:%S")


summary(merged$sleep_midpoint_time)
merged <- merged %>%
  mutate(
    sleep_midpoint_min = period_to_seconds(hms(as.character(sleep_midpoint_time))) / 60
  )




#Data Setup
merged <- merged %>%
  mutate(fhr = ifelse(group %in% c(1, 2, 3), 1, 0))

merged <- merged %>%
  mutate(groupmdd = ifelse(group %in% c(1), 1, 0))
merged <- merged %>% 
  mutate(groupbp = ifelse(group %in% c(2), 1, 0))
merged <- merged %>%
  mutate(grouppsy = ifelse(group %in% c(3), 1, 0))

merged$age <- as.numeric(as.character(merged$age))


merged$fhr <- factor(merged$fhr,
                     levels = c(0, 1),
                     labels = c("Control", "FHR"))
merged$group <- factor(merged$group,
                       levels = c(0, 1, 2, 3),
                       labels = c("Control", "MDD Risk", "BD Risk", "Psy Risk"))
merged$sex <- factor(merged$sex,
                     levels = c(0, 1),
                     labels = c("Male", "Female"))
merged <- merged %>% 
  filter(!is.na(group))

merged2 <- merged
merged <- merged %>%
  filter(group != 3)

unique_data <- unique(merged[, c("subject_id", "group")])

group_table <- table(unique_data$group)
pander(group_table)


unique_data <- unique(merged1[, c("subject_id", "group")])

group_table <- table(unique_data$group)
pander(group_table)

unique_data <- unique(merged2[, c("subject_id", "group")])

group_table <- table(unique_data$group)
pander(group_table)

pander(table(merged$group))

merged <- merged %>%
  mutate(fhr_num = ifelse(fhr == "FHR", 1, 0))





##Sleep Midpoint
summary(merged$sleeponset)
summary(merged$wakeup)
summary(merged$midpoint)

merged$sleeponsetadj <- merged$sleeponset 
merged$midpointadj <- (merged$sleep_midpoint_min/60) + 24
merged$wakeupadj <- merged$wakeup

y_breaks <- seq(24, 32, by = 1)

# Create labels that wrap hours past 24 back to 0-8, keeping the "24:" style
y_labels <- sapply(y_breaks, function(h) {
  hr <- h %% 24
  sprintf("%02d:00", hr)
})

ggplot(merged, aes(x = age, y = midpointadj, color = fhr)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = FALSE) +
  scale_y_continuous(
    breaks = y_breaks,
    labels = y_labels,
    limits = c(24, 32)
  ) +
  coord_cartesian(xlim = c(8, 22)) +  # limits x-axis without dropping points
  labs(
    x = "Age",
    y = "Sleep time (HH:MM)"
  ) +
  theme_minimal()


longdat <- merged %>%
  pivot_longer(
    cols = c(sleeponsetadj, midpointadj, wakeupadj),
    names_to = "sleep_measure",
    values_to = "minutes"
  )

# Plot
# Create combined variable
longdat$group <- interaction(longdat$fhr, longdat$sleep_measure)

ggplot(longdat, aes(x = age, y = minutes, color = group)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = FALSE) +
  #scale_color_manual(values = my_colors, name = "Group & Measure") +
  scale_y_continuous(
    labels = function(x) sprintf("%02d:%02d", floor(x), round((x %% 1) * 60)),
    breaks = seq(0, 36, by = 4)
  ) +
  labs(
    x = "Age",
    y = "Sleep time (HH:MM)"
  ) +
  theme_minimal() + facet_wrap(~fhr)


ggplot(longdat, aes(x = age, y = minutes, color = sleep_measure)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = FALSE) +
  scale_y_continuous(
    labels = function(x) sprintf("%02d:%02d", floor(x), round((x %% 1) * 60)),
    breaks = seq(0, 36, by = 4)
  ) +
  labs(
    title = "Sleep Onset, Midpoint, and Wake on a 12-36 Hour Scale (24 is Midnight)",
    x = "Age",
    y = "Sleep time (HH:MM)"
  ) +
  theme_minimal()


sum <- merged %>%
  group_by(filename) %>%
  summarise(midpointadj = mean((midpointadj), na.rm=TRUE),
            sleeponsetadj = mean((sleeponsetadj), na.rm=TRUE),
            wakeupadj = mean((wakeupadj), na.rm=TRUE),
            age = mean((age), na.rm=TRUE),
            fhr = median((fhr_num), na.rm=TRUE))

longdat <- sum %>%
  pivot_longer(
    cols = c(sleeponsetadj, midpointadj, wakeupadj),
    names_to = "sleep_measure",
    values_to = "minutes"
  )

longdat$group <- interaction(longdat$fhr, longdat$sleep_measure)


ggplot(longdat, aes(x = age, y = minutes, color = group)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = FALSE) +
  #scale_color_manual(values = my_colors, name = "Group & Measure") +
  scale_y_continuous(
    labels = function(x) sprintf("%02d:%02d", floor(x), round((x %% 1) * 60)),
    breaks = seq(0, 36, by = 4)
  ) +
  labs(
    title = "Mean Sleep Onset, Midpoint, and Wakeup Time on 12-36 Scale (24 is Midnight)",
    x = "Age",
    y = "Sleep time (HH:MM)"
  ) +
  theme_minimal() + facet_wrap(~fhr)





#Main Analysis
##Hypothesis 1: Trajectories  
merged <- merged %>% filter(age >= 8 & age <= 22)
merged <- merged %>% filter(group != 'Psy Risk')

table(merged$group)

# Create weekend variable: 1 for Saturday/Sunday, 0 otherwise
merged$weekend <- ifelse(merged$weekday %in% 
                           c("Sat", "Saturday", "Fri", "Friday"), 1, 0)

merged$subject_id <- as.factor(merged$subject_id)

table(merged$fhr)




###Midpoint
merged$age_c <- merged$age - 12

linear <- lmer(sleep_midpoint_min ~fhr*age_c + sex + weekend + (1|fid/subject_id), data = merged)

merged2$age_c <- merged2$age - 12
merged2$weekend <- ifelse(merged2$weekday %in% c("Sat", "Saturday", "Fri", "Friday"), 1, 0)

linear_explore <- lmer(sleep_midpoint_min ~ groupmdd*age_c + groupbp*age_c + grouppsy*age_c + sex + weekend + (1|fid/subject_id), data = merged2)

midpoint_table <- merged2 %>%
  mutate(age = as.numeric(age)) %>%
  group_by(subject_id, group) %>%
  summarise(
    median_age = median(age, na.rm = TRUE),
    median_midpoint = median(sleep_midpoint_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(group) %>%
  summarise(
    n            = n(),
    mean_age     = mean(median_age, na.rm = TRUE),
    sd_age       = sd(median_age, na.rm = TRUE),
    mean_midpoint = mean(median_midpoint, na.rm = TRUE),
    sd_midpoint   = sd(median_midpoint, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  bind_rows(
    merged %>%
      mutate(age = as.numeric(age)) %>%
      group_by(subject_id) %>%
      summarise(
        median_age = median(age, na.rm = TRUE),
        median_midpoint = median(sleep_midpoint_min, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      summarise(
        n            = n(),
        mean_age     = mean(median_age, na.rm = TRUE),
        sd_age       = sd(median_age, na.rm = TRUE),
        mean_midpoint = mean(median_midpoint, na.rm = TRUE),
        sd_midpoint   = sd(median_midpoint, na.rm = TRUE)
      ) %>%
      mutate(group = "All")
  ) %>%
  relocate(group, .before = n)
