#Library
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


#Non-Mean Exploration
# setwd("C:/Users/hubshmz/Documents/Code/git/data")  # FORBOW computer
setwd("C:/Users/zachh/Documents/Code/git/Data")  # Laptop
# setwd("C:/Users/zacha/Documents/RStudio/forbow/git/Data") # Tupper Windows Comp
# setwd("C:/Users/zacha/Documents/forbow/code/git") # Home Computer

FOR <- readr::read_csv("FOR.csv", col_types = cols(.default = col_character()))
Night <- readr::read_csv("part4_nightsummary_sleep_cleaned.csv", col_types = cols(.default = col_character()))
NightM <- read_excel("motionloggerdata.xlsx")

FOR <- FOR %>%
  mutate(
    oheightinch  = as.numeric(as.character(oheightinch)),
    oweightlb  = as.numeric(as.character(oweightlb)),
    oheightcm  = as.numeric(as.character(oheightcm)),
    oweightkg  = as.numeric(as.character(oweightkg))
  )


FOR <- FOR %>%
  mutate(oheightcm = ifelse(is.na(oheightcm), oheightinch * 2.54, oheightcm))
FOR <- FOR %>%
  mutate(oweightkg = ifelse(is.na(oweightkg), oweightlb * 0.453592, oweightkg))
FOR <- FOR %>%
  mutate(
    BMI = round(oweightkg / ((oheightcm / 100)^2), 1)
  )

FOR <- FOR %>%
    # Sort by subject and year
  group_by(subject_id) %>%
  arrange(time_point) %>%
  mutate(
    BMI_filled = BMI,  # create new BMI_filled variable
    
    BMI_filled = if_else(
      is.na(BMI_filled),
      rollapplyr(BMI_filled, width = 5, FUN = function(x) {
        median(x[!is.na(x)], na.rm = TRUE)
      }, partial = TRUE, fill = NA),
      BMI_filled
    )
  ) %>%
  ungroup()
summary(FOR$BMI_filled)
summary(FOR$BMI)



Night$Subject_ID <- substr(Night$filename, 1, 12)
Night$Assessment_Date <- str_extract(Night$filename, "\\d{4}-\\d{2}-\\d{2}")
Night <- Night %>% 
  filter(!str_detect(filename, "F[1-3]|f[1-3]|M[1-3]|m[1-3]|\\$R"))


bin_files_to_remove <- c(
  "8000103174_left wrist_068158_2025-06-03 10-40-36.bin",
  "8000156273_left wrist_060381_2024-11-05 12-04-08_partial.bin",
  "8000156273_left wrist_060381_2024-11-05 12-27-40.bin",
  "800304581_right wrist_060843_2024-11-05 13-16-09.bin",
  "800304582__060365_2024-11-05 12-03-44.bin",
  "800304582__060365_2024-11-05 12-28-01.bin"
)

Night <- Night %>%
  filter(!filename %in% bin_files_to_remove)
Night <- Night %>%
  mutate(Subject_ID = gsub("^800-(\\d{3})-(\\d+)_$", "800-0\\1-\\2", Subject_ID))

Night$Subject_ID <- gsub("-", "", Night$Subject_ID)
Night$Subject_ID <- gsub("[a-zA-Z]", "", Night$Subject_ID)
Night$Subject_ID <- gsub("_", "", Night$Subject_ID)
FOR$Subject_ID <- gsub("-", "", as.character(FOR$subject_id))

setDT(Night)
setDT(FOR)

Night$geneactiv <- 1
FOR[, Assessment_Date := as.Date(assessment_date)]
FOR$Assessment_Date_FOR <- FOR$Assessment_Date
Night[, Assessment_Date := as.Date(Assessment_Date)]  # redundant if already Date
Night$Assessment_Date_Night <- Night$Assessment_Date

# Perform the join with date window logic
merged1 <- Night[FOR, on = .(Subject_ID), allow.cartesian = TRUE]

merged1[, diffdays := abs(as.numeric(difftime(Assessment_Date_Night, Assessment_Date_FOR, units = "days")))]

merged1 <- merged1 %>% filter(diffdays <= 179)

two <- merged1[(Subject_ID %in% c(8000038204, 8000114479))]


NightM <- NightM %>%
  mutate(filename = paste0(ID, "motionlogg"))

NightM$weekday <- NightM$eday
NightM <- NightM %>% 
  filter(!str_detect(Subject_ID, "F[1-3]|f[1-3]|M[1-3]|m[1-3]|\\$R"))
setDT(NightM)
NightM[, Assessment_Date := as.Date(sdate)]
NightM$Assessment_date_nightM <- NightM$Assessment_Date


merged2 <- NightM[FOR, on = .(Subject_ID), allow.cartesian = TRUE]


merged2[, diffdays := abs(as.numeric(difftime(Assessment_date_nightM, Assessment_Date_FOR, units = "days")))]
merged2 <- merged2 %>% filter(diffdays <= 179)
merged2$motionlogger <- 1



merged <- rbindlist(list(merged1, merged2), fill = TRUE)

merged$sleeponset <- as.numeric(merged$sleeponset)
merged$wakeup <- as.numeric(merged$wakeup)

base_date <- as.Date("2000-01-01")
merged$sleeponset_time <- as.POSIXct(base_date) + merged$sleeponset * 3600
merged$wakeup_time     <- as.POSIXct(base_date) + merged$wakeup * 3600



#SleepRegularity - dev
merged <- merged %>%
  group_by(subject_id) %>%
  arrange(desc(time_point)) %>%
  mutate(onset_tmrw = lag(sleeponset, 1),
         wake_tmrw = lag(sleeponset, 1)) %>% ungroup()



merged <- merged %>%
  mutate(across(c(sleeponset, wakeup, onset_tmrw, wake_tmrw), as.POSIXct))



tryone <- merged %>% filter(subject_id == '8000300573')



merged$time_onset <- format(merged$sleeponset_time, format = "%H:%M:%S")
merged$sleeponset_minutes <- hour(hms(merged$time_onset)) * 60
                             minute(hms(merged$time_onset)) +
                             second(hms(merged$time_onset)) / 60
merged$sleeponset_minutes <- merged$sleeponset_minutes + 720

merged$time_wake <- format(merged$wakeup_time, format = "%H:%M:%S")
merged$wakeup_minutes <- hour(hms(merged$time_wake)) * 60 +
                         minute(hms(merged$time_wake)) +
                         second(hms(merged$time_wake)) / 60
merged$wakeup_minutes <- merged$wakeup_minutes + 720




subject_ids <- unique(merged$subject_id)

#This needs to filter for individual actigraphy too
merged$SRI2 <- NA_real_
merged$night <- as.numeric(merged$night)

make_binary <- function(onset, wake) {
  epoch <- 720:(720 + 1440 - 1)
  
  if (is.na(onset) || is.na(wake)) {
    return(rep(NA, length(epoch)))
  }
  
  if (onset <= wake) {
    return(ifelse(epoch >= onset & epoch <= wake, 1, 0))
  } else {
    return(ifelse(epoch >= onset | epoch <= wake, 1, 0))
  }
}

merged <- merged %>%
  arrange(subject_id, filename, night) %>%
  group_by(subject_id, filename) %>%
  mutate(
    epoch_binary = purrr::map2(sleeponset_minutes, wakeup_minutes, make_binary),
    
    next_binary  = dplyr::lead(epoch_binary),
    next_night   = dplyr::lead(night),
    
    SRI2 = purrr::map2_dbl(epoch_binary, next_binary, ~{
      if (is.null(.x) || is.null(.y)) return(NA_real_)
      valid_idx <- !is.na(.x) & !is.na(.y)
      if (!any(valid_idx)) return(NA_real_)
      mean(.x[valid_idx] == .y[valid_idx]) * 100
    }) %>%
      ifelse((next_night - night) != 1, NA_real_, .)
  ) %>%
  ungroup() %>%
  select(-epoch_binary, -next_binary, -next_night) 



merged$SRI2_scaled <- (merged$SRI2 * 2) - 100




merged$midpoint_minutes <- merged$wakeup_minutes + merged$sleeponset_minutes / 2


#Data Setup
merged <- merged %>%
  mutate(fhr = ifelse(group %in% c(1, 2, 3), 1, 0))

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

sum <- merged %>%
  group_by(filename) %>%
  summarise(SRI2m = mean((SRI2_scaled), na.rm=TRUE),
            age = mean((age), na.rm=TRUE),
            fhr = median((fhr_num), na.rm=TRUE))

sum$fhr <- factor(sum$fhr,
                       levels = c(0, 1),
                       labels = c("Control", "FHR"))

#Non-Mean Graphing
##Density plot
library(ggplot2)
library(patchwork)

# Plot 1: SRI2m
p1 <- ggplot(sum, aes(x = SRI2m)) +
  geom_density(fill = "steelblue", alpha = 0.5, color = "black") +
  scale_x_continuous(limits = c(60, 100), breaks = seq(60, 100, 10)) +
  labs(title = "SRI Mean", x = "SRI (0–100)", y = "Density") +
  theme_minimal()
p1


##Sleep Regularity Index
a_f_WE <- ggplot(data = merged, aes(x = age, y = SRI2_scaled, color = fhr)) +
  geom_jitter(alpha = 0.3, width = 0.2, height = 0.5, size = 1.5) +
  geom_smooth(aes(group = fhr, color = as.factor(fhr)), method = "gam", se = FALSE, size = 1) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(-100, 100, by = 20)) +

  labs(
    title = "Sleep Regularity Index Across Age by Group (Hex Binned + Smooth)",
    x = "Age",
    y = "Sleep Regularity Index",
    fill = "Count",
    color = "Group"
  ) 
a_f_WE

a_f_WE <- ggplot(data = merged, aes(x = age, y = SRI2_scaled, color = group)) +
  geom_jitter(alpha = 0.3, width = 0.2, height = 0.5, size = 1.5) +
  geom_smooth(aes(group = group, color = as.factor(group)), method = "lm", se = FALSE, size = 1) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(-100, 100, by = 20)) +

  labs(
    title = "Sleep Regularity Index Across Age by Group (All Risks)",
    x = "Age",
    y = "Sleep Regularity Index",
    fill = "Count",
    color = "Group"
  ) 
a_f_WE

a_f_WE <- ggplot(data = sum, aes(x = age, y = SRI2m, color = fhr)) +
  geom_jitter(alpha = 0.3, width = 0.2, height = 0.5, size = 1.5) +
  geom_smooth(aes(group = fhr, color = as.factor(fhr)), method = "gam", se = FALSE, size = 1) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(-100, 100, by = 20)) +

  labs(
    title = "Mean Sleep Regularity Index Across Age by Group (FHR)",
    x = "Age",
    y = "Sleep Regularity Index",
    fill = "Count",
    color = "Group"
  ) 
a_f_WE





#Main Analysis
##Hypothesis 1: Trajectories  
merged <- merged %>% filter(age >= 8 & age <= 22)
merged <- merged %>% filter(group != 'Psy Risk')

table(merged$group)

# Create weekend variable: 1 for Saturday/Sunday, 0 otherwise
merged$weekend <- ifelse(merged$weekday %in% 
                         c("Sat", "Saturday", "Sun", "Sunday"), 1, 0)

# merged <- merged %>%
#   mutate(
#     sleep_midpoint_min = period_to_seconds(hms(as.character(sleep_midpoint_time))) / 60
#   )

merged$subject_id <- as.factor(merged$subject_id)


###SRI
#Comparing a linear to nonlinear model


linear <- lmer(SRI2_scaled ~ fhr*age + sex + weekend + (1| fid/subject_id), data = merged)


