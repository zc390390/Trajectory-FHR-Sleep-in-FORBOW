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


#Non-mean exploration
# setwd("C:/Users/hubshmz/Documents/Code/git/data")  # FORBOW computer
setwd("C:/Users/zachh/Documents/Code/git/Data")  # Laptop
# setwd("C:/Users/zacha/Documents/RStudio/forbow/git/Data") # Tupper Windows Comp
# setwd("C:/Users/zacha/Documents/forbow/code/git") # Home Computer

FOR <- readr::read_csv("FOR.csv", col_types = cols(.default = col_character()))
Night <- readr::read_csv("part4_nightsummary_sleep_cleaned.csv", col_types = cols(.default = col_character()))
NightM <- read_excel("motionloggerdata.xlsx")




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

store <- summary(unique_data$age)


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
  summarise(TSTm = mean((SleepDurationInSpt), na.rm=TRUE),
            age = mean((age), na.rm=TRUE),
            fhr = median((fhr_num), na.rm=TRUE))

sum$fhr <- factor(sum$fhr,
                       levels = c(0, 1),
                       labels = c("Control", "FHR"))

#Non-Mean Graphing
##Density plot
library(ggplot2)
library(patchwork)


# Plot 3: TST mean
p3 <- ggplot(sum, aes(x = TSTm)) +
  geom_density(fill = "darkorange", alpha = 0.5, color = "black") +
  scale_x_continuous(limits = c(4, 12), breaks = seq(4, 12, 2)) +
  labs(title = "Total Sleep Time Mean", x = "Hours", y = "Density") +
  theme_minimal()

p3


##Total Sleep time
merged$SleepDurationInSpt <- as.numeric(merged$SleepDurationInSpt)


b_f_WE <- ggplot(data = merged, mapping = aes(x = age, y = SleepDurationInSpt, color = fhr)) +
  geom_jitter(alpha = 0.1, width = 0.2, height = 0.5, size = 1.2) +
  geom_smooth(
    method = "gam",
    se = FALSE,
    linetype = "solid",
    size = 2,  # thicker
    aes(group = fhr, color = fhr)  # reassert color by group for clarity
  ) +
  scale_color_brewer(palette = "Dark2") + 
  scale_y_continuous(
    limits = c(4, 12),
    breaks = seq(4, 12, by = 2)
  ) +
  labs(
    title = "Sleep Duration Across Age by Group All Days (Faceted)",
    x = "Age",
    y = "Sleep Duration (Hours)",
    color = "Group"
  )  +
  theme_minimal()
b_f_WE 

ggplot(merged, aes(x = age, color = fhr, fill = fhr)) +
  geom_density(alpha = 0.4) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Age Density by Group",
    x = "Age",
    y = "Density",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal() + facet_wrap(~ fhr)

merged$day_type <- ifelse(merged$weekday %in% c("Saturday", "Sunday", "Sat", "Sun"), "Weekend", "Weekday")
merged$day_type <- factor(merged$day_type, levels = c("Weekday", "Weekend"))

b_f_WE <- ggplot(data = merged, mapping = aes(x = age, y = SleepDurationInSpt, color = fhr)) +
  geom_jitter(alpha = 0.1, width = 0.2, height = 0.5, size = 1.2) +
  geom_smooth(
    method = "gam",
    se = FALSE,
    linetype = "solid",
    size = 2,
    aes(group = fhr, color = fhr)
  ) +
  scale_color_brewer(palette = "Dark2") + 
  scale_y_continuous(
    limits = c(4, 12),
    breaks = seq(4, 12, by = 2)
  ) +
  labs(
    title = "Sleep Duration Across Age by Group and Day Type",
    x = "Age",
    y = "Sleep Duration (Hours)",
    color = "Group"
  )  +
  facet_grid(day_type ~ fhr) +   # rows = day type, columns = group
  theme_minimal()
b_f_WE


b_f_WE <- ggplot(data = sum, mapping = aes(x = age, y = TSTm, color = fhr)) +
  geom_jitter(alpha = 0.1, width = 0.2, height = 0.5, size = 1.2) +
  geom_smooth(
    method = "gam",
    se = FALSE,
    linetype = "solid",
    size = 2,  # thicker
    aes(group = fhr, color = fhr)  # reassert color by group for clarity
  ) +
  scale_color_brewer(palette = "Dark2") + 
  scale_y_continuous(
    limits = c(4, 12),
    breaks = seq(4, 12, by = 2)
  ) +
  labs(
    title = "Sleep Duration Across Age by Group All Days (Faceted)",
    x = "Age",
    y = "Sleep Duration (Hours)",
    color = "Group"
  )  +
  theme_minimal()
b_f_WE


#Main Analysis
##Hypothesis 1: Trajectories  
merged <- merged %>% filter(age >= 8 & age <= 22)
merged <- merged %>% filter(group != 'Psy Risk')

table(merged$group)

# Create weekend variable: 1 for Saturday/Sunday, 0 otherwise
merged$weekend <- ifelse(merged$weekday %in% 
                         c("Sat", "Saturday", "Sun", "Sunday"), 1, 0)


merged$subject_id <- as.factor(merged$subject_id)


###TST
#Comparing a linear to nonlinear model

linear <- lmer (SleepDurationInSpt ~ fhr*age + sex + age*weekend + (1|fid/subject_id), data = merged)