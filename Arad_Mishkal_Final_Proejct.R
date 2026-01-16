# Final Project: Physical Activity & Mental Health
library(ggplot2)
library(dplyr)
library(tableone)
library(broom)
library(knitr)
library(kableExtra)
library(NHANES)

# Load Data 
data("NHANES")

# Keep one row per person and adults ≥18
df <- NHANES[!duplicated(NHANES$ID), ]
df <- subset(df, Age >= 18)

# Keep only needed variables
vars <- c("ID", "Age", "Gender", "Race1",
          "PhysActive", "DaysMentHlthBad", "Depressed")
df <- df[, vars]

# Remove missing values
df_main <- subset(df,
                  !is.na(PhysActive) &
                    !is.na(DaysMentHlthBad) &
                    !is.na(Age) &
                    !is.na(Gender) &
                    !is.na(Race1))

# Create age groups
df_main$AgeGroup <- cut(
  df_main$Age,
  breaks = c(18, 39, 59, 80),
  labels = c("18-39", "40-59", "60+"),
  right = TRUE
)

df_main <- subset(df_main, !is.na(AgeGroup))

df_main$PhysActive <- factor(df_main$PhysActive, levels = c("No", "Yes"))
df_main$Gender <- factor(df_main$Gender)
df_main$Race1 <- factor(df_main$Race1)
df_main$AgeGroup <- factor(df_main$AgeGroup, levels = c("18-39", "40-59", "60+"))

# Table 1: Descriptive Statistics
vars_table1 <- c("Age", "Gender", "Race1", "DaysMentHlthBad")
factorVars <- c("Gender", "Race1")

table1 <- CreateTableOne(
  vars = vars_table1,
  strata = "PhysActive",
  data = df_main,
  factorVars = factorVars,
  test = TRUE
)

table1_print <- print(
  table1,
  showAllLevels = TRUE,
  quote = FALSE,
  noSpaces = TRUE,
  printToggle = FALSE
)

kable(
  table1_print,
  caption = "Table 1. Participant characteristics stratified by physical activity status"
) %>%
  kable_styling(full_width = FALSE)

# Figre 1: Box Plot
fig1 <- ggplot(df_main, aes(x = PhysActive, y = DaysMentHlthBad)) +
  geom_boxplot(fill = "lightgray", color = "black") +
  labs(
    title = "Mentally Unhealthy Days by Physical Activity Status",
    x = "Physically Active",
    y = "Mentally Unhealthy Days (Past 30 Days)"
  ) +
  theme_classic(base_size = 12)

print(fig1)

# Figure 2: Age-Statified Bar Plot
df_summary <- df_main %>%
  group_by(AgeGroup, PhysActive) %>%
  summarise(
    mean_days = mean(DaysMentHlthBad),
    se = sd(DaysMentHlthBad) / sqrt(n()),
    .groups = "drop"
  )

fig2 <- ggplot(df_summary,
               aes(x = AgeGroup, y = mean_days, fill = PhysActive)) +
  geom_col(position = position_dodge(0.8), color = "black") +
  geom_errorbar(
    aes(ymin = mean_days - se, ymax = mean_days + se),
    width = 0.2,
    position = position_dodge(0.8)
  ) +
  labs(
    title = "Mean Mentally Unhealthy Days by Age Group and Physical Activity",
    x = "Age Group",
    y = "Mean Mentally Unhealthy Days"
  ) +
  theme_classic(base_size = 12)

print(fig2)

# Table 2: Linear Regression
lin_results <- df_main %>%
  group_by(AgeGroup) %>%
  do(tidy(lm(DaysMentHlthBad ~ PhysActive + Age + Gender + Race1, data = .))) %>%
  filter(term == "PhysActiveYes", !is.na(AgeGroup))%>%
  select(
    AgeGroup,
    Beta = estimate,
    `p-value` = p.value
  )

kable(
  lin_results,
  digits = 3,
  caption = "Table 2. Age-stratified adjusted linear regression models for mentally unhealthy days"
) %>%
  kable_styling(full_width = FALSE)

# Table 3: Logistic Regression
df_dep <- subset(df_main, !is.na(Depressed))
df_dep$Depressed_bin <- ifelse(df_dep$Depressed == "None", 0, 1)

log_results <- df_dep %>%
  group_by(AgeGroup) %>%
  do(
    tidy(
      glm(Depressed_bin ~ PhysActive + Age + Gender + Race1,
          family = binomial,
          data = .),
      conf.int = TRUE,
      exponentiate = TRUE
    )
  ) %>%
  filter(term == "PhysActiveYes") %>%
  select(
    AgeGroup,
    OR = estimate,
    `95% CI Low` = conf.low,
    `95% CI High` = conf.high,
    `p-value` = p.value
  )

kable(
  log_results,
  digits = 3,
  caption = "Table 3. Age-stratified adjusted logistic regression models for depression"
) %>%
  kable_styling(full_width = FALSE)


# Interaction Test
model_int <- lm(
  DaysMentHlthBad ~ PhysActive * AgeGroup + Age + Gender + Race1,
  data = df_main
)

interaction_p <- summary(model_int)$coefficients[
  c("PhysActiveYes:AgeGroup40-59", "PhysActiveYes:AgeGroup60+"),
  "Pr(>|t|)"
]

interaction_p
