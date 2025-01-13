##Filtering 
# Read the file
lines <- readLines("HABlist.txt")

# Remove lines with only one word
lines_filtered <- lines[grepl(" ", lines)]

# Write the filtered lines back to a file
writeLines(lines_filtered, "HABlist_filtered.txt")

length(lines_filtered)

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

#from Model.R modelbio and modelenv are generated in Environmental.R
modelbio$red_tide_occurrences <- 0

for (i in 1:nrow(df_wide)) {
  modelbio$red_tide_occurrences[i] <- sum(df_wide[2:ncol(df_wide)][i, -1])  # Assuming the first column is not a species column
}

modeldata = data.frame(modelbio, modelenv)

colnames(modeldata) = c(colnames(modelbio) , colnames(modelenv))

Frequency_BOD5 = data.frame(modeldata$Sample, modeldata$red_tide_occurrences, modeldata$`BOD5 (mg/L)`)
colnames(Frequency_BOD5) = c("Sample", "Red tide frequency", "BOD5")

##For each WCZ
Frequency_BOD5
Frequency_BOD5 <- Frequency_BOD5 %>%
  mutate(WCZ = substr(Sample, 10, nchar(as.character(Sample))))

library(broom)

# Create a function to detect and remove outliers
remove_outliers <- function(df, col1, col2) {
  df %>%
    mutate(z_score = abs(scale(!!enquo(col1)))) %>%
    filter(z_score < 3) %>%
    mutate(z_score = abs(scale(!!enquo(col2)))) %>%
    filter(z_score < 3)
}

# Remove outliers
Frequency_BOD5_clean <- remove_outliers(Frequency_BOD5, `Red tide frequency`, BOD5)

# Calculate correlation, linear regression coefficients, and p-values for each WCZ (without outliers)
results_by_wcz <- Frequency_BOD5_clean %>%
  group_by(WCZ) %>%
  do({
    lm_result <- lm(`Red tide frequency` ~ BOD5, data = .)
    data.frame(
      correlation = cor(.$`Red tide frequency`, .$BOD5),
      p_value_correlation = cor.test(.$`Red tide frequency`, .$BOD5)$p.value,
      intercept = coef(lm_result)[1],
      p_value_intercept = summary(lm_result)$coefficients[1, 4],
      slope = coef(lm_result)[2],
      p_value_slope = summary(lm_result)$coefficients[2, 4]
    )
  }) %>%
  ungroup()

# Calculate correlation, linear regression coefficients, and p-values for all WCZ combined
all_wcz_result <- Frequency_BOD5_clean %>%
  do({
    lm_result <- lm(`Red tide frequency` ~ BOD5, data = .)
    data.frame(
      WCZ = "All WCZ",
      correlation = cor(.$`Red tide frequency`, .$BOD5),
      p_value_correlation = cor.test(.$`Red tide frequency`, .$BOD5)$p.value,
      intercept = coef(lm_result)[1],
      p_value_intercept = summary(lm_result)$coefficients[1, 4],
      slope = coef(lm_result)[2],
      p_value_slope = summary(lm_result)$coefficients[2, 4]
    )
  })

# Combine the results for individual WCZ and all WCZ
final_results <- bind_rows(results_by_wcz, all_wcz_result)

# Create the "All WCZ" data
Frequency_BOD5_all <- transform(Frequency_BOD5_clean, WCZ = "All WCZ")
colnames(Frequency_BOD5_all) = colnames(Frequency_BOD5_clean)

# Plot the data
p <- ggplot(Frequency_BOD5_clean, aes(x = BOD5, y = `Red tide frequency`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Add the "All WCZ" data and regression line to the first plot
p + geom_point(data = Frequency_BOD5_all) +
  geom_smooth(data = Frequency_BOD5_all, method = "lm", se = FALSE) +
  facet_wrap(~ WCZ, ncol = 3, scales = "free") +
  theme_classic() +
  labs(
    x = "Mean monthly BOD5",
    y = "Monthly Red tide sightings"
  )

#write.csv(x = final_results, file = "Supplementary_Table_04.csv")

##############VSS or other values

#from Model.R modelbio and modelenv are generated in Environmental.R
modelbio$red_tide_occurrences <- 0

for (i in 1:nrow(df_wide)) {
  modelbio$red_tide_occurrences[i] <- sum(df_wide[2:ncol(df_wide)][i, -1])  # Assuming the first column is not a species column
}

modeldata = data.frame(modelbio, modelenv)

colnames(modeldata) = c(colnames(modelbio) , colnames(modelenv))

#change the modeldata$XYZ value for other factor
Frequency_BOD5 = data.frame(modeldata$Sample, modeldata$red_tide_occurrences, modeldata$`Volatile suspended solids (mg/L)`)
colnames(Frequency_BOD5) = c("Sample", "Red tide frequency", "BOD5")

##For each WCZ
Frequency_BOD5
Frequency_BOD5 <- Frequency_BOD5 %>%
  mutate(WCZ = substr(Sample, 10, nchar(as.character(Sample))))

library(broom)

# Create a function to detect and remove outliers
remove_outliers <- function(df, col1, col2) {
  df %>%
    mutate(z_score = abs(scale(!!enquo(col1)))) %>%
    filter(z_score < 3) %>%
    mutate(z_score = abs(scale(!!enquo(col2)))) %>%
    filter(z_score < 3)
}

# Remove outliers
Frequency_BOD5_clean <- remove_outliers(Frequency_BOD5, `Red tide frequency`, BOD5)

# Calculate correlation, linear regression coefficients, and p-values for each WCZ (without outliers)
results_by_wcz <- Frequency_BOD5_clean %>%
  group_by(WCZ) %>%
  do({
    lm_result <- lm(`Red tide frequency` ~ BOD5, data = .)
    data.frame(
      correlation = cor(.$`Red tide frequency`, .$BOD5),
      p_value_correlation = cor.test(.$`Red tide frequency`, .$BOD5)$p.value,
      intercept = coef(lm_result)[1],
      p_value_intercept = summary(lm_result)$coefficients[1, 4],
      slope = coef(lm_result)[2],
      p_value_slope = summary(lm_result)$coefficients[2, 4]
    )
  }) %>%
  ungroup()

# Calculate correlation, linear regression coefficients, and p-values for all WCZ combined
all_wcz_result <- Frequency_BOD5_clean %>%
  do({
    lm_result <- lm(`Red tide frequency` ~ BOD5, data = .)
    data.frame(
      WCZ = "All WCZ",
      correlation = cor(.$`Red tide frequency`, .$BOD5),
      p_value_correlation = cor.test(.$`Red tide frequency`, .$BOD5)$p.value,
      intercept = coef(lm_result)[1],
      p_value_intercept = summary(lm_result)$coefficients[1, 4],
      slope = coef(lm_result)[2],
      p_value_slope = summary(lm_result)$coefficients[2, 4]
    )
  })

# Combine the results for individual WCZ and all WCZ
final_results <- bind_rows(results_by_wcz, all_wcz_result)
final_results

# Create the "All WCZ" data
Frequency_BOD5_all <- transform(Frequency_BOD5_clean, WCZ = "All WCZ")
colnames(Frequency_BOD5_all) = colnames(Frequency_BOD5_clean)

# Plot the data
p <- ggplot(Frequency_BOD5_clean, aes(x = BOD5, y = `Red tide frequency`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Add the "All WCZ" data and regression line to the first plot
p + geom_point(data = Frequency_BOD5_all) +
  geom_smooth(data = Frequency_BOD5_all, method = "lm", se = FALSE) +
  facet_wrap(~ WCZ, ncol = 3, scales = "free") +
  theme_classic() +
  labs(
    x = "Mean monthly Env",
    y = "Monthly Red tide sightings"
  )

write.csv(final_results, file = "Envcor.csv")

