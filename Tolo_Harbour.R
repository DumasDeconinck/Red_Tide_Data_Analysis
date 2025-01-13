##
#Tolo Harbour
#

library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggtext)
library(stringr)

df = read.csv(file = "Red_tide_and_WCZ_02162024.csv")

df$WCZ = gsub("Tolo Harbour & Channel", "Tolo Harbour", df$WCZ) #name too long

df = subset(df, df$WCZ == "Tolo Harbour")

#
# Calculate the count of each species
species_count <- df %>%
  count(Species) %>%
  arrange(desc(n))

#calculate top 8
top_8_species <- head(subset(species_count, !is.na(species_count$Species)), 7)

df$Species <- ifelse(df$Species %in% top_8_species$Species, df$Species, "Other")

df_Species <- df %>%
  group_by(Year, Species) %>%
  summarize(count = n())

df_Species$Species <- factor(df_Species$Species, levels = sort(unique(df_Species$Species)))

#Plot
pk = ggplot(df_Species, aes(fill = Species, y = count, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Species$Year), breaks = df_Species$Year) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(angle = 90),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = -15)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
pk

df_Species$Species <- ifelse(df_Species$Species == "Other", "Other",
                             str_replace(df_Species$Species, "([A-Za-z]+(\\s+[A-Za-z]+)*)", "*\\1*"))
df_Species$Species <- factor(df_Species$Species, levels = sort(unique(df_Species$Species)))

pl = ggplot(df_Species, aes(fill = Species, y = count, x = Year)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Species$Year), breaks = df_Species$Year) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_markdown(size = 15),
        axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1, margin = margin(t = -15)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
pl

#calculate top 10
df = read.csv(file = "Red_tide_and_WCZ_02162024.csv")

df$WCZ = gsub("Tolo Harbour & Channel", "Tolo Harbour", df$WCZ) #name too long

df = subset(df, df$WCZ == "Tolo Harbour")

top_8_species <- head(subset(species_count, !is.na(species_count$Species)), 9)

df$Species <- ifelse(df$Species %in% top_8_species$Species, df$Species, "Other")

df_Species <- df %>%
  group_by(Year, Species) %>%
  summarize(count = n())

df_Species$Species <- factor(df_Species$Species, levels = sort(unique(df_Species$Species)))

#Plot
pl = ggplot(df_Species, aes(fill = Species, y = count, x = Year)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Species$Year), breaks = df_Species$Year) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_markdown(size = 15),
        axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1, margin = margin(t = -15)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
pl
#No top
df = read.csv(file = "Red_tide_and_WCZ_02162024.csv")

df$WCZ = gsub("Tolo Harbour & Channel", "Tolo Harbour", df$WCZ) #name too long

df = subset(df, df$WCZ == "Tolo Harbour")

df_Species <- df %>%
  group_by(Year, Species) %>%
  summarize(count = n())

df_Species$Species <- factor(df_Species$Species, levels = sort(unique(df_Species$Species)))

pl = ggplot(df_Species, aes(fill = Species, y = count, x = Year)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Species$Year), breaks = df_Species$Year) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_markdown(size = 15),
        axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1, margin = margin(t = -15)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
pl

#########Regression
###Sightings per year, regression
df_Year <- df %>%
  group_by(Year) %>%
  summarize(count = n()) %>%
  complete(Year = seq(from = 1977, to = 2023), fill = list(count = 0))  

df_Year$Year <- as.numeric(df_Year$Year)

df_Year <- df_Year %>%
  filter(Year != 2024) #Incomplete

# Step 1: Check for normality and heteroscedasticity (visual inspection)
ggplot(data = df_Year, aes(x = Year, y = count)) +
  geom_point() +
  theme_minimal()

# Step 2: Calculate the correlation
correlation <- cor(df_Year$Year, df_Year$count)

# Step 3: Perform a regression analysis
regression_model <- lm(count ~ Year, data = df_Year)

# Step 4: Create a graph with points and an abline
regplot = ggplot(data = df_Year, aes(x = Year, y = count)) +
  geom_point() +
  scale_x_continuous("Year", labels = as.character(df_Year$Year), breaks = df_Year$Year) +
  geom_abline(slope = regression_model$coefficients[2], intercept = regression_model$coefficients[1], color = "red") +
  theme_minimal() +
  ylab("Sightings") +
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 15))
regplot

summary(regression_model)

#Month
df = read.csv(file = "Red_tide_and_WCZ_02162024.csv")

df$WCZ = gsub("Tolo Harbour & Channel", "Tolo Harbour", df$WCZ) #name too long

df = subset(df, df$WCZ == "Tolo Harbour")

#Months
df$Month = substr(df$Date, start = 6, stop = 7)

# Sort df by numeric values of Month column
df <- df[order(df$Month), ] 

# Convert Month column to factor with desired levels
df$Month <- factor(df$Month, levels = unique(df$Month))

# Map numeric values to month abbreviations
df$Month <- gsub("01", "Jan", df$Month)
df$Month <- gsub("02", "Feb", df$Month)
df$Month <- gsub("03", "Mar", df$Month)
df$Month <- gsub("04", "Apr", df$Month)
df$Month <- gsub("05", "May", df$Month)
df$Month <- gsub("06", "Jun", df$Month)
df$Month <- gsub("07", "Jul", df$Month)
df$Month <- gsub("08", "Aug", df$Month)
df$Month <- gsub("09", "Sep", df$Month)
df$Month <- gsub("10", "Oct", df$Month)
df$Month <- gsub("11", "Nov", df$Month)
df$Month <- gsub("12", "Dec", df$Month)

df$Month <- factor(df$Month, levels = unique(df$Month))

# Create a new column for season based on Month
df$Season <- ifelse(df$Month %in% c("May", "Jun", "Jul", "Aug", "Sep"), "Wet Season", "Dry Season")



# Calculate the count of each species
species_count <- df %>%
  count(Species) %>%
  arrange(desc(n))

#calculate top 8
top_8_species <- head(subset(species_count, !is.na(species_count$Species)), 7)

df$Species <- ifelse(df$Species %in% top_8_species$Species, df$Species, "Other")

df_Species <- df %>%
  group_by(Month, Species) %>%
  summarize(count = n())

df_Species$Species <- factor(df_Species$Species, levels = sort(unique(df_Species$Species)))

#Plot
pk = ggplot(df_Species, aes(fill = Species, y = count, x = Month)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(angle = 90),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = -15)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
pk

df_Species$Species <- ifelse(df_Species$Species == "Other", "Other",
                             str_replace(df_Species$Species, "([A-Za-z]+(\\s+[A-Za-z]+)*)", "*\\1*"))
df_Species$Species <- factor(df_Species$Species, levels = sort(unique(df_Species$Species)))

pl = ggplot(df_Species, aes(fill = Species, y = count, x = Month)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_markdown(size = 15),
        axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1, margin = margin(t = -15)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
pl


#HATS
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

#from Environmental
overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

BOD5_data = subset(env_long, Variable == "BOD5 (mg/L)")
BOD5_data = subset(BOD5_data, WCZ == "Tolo Harbour")

# Convert Month to numeric format
month_to_numeric <- function(month) {
  match(month, month.abb)
}

BOD5_data$Month <- month_to_numeric(BOD5_data$Month)

# Convert Year and Month to Date format
BOD5_data$Date <- as.Date(paste(BOD5_data$Year, BOD5_data$Month, "01", sep = "-"))

# Group the data by Year and calculate the mean value for each year
BOD5_yearly <- BOD5_data %>%
  group_by(Year) %>%
  summarize(Value = mean(Value))

# Create the plot for value per year with regression lines
plot_value_per_year <- ggplot(BOD5_yearly, aes(x = Year, y = Value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("Year") +
  ylab("Mean Value")

# Calculate regression line equation and R-squared value
regression_model <- lm(Value ~ Year, data = BOD5_yearly)
regression_eqn <- paste0("y = ", round(regression_model$coefficients[1], 2), " + ",
                         round(regression_model$coefficients[2], 2), "x")
r_squared <- round(summary(regression_model)$r.squared, 2)

# Add R-squared value on top of the graph
plot_value_per_year <- plot_value_per_year +
  annotate("text", x = mean(BOD5_yearly$Year), y = mean(BOD5_yearly$Value)+0.05,
           label = paste("R-squared:", r_squared, "\np-value:", round(overall_p(regression_model), 2)), hjust = 0.5, vjust = -1, colour = "blue")

# Regression analysis for different time periods
regression_periods <- list(
  list(start_year = 1986, end_year = 1997),
  list(start_year = 1997, end_year = 2001),
  list(start_year = 2001, end_year = 2015),
  list(start_year = 2015, end_year = 2023)
)

lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}


a = list()
b = list()
c = c()
d = c()

# Perform regression analysis for each time period and plot regression lines
for (period in regression_periods) {
  start_year <- period$start_year
  end_year <- period$end_year
  
  # Filter the data for the current time period
  period_data <- filter(BOD5_data, Year >= start_year & Year <= end_year)
  a = lappend(a, period_data$Year)
  b = lappend(b, period_data$Value)
  
  # Calculate regression line equation and R-squared value
  regression_model <- lm(Value ~ Year, data = period_data)
  regression_eqn <- paste0("y = ", round(regression_model$coefficients[1], 2), " + ",
                           round(regression_model$coefficients[2], 2), "x")
  r_squared <- round(summary(regression_model)$r.squared, 2)
  c = append(c, paste("R-squared:", r_squared, "\np-value:", round(overall_p(regression_model), 2)))
  d = append(d, regression_eqn)
  
  # Add regression line to the plot
  plot_value_per_year <- plot_value_per_year +
    geom_smooth(data = period_data, method = "lm", se = FALSE, color = "red")
  
}

# Display the plot
BOD5PLOT2 = plot_value_per_year +
  annotate("text", x = mean(unlist(a[1])), y = mean(unlist(b[1]))-0.1,
           label = d[1], hjust = 0.5, vjust = -1, colour = "red") +
  annotate("text", x = mean(unlist(a[2])), y = mean(unlist(b[2]))-0.1,
           label = d[2], hjust = 0.5, vjust = -1, colour = "red") +
  annotate("text", x = mean(unlist(a[3])), y = mean(unlist(b[3]))-0.1,
           label = d[3], hjust = 0.5, vjust = -1, colour = "red") +
  annotate("text", x = mean(unlist(a[4])), y = mean(unlist(b[4]))-0.1,
           label = d[4], hjust = 0.5, vjust = -1, colour = "red") +
  
  annotate("text", x = mean(unlist(a[1])), y = max(BOD5_yearly$Value),
           label = c[1], hjust = 0.5, vjust = 1, colour = "red") +
  annotate("text", x = mean(unlist(a[2])), y = max(BOD5_yearly$Value),
           label = c[2], hjust = 0.5, vjust = 1, colour = "red") +
  annotate("text", x = mean(unlist(a[3])), y = max(BOD5_yearly$Value),
           label = c[3], hjust = 0.5, vjust = 1, colour = "red") +
  annotate("text", x = mean(unlist(a[4])), y = max(BOD5_yearly$Value),
           label = c[4], hjust = 0.5, vjust = 1, colour = "red") +
  
  theme_classic() +
  theme(axis.title.y = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.title.x = element_blank())

BOD5PLOT2

###
#Repeat for volatile suspended solids
BOD5_data = subset(env_long, Variable == "Volatile suspended\nsolids (mg/L)")
BOD5_data = subset(BOD5_data, WCZ == "Tolo Harbour")

# Convert Month to numeric format
month_to_numeric <- function(month) {
  match(month, month.abb)
}

BOD5_data$Month <- month_to_numeric(BOD5_data$Month)

# Convert Year and Month to Date format
BOD5_data$Date <- as.Date(paste(BOD5_data$Year, BOD5_data$Month, "01", sep = "-"))

# Group the data by Year and calculate the mean value for each year
BOD5_yearly <- BOD5_data %>%
  group_by(Year) %>%
  summarize(Value = mean(Value))

# Create the plot for value per year with regression lines
plot_value_per_year <- ggplot(BOD5_yearly, aes(x = Year, y = Value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("Year") +
  ylab("Mean Value")

# Calculate regression line equation and R-squared value
regression_model <- lm(Value ~ Year, data = BOD5_yearly)
regression_eqn <- paste0("y = ", round(regression_model$coefficients[1], 2), " + ",
                         round(regression_model$coefficients[2], 2), "x")
r_squared <- round(summary(regression_model)$r.squared, 2)

# Add R-squared value on top of the graph
plot_value_per_year <- plot_value_per_year +
  annotate("text", x = mean(BOD5_yearly$Year), y = mean(BOD5_yearly$Value)+0.05,
           label = paste("R-squared:", r_squared, "\np-value:", round(overall_p(regression_model), 2)), hjust = 0.5, vjust = -1, colour = "blue")

# Regression analysis for different time periods
regression_periods <- list(
  list(start_year = 1986, end_year = 1997),
  list(start_year = 1997, end_year = 2001),
  list(start_year = 2001, end_year = 2015),
  list(start_year = 2015, end_year = 2023)
)

lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}


a = list()
b = list()
c = c()
d = c()

# Perform regression analysis for each time period and plot regression lines
for (period in regression_periods) {
  start_year <- period$start_year
  end_year <- period$end_year
  
  # Filter the data for the current time period
  period_data <- filter(BOD5_data, Year >= start_year & Year <= end_year)
  a = lappend(a, period_data$Year)
  b = lappend(b, period_data$Value)
  
  # Calculate regression line equation and R-squared value
  regression_model <- lm(Value ~ Year, data = period_data)
  regression_eqn <- paste0("y = ", round(regression_model$coefficients[1], 2), " + ",
                           round(regression_model$coefficients[2], 2), "x")
  r_squared <- round(summary(regression_model)$r.squared, 2)
  c = append(c, paste("R-squared:", r_squared, "\np-value:", round(overall_p(regression_model), 2)))
  d = append(d, regression_eqn)
  
  # Add regression line to the plot
  plot_value_per_year <- plot_value_per_year +
    geom_smooth(data = period_data, method = "lm", se = FALSE, color = "red")
  
}

# Display the plot
VSSPLOT2 = plot_value_per_year +
  annotate("text", x = mean(unlist(a[1])), y = mean(unlist(b[1]))-0.2,
           label = d[1], hjust = 0.5, vjust = -1, colour = "red") +
  annotate("text", x = mean(unlist(a[2])), y = mean(unlist(b[2]))-0.2,
           label = d[2], hjust = 0.5, vjust = -1, colour = "red") +
  annotate("text", x = mean(unlist(a[3])), y = mean(unlist(b[3]))-0.2,
           label = d[3], hjust = 0.5, vjust = -1, colour = "red") +
  annotate("text", x = mean(unlist(a[4])), y = mean(unlist(b[4]))-0.2,
           label = d[4], hjust = 0.5, vjust = -1, colour = "red") +
  
  annotate("text", x = mean(unlist(a[1])), y = max(BOD5_yearly$Value),
           label = c[1], hjust = 0.5, vjust = 1, colour = "red") +
  annotate("text", x = mean(unlist(a[2])), y = max(BOD5_yearly$Value),
           label = c[2], hjust = 0.5, vjust = 1, colour = "red") +
  annotate("text", x = mean(unlist(a[3])), y = max(BOD5_yearly$Value),
           label = c[3], hjust = 0.5, vjust = 1, colour = "red") +
  annotate("text", x = mean(unlist(a[4])), y = max(BOD5_yearly$Value),
           label = c[4], hjust = 0.5, vjust = 1, colour = "red") +
  
  theme_classic() +
  theme(axis.title.y = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.title.x = element_blank())
VSSPLOT2

grid.arrange(BOD5PLOT2 + ggtitle("A") + ylab ("Mean BOD5"), VSSPLOT2 + ggtitle("B") + ylab("Mean VSS"), ncol = 1)


############

df = read.csv(file = "Red_tide_and_WCZ_02162024.csv")

df$WCZ = gsub("Tolo Harbour & Channel", "Tolo Harbour", df$WCZ) #name too long

df = subset(df, df$WCZ == "Tolo Harbour")

df$Month = substr(df$Date, start = 6, stop = 7)

BOD5_data = df

BOD5_data$Month <- month_to_numeric(BOD5_data$Month)

# Group the data by Year and calculate the mean value for each year
BOD5_yearly <- BOD5_data %>%
  group_by(Year) %>%
  summarize(count = n())

colnames(BOD5_yearly) = c("Year", "Value")

# Create the plot for value per year with regression lines
plot_value_per_year <- ggplot(BOD5_yearly, aes(x = Year, y = Value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("Year") +
  ylab("Mean Value")

# Calculate regression line equation and R-squared value
regression_model <- lm(Value ~ Year, data = BOD5_yearly)
regression_eqn <- paste0("y = ", round(regression_model$coefficients[1], 2), " + ",
                         round(regression_model$coefficients[2], 2), "x")
r_squared <- round(summary(regression_model)$r.squared, 2)

# Add R-squared value on top of the graph
plot_value_per_year <- plot_value_per_year +
  annotate("text", x = mean(BOD5_yearly$Year), y = mean(BOD5_yearly$Value)+20,
           label = paste("R-squared:", r_squared, "\np-value:", round(overall_p(regression_model), 2)), hjust = 0.5, vjust = -1, colour = "blue")

# Regression analysis for different time periods
regression_periods <- list(
  list(start_year = 1977, end_year = 1997),
  list(start_year = 1997, end_year = 2001),
  list(start_year = 2001, end_year = 2015),
  list(start_year = 2015, end_year = 2023)
)

lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}


a = list()
b = list()
c = c()
d = c()

BOD5_data = BOD5_yearly

# Perform regression analysis for each time period and plot regression lines
for (period in regression_periods) {
  start_year <- period$start_year
  end_year <- period$end_year
  
  # Filter the data for the current time period
  period_data <- filter(BOD5_data, Year >= start_year & Year <= end_year)
  a = lappend(a, period_data$Year)
  b = lappend(b, period_data$Value)
  
  # Calculate regression line equation and R-squared value
  regression_model <- lm(Value ~ Year, data = period_data)
  regression_eqn <- paste0("y = ", round(regression_model$coefficients[1], 2), " + ",
                           round(regression_model$coefficients[2], 2), "x")
  r_squared <- round(summary(regression_model)$r.squared, 2)
  c = append(c, paste("R-squared:", r_squared, "\np-value:", round(overall_p(regression_model), 2)))
  d = append(d, regression_eqn)
  
  # Add regression line to the plot
  plot_value_per_year <- plot_value_per_year +
    geom_smooth(data = period_data, method = "lm", se = FALSE, color = "red")
  
}

# Display the plot
RedtidePlot2 = plot_value_per_year +
  annotate("text", x = mean(unlist(a[1])), y = mean(unlist(b[1]))-10,
           label = d[1], hjust = 0.5, vjust = -1, colour = "red") +
  annotate("text", x = mean(unlist(a[2])), y = mean(unlist(b[2]))-10,
           label = d[2], hjust = 0.5, vjust = -1, colour = "red") +
  annotate("text", x = mean(unlist(a[3])), y = mean(unlist(b[3]))-10,
           label = d[3], hjust = 0.5, vjust = -1, colour = "red") +
  annotate("text", x = mean(unlist(a[4])), y = mean(unlist(b[4]))-10,
           label = d[4], hjust = 0.5, vjust = -1, colour = "red") +
  
  annotate("text", x = mean(unlist(a[1])), y = max(BOD5_yearly$Value),
           label = c[1], hjust = 0.5, vjust = 1, colour = "red") +
  annotate("text", x = mean(unlist(a[2])), y = max(BOD5_yearly$Value),
           label = c[2], hjust = 0.5, vjust = 1, colour = "red") +
  annotate("text", x = mean(unlist(a[3])), y = max(BOD5_yearly$Value),
           label = c[3], hjust = 0.5, vjust = 1, colour = "red") +
  annotate("text", x = mean(unlist(a[4])), y = max(BOD5_yearly$Value),
           label = c[4], hjust = 0.5, vjust = 1, colour = "red") +
  
  theme_classic() +
  ylab("Red tide sightings") +
  theme(axis.title.y = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.title.x = element_blank())

RedtidePlot2

grid.arrange(BOD5PLOT2 + ggtitle("A") + ylab ("Mean BOD5"), VSSPLOT2 + ggtitle("B") + ylab("Mean VSS"), RedtidePlot2 + ggtitle("C"), ncol = 1)

grid.arrange(BOD5PLOT + ggtitle("A") + ylab ("Mean BOD5"), BOD5PLOT2 + ggtitle("B") + ylab ("Mean BOD5"),
             VSSPLOT + ggtitle("C") + ylab("Mean VSS"), VSSPLOT2 + ggtitle("D") + ylab("Mean VSS"),
             RedtidePlot + ggtitle("E"), RedtidePlot2 + ggtitle("F"), ncol = 2)
