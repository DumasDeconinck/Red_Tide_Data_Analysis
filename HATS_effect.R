library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

#from Environmental
env_long

overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

BOD5_data = subset(env_long, Variable == "BOD5 (mg/L)")

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
BOD5PLOT = plot_value_per_year +
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

BOD5PLOT

###
#Repeat for volatile suspended solids
BOD5_data = subset(env_long, Variable == "Volatile suspended\nsolids (mg/L)")

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
VSSPLOT = plot_value_per_year +
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
VSSPLOT

grid.arrange(BOD5PLOT + ggtitle("A") + ylab ("Mean BOD5"), VSSPLOT + ggtitle("B") + ylab("Mean VSS"), ncol = 1)

#HATS for red tide sightings

df = read.csv(file = "Red_tide_and_WCZ_02162024.csv")

df$WCZ = gsub("Tolo Harbour & Channel", "Tolo Harbour", df$WCZ) #name too long

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
RedtidePlot = plot_value_per_year +
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

RedtidePlot

grid.arrange(BOD5PLOT + ggtitle("A") + ylab ("Mean BOD5"), VSSPLOT + ggtitle("B") + ylab("Mean VSS"), RedtidePlot + ggtitle("C"), ncol = 1)

