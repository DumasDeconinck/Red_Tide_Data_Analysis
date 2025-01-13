library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(gridExtra)

#Noctiluca
df = read.csv("Red_tide_and_WCZ_02162024.csv")

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

df_nscint = subset(df, Species == "Noctiluca scintillans")

###Year
df_nscint_year <- df_nscint %>%
  group_by(Year) %>%
  summarize(count = n())%>%
  complete(Year = min(Year):max(Year), fill = list(count = 0))

df_all_year <- df %>%
  group_by(Year) %>%
  summarize(count = n())%>%
  complete(Year = min(Year):max(Year), fill = list(count = 0))

df_all_year$relative = df_nscint_year$count/df_all_year$count

correlation <- cor(df_nscint_year$Year, df_nscint_year$count)

regression_model <- lm(count ~ Year, data = df_nscint_year)

# Step 4: Create a graph with points and an abline
plotyear = ggplot(data = df_nscint_year, aes(x = Year, y = count)) +
  geom_point() +
  geom_abline(slope = regression_model$coefficients[2], intercept = regression_model$coefficients[1], color = "red") +
  theme_minimal() +
  ylab("Sightings") +
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 15))
plotyear

summary(regression_model)

#relative
correlation <- cor(df_all_year$Year, df_all_year$relative)

regression_model <- lm(relative ~ Year, data = df_all_year)

plotyearrelative = ggplot(data = df_all_year, aes(x = Year, y = relative)) +
  geom_point() +
  geom_abline(slope = regression_model$coefficients[2], intercept = regression_model$coefficients[1], color = "red") +
  theme_minimal() +
  ylab("Sightings") +
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 15))
plotyearrelative

summary(regression_model)

###Month
df_nscint$Month <- factor(df_nscint$Month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

df_nscint_month <- df_nscint %>%
  group_by(Month) %>%
  summarize(count = n()) %>%
  complete(Month = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), fill = list(count = 0))  %>%
  arrange(match(Month, levels(df_nscint$Month)))

df_nscint_month$Month <- factor(df_nscint_month$Month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

df_nscint_month$Season <- ifelse(df_nscint_month$Month %in% c("May", "Jun", "Jul", "Aug", "Sep"), "Wet Season", "Dry Season")

df_all_month <- df %>%
  group_by(Month) %>%
  summarize(count = n()) %>%
  arrange(match(Month, levels(df$Month)))

df_all_month$relative = df_nscint_month$count/df_all_month$count

df_all_month$Month <- factor(df_all_month$Month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

df_all_month$Season <- ifelse(df_all_month$Month %in% c("May", "Jun", "Jul", "Aug", "Sep"), "Wet Season", "Dry Season")

# Create a graph
plotmonth = ggplot(data = df_nscint_month, aes(x = Month, y = count, fill = Season)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  theme_minimal() +
  ylab("Sightings") +
  theme(axis.title.y = element_text(size = 15),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 15),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        legend.position = c(0.9,0.9),
        axis.title.x = element_blank())
plotmonth


observed <- table(df_nscint$Season)

chi_result <- chisq.test(observed)
chi_result

# relative
plotmonthrelative = ggplot(data = df_all_month, aes(x = Month, y = relative, fill = Season)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  theme_minimal() +
  ylab("Sightings") +
  theme(axis.title.y = element_text(size = 15),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 15),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        legend.position = c(0.9,0.9),
        axis.title.x = element_blank())
plotmonthrelative

observed2 <- table(df$Season)
observed/observed2

chi_result <- chisq.test(observed, observed2)
chi_result


###WCZ
df_nscint_wcz <- df_nscint %>%
  group_by(WCZ) %>%
  summarize(count = n())

df_all_WCZ <- df %>%
  group_by(WCZ) %>%
  summarize(count = n())

df_all_WCZ$relative = df_nscint_wcz$count/df_all_WCZ$count

df_all_WCZ$WCZ = gsub("Tolo Harbour & Channel", "Tolo Harbour", df_all_WCZ$WCZ) #name too long

# Create a new column "area" based on the mapping
df_all_WCZ$area <- ifelse(df_all_WCZ$WCZ %in% c("Tolo Harbour", "Port Shelter", "Mirs Bay"), "Eastern",
                  ifelse(df_all_WCZ$WCZ %in% c("Southern"), "Southern",
                         ifelse(df_all_WCZ$WCZ %in% c("Junk Bay", "Western Buffer", "Eastern Buffer", "Victoria Harbour"), "Central",
                                ifelse(df_all_WCZ$WCZ %in% c("Deep Bay", "North Western"), "Western", NA))))

df_all_WCZ$WCZ = factor(df_all_WCZ$WCZ, levels = c("Deep Bay", "North Western","Junk Bay", "Western Buffer", "Eastern Buffer", "Victoria Harbour","Southern" ,"Tolo Harbour", "Port Shelter", "Mirs Bay"))

# Create a graph
plotwcz = ggplot(data = df_all_WCZ, aes(x = WCZ, y = count, fill = area)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ylab("Sightings") +
  theme(axis.title.y = element_text(size = 15),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 15),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        legend.position = c(0.1,0.9),
        axis.title.x = element_blank())
plotwcz

observed <- table(df_nscint$WCZ)

chi_result <- chisq.test(observed)
chi_result

# Create a graph
plotwczrelative = ggplot(data = df_all_WCZ, aes(x = WCZ, y = relative, fill = area)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ylab("Sightings") +
  theme(axis.title.y = element_text(size = 15),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 15),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        legend.position = c(0.1,0.9),
        axis.title.x = element_blank())
plotwczrelative

observed2 <- table(df$WCZ)
round(observed/observed2*100, 2)

chi_result <- chisq.test(observed, observed2)
chi_result

png(file = "Output/Supplementary_Figure_1.png", width = 1920, height = 1080)
grid.arrange(plotyear + theme(axis.title.x = element_blank()) +
               ggtitle("A"),
             plotyearrelative + theme(axis.title.x = element_blank()) +
               ggtitle("B"),
             plotmonth + theme(axis.title.x = element_blank()) +
               ggtitle("C"),
             plotmonthrelative + theme(axis.title.x = element_blank()) +
               ggtitle("D"),
             plotwcz + theme(axis.title.x = element_blank()) +
               ggtitle("E"),
             plotwczrelative + theme(axis.title.x = element_blank()) +
               ggtitle("F"),
             ncol = 2)
dev.off()