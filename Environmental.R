library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(linkET)
library(gridExtra)
library(ggtext)
library(stringr)

#load the red tide data
df = read.csv("Red_tide_and_WCZ_02162024.csv")

#add months
df$Month = substr(df$Date, start = 6, stop = 7)

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

#add an extra Sample column Sample for later merging
df$Sample = paste(df$Month, df$Year, df$WCZ)

#Group by sample, and species
df2 <- df %>%
  group_by(Sample, Species) %>%
  summarize(count = n())

#Group by sample, and Phylum 
df3 <- df %>%
  group_by(Sample, Phylum) %>%
  summarize(count = n())

#Group by sample, and Group 
df4 <- df %>%
  group_by(Sample, Group) %>%
  summarize(count = n()) 

## Calculate the count of each species
species_count <- df %>%
  count(Species) %>%
  arrange(desc(n))

#calculate top 8
top_15_species <- head(subset(species_count, !is.na(species_count$Species)), 15)

# Calculate the relative counts for each species within a sample
#df_relative <- df2 %>%
#  group_by(Sample) %>%
#  mutate(Relative_Count = count / sum(count)) %>%
#  ungroup()

#df = df_relative
df = df2

#load the environmental data
file_names <- dir("./EPD_Marine_Historical_Data")

originalwd = getwd()#store the original wd
setwd("./EPD_Marine_Historical_Data") #enter the folder that contains the environmental data

env <- do.call(rbind,lapply(file_names,read.csv)) #load all the environmental csv files into one data frame

setwd(originalwd) #return to the original data frame

#combine the data
#keep only surface data
env = subset(env, Depth == "Surface Water")

#Months
env$Month = substr(env$Dates, start = 6, stop = 7)

# Map numeric values to month abbreviations
env$Month <- gsub("01", "Jan", env$Month)
env$Month <- gsub("02", "Feb", env$Month)
env$Month <- gsub("03", "Mar", env$Month)
env$Month <- gsub("04", "Apr", env$Month)
env$Month <- gsub("05", "May", env$Month)
env$Month <- gsub("06", "Jun", env$Month)
env$Month <- gsub("07", "Jul", env$Month)
env$Month <- gsub("08", "Aug", env$Month)
env$Month <- gsub("09", "Sep", env$Month)
env$Month <- gsub("10", "Oct", env$Month)
env$Month <- gsub("11", "Nov", env$Month)
env$Month <- gsub("12", "Dec", env$Month)

#Years
env$Year = substr(env$Dates, start = 1, stop = 4)

#turn N/A into NA
env <- replace(env, env == "N/A", NA)

#impute the values with the midpoint between zero and the reported lower limit.
#for (col in names(env)) {
  # Check if column contains "<" values
  #if (any(grepl("<", env[[col]]))) {
    # Replace "<" values with imputed values
    #env[[col]] <- gsub("<", "", env[[col]])  # Remove the "<" symbol
    #env[[col]] <- as.numeric(env[[col]])  # Convert to numeric
    #env[[col]] <- ifelse(is.na(env[[col]]), NA, (env[[col]] + 0) / 2)  # Replace with midpoint between zero and the reported lower limit
  #}
#} #Old script, divides every value by two for some reason.

for (col in names(env)) {
  # Check if column contains "<" values
  if (any(grepl("<", env[[col]]))) {
    # Replace "<" values with imputed values
    env[[col]] <- as.character(env[[col]])  # Convert to character
    env[[col]] <- ifelse(grepl("<", env[[col]]), as.numeric(gsub("<", "", env[[col]])) / 2, env[[col]])  # Divide the values by 2 if they contain "<"
    env[[col]] <- as.numeric(env[[col]])  # Convert back to numeric
  }
}

env


#Make sure the WCZ names are the same
env$Water.Control.Zone = gsub("and" ,"&"  , env$Water.Control.Zone)

#Every WCZ has multiple Monitoring stations. A way to get around it could be to take averages
env$Sample = paste(env$Month, env$Year, env$Water.Control.Zone)

#remove every column except the numerical ones and Sample
env2 = env[, -(1:5)]
env2 <- env2[, -c(length(env2)-1, length(env2)-2)]

# Convert numeric columns to numeric format
env2 <- env2 %>%
  mutate_at(vars(-Sample), as.numeric)

# Calculate averages by sample
env3 <- aggregate(. ~ Sample, data = env2, FUN = function(x) mean(x, na.rm = TRUE))

#change df to wide format
df_wide <- df %>% spread(key = Species, value = count)

# Merge the two dataframes by Sample 
merged_df <- merge(df_wide, env3, by = "Sample")

#Nov 2004 Deep Bay and August 2022 North Western make the graph unreadable due to how far away they are
merged_df = filter(merged_df, Sample != "Nov 2004 Deep Bay")
merged_df = filter(merged_df, Sample != "Aug 2022 North Western")

#Return back to two different datasets, but this time it makes sure they are in the same order
df_wide = merged_df[0:length(unique(df$Species)) + 1]
df_wide$Sample <- factor(df_wide$Sample, levels = unique(df_wide$Sample))

df_env = data.frame(merged_df$Sample ,merged_df[(length(unique(df$Species)) + 2): ncol(merged_df)])
df_env$merged_df.Sample <- factor(df_env$merged_df.Sample, levels = unique(df_env$merged_df.Sample))

#CCA
df_wide[is.na(df_wide)] = 0 #Non detections should be 0 instead of NA

#Run the cca
cc3 = cca(df_wide[, 2:ncol(df_wide)] ~ ., data = df_env[, 2:ncol(df_env)])

#Plot
##ggplot
#extracting the data as data frame; env data
veg_1 = as.data.frame(cc3$CCA$biplot)
veg_1["env"] = row.names(veg_1)

#extracting the data; species
veg_2 = as.data.frame(cc3$CCA$u)
veg_2["sites"] = df_wide[1]

#Add back the Month, Year, and WCZ
veg_2$sites
veg_2$Month = substr(veg_2$sites, start = 1, stop = 3)
veg_2$Year = substr(veg_2$sites, start = 5, stop = 8)
veg_2$WCZ = substr(veg_2$sites, start = 10, stop = length(veg_2$sites))

## add polygon for WCZ
#Prepare polygon
grp.a <- subset(veg_2, WCZ == unique(veg_2$WCZ)[1])
grp.b <- subset(veg_2, WCZ == unique(veg_2$WCZ)[2])
grp.c <- subset(veg_2, WCZ == unique(veg_2$WCZ)[3])
grp.d <- subset(veg_2, WCZ == unique(veg_2$WCZ)[4])
grp.e <- subset(veg_2, WCZ == unique(veg_2$WCZ)[5])
grp.f <- subset(veg_2, WCZ == unique(veg_2$WCZ)[6])
grp.g <- subset(veg_2, WCZ == unique(veg_2$WCZ)[7])
grp.h <- subset(veg_2, WCZ == unique(veg_2$WCZ)[8])
grp.i <- subset(veg_2, WCZ == unique(veg_2$WCZ)[9])
grp.j <- subset(veg_2, WCZ == unique(veg_2$WCZ)[10])


grp.a <- veg_2[veg_2$WCZ == unique(veg_2$WCZ)[1], ][chull(veg_2[veg_2$WCZ == 
                                                                  unique(veg_2$WCZ)[1], c("CCA1", "CCA2")]), ]  # hull values
grp.b <- veg_2[veg_2$WCZ == unique(veg_2$WCZ)[2], ][chull(veg_2[veg_2$WCZ == 
                                                                  unique(veg_2$WCZ)[2], c("CCA1", "CCA2")]), ]  
grp.c <- veg_2[veg_2$WCZ == unique(veg_2$WCZ)[3], ][chull(veg_2[veg_2$WCZ == 
                                                                  unique(veg_2$WCZ)[3], c("CCA1", "CCA2")]), ]  
grp.d <- veg_2[veg_2$WCZ == unique(veg_2$WCZ)[4], ][chull(veg_2[veg_2$WCZ == 
                                                                  unique(veg_2$WCZ)[4], c("CCA1", "CCA2")]), ]  
grp.e <- veg_2[veg_2$WCZ == unique(veg_2$WCZ)[5], ][chull(veg_2[veg_2$WCZ == 
                                                                  unique(veg_2$WCZ)[5], c("CCA1", "CCA2")]), ]  
grp.f <- veg_2[veg_2$WCZ == unique(veg_2$WCZ)[6], ][chull(veg_2[veg_2$WCZ == 
                                                                  unique(veg_2$WCZ)[6], c("CCA1", "CCA2")]), ]  
grp.g <- veg_2[veg_2$WCZ == unique(veg_2$WCZ)[7], ][chull(veg_2[veg_2$WCZ == 
                                                                  unique(veg_2$WCZ)[7], c("CCA1", "CCA2")]), ]  
grp.h <- veg_2[veg_2$WCZ == unique(veg_2$WCZ)[8], ][chull(veg_2[veg_2$WCZ == 
                                                                  unique(veg_2$WCZ)[8], c("CCA1", "CCA2")]), ]  
grp.i <- veg_2[veg_2$WCZ == unique(veg_2$WCZ)[9], ][chull(veg_2[veg_2$WCZ == 
                                                                  unique(veg_2$WCZ)[9], c("CCA1", "CCA2")]), ]  
grp.j <- veg_2[veg_2$WCZ == unique(veg_2$WCZ)[10], ][chull(veg_2[veg_2$WCZ == 
                                                                  unique(veg_2$WCZ)[10], c("CCA1", "CCA2")]), ]  

hull.data <- rbind(grp.a, grp.b, grp.c, grp.d, grp.e, grp.f, grp.g, grp.h, grp.i, grp.j)  # combine grp.a and grp.b
hull.data

veg_1$env
veg_1$env = gsub("E..coli..cfu.100mL.", "E.coli", veg_1$env)
veg_1$env = gsub("X5.day.Biochemical.Oxygen.Demand..mg.L.", "BOD5", veg_1$env)
veg_1$env = sub("\\.\\..*$", "", veg_1$env)
veg_1$env = sub("\\.", " ", veg_1$env)

## add polygon for season
veg_2$Season <- ifelse(veg_2$Month %in% c("May", "Jun", "Jul", "Aug", "Sep"), "Wet Season", "Dry Season")

#Plot
color_palette <- brewer.pal(10, "Set3")  # Adjust the number of colors as needed
graph <- ggplot() +
  geom_polygon(data = hull.data, aes(x = CCA1, y = CCA2, fill = WCZ, group = WCZ), alpha = 0.3) +
  geom_point(data = veg_2, aes(x = CCA1, y = CCA2, colour = Season), size = 1) +
  geom_segment(
    data = veg_1,
    aes(
      x = 0,
      y = 0,
      xend = CCA1*3,#note the times 3, so inaccurate, but makes it legible.
      yend = CCA2*3
    ), colour = "blue",
    arrow = arrow(length = unit(0.25, "cm"))
  ) +
  geom_text_repel(
    data = veg_1,
    aes(x = CCA1*3, y = CCA2*3, label = veg_1$env),
    nudge_y = -0.05,
    color = "red",
    size = 2
  ) +
  scale_colour_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  labs(x = "CCA1", y = "CCA2") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

# Display the graph
print(graph)

#Takes a while to run, so disabled untill needed.
AovRes = anova(cc3,by="mar",permutations=999) #analyse significances of marginal effects (“TypeIII effects”)
AovRes
cc3

#Analysis of cca
#Monte carlo
perm_test <- anova.cca(cc3, parallel = TRUE, permutations = 999)
perm_test

# Set the significance level
significance_level <- 0.1

# Extract the significant factors
significant_factors <- AovRes$"Pr(>F)" < significance_level
significant_factors = which(significant_factors == TRUE)

df_env_significant = df_env[significant_factors+1]
colnames(df_env_significant) #returns a list of the names of the significant (0.1 p value) environmental factors
#df_env_significant = cbind("Sample" = env3$Sample,df_env_significant)

##Environmental Correlation, and Mantle tests!

#start by making the colnames more legible.
#Non imputed data: colnames(df_env_significant) = c("BOD5 (Mg/L)", "Salinity (psu)",  "Silica (Mg/L)", "Suspended solids (Mg/L)", "Temperature (°C)","Total phosporus (Mg/L)","Turbidity (NTU)")
colnames(df_env_significant) = c("BOD5 (mg/L)",
                                 "Orthophosphate Phosphorus (mg/L)",
                                 "pH",
                                 "Salinity (psu)",
                                 "Silica (mg/L)",
                                 "Suspended solids (mg/L)",
                                 "Temperature (°C)",
                                 "Total phosphorus (mg/L)",
                                 "Turbidity (NTU)",
                                 "Volatile suspended solids (mg/L)")

#store for later use:
modelbio = df_wide
modelenv = df_env_significant

##For species
#One factor effect:
# Calculate the correlation between BOD5 and each species column
correlation <- cor(df_wide[2:ncol(df_wide)], df_env_significant$`BOD5 (mg/L)`)

# Count the number of positively and negatively correlated species
positive_correlation <- sum(na.omit(correlation) > 0)
negative_correlation <- sum(na.omit(correlation) < 0)

#One species effect:
correlation = correlate(df_wide$`Noctiluca scintillans`, df_env_significant)
correlation$r
correlation$p


#Correlation for all species
cora = correlate(df_wide[2:ncol(df_wide)], df_env_significant) %>% 
  qcorrplot() +
  geom_square() +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        legend.position='left',
        axis.text.x = element_text(angle = 45, hjust = 1, size= 6),
        legend.key.size = unit(1.2, "lines"),
        legend.background = element_rect(fill = "white", color = "black"))
cora

#Filter for the 15 most abundant red tide species
df_wide_top <- df_wide[, top_15_species$Species]
df_wide_top_2 = cbind(df_wide$Sample, df_wide_top)
colnames(df_wide_top_2)[1] = "Sample"
df_wide = df_wide_top_2

coraa = correlate(df_wide[2:ncol(df_wide)], df_env_significant) %>% 
  qcorrplot() +
  geom_square() +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        legend.position='left',
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(1.2, "lines"),
        legend.background = element_rect(fill = "white", color = "black"))
coraa

#Mantel
#Mantel test becomes illegible with too much species. Additionally, it takes forever, and it is nonsensical to look at species that don't appear much
# Get the column names excluding the first column from df_wide
column_names <- names(df_wide)[-1]

column_list <- as.list(column_names)

# Get the number of columns in df_env excluding the first column
num_env_columns <- ncol(df_env_significant)

# Repeat the species column names based on the number of df_env columns
repeated_column_names <- rep(column_names, each = num_env_columns)

mantel <- mantel_test(df_wide[2:ncol(df_wide)], df_env_significant,
                      spec_select = column_list) %>% 
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))

#mantel2 = mantel %>% 
#  mutate(rd = cut(r, breaks = c(-Inf, 0, Inf),
#                  labels = c("< 0", ">= 0")),
#       pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
#                labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))
#mantel_species = mantel #store for later use (takes very long to repeat)
#mantel = mantel2 #
mantel$spec = repeated_column_names #this is to ensure the species names are displayed on the graph

#mantel$spec <- ifelse(is.na(mantel$spec), "Undetermined",
#                     str_replace(mantel$spec, "(.*?)(\\ssp\\.)?$", "*\\1*\\2"))


#Plot it
mantela= qcorrplot(correlate(df_env_significant), type = "lower", diag = FALSE) +
  geom_square() +
  geom_couple(aes(colour = rd, size = pd, alpha = pd), 
              data = mantel, 
              curvature = nice_curvature(),
              nudge = .1,
              label.size = 3) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_size_manual(values = c(2, 1, 0.5)) +
  scale_colour_manual(values = color_pal(3)) +
  scale_alpha_manual(values = c(1,1,0.3)) +
  guides(size = guide_legend(title = "Mantel's p",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's r", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3),
         alpha = "none") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position='left',
        legend.key.size = unit(1.2, "lines"),
        legend.background = element_rect(fill = "white", color = "black"))

mantela

mantela = qcorrplot(correlate(df_env_significant), type = "lower", diag = FALSE) +
  geom_square() +
  geom_couple(aes(colour = pd, size = rd), 
              data = mantel, 
              curvature = nice_curvature(),
              nudge = .1,
              label.size = 3) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values = color_pal(3)) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 45, hjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position='left',
        legend.key.size = unit(1.2, "lines"),
        legend.background = element_rect(fill = "white", color = "black"))
mantela

##For Phyla
#change df to wide format
df = df3
df_wide <- df %>% spread(key = Phylum, value = count)

# Merge the two dataframes by Sample 
merged_df <- merge(df_wide, env3, by = "Sample")

#Nov 2004 Deep Bay and August 2022 North Western make the graph unreadable due to how far away they are
merged_df = filter(merged_df, Sample != "Nov 2004 Deep Bay")
merged_df = filter(merged_df, Sample != "Aug 2022 North Western")

#Return back to two different datasets, but this time it makes sure they are in the same order
df_wide = merged_df[0:length(unique(df$Phylum)) + 1]
df_wide$Sample <- factor(df_wide$Sample, levels = unique(df_wide$Sample))

df_env = data.frame(merged_df$Sample ,merged_df[(length(unique(df$Phylum)) + 2): ncol(merged_df)])
df_env$merged_df.Sample <- factor(df_env$merged_df.Sample, levels = unique(df_env$merged_df.Sample))

#
df_wide[is.na(df_wide)] = 0 #Non detections should be 0 instead of NA

colnames(df_wide) = gsub("<NA>", "Undetermined", colnames(df_wide))

#Correlation
corb = correlate(df_wide[2:ncol(df_wide)], df_env_significant) %>% 
  qcorrplot() +
  geom_square() +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        legend.position='left',
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(1.2, "lines"),
        legend.background = element_rect(fill = "white", color = "black"))
corb

#Mantel
#Mantel test becomes illegible with too much species. Additionally, it takes forever, and it is nonsensical to look at species that don't appear much
# Get the column names excluding the first column from df_wide
column_names <- names(df_wide)[-1]

column_list <- as.list(column_names)

# Get the number of columns in df_env excluding the first column
num_env_columns <- ncol(df_env_significant)

# Repeat the species column names based on the number of df_env columns
repeated_column_names <- rep(column_names, each = num_env_columns)

mantel <- mantel_test(df_wide[2:ncol(df_wide)], df_env_significant,
                      spec_select = column_list) %>% 
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))

#mantel2 = mantel %>% 
#  mutate(rd = cut(r, breaks = c(-Inf, 0, Inf),
#                  labels = c("< 0", ">= 0")),
#         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
#                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))
#mantel_phylum = mantel
#mantel = mantel2

mantel$spec = repeated_column_names #this is to ensure the species names are displayed on the graph
mantel$spec <- gsub("<NA>", "Undetermined", mantel$spec)

#Plot it
mantelb = qcorrplot(correlate(df_env_significant), type = "lower", diag = FALSE) +
  geom_square() +
  geom_couple(aes(colour = rd, size = pd, alpha = pd), 
              data = mantel, 
              curvature = nice_curvature(),
              nudge = .1,
              label.size = 3) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_size_manual(values = c(2, 1, 0.5)) +
  scale_colour_manual(values = color_pal(3)) +
  scale_alpha_manual(values = c(1,1,0.3)) +
  guides(size = guide_legend(title = "Mantel's p",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's r", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3),
         alpha = "none") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position='left',
        legend.key.size = unit(1.2, "lines"),
        legend.background = element_rect(fill = "white", color = "black"))
mantelb

mantelb = qcorrplot(correlate(df_env_significant), type = "lower", diag = FALSE) +
  geom_square() +
  geom_couple(aes(colour = pd, size = rd), 
              data = mantel, 
              curvature = nice_curvature(),
              nudge = .1,
              label.size = 3) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values = color_pal(3)) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 45, hjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position='left',
        legend.key.size = unit(1.2, "lines"),
        legend.background = element_rect(fill = "white", color = "black"))
mantelb

#For groups
#change df to wide format
df = df4
df_wide <- df %>% spread(key = Group, value = count)

# Merge the two dataframes by Sample 
merged_df <- merge(df_wide, env3, by = "Sample")

#Nov 2004 Deep Bay and August 2022 North Western make the graph unreadable due to how far away they are
merged_df = filter(merged_df, Sample != "Nov 2004 Deep Bay")
merged_df = filter(merged_df, Sample != "Aug 2022 North Western")

#Return back to two different datasets, but this time it makes sure they are in the same order
df_wide = merged_df[0:length(unique(df$Group)) + 1]
df_wide$Sample <- factor(df_wide$Sample, levels = unique(df_wide$Sample))

df_env = data.frame(merged_df$Sample ,merged_df[(length(unique(df$Group)) + 2): ncol(merged_df)])
df_env$merged_df.Sample <- factor(df_env$merged_df.Sample, levels = unique(df_env$merged_df.Sample))

#
df_wide[is.na(df_wide)] = 0 #Non detections should be 0 instead of NA

#Correlation
corc = correlate(df_wide[2:ncol(df_wide)], df_env_significant) %>% 
  qcorrplot() +
  geom_square() +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        legend.position='left',
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(1.2, "lines"),
        legend.background = element_rect(fill = "white", color = "black"))
corc

#One species effect:
correlation = correlate(df_wide$Diatom, df_env_significant)
correlation$r
correlation$p

#Mantel
#Mantel test becomes illegible with too much species. Additionally, it takes forever, and it is nonsensical to look at species that don't appear much
# Get the column names excluding the first column from df_wide
column_names <- names(df_wide)[-1]

column_list <- as.list(column_names)

# Get the number of columns in df_env excluding the first column
num_env_columns <- ncol(df_env_significant)

# Repeat the species column names based on the number of df_env columns
repeated_column_names <- rep(column_names, each = num_env_columns)

mantel <- mantel_test(df_wide[2:ncol(df_wide)], df_env_significant,
                      spec_select = column_list) %>% 
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))

#mantel2 = mantel %>% 
#  mutate(rd = cut(r, breaks = c(-Inf, 0, Inf),
#                  labels = c("< 0", ">= 0")),
#         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
#                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))
#mantel_groups = mantel
#mantel = mantel2

mantel$spec = repeated_column_names #this is to ensure the species names are displayed on the graph

#Plot it
mantelc = qcorrplot(correlate(df_env_significant), type = "lower", diag = FALSE) +
  geom_square() +
  geom_couple(aes(colour = rd, size = pd, alpha = pd), 
               data = mantel, 
               curvature = nice_curvature(),
               nudge = .1,
               label.size = 3) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_size_manual(values = c(2,1, 0.5)) +
  scale_colour_manual(values = color_pal(3)) +
  scale_alpha_manual(values = c(1,1,0.3)) +
  guides(size = guide_legend(title = "Mantel's p",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's r", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3),
         alpha = "none") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position='left',
        legend.key.size = unit(1.2, "lines"),
        legend.background = element_rect(fill = "white", color = "black"))
mantelc

mantelc = qcorrplot(correlate(df_env_significant), type = "lower", diag = FALSE) +
  geom_square() +
  geom_couple(aes(colour = pd, size = rd), 
              data = mantel, 
              curvature = nice_curvature(),
              nudge = .1,
              label.size = 3) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values = color_pal(3)) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 45, hjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position='left',
        legend.key.size = unit(1.2, "lines"),
        legend.background = element_rect(fill = "white", color = "black"))
mantelc

#Put together
common_legend <- cowplot::get_legend(mantela)

grid.arrange(corc, 
             mantelc + theme(legend.position = "none"), 
             corb,
             mantelb + theme(legend.position = "none"),
             coraa,
             mantela + theme(legend.position = "none"), ncol = 2)

cowplot::plot_grid(
  grid.arrange(corc, 
               mantelc + theme(legend.position = "none"), 
               corb,
               mantelb + theme(legend.position = "none"),
               coraa,
               mantela + theme(legend.position = "none"), 
               ncol = 2),
  common_legend, ncol = 2, rel_widths = c(1.2, 1))

cowplot::plot_grid(grid.arrange(coraa, corb, corc, 
             mantela + theme(legend.position = "none"), 
             mantelb + theme(legend.position = "none"), 
             mantelc + theme(legend.position = "none"), 
             ncol = 3), common_legend, nrow = 2, rel_heights = c(4,1))

grid.arrange(coraa, corb, corc, 
             mantela + theme(legend.position = "none", plot.margin = margin(0, 0, 0, 50)), 
             mantelb + theme(legend.position = "none"), 
             mantelc + theme(legend.position = "none"), 
             ncol = 3)

#Permanova
df = df2
df_wide <- df %>% spread(key = Species, value = count)

df_wide[is.na(df_wide)] = 0

# Merge the two dataframes by Sample 
merged_df <- merge(df_wide, env3, by = "Sample")

#add back the months
merged_df$Season = substr(merged_df$Sample, start = 1, stop = 3)
merged_df$Year = substr(merged_df$Sample, start = 5, stop = 8)

# Create a new column for season based on Month
merged_df$Season <- ifelse(merged_df$Season %in% c("May", "Jun", "Jul", "Aug", "Sep"), "Wet Season", "Dry Season")

#add backthe WCZ
merged_df$WCZ = substr(merged_df$Sample, start = 10, stop = length(merged_df$Sample))
  
#Nov 2004 Deep Bay and August 2022 North Western make the graph unreadable due to how far away they are
merged_df = filter(merged_df, Sample != "Nov 2004 Deep Bay")
merged_df = filter(merged_df, Sample != "Aug 2022 North Western")

#merged_df[is.na(merged_df$Sample)] = 0 #Non detections should be 0 instead of NA

#Return back to two different datasets, but this time it makes sure they are in the same order
df_wide = merged_df[0:length(unique(df$Species)) + 1]
df_wide$Sample <- factor(df_wide$Sample, levels = unique(df_wide$Sample))

df_env = data.frame(merged_df$Sample ,merged_df[(length(unique(df$Species)) + 2): ncol(merged_df)])
df_env$merged_df.Sample <- factor(df_env$merged_df.Sample, levels = unique(df_env$merged_df.Sample))

df_wide[is.na(df_wide)] = 0 #Non detections should be 0 instead of NA

#dissimilarity matrix
species_matrix <- as.matrix(df_wide[, -1])  # Exclude the first column (sample ID)
dissimilarity_matrix <- vegdist(species_matrix, method = "bray")

#Perform the permanova
result <- adonis2(dissimilarity_matrix ~ Season + WCZ + Year, data = merged_df)
result

#Presenting environmental data
# Extract the significant factors
significant_factors <- AovRes$"Pr(>F)" < significance_level
significant_factors = which(significant_factors == TRUE)

df_env_significant = df_env[significant_factors+1]
significant_factors = colnames(df_env_significant)
significant_factors = append(significant_factors, c("Water.Control.Zone","Station", "Month", "Year"))

envbackup = env
env <- env[, colnames(env) %in% significant_factors]

colnames(env) = c("WCZ",
                  "Station",
                  "BOD5 (mg/L)",
                  "Orthophosphate Phosphorus (mg/L)",
                  "pH",
                  "Salinity (psu)",
                  "Silica (mg/L)",
                  "Suspended solids (mg/L)",
                  "Temperature (°C)",
                  "Total phosphorus (mg/L)",
                  "Turbidity (NTU)",
                  "Volatile suspended solids (mg/L)",
                  "Month",
                  "Year")

env$pH = as.integer(env$pH)
env$`Salinity (psu)` = as.integer(env$`Salinity (psu)`)
env$`Temperature (°C)` = as.integer(env$`Temperature (°C)`)
#env$`Total Nitrogen (mg/L)` = as.integer(env$`Total Nitrogen (mg/L)`)
env$`Turbidity (NTU)` = as.integer(env$`Turbidity (NTU)`)

#Change the names to better fit the plots
colnames(env) = c("WCZ",
                  "Station",
                  "BOD5 (mg/L)",
                  "Orthophosphate\nphosphorus (mg/L)",
                  "pH",
                  "Salinity (psu)",
                  "Silica (mg/L)",
                  "Suspended\n solids (mg/L)",
                  "Temperature (°C)",
                  "Total\nphosphorus (mg/L)",
                  "Turbidity (NTU)",
                  "Volatile suspended\nsolids (mg/L)",
                  "Month",
                  "Year")

env_long <- env %>%
  pivot_longer(cols = -c(WCZ, Station, Month, Year),
               names_to = "Variable",
               values_to = "Value")

#Tolo Harbour name too long
env_long$WCZ = gsub("Tolo Harbour & Channel", "Tolo Harbour", env_long$WCZ)

env_long$Value = as.numeric(env_long$Value)
env_long = na.omit(env_long)

#Per WCZ
enva = ggplot(env_long, aes(x = WCZ, y = Value, fill = WCZ)) +
  stat_summary(geom = "bar", fun = "mean", position = "dodge") +
  facet_wrap(~ Variable, scales = "free_y", nrow = 2) +
  labs(x = "WCZ", y = "Mean Value") +
  scale_fill_manual(values = brewer.pal(10, "Set3")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.7, "lines"))
enva

#Per Season
env_long$Season <- ifelse(env_long$Month %in% c("May", "Jun", "Jul", "Aug", "Sep"), "Wet Season", "Dry Season")

# Convert Month to factor with desired levels

env_long$Month <- factor(env_long$Month, levels = substr(month.name, start = 1, stop = 3))

envb = ggplot(env_long, aes(x = Month, y = Value, fill = Season)) +
  stat_summary(geom = "bar", fun = "mean", position = "dodge") +
  facet_wrap(~ Variable, scales = "free_y", nrow = 2) +
  labs(x = "Month", y = "Mean Value") +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
envb

Sumdat = as.data.frame(ggplot_build(envb)$data)

env_long$Year = as.numeric(env_long$Year) #needs to 

#slopes
slopes = env_long %>%
  group_by(Variable, Year) %>%
  summarize(Mean_Value = mean(Value)) %>%
  group_by(Variable) %>%
  do({
    mod = lm(Mean_Value ~ Year, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  })
slopes$Formula = paste("f(x)=", round(slopes$Intercept, 2), "+(", formatC(slopes$Slope, format = "e", digits = 2),")*x", sep = "")

write.csv(slopes, "Slopes.csv")

#Per Year
envc = ggplot(env_long, aes(x = Year, y = Value)) +
  stat_summary(geom = "point", fun = "mean", position = position_dodge(width = 0.5)) +
  geom_smooth(aes(group = Variable), method = "lm", se = F, color = "red") +
  facet_wrap(~ Variable, scales = "free_y", nrow = 2) +
  labs(x = "Year", y = "Mean Value") +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

envc

grid.arrange(envc, envb, enva, ncol = 1)



#Check significances
# Create an empty list to store the ANOVA results
anova_results <- list()

# Perform ANOVA for each environmental variable
for (variable in colnames(df_env_significant)) {
  formula <- as.formula(paste(variable, "~ WCZ + Season + Year"))
  anova_result <- anova(lm(formula, data = merged_df))
  anova_results[[variable]] <- anova_result
}

# Combine the ANOVA results into a data frame
anova_results_df <- do.call(rbind, anova_results)

# Print the ANOVA results
print(anova_results_df)

#Which ones are not significant?
print(anova_results_df[which(anova_results_df$"Pr(>F)" > 0.05), ])

