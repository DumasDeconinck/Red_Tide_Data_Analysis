#library
library(ggplot2)
library(vegan)
library(ape)
library(RColorBrewer)
library(dplyr)
library(ggrepel)
library(ggdendro)
library(grid)
library(tidyr)
library(dplyr)
library(stringr)
library(ggtext)
library(indicspecies)

df = read.csv("Red_tide_and_WCZ_02162024.csv")

df$WCZ = gsub("Tolo Harbour & Channel", "Tolo Harbour", df$WCZ) #name too long


#use data from maps that has the WSZ
df <- df %>%
  group_by(WCZ, Species) %>%
  summarize(count = n())

df_relative <- df %>%
  group_by(WCZ) %>%
  mutate(Relative_Count = count / sum(count)) %>%
  ungroup()

df$count = df_relative$Relative_Count

#wide
df_wide <- df %>% spread(key = Species, value = count)

# perserve order
df$WCZ <- factor(df$WCZ, levels = unique(df$WCZ))
df$Species = factor(df$Species, levels = unique(df$Species))

# Calculate minimum and maximum count values
min_count <- min(df$count)
max_count <- max(df$count)

# Calculate the increment size for labels
increment <- (max_count - min_count) / 5

# Generate the breaks and labels
labels<- round(seq(min_count, max_count, by = increment),2)

#scale to remove zeros
df_scaled = df_wide
df_scaled[, c(2:ncol(df_scaled))] <- scale(df_scaled[, 2:ncol(df_scaled)])

#run clustering
df_matrix <- as.matrix(df_scaled[, -1])
rownames(df_matrix) <- df_scaled$WCZ
hclust_obj <- hclust(d = dist(x = df_matrix))
df_dendro <- as.dendrogram(hclust_obj)

# Create dendro
dendro_plot <- ggdendrogram(data = df_dendro, rotate = F)

#Analyze the clustering
cutree_hclust <- cutree(hclust_obj, k = 4)
k=4
df_list <- lapply(1 : k, function(x) df_matrix[which(cutree_hclust == x), ])

names(df_list[[1]][,1])
names(df_list[[2]][,1])
names(df_list[[3]][,1])
names(df_list[[4]][,1])

plot(hclust_obj)
rect.hclust(hclust_obj, k = 4, border = 2:6)
rplot = recordPlot()

colours = brewer.pal(4, "Dark2")

dend_data = dendro_data(df_dendro, type = "rectangle")

names(dend_data)

head(dend_data$segments)

head(dend_data$labels)

dend_data$group <- ifelse(dend_data$labels$label %in% names(df_list[[1]][,1]), "Group 1",
                  ifelse(dend_data$labels$label %in% names(df_list[[2]][,1]), "Group 2",
                         ifelse(dend_data$labels$label %in% names(df_list[[3]][,1]), "Group 3",
                                ifelse(dend_data$labels$label %in% names(df_list[[4]][,1]), "Group 4", NA))))

p <- ggplot(dend_data$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
  geom_text(data = dend_data$labels, aes(x, y, label = label, colour = dend_data$group),
            hjust = 1, angle = 90, size = 3)
print(p)

ggdendrogram(dend_data) + geom_text(data = dend_data$labels, aes(x, y, label = label, colour = dend_data$group),
                                    hjust = 1, angle = 90, size = 3)

clusters = df_scaled

clusters$Association <- ifelse(clusters$WCZ %in% names(df_list[[1]][,1]), "Group 1",
                          ifelse(clusters$WCZ %in% names(df_list[[2]][,1]), "Group 2",
                                 ifelse(clusters$WCZ %in% names(df_list[[3]][,1]), "Group 3",
                                        ifelse(clusters$WCZ %in% names(df_list[[4]][,1]), "Group 4", NA))))

abundance = clusters[3:ncol(clusters)-1]
association = clusters$Association

abundance[is.na(abundance)] <- 0


# Call the 'multipatt' function with the converted 'abundance' variable
indicator_r.g <- multipatt(abundance, association, func = "r.g", control = how(nperm = 9999))
summary(indicator_r.g)

#order
df_order <- order.dendrogram(df_dendro)

# Order the levels according to their position in the cluster
df$WCZ <- factor(x = df$WCZ,
                    levels = df_scaled$WCZ[df_order], 
                    ordered = TRUE)

df$Species <- ifelse(is.na(df$Species), "Undetermined",
                     str_replace(df$Species, "(.*?)(\\ssp\\.)?$", "*\\1*\\2"))

# Plot the data as a heatmap
plot <- ggplot() +
  geom_tile(data = df, aes(x = WCZ, y = Species, fill = count),color = "white") +
  scale_fill_gradientn(colours = c("white",RColorBrewer::brewer.pal(11, "RdBu")[2])) +
  labs(x = "WCZ", y = "Species", fill = "Relative\nAbundance") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_markdown(size = 7.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key.size = unit(1.2, "lines"),
        legend.background = element_rect(fill = "white", color = "black"))
plot

#put together
grid.newpage()
print(dendro_plot, 
      vp = viewport(x = 0.55, y = 0.87, width = 0.48, height = 0.25))
print(plot, 
      vp = viewport(x = 0.5, y = 0.4, width = 0.9, height = 0.75))

#To save this one, I recommend using Rstudio's plot pane, click zoom, go full screen and change the width untill the tree correctly fits the heatmap

#Same but for Years!

df = read.csv("Red_tide_and_WCZ_02162024.csv")

df <- df %>%
  group_by(Year, Species) %>%
  summarize(count = n())

df_relative <- df %>%
  group_by(Year) %>%
  mutate(Relative_Count = count / sum(count)) %>%
  ungroup()

df$count = df_relative$Relative_Count

#wide
df_wide <- df %>% spread(key = Species, value = count)

# perserve order
df$Year <- factor(df$Year, levels = unique(df$Year))
df$Species = factor(df$Species, levels = unique(df$Species))

# Calculate minimum and maximum count values
min_count <- min(df$count)
max_count <- max(df$count)

# Calculate the increment size for labels
increment <- (max_count - min_count) / 5

# Generate the breaks and labels
labels<- round(seq(min_count, max_count, by = increment),2)

#scale to remove zeros
df_scaled = df_wide
df_scaled[, c(2:ncol(df_scaled))] <- scale(df_scaled[, 2:ncol(df_scaled)])

#run clustering
df_matrix <- as.matrix(df_scaled[, -1])
rownames(df_matrix) <- df_scaled$Year
df_matrix[is.na(df_matrix)] <- 0
hclust_obj <- hclust(d = dist(x = df_matrix))
df_dendro <- as.dendrogram(hclust_obj)

# Create dendro
dendro_plot <- ggdendrogram(data = df_dendro, rotate = F)

#Analyze the clustering
cutree_hclust <- cutree(hclust_obj, k = 4)
df_list <- lapply(1 : k, function(x) df_matrix[which(cutree_hclust == x), ])

names(df_list[[1]][,1])
names(df_list[[2]][,1])
names(df_list[[3]][,1])
names(df_list[[4]][,1])

plot(hclust_obj)
rect.hclust(hclust_obj, k = 4, border = 2:6)
rplot = recordPlot()

colours = brewer.pal(4, "Dark2")

dend_data = dendro_data(df_dendro, type = "rectangle")

names(dend_data)

head(dend_data$segments)

head(dend_data$labels)

dend_data$group <- ifelse(dend_data$labels$label %in% names(df_list[[1]][,1]), "Group 1",
                          ifelse(dend_data$labels$label %in% names(df_list[[2]][,1]), "Group 2",
                                 ifelse(dend_data$labels$label %in% names(df_list[[3]][,1]), "Group 3",
                                        ifelse(dend_data$labels$label %in% names(df_list[[4]][,1]), "Group 4", NA))))

p <- ggplot(dend_data$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
  geom_text(data = dend_data$labels, aes(x, y, label = label, colour = dend_data$group),
            hjust = 1, angle = 90, size = 3)
print(p)

x = dend_data$group

dendroyear = ggdendrogram(dend_data, labels = F) + geom_text(data = dend_data$labels, aes(x, y, label = label, colour = dend_data$group),
                                    hjust = 1, size = 3) + coord_flip() + theme(legend.title = element_blank())
dendroyear

clusters = df_scaled

clusters$Association <- ifelse(clusters$Year %in% names(df_list[[1]][,1]), "Group 1",
                               ifelse(clusters$Year %in% names(df_list[[2]][,1]), "Group 2",
                                      ifelse(clusters$Year %in% names(df_list[[3]][,1]), "Group 3",
                                             ifelse(clusters$Year %in% names(df_list[[4]][,1]), "Group 4", NA))))

abundance = clusters[3:ncol(clusters)-1]
association = clusters$Association

abundance[is.na(abundance)] <- 0


# Call the 'multipatt' function with the converted 'abundance' variable
indicator_r.g <- multipatt(abundance, association, func = "r.g", control = how(nperm = 9999))
dendrosumyear = summary(indicator_r.g)


######### Months
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

df <- df %>%
  group_by(Month, Species) %>%
  summarize(count = n())

df_relative <- df %>%
  group_by(Month) %>%
  mutate(Relative_Count = count / sum(count)) %>%
  ungroup()

df$count = df_relative$Relative_Count

#wide
df_wide <- df %>% spread(key = Species, value = count)

# perserve order
df$Species = factor(df$Species, levels = unique(df$Species))

# Calculate minimum and maximum count values
min_count <- min(df$count)
max_count <- max(df$count)

# Calculate the increment size for labels
increment <- (max_count - min_count) / 5

# Generate the breaks and labels
labels<- round(seq(min_count, max_count, by = increment),2)

#scale to remove zeros
df_scaled = df_wide
df_scaled[, c(2:ncol(df_scaled))] <- scale(df_scaled[, 2:ncol(df_scaled)])

#run clustering
df_matrix <- as.matrix(df_scaled[, -1])
rownames(df_matrix) <- df_scaled$Month
df_matrix[is.na(df_matrix)] <- 0
hclust_obj <- hclust(d = dist(x = df_matrix))
df_dendro <- as.dendrogram(hclust_obj)

# Create dendro
dendro_plot <- ggdendrogram(data = df_dendro, rotate = F)

#Analyze the clustering
cutree_hclust <- cutree(hclust_obj, k = 2)
df_list <- lapply(1 : k, function(x) df_matrix[which(cutree_hclust == x), ])

names(df_list[[1]][,1])
names(df_list[[2]][,1])

plot(hclust_obj)
rect.hclust(hclust_obj, k = 2, border = 2:6)
rplot = recordPlot()

colours = brewer.pal(4, "Dark2")

dend_data2 = dendro_data(df_dendro, type = "rectangle")

names(dend_data2)

head(dend_data2$segments)

head(dend_data2$labels)

dend_data2$group <- ifelse(dend_data2$labels$label %in% names(df_list[[1]][,1]), "Group 1",
                          ifelse(dend_data2$labels$label %in% names(df_list[[2]][,1]), "Group 2", NA))

p <- ggplot(dend_data2$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
  geom_text(data = dend_data2$labels, aes(x, y, label = label, colour = dend_data2$group),
            hjust = 1, angle = 90, size = 3)
print(p)

dendromonth =ggdendrogram(dend_data2, labels = F) + geom_text(data = dend_data2$labels, aes(x, y, label = label, colour = dend_data2$group),
                                                hjust = 1, size = 3) + coord_flip() + theme(legend.title = element_blank())

dendromonth

clusters = df_scaled

clusters$Association <- ifelse(clusters$Month %in% names(df_list[[1]][,1]), "Group 1",
                               ifelse(clusters$Month %in% names(df_list[[2]][,1]), "Group 2",NA))

abundance = clusters[3:ncol(clusters)-1]
association = clusters$Association

abundance[is.na(abundance)] <- 0


# Call the 'multipatt' function with the converted 'abundance' variable
indicator_r.g <- multipatt(abundance, association, func = "r.g", control = how(nperm = 9999))
dendrosummonth = summary(indicator_r.g)


library(gridExtra)
grid.arrange(dendroyear, dendromonth, ncol = 2)


###########################################################


#NMDS By WCZ
# Remove the first column to only keep the numeric data
df_numeric <- df_wide[, -1]                 

df_numeric <- df_wide[, sapply(df_wide, is.numeric)]

#Change NA values into 0
df_numeric[is.na(df_numeric)] = 0

NMDS3 <- metaMDS(df_numeric, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
plot(NMDS3)
plot(NMDS3, display = "sites", type = "n")
points(NMDS3, display = "sites", col = "red", cex = 1.25)
text(NMDS3, display ="species")

#Make sure the sample names are presented instead of the indices
rownames(NMDS3$points) = paste(unique(df$WCZ))

##GGPLOT
data.scores <- as.data.frame(scores(NMDS3, "sites")) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
head(data.scores)  #look at the data

species.scores <- as.data.frame(scores(NMDS3, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)

#ef.scores = as.data.frame(scores(ef, "vectors"))
#ef.scores$factors = rownames(ef.scores)
#head(ef.scores)

#geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2),size=3) + # add the point markers
  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw()


############
#NMDS by month and WCZ
df = read.csv("Red_tide_and_WCZ_02162024.csv")

df$WCZ = gsub("Tolo Harbour & Channel", "Tolo Harbour", df$WCZ) #name too long

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

df <- df %>%
  group_by(WCZ, Month, Species) %>%
  summarize(count = n())

df_relative <- df %>%
  group_by(WCZ) %>%
  mutate(Relative_Count = count / sum(count)) %>%
  ungroup()

df$count = df_relative$Relative_Count

#wide
df_wide <- df %>% spread(key = Species, value = count)

# Remove the first column to only keep the numeric data
df_numeric <- df_wide[, -1][-1]                

df_numeric <- df_wide[, sapply(df_wide, is.numeric)]

#Change NA values into 0
df_numeric[is.na(df_numeric)] = 0

NMDS3 <- metaMDS(df_numeric, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
plot(NMDS3)
plot(NMDS3, display = "sites", type = "n")
points(NMDS3, display = "sites", col = "red", cex = 1.25)
text(NMDS3, display ="species")

#Create a merged name
Mergednames = paste(df_wide$WCZ, df_wide$Month)

#Make sure the sample names are presented instead of the indices
rownames(NMDS3$points) = Mergednames

##GGPLOT
data.scores <- as.data.frame(scores(NMDS3, "sites")) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$Month <-  str_sub(data.scores$site, start = -3) #Add months back in
data.scores$WCZ = substr(data.scores$site, 1, nchar(data.scores$site)-4)
head(data.scores)  #look at the data

species.scores <- as.data.frame(scores(NMDS3, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)

#ef.scores = as.data.frame(scores(ef, "vectors"))
#ef.scores$factors = rownames(ef.scores)
#head(ef.scores)

#We remove Junk Bay August because it's a big outlier
storevar = which(data.scores$site == "Junk Bay Aug")
data.scores = data.scores[-storevar,]
data.scores$Season <- ifelse(data.scores$Month %in% c("May", "Jun", "Jul", "Aug", "Sep"), "Wet Season", "Dry Season")

#geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2, shape = Month, colour = WCZ),size=2) + # add the point markers
  scale_color_manual(values = brewer.pal(10, "Set3")) +
  scale_shape_manual(values = c(1:12)) +
  coord_equal() +
  theme_bw()

## add polygon for seasons
#Prepare polygon
grp.a <- subset(data.scores, Season == "Dry Season")
grp.b <- subset(data.scores, Season == "Wet Season")

grp.a <- data.scores[data.scores$Season == "Dry Season", ][chull(data.scores[data.scores$Season == 
                                                                        "Dry Season", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.b <- data.scores[data.scores$Season == "Wet Season", ][chull(data.scores[data.scores$Season == 
                                                                        "Wet Season", c("NMDS1", "NMDS2")]), ]  # hull values for grp B

hull.data <- rbind(grp.a, grp.b)  # combine grp.a and grp.b
hull.data

color_palette <- brewer.pal(2, "Dark2")  # Adjust the number of colors as needed

ggplot() +
  geom_polygon(data = hull.data, aes(x = NMDS1, y = NMDS2, fill = Season, group = Season), colour = "black", size = 1, alpha = 0.1) +
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2, shape = Month, colour = WCZ),size=2) + # add the point markers
  scale_fill_manual(values = c("white", "black")) +
  scale_colour_manual(values = brewer.pal(10, "Set3")) +
  scale_shape_manual(values = c(1:12)) +
  labs(x = "NMDS1", y = "NMDS2") +
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

##Venn Diagrams
library("ggvenn")
library(UpSetR)



df = read.csv("Red_tide_and_WCZ_02162024.csv")

df$WCZ = gsub("Tolo Harbour & Channel", "Tolo Harbour", df$WCZ) #name too long

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

Wet = subset(df, df$Season == "Wet Season")
Wet = Wet$Species
Dry = subset(df, df$Season == "Dry Season")
Dry = Dry$Species

x = list(
  `Wet Season` = Wet,
  `Dry Season` = Dry
)

##ggven
#Default plot
ggvenn(x)

# Change category names
# Change the fill color
ggvenn(
  x, 
  fill_color = c("#0073C2FF", "#EFC000FF"),
  stroke_size = 0.5, set_name_size = 4,
  auto_scale = T
)

#Or upset plot
#using UpsetR
UpsetSeason = UpSetR::upset(fromList(x), order.by = "freq",
              mainbar.y.label = "Number of Shared Red Tide Species", 
              sets.x.label = "Red Tide Species per Season",
              empty.intersections = "on")

UpsetSeason

#Find Unique species to seasons
Unique_Dry <- setdiff(unique(Dry), unique(Wet))
Unique_Wet <- setdiff(unique(Wet), unique(Dry))

#Most occuring species unique to dry season
Sorted_Dry <- subset(df, Species %in% Unique_Dry) %>%
  count(Species) %>%
  arrange(desc(n))
Sorted_Dry

#Most occuring species unique to wet season
Sorted_Wet <- subset(df, Species %in% Unique_Wet) %>%
  count(Species) %>%
  arrange(desc(n))
Sorted_Wet

###Upset plots for WCZ
species_by_wcz <- split(df$Species, df$WCZ)


UpsetWCZ = UpSetR::upset(fromList(species_by_wcz), order.by = "freq",
              nsets=10,
              nintersects = 44,
              mainbar.y.label = "Number of Shared Red Tide Species", 
              sets.x.label = "Red Tide Species per WCZ",
              empty.intersections = "off")

UpsetWCZ

# Find WCZ containing "Noctiluca scintillans"
wcz_with_species <- names(species_by_wcz)[sapply(species_by_wcz, function(x) "Noctiluca scintillans" %in% x)]

# Create an empty list to store the unique species for each WCZ
unique_species_list <- list()

# Find the unique species for each WCZ
for (wcz in names(species_by_wcz)) {
  other_species <- unlist(species_by_wcz[setdiff(names(species_by_wcz), wcz)])
  unique_species <- setdiff(species_by_wcz[[wcz]], other_species)
  unique_species_list[[wcz]] <- unique_species
}

# Convert the list to a dataframe
df_unique_species <- data.frame(
  WCZ = rep(names(unique_species_list), lengths(unique_species_list)),
  Unique_Species = unlist(unique_species_list)
)

###Pour it all together
#Arranging them in one plot gives a lot of problems, as these are not grob objects. Best to just save the images and combine them in inskcape
# Create an empty list to store the unique species for each WCZ
unique_species_list <- list()

# Find the unique species for each WCZ and count their occurrences
for (wcz in names(species_by_wcz)) {
  other_species <- unlist(species_by_wcz[setdiff(names(species_by_wcz), wcz)])
  unique_species <- setdiff(species_by_wcz[[wcz]], other_species)
  counts <- table(species_by_wcz[[wcz]])
  unique_species_with_counts <- data.frame(
    WCZ = rep(wcz, length(unique_species)),
    Species = unique_species,
    Count = ifelse(unique_species %in% names(counts), counts[unique_species], 0),
    stringsAsFactors = FALSE
  )
  unique_species_list[[wcz]] <- unique_species_with_counts
}

# Combine the list of data frames into a single data frame
df_unique_species <- dplyr::bind_rows(unique_species_list)

# Reorder the columns if needed
df_unique_species <- df_unique_species[, c("WCZ", "Species", "Count")]

# View the resulting data frame
df_unique_species

write.csv(Sorted_Dry, file = "Unique_Dry.csv")
write.csv(Sorted_Wet, file = "Unique_Wet.csv")
write.csv(df_unique_species, file = "Unique_wcz.csv")
