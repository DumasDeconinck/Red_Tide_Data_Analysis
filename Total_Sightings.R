unloadNamespace("worms")
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(gridExtra)

#read the data
df = read.csv("WoRMSCorrected_redtides.csv")
df$Species[is.na(df$Species)] = "Unknown"

#avoid any oddities by factoring them
df$Species <- factor(df$Species, levels = unique(df$Species)) 

###Plot1
# Calculate the count of each species
species_count <- df %>%
  dplyr::count(Species) %>%
  dplyr::arrange(desc(n))


# Get the top 5 species
top_5_species1 <- head(species_count$Species, 5)

# Create a new column for labels
df_with_labels <- df %>%
  mutate(label = ifelse(Species %in% top_5_species1, as.character(table(df$Species)[Species]), ""))

# Create the plot
p1 = ggplot(data = df_with_labels) +
  geom_bar(aes(x = Species, fill = Species %in% top_5_species1)) +
  geom_text(data = df_with_labels %>% filter(Species %in% top_5_species1),
            aes(x = Species, y = ..count.., label = label),
            vjust = -0, color = "black", size = 5, stat = "count") +
  scale_fill_manual(values = c("black", brewer.pal(4,"Dark2")[4]), guide = FALSE) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = ifelse(unique(df_with_labels$Species) %in% top_5_species1 & unique(df_with_labels$Species) != "Unknown", "italic", "plain"), 
                                   size = 15, angle = 45, hjust = 1, color = ifelse(unique(df_with_labels$Species) %in% top_5_species1, brewer.pal(4,"Dark2")[4], "black")),
        plot.margin = margin(0,0,0, 3, "cm"),
        axis.title=element_blank()) +
  labs(x = "Species", y = "Red tide reportings") +
  scale_y_sqrt(breaks = c(100,200,300,400,500,600))
p1

#+ scale_y_break(c(table(df$Species== head(species_count$Species, 5)[2])[2] + 10, table(df$Species== head(species_count$Species, 1))[2]) - 44, scale = 0.1)

# From 1975 to 2000 (pre HATS)
df10 = subset(df, Year %in% seq(from = 1975, to = 2000))
df10$Species <- factor(df10$Species, levels = unique(df10$Species)) 

# Calculate the count of each species
species_count <- df10 %>%
  dplyr::count(Species) %>%
  dplyr::arrange(desc(n))

# Get the top 5 species
top_5_species2 <- head(species_count$Species, 5)

# Create a new column for labels
df_with_labels <- df10 %>%
  mutate(label = ifelse(Species %in% top_5_species2, as.character(table(df10$Species)[Species]), ""))

# Create the plot
p2 = ggplot(data = df_with_labels) +
  geom_bar(aes(x = Species, fill = Species %in% top_5_species2)) +
  geom_text(data = df_with_labels %>% filter(Species %in% top_5_species2),
            aes(x = Species, y = ..count.., label = label),
            vjust = 0, color = "black", size = 5, stat = "count") +
  scale_fill_manual(values = c("black", brewer.pal(4,"Dark2")[4]), guide = FALSE) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = ifelse(unique(df_with_labels$Species) %in% top_5_species2 & unique(df_with_labels$Species) != "Unknown", "italic", "plain"),
                                   size = 15, angle = 45, hjust = 1, color = ifelse(unique(df_with_labels$Species) %in% top_5_species2, brewer.pal(4,"Dark2")[4], "black")),
        plot.margin = margin(0,0,0, 3, "cm"),
        axis.title=element_blank()) +
  labs(x = "Species", y = "Red tide reportings") +
  scale_y_sqrt(breaks = c(100,200,300,400,500,600))
p2

# From 2001 to 2014 (post HATS 1)
df10 = subset(df, Year %in% seq(from = 2001, to = 2014))
df10$Species <- factor(df10$Species, levels = unique(df10$Species)) 

# Calculate the count of each species
species_count <- df10 %>%
  dplyr::count(Species) %>%
  dplyr::arrange(desc(n))

# Get the top 5 species
top_5_species3 <- head(species_count$Species, 5)

# Create a new column for labels
df_with_labels <- df10 %>%
  mutate(label = ifelse(Species %in% top_5_species3, as.character(table(df10$Species)[Species]), ""))

# Create the plot
p3 = ggplot(data = df_with_labels) +
  geom_bar(aes(x = Species, fill = Species %in% top_5_species3)) +
  geom_text(data = df_with_labels %>% filter(Species %in% top_5_species3),
            aes(x = Species, y = ..count.., label = label),
            vjust = 0, color = "black", size = 5, stat = "count") +
  scale_fill_manual(values = c("black", brewer.pal(4,"Dark2")[4]), guide = FALSE) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = ifelse(unique(df_with_labels$Species) %in% top_5_species3 & unique(df_with_labels$Species) != "Unknown", "italic", "plain"), 
                                   size = 15, angle = 45, hjust = 1, color = ifelse(unique(df_with_labels$Species) %in% top_5_species3, brewer.pal(4,"Dark2")[4], "black")),
        plot.margin = margin(0,0,0, 3, "cm"),
        axis.title=element_blank()) +
  labs(x = "Species", y = "Red tide reportings") +
  scale_y_sqrt(breaks = c(100,200,300,400,500,600))
p3

# From 2015 to 2024 (post HATS 2A)
df10 = subset(df, Year %in% seq(from = 2015, to = 2024))
df10$Species <- factor(df10$Species, levels = unique(df10$Species)) 

# Calculate the count of each species
species_count <- df10 %>%
  dplyr::count(Species) %>%
  dplyr::arrange(desc(n))

# Get the top 5 species
top_5_species4 <- head(species_count$Species, 5)

# Create a new column for labels
df_with_labels <- df10 %>%
  mutate(label = ifelse(Species %in% top_5_species4, as.character(table(df10$Species)[Species]), ""))

# Create the plot
p4 = ggplot(data = df_with_labels) +
  geom_bar(aes(x = Species, fill = Species %in% top_5_species4)) +
  geom_text(data = df_with_labels %>% filter(Species %in% top_5_species4),
            aes(x = Species, y = ..count.., label = label),
            vjust = 0, color = "black", size = 5, stat = "count") +
  scale_fill_manual(values = c("black", brewer.pal(4,"Dark2")[4]), guide = FALSE) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = ifelse(unique(df_with_labels$Species) %in% top_5_species4 & unique(df_with_labels$Species) != "Unknown", "italic", "plain"), 
                                   size = 15, angle = 45, hjust = 1, color = ifelse(unique(df_with_labels$Species) %in% top_5_species4, brewer.pal(4,"Dark2")[4], "black")),
        plot.margin = margin(0,0,0, 3, "cm"),
        axis.title=element_blank()) +
  labs(x = "Species", y = "Red tide reportings") +
  scale_y_sqrt(breaks = c(100,200,300,400,500,600))
p4



arangedplot = 
  grid.arrange(p1 + ggtitle("A") +
                theme(title = element_text(size = 20)),
               p2 + ggtitle("B") +
                 theme(title = element_text(size = 20)),
               p3 + ggtitle("C") +
                 theme(title = element_text(size = 20)),
               p4 + ggtitle("D") +
                 theme(title = element_text(size = 20)),
               ncol = 1)



###Pour it all together
png(file = "Output/Figure_01.png", width = 1920, height = 2000)
grid.arrange(p1 + ggtitle("A") +
               theme(title = element_text(size = 20)),
             p2 + ggtitle("B") +
               theme(title = element_text(size = 20)),
             p3 + ggtitle("C") +
               theme(title = element_text(size = 20)),
             p4 + ggtitle("D") +
               theme(title = element_text(size = 20)),
             ncol = 1)

dev.off()


