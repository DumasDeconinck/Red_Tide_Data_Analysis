library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggtext)
library(stringr)


df = read.csv("WoRMSCorrected_redtides.csv")

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

# Plot bar graph with colored bars
seasonPlot = ggplot(data = df) +
  geom_bar(aes(x = Month, fill = Season), stat = "count") +
  scale_fill_manual(values = c("Wet Season" = brewer.pal(3,"Dark2")[1], "Dry Season" = brewer.pal(3,"Dark2")[2])) +
  labs(fill = "Season") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 15),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 15),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        legend.position = c(0.9,0.9),
        axis.title.x = element_blank()) +
  labs(x = "Month", y = "Sightings")
seasonPlot

observed <- table(df$Season)

chi_result <- chisq.test(observed)

###Plot3
#Composition by Month
#by group
df_Group <- df %>%
  group_by(Month, Group) %>%
  summarize(count = n())

df_Group$Group <- factor(df_Group$Group, levels = sort(unique(df_Group$Group)))

#Plot
pa = ggplot(df_Group, aes(fill = Group, y = count, x = Month)) +
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
pa

pb = ggplot(df_Group, aes(fill = Group, y = count, x = Month)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1, margin = margin(t = -15)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
pb

#By kingdom
#df_kingdom <- df[!is.na(df$Kingdom), ] #removing NA values
#unique(df_kingdom$Kingdom)

df_Kingdom <- df %>%
  group_by(Month, Kingdom) %>%
  summarize(count = n())

df_Kingdom$Kingdom <- factor(df_Kingdom$Kingdom, levels = sort(unique(df_Kingdom$Kingdom)))

#Plot
pc = ggplot(df_Kingdom, aes(fill = Kingdom, y = count, x = Month)) +
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
pc

pd = ggplot(df_Kingdom, aes(fill = Kingdom, y = count, x = Month)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1, margin = margin(t = -15)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
pd

#By Phylum
phyla_count <- df %>%
  count(Phylum) %>%
  arrange(desc(n))

# Get the top 8 orders
top_8_phyla <- head(subset(phyla_count, !is.na(phyla_count$Phylum)), 7)

df$Phylum <- ifelse(df$Phylum %in% top_8_phyla$Phylum, df$Phylum, "Other")

df_Phylum <- df %>%
  group_by(Month, Phylum) %>%
  summarize(count = n())

df_Phylum$Phylum <- factor(df_Phylum$Phylum, levels = sort(unique(df_Phylum$Phylum)))

#Plot
pfor = ggplot(df_Phylum, aes(fill = Phylum, y = count, x = Month)) +
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
pfor

pfora = ggplot(df_Phylum, aes(fill = Phylum, y = count, x = Month)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1, margin = margin(t = -15)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
pfora

#by order (top 8)
unique(df$Order)
#calculate order
order_count <- df %>%
  count(Order) %>%
  arrange(desc(n))

# Get the top 8 orders
top_8_order <- head(subset(order_count, !is.na(order_count$Order)), 7)

df$Order <- ifelse(df$Order %in% top_8_order$Order, df$Order, "Other")

df_Order <- df %>%
  group_by(Month, Order) %>%
  summarize(count = n())

df_Order$Order <- factor(df_Order$Order, levels = sort(unique(df_Order$Order)))

#Plot
pe = ggplot(df_Order, aes(fill = Order, y = count, x = Month)) +
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
pe

pf = ggplot(df_Order, aes(fill = Order, y = count, x = Month)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1, margin = margin(t = -15)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
pf

#by Phamily (top 8)
unique(df$Phamily)
# Calculate the count of each species
family_count <- df %>%
  count(Phamily) %>%
  arrange(desc(n))

# Get the top 8 orders
top_8_family <- head(subset(family_count, !is.na(family_count$Phamily)), 7)

df$Phamily <- ifelse(df$Phamily %in% top_8_family$Phamily, df$Phamily, "Other")

df_family <- df %>%
  group_by(Month, Phamily) %>%
  summarize(count = n())

df_family$Phamily <- factor(df_family$Phamily, levels = sort(unique(df_family$Phamily)))

#Plot
pg = ggplot(df_family, aes(fill = Phamily, y = count, x = Month)) +
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
pg

df_family$Phamily = ifelse(df_family$Phamily == "Other", "Other",
                           str_replace(df_family$Phamily, "([A-Za-z]+)", "*\\1*"))
df_family$Phamily <- factor(df_family$Phamily, levels = sort(unique(df_family$Phamily)))

ph = ggplot(df_family, aes(fill = Phamily, y = count, x = Month)) +
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
ph

#By genus (top 8)
unique(df$Genus)
# Calculate the count of each species
genus_count <- df %>%
  count(Genus) %>%
  arrange(desc(n))

# Get the top 8 orders
top_8_Genus <- head(subset(genus_count, !is.na(genus_count$Genus)), 7)

df$Genus <- ifelse(df$Genus %in% top_8_Genus$Genus, df$Genus, "Other")

df_Genus <- df %>%
  group_by(Month, Genus) %>%
  summarize(count = n())

df_Genus$Genus <- factor(df_Genus$Genus, levels = sort(unique(df_Genus$Genus)))

#Plot
pi = ggplot(df_Genus, aes(fill = Genus, y = count, x = Month)) +
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
pi

df_Genus$Genus = ifelse(df_Genus$Genus == "Other", "Other",
                        str_replace(df_Genus$Genus, "([A-Za-z]+)", "*\\1*"))
df_Genus$Genus <- factor(df_Genus$Genus, levels = sort(unique(df_Genus$Genus)))

pj = ggplot(df_Genus, aes(fill = Genus, y = count, x = Month)) +
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
pj

#by species (top 8)
unique(df$Species)
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

###Pour it all together
png(file = "Output/Figure_03.png", width = 1920, height = 1080)
grid.arrange(seasonPlot +
               ggtitle("A"),
             pb + theme(axis.title.x = element_blank()) +
               ggtitle("B"),
             pd + theme(axis.title.x = element_blank()) +
               ggtitle("C"),
             pfora + theme(axis.title.x = element_blank()) +
               ggtitle("D"),
             pf + theme(axis.title.x = element_blank()) +
               ggtitle("E"),
             ph + theme(axis.title.x = element_blank()) +
               ggtitle("F"),
             pj + theme(axis.title.x = element_blank()) +
               ggtitle("G"),
             pl + theme(axis.title.x = element_blank()) +
               ggtitle("H"),
             ncol = 2)
dev.off()


###Prorocentrum cordatum
df = read.csv("WoRMSCorrected_redtides.csv")

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

df = subset(df, df$Species == "Prorocentrum cordatum")

df_Month <- df %>%
  group_by(Month) %>%
  summarize(count = n())

df_Month

ggplot(data = df_Month, aes(x = Month, y = count)) +
  geom_point() +
  theme_minimal()

###Heterosigma akashiwo
df = read.csv("WoRMSCorrected_redtides.csv")

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

df = subset(df, df$Species == "Heterosigma akashiwo")

df_Month <- df %>%
  group_by(Month) %>%
  summarize(count = n())

df_Month

ggplot(data = df_Month, aes(x = Month, y = count)) +
  geom_point() +
  theme_minimal()

###Karenia mikimotoi
df = read.csv("WoRMSCorrected_redtides.csv")

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

df = subset(df, df$Species == "Karenia mikimotoi")

df_Month <- df %>%
  group_by(Month) %>%
  summarize(count = n())

df_Month

ggplot(data = df_Month, aes(x = Month, y = count)) +
  geom_point() +
  theme_minimal()
