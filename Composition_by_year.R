library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggtext)
library(stringr)

df = read.csv("WoRMSCorrected_redtides.csv")

#Total composition
df %>%
  group_by(Group) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(relative = count / sum(count)*100)

df %>%
  group_by(Phylum) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(relative = count / sum(count)*100)

###Plot
#Composition by year
#by group
df_Group <- df %>%
  group_by(Year, Group) %>%
  summarize(count = n())

df_Group$Group <- factor(df_Group$Group, levels = sort(unique(df_Group$Group)))

#Plot
pa = ggplot(df_Group, aes(fill = Group, y = count, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Group$Year), breaks = df_Group$Year) +
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

pb = ggplot(df_Group, aes(fill = Group, y = count, x = Year)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Group$Year), breaks = df_Group$Year) +
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
  group_by(Year, Kingdom) %>%
  summarize(count = n())

df_Kingdom$Kingdom <- factor(df_Kingdom$Kingdom, levels = sort(unique(df_Kingdom$Kingdom)))

#Plot
pc = ggplot(df_Kingdom, aes(fill = Kingdom, y = count, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Kingdom$Year), breaks = df_Kingdom$Year) +
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

pd = ggplot(df_Kingdom, aes(fill = Kingdom, y = count, x = Year)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Kingdom$Year), breaks = df_Kingdom$Year) +
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
  group_by(Year, Phylum) %>%
  summarize(count = n())

df_Phylum$Phylum <- factor(df_Phylum$Phylum, levels = sort(unique(df_Phylum$Phylum)))

#Plot
pfor = ggplot(df_Phylum, aes(fill = Phylum, y = count, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Phylum$Year), breaks = df_Phylum$Year) +
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

pfora = ggplot(df_Phylum, aes(fill = Phylum, y = count, x = Year)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Phylum$Year), breaks = df_Phylum$Year) +
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
# Calculate the count of each species
order_count <- df %>%
  count(Order) %>%
  arrange(desc(n))

# Get the top 8 orders
top_8_order <- head(subset(order_count, !is.na(order_count$Order)), 7)

df$Order <- ifelse(df$Order %in% top_8_order$Order, df$Order, "Other")

df_Order <- df %>%
  group_by(Year, Order) %>%
  summarize(count = n())

df_Order$Order <- factor(df_Order$Order, levels = sort(unique(df_Order$Order)))

#Plot
pe = ggplot(df_Order, aes(fill = Order, y = count, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Order$Year), breaks = df_Order$Year) +
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

pf = ggplot(df_Order, aes(fill = Order, y = count, x = Year)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Order$Year), breaks = df_Order$Year) +
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
  group_by(Year, Phamily) %>%
  summarize(count = n())

df_family$Phamily <- factor(df_family$Phamily, levels = sort(unique(df_family$Phamily)))

#Plot
pg = ggplot(df_family, aes(fill = Phamily, y = count, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_family$Year), breaks = df_family$Year) +
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
    
ph = ggplot(df_family, aes(fill = Phamily, y = count, x = Year)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_family$Year), breaks = df_family$Year) +
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
  group_by(Year, Genus) %>%
  summarize(count = n())

df_Genus$Genus <- factor(df_Genus$Genus, levels = sort(unique(df_Genus$Genus)))

#Plot
pi = ggplot(df_Genus, aes(fill = Genus, y = count, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Genus$Year), breaks = df_Genus$Year) +
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

pj = ggplot(df_Genus, aes(fill = Genus, y = count, x = Year)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(8, "Dark2")) +
  ylab("Sightings") +
  scale_x_continuous("Year", labels = as.character(df_Genus$Year), breaks = df_Genus$Year) +
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

###Sightings per year, regression
df_Year <- df %>%
  group_by(Year) %>%
  summarize(count = n())

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

#extra: El nino years
el_nino_years = c(1977,1978,1980,2005,2007,2015,2019,
                  1987,1995,2003,2010,
                  1988,1992,2024,
                  1983,1998,2016)
#el_nino_years = c(el_nino_years, el_nino_years-1)

la_nina_years = c(1984,1985,2001,2006,2009,2017,2018,2023,
                  1996,2012,2021,2022,
                  1976,1989,1999,2000,2008,2011)

# Step 4: Create a graph with points and an abline
regplot = ggplot(data = df_Year, aes(x = Year, y = count)) +
  geom_point() +
  scale_x_continuous("Year", labels = as.character(df_Species$Year), breaks = df_Species$Year) +
  geom_abline(slope = regression_model$coefficients[2], intercept = regression_model$coefficients[1], color = "red") +
  theme_minimal() +
  ylab("Sightings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_line(color = ifelse(df_Year$Year %in% el_nino_years, "blue", 
                                                         ifelse(df_Year$Year %in% la_nina_years, "red", "gray"))),
        panel.grid.minor = element_blank())
regplot

regplot = ggplot(data = df_Year, aes(x = Year, y = count)) +
  geom_point() +
  scale_x_continuous("Year", labels = as.character(df_Species$Year), breaks = df_Species$Year) +
  geom_abline(slope = regression_model$coefficients[2], intercept = regression_model$coefficients[1], color = "red") +
  theme_minimal() +
  ylab("Sightings") +
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 15))
regplot

summary(regression_model)

grid.arrange(regplot + theme(axis.title.x = element_blank()) +
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

###Pour it all together
png(file = "Output/Figure_02.png", width = 1920, height = 1080)
grid.arrange(regplot + theme(axis.title.x = element_blank()) +
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

##########
###Prorocentrum cordatum
df = read.csv("WoRMSCorrected_redtides.csv")
df = subset(df, df$Species == "Prorocentrum cordatum")

df_Year <- df %>%
  group_by(Year) %>%
  summarize(count = n())

df_Year$Year <- as.numeric(df_Year$Year)

df_Year <- df_Year %>%
  filter(Year != 2024) #Incomplete

df_Year

ggplot(data = df_Year, aes(x = Year, y = count)) +
  geom_point() +
  theme_minimal()

###Heterosigma Akashiwo
df = read.csv("WoRMSCorrected_redtides.csv")
df = subset(df, df$Species == "Heterosigma akashiwo")

df_Year <- df %>%
  group_by(Year) %>%
  summarize(count = n())

df_Year$Year <- as.numeric(df_Year$Year)

df_Year <- df_Year %>%
  filter(Year != 2024) #Incomplete

df_Year

ggplot(data = df_Year, aes(x = Year, y = count)) +
  geom_point() +
  theme_minimal()

###Karenia Mikimotoi
df = read.csv("WoRMSCorrected_redtides.csv")
df = subset(df, df$Species == "Karenia mikimotoi")

df_Year <- df %>%
  group_by(Year) %>%
  summarize(count = n())

df_Year$Year <- as.numeric(df_Year$Year)

df_Year

ggplot(data = df_Year, aes(x = Year, y = count)) +
  geom_point() +
  theme_minimal()
