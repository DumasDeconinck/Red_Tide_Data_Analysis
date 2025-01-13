library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggtext)
library(stringr)


#Use the data from Map.R to have WCZ

df = read.csv(file = "Red_tide_and_WCZ_02162024.csv")

df$WCZ = gsub("Tolo Harbour & Channel", "Tolo Harbour", df$WCZ) #name too long

# Create a new column "area" based on the mapping
df$area <- ifelse(df$WCZ %in% c("Tolo Harbour", "Port Shelter", "Mirs Bay"), "Eastern",
                  ifelse(df$WCZ %in% c("Southern"), "Southern",
                         ifelse(df$WCZ %in% c("Junk Bay", "Western Buffer", "Eastern Buffer", "Victoria Harbour"), "Central",
                                ifelse(df$WCZ %in% c("Deep Bay", "North Western"), "Western", NA))))

df$WCZ = factor(df$WCZ, levels = c("Deep Bay", "North Western","Junk Bay", "Western Buffer", "Eastern Buffer", "Victoria Harbour","Southern" ,"Tolo Harbour", "Port Shelter", "Mirs Bay"))

WCZPlot = ggplot(data = df) +
  geom_bar(aes(x = WCZ, fill = area), stat = "count") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 15),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 15),
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        legend.position = c(0.9,0.9),
        axis.title.x = element_blank()) +
  labs(x = "Water Control Zone", y = "Sightings")

WCZPlot

observed <- table(df$WCZ)

chi_result <- chisq.test(observed)

observed <- table(df$area)

chi_result <- chisq.test(observed)

#Shorten names for graphical display
df$WCZ = gsub("Deep Bay", "DB", df$WCZ)
df$WCZ = gsub("Eastern Buffer", "EB", df$WCZ)
df$WCZ = gsub("Junk Bay", "JB", df$WCZ)
df$WCZ = gsub("Mirs Bay", "MB", df$WCZ)
df$WCZ = gsub("North Western", "NW", df$WCZ)
df$WCZ = gsub("Port Shelter", "PS", df$WCZ)
df$WCZ = gsub("Southern", "S", df$WCZ)
df$WCZ = gsub("Tolo Harbour", "TH", df$WCZ)
df$WCZ = gsub("Victoria Harbour", "VH", df$WCZ)
df$WCZ = gsub("Western Buffer", "WB", df$WCZ)
#Composition by WCZ
#by group
df_Group <- df %>%
  group_by(WCZ, Group) %>%
  summarize(count = n())

df_Group$WCZ = factor(df_Group$WCZ, levels = c("DB", "NW","JB", "WB", "EB", "VH","S" ,"TH", "PS", "MB"))
df_Group$area <- ifelse(df_Group$WCZ %in% c("TH", "PS", "MB"), "Eastern",
                  ifelse(df_Group$WCZ %in% c("S"), "Southern",
                         ifelse(df_Group$WCZ %in% c("JB", "WB", "EB", "VH"), "Central",
                                ifelse(df_Group$WCZ %in% c("DB", "NW"), "Western", NA))))
df_Group$Group <- factor(df_Group$Group, levels = sort(unique(df_Group$Group)))

#Plot
pa = ggplot(df_Group, aes(fill = Group, y = count, x = WCZ)) +
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


pb = ggplot(df_Group, aes(fill = Group, y = count, x = WCZ)) +
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
  group_by(WCZ, Kingdom) %>%
  summarize(count = n())

df_Kingdom$WCZ = factor(df_Kingdom$WCZ, levels = c("DB", "NW","JB", "WB", "EB", "VH","S" ,"TH", "PS", "MB"))
df_Kingdom$area <- ifelse(df_Kingdom$WCZ %in% c("TH", "PS", "MB"), "Eastern",
                        ifelse(df_Kingdom$WCZ %in% c("S"), "Southern",
                               ifelse(df_Kingdom$WCZ %in% c("JB", "WB", "EB", "VH"), "Central",
                                      ifelse(df_Kingdom$WCZ %in% c("DB", "NW"), "Western", NA))))
df_Kingdom$Kingdom <- factor(df_Kingdom$Kingdom, levels = sort(unique(df_Kingdom$Kingdom)))

#Plot
pc = ggplot(df_Kingdom, aes(fill = Kingdom, y = count, x = WCZ)) +
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

pd = ggplot(df_Kingdom, aes(fill = Kingdom, y = count, x = WCZ)) +
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
  group_by(WCZ, Phylum) %>%
  summarize(count = n())

df_Phylum$WCZ = factor(df_Phylum$WCZ, levels = c("DB", "NW","JB", "WB", "EB", "VH","S" ,"TH", "PS", "MB"))
df_Phylum$area <- ifelse(df_Phylum$WCZ %in% c("TH", "PS", "MB"), "Eastern",
                          ifelse(df_Phylum$WCZ %in% c("S"), "Southern",
                                 ifelse(df_Phylum$WCZ %in% c("JB", "WB", "EB", "VH"), "Central",
                                        ifelse(df_Phylum$WCZ %in% c("DB", "NW"), "Western", NA))))
df_Phylum$Phylum <- factor(df_Phylum$Phylum, levels = sort(unique(df_Phylum$Phylum)))

#Plot
pfor = ggplot(df_Phylum, aes(fill = Phylum, y = count, x = WCZ)) +
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

pfora = ggplot(df_Phylum, aes(fill = Phylum, y = count, x = WCZ)) +
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
  group_by(WCZ, Order) %>%
  summarize(count = n())

df_Order$WCZ = factor(df_Order$WCZ, levels = c("DB", "NW","JB", "WB", "EB", "VH","S" ,"TH", "PS", "MB"))
df_Order$area <- ifelse(df_Order$WCZ %in% c("TH", "PS", "MB"), "Eastern",
                         ifelse(df_Order$WCZ %in% c("S"), "Southern",
                                ifelse(df_Order$WCZ %in% c("JB", "WB", "EB", "VH"), "Central",
                                       ifelse(df_Order$WCZ %in% c("DB", "NW"), "Western", NA))))
df_Order$Order <- factor(df_Order$Order, levels = sort(unique(df_Order$Order)))

#Plot
pe = ggplot(df_Order, aes(fill = Order, y = count, x = WCZ)) +
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

pf = ggplot(df_Order, aes(fill = Order, y = count, x = WCZ)) +
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
  group_by(WCZ, Phamily) %>%
  summarize(count = n())

df_family$WCZ = factor(df_family$WCZ, levels = c("DB", "NW","JB", "WB", "EB", "VH","S" ,"TH", "PS", "MB"))
df_family$area <- ifelse(df_family$WCZ %in% c("TH", "PS", "MB"), "Eastern",
                        ifelse(df_family$WCZ %in% c("S"), "Southern",
                               ifelse(df_family$WCZ %in% c("JB", "WB", "EB", "VH"), "Central",
                                      ifelse(df_family$WCZ %in% c("DB", "NW"), "Western", NA))))
df_family$Phamily <- factor(df_family$Phamily, levels = sort(unique(df_family$Phamily)))

#Plot
pg = ggplot(df_family, aes(fill = Phamily, y = count, x = WCZ)) +
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

ph = ggplot(df_family, aes(fill = Phamily, y = count, x = WCZ)) +
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
  group_by(WCZ, Genus) %>%
  summarize(count = n())

df_Genus$WCZ = factor(df_Genus$WCZ, levels = c("DB", "NW","JB", "WB", "EB", "VH","S" ,"TH", "PS", "MB"))
df_Genus$area <- ifelse(df_Genus$WCZ %in% c("TH", "PS", "MB"), "Eastern",
                         ifelse(df_Genus$WCZ %in% c("S"), "Southern",
                                ifelse(df_Genus$WCZ %in% c("JB", "WB", "EB", "VH"), "Central",
                                       ifelse(df_Genus$WCZ %in% c("DB", "NW"), "Western", NA))))
df_Genus$Genus <- factor(df_Genus$Genus, levels = sort(unique(df_Genus$Genus)))


#Plot
pi = ggplot(df_Genus, aes(fill = Genus, y = count, x = WCZ)) +
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

pj = ggplot(df_Genus, aes(fill = Genus, y = count, x = WCZ)) +
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
  group_by(WCZ, Species) %>%
  summarize(count = n())

df_Species$WCZ = factor(df_Species$WCZ, levels = c("DB", "NW","JB", "WB", "EB", "VH","S" ,"TH", "PS", "MB"))
df_Species$area <- ifelse(df_Species$WCZ %in% c("TH", "PS", "MB"), "Eastern",
                        ifelse(df_Species$WCZ %in% c("S"), "Southern",
                               ifelse(df_Species$WCZ %in% c("JB", "WB", "EB", "VH"), "Central",
                                      ifelse(df_Species$WCZ %in% c("DB", "NW"), "Western", NA))))
df_Species$Species <- factor(df_Species$Species, levels = sort(unique(df_Species$Species)))

#Plot
pk = ggplot(df_Species, aes(fill = Species, y = count, x = WCZ)) +
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

pl = ggplot(df_Species, aes(fill = Species, y = count, x = WCZ)) +
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
png(file = "Output/Figure_05.png", width = 1920, height = 1080)
grid.arrange(WCZPlot + theme(axis.title.x = element_blank()) +
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
df = read.csv(file = "Red_tide_and_WCZ_02162024.csv")
df = subset(df, df$Species == "Prorocentrum cordatum")

df_WCZ <- df %>%
  group_by(WCZ) %>%
  summarize(count = n())

df_WCZ

ggplot(data = df_WCZ, aes(x = WCZ, y = count)) +
  geom_point() +
  theme_minimal()

###Heterosigma akashiwo
df = read.csv(file = "Red_tide_and_WCZ_02162024.csv")
df = subset(df, df$Species == "Heterosigma akashiwo")

df_WCZ <- df %>%
  group_by(WCZ) %>%
  summarize(count = n())

df_WCZ

ggplot(data = df_WCZ, aes(x = WCZ, y = count)) +
  geom_point() +
  theme_minimal()

###Karenia mikimotoi
df = read.csv(file = "Red_tide_and_WCZ_02162024.csv")
df = subset(df, df$Species == "Karenia mikimotoi")

df_WCZ <- df %>%
  group_by(WCZ) %>%
  summarize(count = n())

df_WCZ

ggplot(data = df_WCZ, aes(x = WCZ, y = count)) +
  geom_point() +
  theme_minimal()