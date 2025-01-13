library(ggplot2)
library(igraph)
library(ggraph)
library(dplyr)
library(tidyr)

df = read.csv("Red_tide_and_WCZ_02162024.csv")

#For Phylum
#create relative wide format
df <- df %>%
  group_by(WCZ, Phylum) %>%
  summarize(count = n())

df_relative <- df %>%
  group_by(WCZ) %>%
  mutate(Relative_Count = count / sum(count)) %>%
  ungroup()

df$count = df_relative$Relative_Count

#wide
df_wide <- df %>% spread(key = Phylum, value = count)

wide2 <- df_wide[,-1]
wide2[is.na(wide2)] = 0
m_obs = as.matrix(wide2)

# Calculate the co-occurrence matrix
co_occurrence <- t(m_obs) %*% m_obs

# Create a graph from the co-occurrence matrix
graph <- graph_from_adjacency_matrix(co_occurrence, mode = "undirected", weighted = TRUE)

# Set the vertex names as species labels
V(graph)$name <- colnames(co_occurrence)

# Determine the abundance of each species
abundance <- colSums(wide2)

# Calculate Pearson correlation coefficients
correlation <- cor(m_obs)

# Calculate p-values for correlations
p_values <- matrix(0, ncol = ncol(m_obs), nrow = ncol(m_obs))
for (i in 1:(ncol(m_obs) - 1)) {
  for (j in (i + 1):ncol(m_obs)) {
    p_values[i, j] <- p_values[j, i] <- cor.test(m_obs[, i], m_obs[, j])$p.value
  }
}

# Define the threshold for significance
cor_threshold <- 0.6
p_value_threshold <- 0.05

# Filter edges based on significance criteria
correlated_edges <- E(graph)[abs(correlation[as.integer(E(graph))]) > cor_threshold]
significant_edges <- E(graph)[p_values[as.integer(E(graph))] < p_value_threshold]

# Create a subgraph with only significant edges
subgraph <- delete_edges(graph, E(graph)[!E(graph) %in% correlated_edges])
subgraph <- delete_edges(subgraph, E(subgraph)[!E(subgraph) %in% significant_edges])

#subgraph = delete.vertices(subgraph , which(degree(subgraph)==0))#delete points with no connections
abundance = abundance[names(abundance) %in% V(subgraph)$name]

# Positive and negative correlations
neg_edges <- E(subgraph)[correlation[as.integer(E(subgraph))] < 0]
pos_edges <- E(subgraph)[correlation[as.integer(E(subgraph))] > 0]


# Assign different colors to each species
species_colors <- rainbow(length(V(subgraph)))

# Create a color vector for edges based on their sign (positive/negative)
edge_colors <- ifelse(E(subgraph) %in% neg_edges, "negative", "positive") #The colours can't be set as they are already in use by scale_colour manual actually, so there is o pink or green.
Correlations = edge_colors

# Plot the co-occurrence network with significant correlations 
#width = abs(E(subgraph)$weight), to edge_arc for width associated with edge
#alpha = abs(E(subgraph)$weight),

ggraph(subgraph, layout = 'circle') +
  geom_edge_arc(strength = 0.2, aes(color = Correlations)) +
  geom_node_point(aes(size = abundance, color = name), alpha = 0.5) +
  scale_size_continuous(range = c(2, 10)) +
  scale_color_manual(values = species_colors) +
  geom_node_text(aes(label = name), col = "darkblue", size = 3, repel = T, force = 0.004) +
  theme_void() +
  guides(color = "none", size = "none")


###
#For Genus
#create relative wide format
df = read.csv("Red_tide_and_WCZ_02162024.csv")

df <- df %>%
  group_by(WCZ, Genus) %>%
  summarize(count = n())

df_relative <- df %>%
  group_by(WCZ) %>%
  mutate(Relative_Count = count / sum(count)) %>%
  ungroup()

df$count = df_relative$Relative_Count

#wide
df_wide <- df %>% spread(key = Genus, value = count)

wide2 <- df_wide[,-1]
wide2[is.na(wide2)] = 0
m_obs = as.matrix(wide2)

# Calculate the co-occurrence matrix
co_occurrence <- t(m_obs) %*% m_obs

# Create a graph from the co-occurrence matrix
graph <- graph_from_adjacency_matrix(co_occurrence, mode = "undirected", weighted = TRUE)

# Set the vertex names as species labels
V(graph)$name <- colnames(co_occurrence)

# Determine the abundance of each species
abundance <- colSums(wide2)

# Calculate Pearson correlation coefficients
correlation <- cor(m_obs)

# Calculate p-values for correlations
p_values <- matrix(0, ncol = ncol(m_obs), nrow = ncol(m_obs))
for (i in 1:(ncol(m_obs) - 1)) {
  for (j in (i + 1):ncol(m_obs)) {
    p_values[i, j] <- p_values[j, i] <- cor.test(m_obs[, i], m_obs[, j])$p.value
  }
}

# Define the threshold for significance
cor_threshold <- 0.6
p_value_threshold <- 0.05

# Filter edges based on significance criteria
correlated_edges <- E(graph)[abs(correlation[as.integer(E(graph))]) > cor_threshold]
significant_edges <- E(graph)[p_values[as.integer(E(graph))] < p_value_threshold]

# Create a subgraph with only significant edges
subgraph <- delete_edges(graph, E(graph)[!E(graph) %in% correlated_edges])
subgraph <- delete_edges(subgraph, E(subgraph)[!E(subgraph) %in% significant_edges])

#subgraph = delete.vertices(subgraph , which(degree(subgraph)==0))#delete points with no connections
abundance = abundance[names(abundance) %in% V(subgraph)$name]

# Positive and negative correlations
neg_edges <- E(subgraph)[correlation[as.integer(E(subgraph))] < 0]
pos_edges <- E(subgraph)[correlation[as.integer(E(subgraph))] > 0]


# Assign different colors to each species
species_colors <- rainbow(length(V(subgraph)))

# Create a color vector for edges based on their sign (positive/negative)
edge_colors <- ifelse(E(subgraph) %in% neg_edges, "negative", "positive") #The colours can't be set as they are already in use by scale_colour manual actually, so there is o pink or green.
Correlations = edge_colors

# Plot the co-occurrence network with significant correlations 
#width = abs(E(subgraph)$weight), to edge_arc for width associated with edge
#alpha = abs(E(subgraph)$weight),

ggraph(subgraph, layout = 'circle') +
  geom_edge_arc(strength = 0.2, aes(color = Correlations)) +
  geom_node_point(aes(size = abundance, color = name), alpha = 0.5) +
  scale_size_continuous(range = c(2, 10)) +
  scale_color_manual(values = species_colors) +
  geom_node_text(aes(label = name), col = "darkblue", size = 3, repel = T, force = 0.004) +
  theme_void() +
  guides(color = "none", size = "none")

###
#For Species
#create relative wide format
df = read.csv("Red_tide_and_WCZ_02162024.csv")

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

wide2 <- df_wide[,-1]
wide2[is.na(wide2)] = 0
m_obs = as.matrix(wide2)

# Calculate the co-occurrence matrix
co_occurrence <- t(m_obs) %*% m_obs

# Create a graph from the co-occurrence matrix
graph <- graph_from_adjacency_matrix(co_occurrence, mode = "undirected", weighted = TRUE)

# Set the vertex names as species labels
V(graph)$name <- colnames(co_occurrence)

# Determine the abundance of each species
abundance <- colSums(wide2)

# Calculate Pearson correlation coefficients
correlation <- cor(m_obs)

# Calculate p-values for correlations
p_values <- matrix(0, ncol = ncol(m_obs), nrow = ncol(m_obs))
for (i in 1:(ncol(m_obs) - 1)) {
  for (j in (i + 1):ncol(m_obs)) {
    p_values[i, j] <- p_values[j, i] <- cor.test(m_obs[, i], m_obs[, j])$p.value
  }
}

# Define the threshold for significance
cor_threshold <- 0.6
p_value_threshold <- 0.05

# Filter edges based on significance criteria
correlated_edges <- E(graph)[abs(correlation[as.integer(E(graph))]) > cor_threshold]
significant_edges <- E(graph)[p_values[as.integer(E(graph))] < p_value_threshold]

# Create a subgraph with only significant edges
subgraph <- delete_edges(graph, E(graph)[!E(graph) %in% correlated_edges])
subgraph <- delete_edges(subgraph, E(subgraph)[!E(subgraph) %in% significant_edges])

subgraph = delete.vertices(subgraph , which(degree(subgraph)==0))#delete points with no connections
abundance = abundance[names(abundance) %in% V(subgraph)$name]

# Positive and negative correlations
neg_edges <- E(subgraph)[correlation[as.integer(E(subgraph))] < 0]
pos_edges <- E(subgraph)[correlation[as.integer(E(subgraph))] > 0]


# Assign different colors to each species
species_colors <- rainbow(length(V(subgraph)))

# Create a color vector for edges based on their sign (positive/negative)
edge_colors <- ifelse(E(subgraph) %in% neg_edges, "negative", "positive") #The colours can't be set as they are already in use by scale_colour manual actually, so there is o pink or green.
Correlations = edge_colors

# Plot the co-occurrence network with significant correlations 
#width = abs(E(subgraph)$weight), to edge_arc for width associated with edge
#alpha = abs(E(subgraph)$weight),


ggraph(subgraph, layout = 'circle') +
  geom_edge_arc(strength = 0.2, aes(color = Correlations)) +
  geom_node_point(aes(size = abundance, color = name), alpha = 0.5) +
  scale_size_continuous(range = c(2, 10)) +
  scale_color_manual(values = species_colors) +
  geom_node_text(aes(label = name), col = "black", size = 4, repel = T, force = 0.05) +
  theme_void() +
  guides(color = "none", size = "none") +
  theme(
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  )


num_pink_lines <- length(E(subgraph)[correlation[as.integer(E(subgraph))] < 0])
num_green_lines <- length(E(subgraph)[correlation[as.integer(E(subgraph))] > 0])

ggraph(subgraph, layout = "dh") +
  geom_edge_arc(strength = 0.2, aes(color = edge_colors)) +
  geom_node_point(aes(size = abundance, color = name), alpha = 0.2) +
  scale_size_continuous(range = c(2, 10)) +
  scale_color_manual(values = species_colors) +
  geom_node_text(aes(label = name), col = "darkblue", size = 3) +
  theme_void() +
  theme(legend.position = "none")

ggraph(subgraph, layout = "grid") +
  geom_edge_arc(strength = 0.2, aes(color = edge_colors)) +
  geom_node_point(aes(size = abundance, color = name), alpha = 0.5) +
  scale_size_continuous(range = c(2, 10)) +
  scale_color_manual(values = species_colors) +
  geom_node_text(aes(label = name), col = "darkblue", size = 3) +
  theme_void() +
  theme(legend.position = "none")


