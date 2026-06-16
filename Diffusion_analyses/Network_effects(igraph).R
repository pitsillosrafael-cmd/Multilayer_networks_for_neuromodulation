library(igraph)

# Create network showing the change pre and post modulation
# ASEG atlas (subcortical)
pre_idx_aseg <- aseg_corrected_z_time$Timepoint == "preop"
post_idx_aseg <- aseg_corrected_z_time$Timepoint == "postop12m"

pre_data_aseg <- aseg_corrected_z_time[pre_idx_aseg, ]
pre_data_aseg  <- pre_data_aseg[!grepl("sub-DBS01|sub-DBS03", pre_data_aseg$Subjects), ]
post_data_aseg <- aseg_corrected_z_time[post_idx_aseg, ]

# To make IDs clear
pre_data_aseg$ID  <- sub("_.*", "", pre_data_aseg$Subjects)
post_data_aseg$ID <- sub("_.*", "", post_data_aseg$Subjects)

# match the subjects
common_subj_aseg <- intersect(pre_data_aseg$ID, post_data_aseg$ID)
pre_data_aseg <- pre_data_aseg[match(common_subj_aseg, pre_data_aseg$ID), ]
post_data_aseg <- post_data_aseg[match(common_subj_aseg, post_data_aseg$ID), ]

# Compute the change
delta_aseg <- post_data_aseg[, regions_aseg] - pre_data_aseg[, regions_aseg]

# Similarity between regions changes
cor_mat_aseg <- cor(delta_aseg, use = "pairwise.complete.obs")

# Threshold to keep only the strong connections
threshold <- 0.3
adj_mat <- cor_mat_aseg
adj_mat[abs(adj_mat) < threshold] <- 0

# plot simple network
E(g)$color <- ifelse(E(g)$weight >0, "red", "blue")
E(g)$width <- abs(E(g)$weight) *8
layout <- layout_with_fr(g, weights = abs(E(g)$weight))

g <- graph_from_adjacency_matrix(adj_mat,
                                 mode = "undirected",
                                 weighted = TRUE,
                                 diag = FALSE)
plot(g,
     edge.width = E(g)$width,
     edge.color = E(g)$color,
     vertex.label.cex = 0.7,
     vertex.size = 20,
     main = "DBS-Induced Structural Change Network (Post–Pre)")


# Alternative network with blue for correlation red for anti-correl (weights represent how SIMILAR is their change)
# keep original signed weights
E(g)$weight
# create positive weights ONLY for layout
E(g)$layout_weight <- abs(E(g)$weight)
# colors = sign
E(g)$color <- ifelse(E(g)$weight > 0, "#E41A1C", "#377EB8")
# width = strength
E(g)$width <- abs(E(g)$weight) * 5
# compute layout explicitly
lay <- layout_with_fr(g, weights = E(g)$layout_weight)
# plot
plot(g,
     layout = lay,
     edge.width = E(g)$width,
     edge.color = E(g)$color,
     vertex.size = 25,
     vertex.color = "#CDC673",
     vertex.frame.color = "black",
     vertex.label.cex = 0.8,
     main = "DBS-Induced Structural Change Network (Post–Pre)")
legend("topleft",
       legend = c("Positive correlation (co-change)",
                  "Negative correlation (anti-change)"),
       col = c("#E41A1C", "#377EB8"),
       lwd = 3,
       bty = "n")

# Quantitative metrics
# Find hubs
degree(g)
strength(g)
# Identify communities
cluster_louvain(g)







# Create network showing the change pre and post modulation
# DSK atlas (cortical)
pre_idx_aparc<- aparc_corrected_z_time$Timepoint == "preop"
post_idx_aparc<- aparc_corrected_z_time$Timepoint == "postop12m"

pre_data_aparc<- aparc_corrected_z_time[pre_idx_aparc, ]
pre_data_aparc <- pre_data_aparc[!grepl("sub-DBS01|sub-DBS03", pre_data_aparc$Subjects), ]
post_data_aparc<- aparc_corrected_z_time[post_idx_aparc, ]

# To make IDs clear
pre_data_aparc$ID  <- sub("_.*", "", pre_data_aparc$Subjects)
post_data_aparc$ID <- sub("_.*", "", post_data_aparc$Subjects)

# match the subjects
common_subj_aparc<- intersect(pre_data_aparc$ID, post_data_aparc$ID)
pre_data_aparc<- pre_data_aparc[match(common_subj_aparc, pre_data_aparc$ID), ]
post_data_aparc<- post_data_aparc[match(common_subj_aparc, post_data_aparc$ID), ]

# Compute the change
delta_aparc<- post_data_aparc[, regions_aparc_clean] - pre_data_aparc[, regions_aparc_clean]

# Similarity between regions changes
cor_mat_aparc<- cor(delta_aparc, use = "pairwise.complete.obs")

# Threshold to keep only the strong connections
# Threshold correlation matrix
threshold <- 0.5
adj_mat <- cor_mat_aparc
adj_mat[abs(adj_mat) < threshold] <- 0

# Build graph
g <- graph_from_adjacency_matrix(adj_mat,
                                 mode = "undirected",
                                 weighted = TRUE,
                                 diag = FALSE)

# Edge properties
E(g)$color <- ifelse(E(g)$weight > 0, "#E41A1C", "#377EB8")
E(g)$width <- abs(E(g)$weight) * 5

# Layout (use absolute weights for positioning only)
lay <- layout_with_fr(
  g,
  coords = matrix(runif(vcount(g)*2), ncol=2),
  weights = abs(E(g)$weight)^0.5,
  niter = 3000
)

# Plot
plot(g,
     layout = lay,
     edge.width = E(g)$width,
     edge.color = E(g)$color,
     vertex.size = 10,
     vertex.color = "#CDC673",
     vertex.frame.color = "black",
     vertex.label.cex = 0.8,
     main = "DBS-Induced Structural Change Network (Post–Pre)")

# Legend
legend("topleft",
       legend = c("Positive correlation (co-change)",
                  "Negative correlation (anti-change)"),
       col = c("#E41A1C", "#377EB8"),
       lwd = 3,
       bty = "n")


# Quantitative metrics
# Find hubs
degree(g)
strength(g)








# COMBINE SUBCORTICAL AND CORTICAL
# DSK and ASEG network
delta_all <- cbind(delta_aparc, delta_aseg)
cor_mat_aseg_and_aparc <- cor(delta_all, use = "pairwise.complete.obs")

threshold <- 0.5
adj_mat_aseg_and_aparc <- cor_mat_aseg_and_aparc
adj_mat_aseg_and_aparc[abs(adj_mat_aseg_and_aparc) < threshold] <- 0


g <- graph_from_adjacency_matrix(adj_mat_aseg_and_aparc,
                                 mode = "undirected",
                                 weighted = TRUE,
                                 diag = FALSE)


# Different shapes for cortical vs subcortical
V(g)$type <- ifelse(V(g)$name %in% regions_aseg, "subcortical", "cortical")
V(g)$shape <- ifelse(V(g)$type == "subcortical", "square", "circle")

# Colors of nodes
V(g)$color <- ifelse(V(g)$type == "subcortical", "lightgreen", "darkgreen")

# Set colors of edges
E(g)$color <- ifelse(E(g)$weight > 0, "#E41A1C", "#377EB8")
E(g)$width <- abs(E(g)$weight) * 5


# Layout
lay <- layout_with_fr(g,
                      weights = abs(E(g)$weight)^0.5,
                      niter = 2000)
# Plot
plot(g,
     layout = lay,
     edge.color = E(g)$color,
     edge.width = E(g)$width,
     vertex.size = 8,
     vertex.label.cex = 0.6,
     main = "Cortical–Subcortical Network (Post–Pre)")

legend("topleft",
       legend = c("Cortical (circle)",
                  "Subcortical (square)",
                  "Positive correlation",
                  "Negative correlation"),
       pch = c(21, 22, NA, NA),
       pt.bg = c("darkgreen", "lightgreen", NA, NA),
       col = c(NA, NA, "#E41A1C", "#377EB8"),
       lwd = c(NA, NA, 3, 3),
       pt.cex = 2,
       bty = "n")


# Quantitative metrics
# Find hubs
degree(g)
strength(g)




# Check for centrality
library(ggraph)

# Keep largest component
g <- induced_subgraph(g, which(components(g)$membership == which.max(components(g)$csize)))
g_plot <- g
V(g_plot)$hemi <- ifelse(grepl("^lh_|Left\\.", V(g_plot)$name), "left", "right")
V(g)$label <- gsub("lh_", "L-", V(g)$name)
V(g)$label <- gsub("rh_", "R-", V(g)$label)
V(g)$label <- gsub("_thickness", "", V(g)$label)

# fix weights for layout
E(g_plot)$weight <- abs(E(g_plot)$weight)

# keep sign from original
E(g_plot)$sign <- E(g)$weight > 0

ggraph(g_plot, layout = "centrality", cent = V(g_plot)$size) +
  
  geom_edge_link0(aes(edge_linewidth = weight,
                      edge_colour = sign),
                  alpha = 0.7) +
  
  geom_node_point(aes(size = size,
                      shape = type,
                      fill = hemi)) +
  
  geom_node_text(aes(label = label),
                 repel = TRUE,
                 size = 3) +
  
  scale_edge_color_manual(values = c("TRUE" = "#377EB8",
                                     "FALSE" = "#E41A1C")) +
  
  scale_shape_manual(values = c("cortical" = 21,
                                "subcortical" = 22)) +
  
  scale_fill_manual(values = c("left" = "#66c2a5",
                               "right" = "#1b7837")) +
  
  scale_edge_width(range = c(0.2, 1.5)) +
  scale_size(range = c(2, 10)) +
  
  coord_fixed() +
  theme_void() +
  
  ggtitle("Cortical–Subcortical Centrality Network")
  
  
  