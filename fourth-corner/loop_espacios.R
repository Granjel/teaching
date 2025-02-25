library(tidyverse) # manipulación, transformación y dataviz
library(mvabund) # manupilación y análisis de datos multivariantes
#library(lattice) # visualización
#library(gtools) # manipulación

# leer matrices
L <- read.table("fourth-corner/data/L_family.txt")
R <- read.table("fourth-corner/data/R_family.txt")
Q <- read.table("fourth-corner/data/Q_family.txt")
R <- R %>% select(-WATER_._19_23, -URBAN_._19_23, -AGRICULTURE_._19_23, -granulometry_median)

# estética general de los plots
theme_set(theme_minimal() +
            theme(axis.title = element_text(size = 15),
                  axis.line = element_line(color = "black", linewidth = 0.5),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  axis.text = element_text(color = "black", size = 12),
                  strip.text.x = element_text(size = 12),
                  axis.ticks = element_line(color = "black")))

# filter by site
site_list <- c("AIZK", "ARAL", "ARTI", "GORB", "IZKI")
plot_list <- list()
coef_list <- list()

for (i in 1:length(site_list)){
  
  l <- L[grepl(site_list[i], rownames(L)), ]
  r <- R[grepl(site_list[i], rownames(R)), ]

  # perform lasso with the matrices
  lasso <- traitglm(l, r, Q, method = "glm1path")
  coef_list[[i]] <- as.data.frame(lasso$fourth.corner)
  
  plot_list[[i]] <- lasso$fourth.corner %>%
    as.tibble(rownames = "traits") %>%
    pivot_longer(cols = -traits, names_to = "env_var", values_to = "value") %>% 
    mutate(value = round(value, 5),
           env_var = factor(env_var, levels = sort(unique(env_var))),
           traits = factor(traits, levels = sort(unique(traits), decreasing = TRUE))) %>% 
    
    # mapa de calor
    ggplot(aes(x = env_var, y = traits, fill = value)) +
    geom_raster() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0) +
    labs(title = site_list[i],
         x = "Var. ambientales",
         y = "Rasgos func.",
         fill = "Coef.") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

}

plot_list[[length(plot_list) + 1]] <- Reduce("*", coef_list) %>%
  as.tibble(rownames = "traits") %>%
  pivot_longer(cols = -traits, names_to = "env_var", values_to = "value") %>% 
  mutate(value = round(value, 5),
         env_var = factor(env_var, levels = sort(unique(env_var))),
         traits = factor(traits, levels = sort(unique(traits), decreasing = TRUE))) %>% 
  
  # mapa de calor
  ggplot(aes(x = env_var, y = traits, fill = value)) +
  geom_raster() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0) +
  labs(title = "Interacciones comunes",
       x = "Var. ambientales",
       y = "Rasgos func.",
       fill = "Coef.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
library(ggpubr)
ggarrange(plotlist = plot_list, ncol = 2, nrow = 3)


















