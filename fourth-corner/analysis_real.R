# cargar paquetes
library(tidyverse) # manipulación, transformación y dataviz
library(mvabund) # manupilación y análisis de datos multivariantes
#library(lattice) # visualización
#library(gtools) # manipulación

# leer matrices
L <- read.table("fourth-corner/data/L_family.txt")
R <- read.table("fourth-corner/data/R_family.txt")
Q <- read.table("fourth-corner/data/Q_family.txt")

# estética general de los plots
theme_set(theme_minimal() +
            theme(axis.title = element_text(size = 15),
                  axis.line = element_line(color = "black", linewidth = 0.5),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  axis.text = element_text(color = "black", size = 12),
                  strip.text.x = element_text(size = 12),
                  axis.ticks = element_line(color = "black")))


# plot de abundancia por sitio
L %>%
  
  rownames_to_column(var = "site") %>% 
  
  # transformamos la matrix para poder hacer el plot
  pivot_longer(cols = -site,
               names_to = "species",
               values_to = "abundance") %>% 
  
  # añadimos el plot
  ggplot(aes(x = abundance,
             y = reorder(species, abundance, FUN = max),
             color = site)) +
  geom_point() + # gráfico de puntos
  labs(x = "Abundancia",
       y = "Especies",
       color = "Sitio") +
  theme(text = element_text(size = 18))


start_time <- Sys.time()

# añadimos la matriz de rasgos funcionales
f.corner <- traitglm(L, R, Q) # glm de nuevo


# observemos la matriz fourth-corner
f.corner$fourth.corner

### select numeric traits.



# hacemos primero un análisis de la interacción ambiente-rasgo
#anova(f.corner, nBoot = 1) # debería ser >=999

# y después las interacciones entre sus componentes
#summary(f.corner, nBoot = 1) # debería ser >=999

# finalmente, podemos representar el mapa de calor
f.corner$fourth.corner %>%
  as_tibble(rownames = "traits") %>%
  pivot_longer(cols = -traits, names_to = "env_var", values_to = "value") %>% 
  mutate(value = round(value, 5),
         env_var = forcats::fct_inorder(env_var)) %>% 
  
  # mapa de calor
  ggplot(aes(x = reorder(env_var, value), y = reorder(traits, value), fill = value)) +
  geom_raster() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0) +
  labs(x = "Variables ambientales",
       y = "Rasgos funcionales",
       fill = "Coeficiente") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# añadimos lasso a través del argumento method = "glm1path"
lasso <- traitglm(L, R, Q, method = "glm1path")

# observemos la matriz fourth-corner con lasso
lasso$fourth.corner

# comprobar residuos y predicted vs data
plot(lasso)
data <- data.frame(unlist(L), c(predict(lasso)))
names(data) <- c("real", "pred")
summary(lm(real ~ pred, data = data))
plot(data$pred, data$real)
abline(0, 1)


# finalmente, podemos representar el mapa de calor
lasso$fourth.corner %>%
  as.tibble(rownames = "traits") %>%
  pivot_longer(cols = -traits, names_to = "env_var", values_to = "value") %>% 
  mutate(value = round(value, 5),
         env_var = forcats::fct_inorder(env_var)) %>% 
  
  # mapa de calor
  ggplot(aes(x = reorder(env_var, value), y = reorder(traits, value), fill = value)) +
  geom_raster() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0) +
  labs(x = "Variables ambientales",
       y = "Rasgos funcionales",
       fill = "Coeficiente") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))








