# cargar paquetes
library(tidyverse) # manipulación, transformación y dataviz
library(mvabund) # manupilación y análisis de datos multivariantes
library(lattice) # visualización
library(gtools) # manipulación

# leer matrices
L <- read.table("fourth-corner/data/L_matrix.txt")
R <- read.table("fourth-corner/data/R_matrix.txt")
Q <- read.table("fourth-corner/data/Q_matrix.txt")

# estética general de los plots
theme_set(theme_minimal() +
            theme(axis.title = element_text(size = 15),
                  axis.line = element_line(color = "black", size = 0.5),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  axis.text = element_text(color = "black", size = 12),
                  strip.text.x = element_text(size = 12),
                  axis.ticks = element_line(color = "black")))

# creamos una copia de L incluyendo el sitio como factor en la matriz
L.site <- cbind("site" = rownames(L), L)

# plot de abundancia por sitio
L.site %>%
  
  # transformamos la matrix para poder hacer el plot
  pivot_longer(cols = 2:ncol(L.site),
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

# creamos un objeto 'mvabund' a partir de la abundancia
spp <- mvabund(L)

# hacemos un modelo para explicar la abundancia en función del sitio
mod <- manyglm(spp ~ L.site$site, family = "poisson") # distr. poisson

# representamos los residuos del modelo
plot(mod)

# análisis de la varianza para ver si el sitio explica la abundancia
#(anov <- anova(mod, nBoot = 999))

# explicamos la matriz de abundancia en función del ambiente
#sdm_env <- traitglm(L, R) # modelo lineal generalizado (glm)
#
## transformar y dataviz
#sdm_env$fourth.corner %>%
#  as.tibble(rownames = "species") %>%
#  pivot_longer(cols = -species, names_to = "env_var", values_to = "value") %>% 
#  mutate(species = sub("^as\\.factor\\.names\\.L\\.\\.", "", species),
#         value = round(value, 5),
#         env_var = forcats::fct_inorder(env_var)) %>% 
#  
#  # mapa de calor
#  ggplot(aes(x = env_var, y = fct_rev(species), fill = value)) +
#  geom_raster() +
#  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
#                       midpoint = 0) +
#  labs(x = "Variables ambientales",
#       y = "Especies",
#       fill = "Coeficiente") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# puede ser un problema para los chavales que haya que hacer tantos cambios!

Q <- Q %>% select(names(.)[13:23])

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
  as.tibble(rownames = "traits") %>%
  pivot_longer(cols = -traits, names_to = "env_var", values_to = "value") %>% 
  mutate(value = round(value, 5),
         env_var = forcats::fct_inorder(env_var)) %>% 
  
  # mapa de calor
  ggplot(aes(x = env_var, y = fct_rev(traits), fill = value)) +
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
  ggplot(aes(x = env_var, y = fct_rev(traits), fill = value)) +
  geom_raster() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0) +
  labs(x = "Variables ambientales",
       y = "Rasgos funcionales",
       fill = "Coeficiente") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))








