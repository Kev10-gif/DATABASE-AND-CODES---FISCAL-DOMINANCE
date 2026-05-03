rm(list = ls())
setwd("C:/Users/kimbe/OneDrive/Escritorio/Work BBVA Research/Proyectos/Curva de Phillips/271125-Entregable")
getwd()
list.files()

# Librerías
library(readxl)
library(ggplot2)

# === 1. Leer los datos ===
df <- read_excel("irfs_modelo_191225_NV.xlsx", sheet = "ex-ti")

# Asegúrate de que haya una columna con los trimestres:
df$trim <- 1:12   # eje x (quarters)

# === 2. Crear el gráfico combinado ===

ggplot(df, aes(x = trim)) +
  
  # --- BVAR (CON bandas) ---
  geom_ribbon(aes(ymin = `Lw. bound`, ymax = `Up. bound`),
              fill = "gray40", alpha = 0.2) +
  geom_line(aes(y = Median), color = "black", linewidth = 1.8) +
  
  # --- DSGE reescalado (SIN bandas) ---
  geom_line(aes(y = `Median (m)`),
            color = "#008B8B", linewidth = 1.8, linetype = "dashed") +
  
  # --- Eje base ---
  geom_hline(yintercept = 0, color = "black", linetype = "dotted", linewidth = 0.6) +
  
  # --- Etiquetas y tema ---
  labs(
    title = "",
    x = "Quarters",
    y = "Response"
  ) +
  
  scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +
 # scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  #coord_cartesian(ylim = c(-2, 1.5)) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = 9.5, y = 2, label = "BVAR model", color = "black", size = 3.5) +
  annotate("text", x = 9.5, y = 1.8
          , label = "DSGE model", color = "#008B8B", size = 3.5)

