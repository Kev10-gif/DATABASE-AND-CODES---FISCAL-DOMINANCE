# ============================================================
# 0) Paquetes
# ============================================================
# install.packages(c("data.table","readxl","zoo","fixest"))

rm(list = ls())
setwd("C:/Users/kimbe/OneDrive/Escritorio/KA/TRABAJO CENTRUM/CHION/TRABAJO CENTRUM_ACTUAL/PAPERS/TR4-Taylor rule with fiscal variables/DATABASE")
getwd()
list.files()


install.packages("zoo")
install.packages("data.table") 
install.packages("fixest")
install.packages("readxl")

library(data.table)
library(readxl)
library(zoo)
library(fixest)

# ============================================================
# 1) Cargar data
# ============================================================
file_path <- "PANEL 2_Emerging.xlsx"   # <-- si está en otro folder, cambia el path
df <- read_excel(file_path, sheet = "Hoja1")
df <- as.data.table(df)

# Verifica columnas esperadas
needed <- c("country","country_code","quarter","mpr","IPC","GDP","Bond","RER","GD")
missing_cols <- setdiff(needed, names(df))
if(length(missing_cols) > 0){
  stop(paste("Faltan columnas en el Excel:", paste(missing_cols, collapse=", ")))
}

# ============================================================
# 2) Orden panel y fecha trimestral
# ============================================================
# quarter viene como "2013q1". Lo pasamos a yearqtr
df[, qtr := as.yearqtr(quarter, format = "%Yq%q")]

# ID país (usa country_code para cluster/FE)
df[, id := as.integer(country_code)]

# Ordenar
setkey(df, id, qtr)

# ============================================================
# 3) Construir variables macro (inflación, depreciación, crecimiento)
# ============================================================
# Convenciones: tasas anualizadas (400*log( / lag))
# Inflación: pi
# Depreciación: dep (usa RER como proxy)
# Crecimiento PBI: g
df[, pi  := 400 * (log(IPC) - log(shift(IPC, 1))), by = id]
df[, dep := 400 * (log(RER) - log(shift(RER, 1))), by = id]
df[, g   := 400 * (log(GDP) - log(shift(GDP, 1))), by = id]

# Deuda: b
df[, b := GD]

# Limpieza básica (quita primeras obs sin lag)
df <- df[!is.na(pi) & !is.na(dep) & !is.na(g) & !is.na(mpr) & !is.na(Bond) & !is.na(b)]

# ============================================================
# 4) Crear rezagos (K rezagos) para shock y LP
# ============================================================
K <- 4
lag_vars <- c("pi","dep","g","mpr","Bond")

for(v in lag_vars){
  for(k in 1:K){
    df[, paste0(v,"_L",k) := shift(get(v), k), by = id]
  }
}

# Mantén filas con rezagos completos para estimación
keep_lags <- c(paste0("pi_L",1:K), paste0("dep_L",1:K),
               paste0("g_L",1:K), paste0("mpr_L",1:K), paste0("Bond_L",1:K))
df_est <- df[complete.cases(df[, ..keep_lags])]

# ============================================================
# 5) Shock exógeno de inflación: residuo de una ecuación de inflación
# ============================================================
# pi_t explicado por: rezagos de pi + dep contemporáneo y rezagos + rezagos de g, mpr, Bond
# con FE país + FE tiempo (trimestre)
infl_formula <- as.formula(paste(
  "pi ~",
  paste(paste0("pi_L",1:K), collapse = " + "), " + ",
  "dep + ", paste(paste0("dep_L",1:K), collapse = " + "), " + ",
  paste(paste0("g_L",1:K), collapse = " + "), " + ",
  paste(paste0("mpr_L",1:K), collapse = " + "), " + ",
  paste(paste0("Bond_L",1:K), collapse = " + "),
  "| id + qtr"
))
m_infl <- feols(infl_formula, data = df_est, vcov = ~id)

# Shock = residuo
df_est[, shock_pi := resid(m_infl)]

# Estandariza deuda (para interpretación clara y estabilidad numérica)
df_est[, b_z := as.numeric(scale(b))]

# ============================================================
# 6) Local Projections (LP): reacción de mpr al shock inflacionario
# ============================================================
H <- 12  # horizontes: 0..12 trimestres (3 años)
controls <- c(paste0("mpr_L",1:K),
              paste0("pi_L",1:K),
              paste0("dep_L",1:K),
              paste0("g_L",1:K),
              paste0("Bond_L",1:K))

lp_models <- vector("list", H+1)

for(h in 0:H){
  # lead de mpr: mpr_{t+h}
  lead_name <- paste0("mpr_F", h)
  df_est[, (lead_name) := shift(mpr, -h), by = id]
  
  # usa solo filas donde exista el lead
  d_h <- df_est[!is.na(get(lead_name))]
  
  lp_formula <- as.formula(paste0(
    lead_name, " ~ shock_pi + shock_pi:b_z + b_z + ",
    paste(controls, collapse = " + "),
    " | id + qtr"
  ))
  
  lp_models[[h+1]] <- feols(lp_formula, data = d_h, vcov = ~id)
  
}

# ============================================================
# 7) IRFs: deuda baja vs alta (P25 vs P75 de b_z)
# ============================================================
get_irf <- function(mod, bval){
  co <- coef(mod)
  V  <- vcov(mod)
  
  beta  <- unname(co["shock_pi"])
  gamma <- unname(co["shock_pi:b_z"])
  
  eff <- beta + gamma * bval
  
  v_eff <- V["shock_pi","shock_pi"] +
    bval^2 * V["shock_pi:b_z","shock_pi:b_z"] +
    2*bval * V["shock_pi","shock_pi:b_z"]
  
  se <- sqrt(v_eff)
  c(est = eff, se = se)
}

b_low  <- as.numeric(quantile(df_est$b_z, 0.25, na.rm = TRUE))
b_high <- as.numeric(quantile(df_est$b_z, 0.75, na.rm = TRUE))

irf <- data.table(h = 0:H,
                  est_low = NA_real_, se_low = NA_real_,
                  est_high = NA_real_, se_high = NA_real_)

for(hh in 0:H){
  outL <- get_irf(lp_models[[hh+1]], b_low)
  outH <- get_irf(lp_models[[hh+1]], b_high)
  
  irf[h == hh, `:=`(
    est_low  = as.numeric(outL["est"]),
    se_low   = as.numeric(outL["se"]),
    est_high = as.numeric(outH["est"]),
    se_high  = as.numeric(outH["se"])
  )]
}

# Bandas 95%
irf[, `:=`(
  lo_low  = est_low  - 1.96*se_low,
  hi_low  = est_low  + 1.96*se_low,
  lo_high = est_high - 1.96*se_high,
  hi_high = est_high + 1.96*se_high
)]

print(irf)

# ============================================================
# 8) Lectura económica (rápida)
# ============================================================
cat("\nInterpretación:\n",
    "- est_low(h): respuesta de la tasa (mpr) a un shock inflacionario con deuda baja (P25)\n",
    "- est_high(h): respuesta con deuda alta (P75)\n",
    "- Si est_high < est_low de manera sistemática => el BC reacciona menos cuando la deuda es alta.\n")




# install.packages("ggplot2") # si no lo tienes
library(ggplot2)
library(data.table)

# Preparar data en formato largo
irf_long <- rbind(
  data.table(h = irf$h, est = irf$est_low,  lo = irf$lo_low,  hi = irf$hi_low,  state = "Low debt (P25)"),
  data.table(h = irf$h, est = irf$est_high, lo = irf$lo_high, hi = irf$hi_high, state = "High debt (P75)")
)

# Gráfico tipo paper (un panel)
p1 <- ggplot(irf_long, aes(x = h, y = est, linetype = state)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = state), alpha = 0.18, color = NA) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = 0:max(irf_long$h)) +
  labs(
    x = "Quarters",
    y = "Response of monetary policy rate", #pp annuealized
   # title = "Respuesta de la tasa de política a un shock inflacionario",
    #subtitle = "Proyecciones locales: comparación entre deuda baja (P25) y deuda alta (P75)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

print(p1)

# Guardar en alta resolución (para paper)
ggsave("IRF_policy_shockpi_deuda_low_vs_high.png", p1, width = 7.2, height = 4.6, dpi = 400)
ggsave("IRF_policy_shockpi_deuda_low_vs_high.pdf", p1, width = 7.2, height = 4.6)
 ##############################################################################3

################################################################################
p2 <- ggplot(irf_long, aes(x = h, y = est)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20, color = NA) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~state, ncol = 1) +
  scale_x_continuous(breaks = 0:max(irf_long$h)) +
  labs(
    x = "Quarters",
    y = "Response of monetary policy rate (pp annualized)",
    #title = "Respuesta de la tasa de política a un shock inflacionario",
    #subtitle = "Proyecciones locales con bandas de confianza al 95%"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

print(p2)

ggsave("IRF_policy_shockpi_facets.png", p2, width = 7.2, height = 6.2, dpi = 400)
ggsave("IRF_policy_shockpi_facets.pdf", p2, width = 7.2, height = 6.2)

