############################################################
# 0. LIMPIAR ENTORNO Y PAQUETES
############################################################

rm(list = ls())

packages <- c("data.table", "readxl", "zoo", "fixest", "ggplot2", "dplyr")

for(p in packages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

############################################################
# 1. CARGAR BASE
############################################################

setwd("C:/Users/kimbe/OneDrive/Escritorio/KA/TRABAJO CENTRUM/CHION/TRABAJO CENTRUM_ACTUAL/PAPERS/TR4-Taylor rule with fiscal variables/ECONOMETRIC MODEL/Extension")

file_path <- "PANEL1_COMPLETO.xlsx"
df        <- read_excel(file_path, sheet = "Emerging")
df        <- as.data.table(df)
df        <- df[, !grepl("^Unnamed", names(df)), with = FALSE]

############################################################
# 2. PANEL
############################################################

df[, qtr := as.yearqtr(quarter, format = "%Yq%q")]
df[, id  := as.integer(as.factor(country_code))]
setkey(df, id, qtr)

############################################################
# 3. VARIABLES MACRO (tasas anualizadas)
############################################################

df[, pi  := 400 * (log(IPC) - log(shift(IPC, 1))), by = id]
df[, dep := 400 * (log(NER) - log(shift(NER, 1))), by = id]
df[, g   := 400 * (log(GDP) - log(shift(GDP, 1))), by = id]

# Deuda pública estandarizada (dominancia fiscal)
df[, b   := GD]
df[, b_z := as.numeric(scale(b))]

df[, CBI := as.integer(CBI)]
df[, IF  := as.integer(IF)]

############################################################
# 4. LIMPIEZA
############################################################

df <- df[!is.na(pi) & !is.na(dep) & !is.na(g)]

############################################################
# 5. SHOCK INFLACIONARIO VÍA RESIDUOS AR(1)
#    → Así el shock NO está correlacionado con los rezagos
#    → Evita la multicolinealidad que aplana las IRFs
############################################################

K <- 4

# Primero creamos rezagos de pi para la regresión AR
for(k in 1:K){
  df[, paste0("pi_L", k) := shift(pi, k), by = id]
}

df_ar <- df[complete.cases(df[, c("pi", paste0("pi_L", 1:K)), with = FALSE])]

# Regresión AR(4) con FE de país — residuo = shock inflacionario
ar_fit       <- feols(pi ~ pi_L1 + pi_L2 + pi_L3 + pi_L4 | id, data = df_ar)
df_ar[, shock_pi := residuals(ar_fit)]

# Unir el shock al dataset original
df <- merge(df, df_ar[, .(id, qtr, shock_pi)], by = c("id", "qtr"), all.x = TRUE)

############################################################
# 6. CREAR REZAGOS DE TODAS LAS VARIABLES DE CONTROL
############################################################

vars <- c("pi", "dep", "g", "mpr", "Bond")

for(v in vars){
  for(k in 1:K){
    df[, paste0(v, "_L", k) := shift(get(v), k), by = id]
  }
}

lags <- c(
  paste0("pi_L",   1:K),
  paste0("dep_L",  1:K),
  paste0("g_L",    1:K),
  paste0("mpr_L",  1:K),
  paste0("Bond_L", 1:K)
)

df_est <- df[complete.cases(df[, c(lags, "shock_pi", "b_z"), with = FALSE])]

############################################################
# 7. FUNCIÓN LOCAL PROJECTIONS CON BANDAS DE CONFIANZA
#
#  Especificación (Jordà 2005):
#  mpr_{i,t+h} = α_i + λ_t + β_h·shock_π_{i,t}
#                + γ_h·shock_π_{i,t}·b_z_{i,t}
#                + δ_h·b_z_{i,t} + Σ controls + ε
#
#  El IRF para nivel de deuda b_z = bval es:
#  IRF_h(bval) = β_h + γ_h·bval
############################################################

run_lp_irf <- function(data, H = 12, b_eval = 0, label = "grupo") {
  
  controls <- c(
    paste0("mpr_L",  1:K),
    paste0("pi_L",   1:K),
    paste0("dep_L",  1:K),
    paste0("g_L",    1:K),
    paste0("Bond_L", 1:K)
  )
  
  results <- vector("list", H + 1)
  
  for(h in 0:H){
    
    lead_var <- paste0("mpr_F", h)
    data[, (lead_var) := shift(mpr, -h), by = id]
    
    d <- data[!is.na(get(lead_var)) & !is.na(shock_pi) & !is.na(b_z)]
    
    # Interacción shock × deuda: captura heterogeneidad de dominancia fiscal
    fml <- as.formula(paste0(
      lead_var, " ~ shock_pi + shock_pi:b_z + b_z + ",
      paste(controls, collapse = " + "),
      " | id + qtr"
    ))
    
    mod <- tryCatch(
      feols(fml, data = d, vcov = ~id),
      error = function(e) NULL
    )
    
    if(is.null(mod)){
      results[[h + 1]] <- data.table(h = h, irf = NA, lo = NA, hi = NA, label = label)
      next
    }
    
    co  <- coef(mod)
    vcv <- vcov(mod)
    
    beta  <- ifelse("shock_pi"      %in% names(co), co["shock_pi"],       0)
    gamma <- ifelse("shock_pi:b_z"  %in% names(co), co["shock_pi:b_z"],   0)
    
    # IRF puntual en b_eval
    irf_h <- beta + gamma * b_eval
    
    # Delta method para la varianza de β + γ·b_eval
    idx_b <- which(names(co) == "shock_pi")
    idx_g <- which(names(co) == "shock_pi:b_z")
    
    if(length(idx_b) > 0 & length(idx_g) > 0){
      v_b  <- vcv[idx_b, idx_b]
      v_g  <- vcv[idx_g, idx_g]
      c_bg <- vcv[idx_b, idx_g]
      se_h <- sqrt(v_b + b_eval^2 * v_g + 2 * b_eval * c_bg)
    } else if(length(idx_b) > 0){
      se_h <- sqrt(vcv[idx_b, idx_b])
    } else {
      se_h <- NA
    }
    
    results[[h + 1]] <- data.table(
      h     = h,
      irf   = irf_h,
      lo    = irf_h - 1.645 * se_h,   # IC 90%
      hi    = irf_h + 1.645 * se_h,
      label = label
    )
  }
  
  return(rbindlist(results))
}

############################################################
# 8. SEPARAR MUESTRAS POR CBI
############################################################

cat("\nDistribución CBI:\n")
print(table(df_est$CBI))

data_low  <- copy(df_est[CBI == 0])
data_high <- copy(df_est[CBI == 1])

############################################################
# 9. EVALUAR IRF EN DOS NIVELES DE DEUDA
#    b_z = 0    → deuda en la media (baja dominancia fiscal)
#    b_z = 1.5  → deuda alta (alta dominancia fiscal)
############################################################

b_low_fiscal  <- 0.0   # deuda promedio
b_high_fiscal <- 1.5   # deuda elevada (~75-80 pct)

irf_cbi_low_bL  <- run_lp_irf(data_low,  b_eval = b_low_fiscal,  label = "Low CBI | Medium debt")
irf_cbi_low_bH  <- run_lp_irf(data_low,  b_eval = b_high_fiscal, label = "Low CBI | High debt")
irf_cbi_high_bL <- run_lp_irf(data_high, b_eval = b_low_fiscal,  label = "High CBI | Medium debt")
irf_cbi_high_bH <- run_lp_irf(data_high, b_eval = b_high_fiscal, label = "High CBI | High debt")

irf_all <- rbindlist(list(
  irf_cbi_low_bL,
  irf_cbi_low_bH,
  irf_cbi_high_bL,
  irf_cbi_high_bH
))

print(irf_all)

############################################################
# 10. GRÁFICO PRINCIPAL
############################################################

cols <- c(
  "Low CBI | Medium debt"  = "#E41A1C",
  "Low CBI | High debt"   = "#FF7F00",
  "High CBI | Medium debt"  = "#377EB8",
  "High CBI | High debt"   = "#4DAF4A"
)

p <- ggplot(irf_all, aes(x = h, y = irf, color = label, fill = label)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  geom_line(size = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values  = cols) +
  scale_x_continuous(breaks = 0:12) +
  theme_classic(base_size = 13) +
  labs(
    x      = "Quarters",
    y      = "Response of monetary policy",
    #title  = "Local Projections: Respuesta de la TPM ante un shock inflacionario",
    #subtitle = "Heterogeneidad por independencia del banco central (CBI) y dominancia fiscal (deuda)",
    color  = "",
    fill   = ""
  ) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),   # ← tamaño de etiquetas
        legend.title = element_text(size = 15) )

print(p)
ggsave("IRF_LP_CBI_Emerging_FiscalDominance.png", p, width = 5.5, height = 6.5, dpi = 400)
#################################################################################################


p <- p +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave("IRF_LP_CBI_FiscalDominance.png",
       p,
       width = 12,
       height = 7,
       dpi = 300)









































#############################################################
# 11. GRÁFICO CON EJE SECUNDARIO
#     SOLO Low CBI | Medium debt y Low CBI | High debt
#     VAN EN EL EJE DERECHO
############################################################

plot_data <- copy(irf_all)

# Series que irán en el eje secundario
secondary_labels <- c("Low CBI | Medium debt", "Low CBI | High debt")

plot_data[, axis_group := ifelse(label %in% secondary_labels, "secondary", "primary")]

# Rangos para definir factor de escala
range_primary   <- max(abs(plot_data[axis_group == "primary", irf]), na.rm = TRUE)
range_secondary <- max(abs(plot_data[axis_group == "secondary", irf]), na.rm = TRUE)

scale_factor <- range_primary / range_secondary

# Crear variables escaladas
plot_data[, irf_plot := irf]
plot_data[, lo_plot  := lo]
plot_data[, hi_plot  := hi]

# Escalar SOLO las series del eje secundario
plot_data[axis_group == "secondary", `:=`(
  irf_plot = irf * scale_factor,
  lo_plot  = lo  * scale_factor,
  hi_plot  = hi  * scale_factor
)]

# Verificación
stopifnot(all(c("irf_plot", "lo_plot", "hi_plot") %in% names(plot_data)))

# Gráfico
p2 <- ggplot(plot_data, aes(x = h, y = irf_plot, color = label, fill = label)) +
  geom_ribbon(aes(ymin = lo_plot, ymax = hi_plot), alpha = 0.15, color = NA) +
  geom_line(size = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = 0:12) +
  scale_y_continuous(
    name = "Response (High CBI series)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Response (Low CBI series)")
  ) +
  theme_classic(base_size = 13) +
  labs(
    x     = "Quarters",
    color = "",
    fill  = ""
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 15),   # ← tamaño de etiquetas
    legend.title = element_text(size = 15)   # ← tamaño del título (aunque está vacío)
  )


print(p2)
ggsave("IRF_LP_CBI_Emerging.png", p2, width = 10, height = 6, dpi = 300)













############################################################
# 11. GRÁFICO ALTERNATIVO: solo coeficiente β_h (efecto base)
#     + γ_h (interacción) para ver dominancia fiscal pura
############################################################

get_coefs_by_h <- function(data, H = 12, label = "grupo"){
  
  controls <- c(
    paste0("mpr_L",  1:K),
    paste0("pi_L",   1:K),
    paste0("dep_L",  1:K),
    paste0("g_L",    1:K),
    paste0("Bond_L", 1:K)
  )
  
  out <- vector("list", H + 1)
  
  for(h in 0:H){
    lead_var <- paste0("mpr_F", h)
    data[, (lead_var) := shift(mpr, -h), by = id]
    d <- data[!is.na(get(lead_var)) & !is.na(shock_pi) & !is.na(b_z)]
    
    fml <- as.formula(paste0(
      lead_var, " ~ shock_pi + shock_pi:b_z + b_z + ",
      paste(controls, collapse = " + "),
      " | id + qtr"
    ))
    
    mod <- tryCatch(feols(fml, data = d, vcov = ~id), error = function(e) NULL)
    if(is.null(mod)){ next }
    
    co  <- coef(mod)
    vcv <- vcov(mod)
    
    get_est <- function(name){
      if(name %in% names(co)){
        idx <- which(names(co) == name)
        list(est = co[name], se = sqrt(vcv[idx, idx]))
      } else {
        list(est = NA, se = NA)
      }
    }
    
    b_res <- get_est("shock_pi")
    g_res <- get_est("shock_pi:b_z")
    
    out[[h + 1]] <- data.table(
      h      = h,
      beta   = b_res$est,
      beta_lo = b_res$est - 1.645 * b_res$se,
      beta_hi = b_res$est + 1.645 * b_res$se,
      gamma  = g_res$est,
      gamma_lo = g_res$est - 1.645 * g_res$se,
      gamma_hi = g_res$est + 1.645 * g_res$se,
      label  = label
    )
  }
  rbindlist(out)
}

coefs_low  <- get_coefs_by_h(copy(data_low),  label = "Low CBI")
coefs_high <- get_coefs_by_h(copy(data_high), label = "High CBI")
coefs_all  <- rbindlist(list(coefs_low, coefs_high))

# β_h: respuesta base (deuda = media)
p_beta <- ggplot(coefs_all, aes(x = h, y = beta, color = label, fill = label)) +
  geom_ribbon(aes(ymin = beta_lo, ymax = beta_hi), alpha = 0.15, color = NA) +
  geom_line(size = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Low CBI" = "#E41A1C", "High CBI" = "#377EB8")) +
  scale_fill_manual(values  = c("Low CBI" = "#E41A1C", "High CBI" = "#377EB8")) +
  scale_x_continuous(breaks = 0:12) +
  theme_classic(base_size = 13) +
  labs(
    x = "Quarters",
    y = "β_h",
    #title = "Coeficiente base shock inflación → TPM",
    color = "", fill = ""
  ) +
  theme(legend.position = "bottom")

# γ_h: cómo la deuda modifica la respuesta (dominancia fiscal)
p_gamma <- ggplot(coefs_all, aes(x = h, y = gamma, color = label, fill = label)) +
  geom_ribbon(aes(ymin = gamma_lo, ymax = gamma_hi), alpha = 0.15, color = NA) +
  geom_line(size = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Low CBI" = "#E41A1C", "High CBI" = "#377EB8")) +
  scale_fill_manual(values  = c("Low CBI" = "#E41A1C", "High CBI" = "#377EB8")) +
  scale_x_continuous(breaks = 0:12) +
  theme_classic(base_size = 13) +
  labs(
    x = "Quarter",
    y = "γ_h (interaction shock x debt)",
    #title = "Efecto moderador de la deuda pública (dominancia fiscal)",
    #subtitle = "γ_h < 0 → mayor deuda atenúa la respuesta monetaria",
    color = "", fill = ""
  ) +
  theme(legend.position = "bottom")

print(p_beta)
print(p_gamma)

ggsave("IRF_beta_CBI.png",  p_beta,  width = 9, height = 5, dpi = 300)
ggsave("IRF_gamma_CBI.png", p_gamma, width = 9, height = 5, dpi = 300)

cat("\n✓ Análisis completado. Archivos guardados.\n")


############################################################
# CIRF: IRF Acumulado
############################################################

irf_all[, cirf := cumsum(irf), by = label]
irf_all[, cirf_lo := cumsum(lo), by = label]
irf_all[, cirf_hi := cumsum(hi), by = label]

p_cirf <- ggplot(irf_all, aes(x = h, y = cirf, color = label, fill = label)) +
  geom_ribbon(aes(ymin = cirf_lo, ymax = cirf_hi), alpha = 0.15, color = NA) +
  geom_line(size = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values  = cols) +
  scale_x_continuous(breaks = 0:12) +
  theme_classic(base_size = 13) +
  labs(
    x        = "Quarters",
    y        = "Accumulated Response of the MPR",
   # title    = "CIRF: Respuesta Acumulada de la TPM ante shock inflacionario",
    #subtitle = "Heterogeneidad por CBI y dominancia fiscal",
    color    = "", fill   = ""
  ) +
  theme(legend.position = "bottom")

print(p_cirf)
ggsave("CIRF_LP_CBI_FiscalDominance.png", p_cirf, width = 10, height = 6, dpi = 300)




############################################################
# 15. 4 GRÁFICOS SEPARADOS — uno por escenario
############################################################

# Fijar el mismo eje Y para todos (comparación honesta)
y_min <- min(irf_all$lo, na.rm = TRUE) - 0.01
y_max <- max(irf_all$hi, na.rm = TRUE) + 0.01

hacer_grafico <- function(datos, color_linea, color_banda, titulo){
  ggplot(datos, aes(x = h, y = irf)) +
    geom_ribbon(aes(ymin = lo, ymax = hi),
                fill = color_banda, alpha = 0.25, color = NA) +
    geom_line(color = color_linea, size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    scale_x_continuous(breaks = 0:12) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    theme_classic(base_size = 13) +
    labs(
      x        = "Horizonte (trimestres)",
      y        = "Respuesta TPM (pp)",
      title    = titulo
    )
}

p1 <- hacer_grafico(
  irf_cbi_low_bL,
  color_linea = "#E41A1C",
  color_banda = "#E41A1C",
  titulo      = "CBI bajo | Deuda media"
) + coord_cartesian(ylim = c(
  min(irf_cbi_low_bL$lo, na.rm = TRUE) - 0.005,
  max(irf_cbi_low_bL$hi, na.rm = TRUE) + 0.005
))

p2 <- hacer_grafico(
  irf_cbi_low_bH,
  color_linea  = "#FF7F00",
  color_banda  = "#FF7F00",
  titulo       = "CBI bajo | Deuda alta"
)

p3 <- hacer_grafico(
  irf_cbi_high_bL,
  color_linea  = "#377EB8",
  color_banda  = "#377EB8",
  titulo       = "CBI alto | Deuda media"
)

p4 <- hacer_grafico(
  irf_cbi_high_bH,
  color_linea  = "#4DAF4A",
  color_banda  = "#4DAF4A",
  titulo       = "CBI alto | Deuda alta"
)

# Imprimir individualmente
print(p1)
print(p2)
print(p3)
print(p4)

# Guardar individualmente
ggsave("IRF_CBI_bajo_deuda_media.png", p1, width = 7, height = 5, dpi = 300)
ggsave("IRF_CBI_bajo_deuda_alta.png",  p2, width = 7, height = 5, dpi = 300)
ggsave("IRF_CBI_alto_deuda_media.png", p3, width = 7, height = 5, dpi = 300)
ggsave("IRF_CBI_alto_deuda_alta.png",  p4, width = 7, height = 5, dpi = 300)

# Panel 2x2 para el paper
p_2x2 <- grid.arrange(
  p1, p2, p3, p4,
  ncol = 2,
  top  = "Respuesta TPM ante shock inflacionario — por CBI y nivel de deuda"
)

ggsave("IRF_CBI_panel2x2.png", p_2x2, width = 14, height = 10, dpi = 300)

cat("\nTodos los graficos guardados.\n")
