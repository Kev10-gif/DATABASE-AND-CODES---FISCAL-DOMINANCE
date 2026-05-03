############################################################
# 0. LIMPIAR ENTORNO Y PAQUETES
############################################################

rm(list = ls())

packages <- c("data.table", "readxl", "zoo", "fixest", "ggplot2", "gridExtra")

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
# 3. VARIABLES MACRO
############################################################

df[, pi  := 400 * (log(IPC) - log(shift(IPC, 1))), by = id]
df[, dep := 400 * (log(NER) - log(shift(NER, 1))), by = id]
df[, g   := 400 * (log(GDP) - log(shift(GDP, 1))), by = id]

df[, b   := GD]
df[, b_z := as.numeric(scale(b))]

df[, CBI := as.integer(CBI)]
df[, IF  := as.integer(IF)]

############################################################
# 4. LIMPIEZA
############################################################

df <- df[!is.na(pi) & !is.na(dep) & !is.na(g)]

############################################################
# 5. SHOCK INFLACIONARIO VÍA RESIDUOS AR(4)
############################################################

K <- 4

for(k in 1:K){
  df[, paste0("pi_L", k) := shift(pi, k), by = id]
}

df_ar <- df[complete.cases(df[, c("pi", paste0("pi_L", 1:K)), with = FALSE])]

ar_fit     <- feols(pi ~ pi_L1 + pi_L2 + pi_L3 + pi_L4 | id, data = df_ar)
df_ar[, shock_pi := residuals(ar_fit)]

df <- merge(df, df_ar[, .(id, qtr, shock_pi)], by = c("id", "qtr"), all.x = TRUE)

############################################################
# 6. REZAGOS DE VARIABLES DE CONTROL
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
# 7. FUNCIÓN LOCAL PROJECTIONS
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
    
    beta  <- ifelse("shock_pi"     %in% names(co), co["shock_pi"],     0)
    gamma <- ifelse("shock_pi:b_z" %in% names(co), co["shock_pi:b_z"], 0)
    
    irf_h <- beta + gamma * b_eval
    
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
      lo    = irf_h - 1.645 * se_h,
      hi    = irf_h + 1.645 * se_h,
      label = label
    )
  }
  
  return(rbindlist(results))
}

############################################################
# 8. SEPARAR MUESTRAS POR IF (forzado explícito)
############################################################

# Verificar distribución
cat("\nDistribución IF en df_est:\n")
print(table(df_est$IF))

# Forzar creación limpia de subgrupos por IF
if(exists("data_low"))  rm(data_low)
if(exists("data_high")) rm(data_high)

data_low  <- copy(df_est[IF == 0])
data_high <- copy(df_est[IF == 1])

# Verificar que son correctos
cat("\nVerificación data_low  (debe ser todo IF=0):", all(data_low$IF  == 0),
    "| n =", nrow(data_low), "\n")
cat("Verificación data_high (debe ser todo IF=1):", all(data_high$IF == 1),
    "| n =", nrow(data_high), "\n")

# Verificar que CBI tiene variación dentro de cada grupo (no están confundidos)
cat("\nCBI dentro de IF=0:\n"); print(table(data_low$CBI))
cat("CBI dentro de IF=1:\n");  print(table(data_high$CBI))

############################################################
# 9. NIVELES DE DEUDA: MEDIA Y ALTA
############################################################

b_med_fiscal  <- 0.0  # deuda en la media
b_high_fiscal <- 1.5  # deuda alta (~p75-p80)

cat("\nDeuda media  b_z =", b_med_fiscal,
    "->", round(mean(df_est$b, na.rm = TRUE), 1), "% GDP\n")
cat("Deuda alta   b_z =", b_high_fiscal,
    "->", round(mean(df_est$b, na.rm = TRUE) + b_high_fiscal * sd(df_est$b, na.rm = TRUE), 1), "% GDP\n")

############################################################
# 10. ESTIMAR IRFs
############################################################

cat("\nEstimando IF bajo | Deuda media...\n")
irf_if_low_bM  <- run_lp_irf(copy(data_low),  b_eval = b_med_fiscal,  label = "Low IF | Medium debt")

cat("Estimando IF bajo | Deuda alta...\n")
irf_if_low_bH  <- run_lp_irf(copy(data_low),  b_eval = b_high_fiscal, label = "Low IF | High debt")

cat("Estimando IF alto | Deuda media...\n")
irf_if_high_bM <- run_lp_irf(copy(data_high), b_eval = b_med_fiscal,  label = "High IF | Medium debt")

cat("Estimando IF alto | Deuda alta...\n")
irf_if_high_bH <- run_lp_irf(copy(data_high), b_eval = b_high_fiscal, label = "High IF | High debt")

irf_all <- rbindlist(list(
  irf_if_low_bM,
  irf_if_low_bH,
  irf_if_high_bM,
  irf_if_high_bH
))

cat("\nIRF estimados:\n")
print(irf_all)

############################################################
# 11. COLORES
############################################################

cols <- c(
  "Low IF | Medium debt"  = "#E41A1C",
  "Low IF | High debt"   = "#FF7F00",
  "High IF | Medium debt"  = "#377EB8",
  "High IF | High debt"   = "#4DAF4A"
)

############################################################
# 12. GRÁFICO CONJUNTO IRF
############################################################

p_irf <- ggplot(irf_all, aes(x = h, y = irf, color = label, fill = label)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  geom_line(size = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = cols) +
  scale_fill_manual(values  = cols) +
  scale_x_continuous(breaks = 0:12) +
  theme_classic(base_size = 13) +
  labs(
    x        = "Quarters",
    y        = "Response of the MPR",
    #title    = "Local Projections: Respuesta de la TPM ante shock inflacionario",
    #subtitle = "Heterogeneidad por integración financiera (IF) y dominancia fiscal",
    color    = "", fill = ""
  ) +
  theme(legend.position = "bottom")

print(p_irf)
ggsave("IRF_LP_IF_conjunto.png", p_irf, width = 10, height = 6, dpi = 300)

############################################################
# 13. GRÁFICO CONJUNTO CIRF
############################################################

irf_all[, cirf    := cumsum(irf), by = label]
irf_all[, cirf_lo := cumsum(lo),  by = label]
irf_all[, cirf_hi := cumsum(hi),  by = label]

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
    y        = " Accumulated Response of the MPR",
    #title    = "CIRF: Respuesta Acumulada de la TPM ante shock inflacionario",
    #subtitle = "Heterogeneidad por integración financiera (IF) y dominancia fiscal",
    color    = "", fill = ""
  ) +
  theme(legend.position = "bottom")

print(p_cirf)
ggsave("CIRF_LP_IF_conjunto.png", p_cirf, width = 10, height = 6, dpi = 300)

############################################################
# 14. GRÁFICOS SEPARADOS POR IF
############################################################

# --- IF BAJO ---
irf_low_if <- irf_all[label %in% c("IF bajo | Deuda media", "IF bajo | Deuda alta")]

cols_low <- c(
  "IF bajo | Deuda media" = "#2166AC",
  "IF bajo | Deuda alta"  = "#D6604D"
)

p_low <- ggplot(irf_low_if, aes(x = h, y = irf, color = label, fill = label)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20, color = NA) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = cols_low) +
  scale_fill_manual(values  = cols_low) +
  scale_x_continuous(breaks = 0:12) +
  coord_cartesian(ylim = c(-0.05, 0.30)) +
  theme_classic(base_size = 13) +
  labs(
    x        = "Horizonte (trimestres)",
    y        = "Respuesta TPM (pp)",
    title    = "IF Bajo (baja integración financiera)",
    subtitle = "Deuda media vs Deuda alta",
    color    = "", fill = ""
  ) +
  theme(legend.position = "bottom")

# --- IF ALTO ---
irf_high_if <- irf_all[label %in% c("IF alto | Deuda media", "IF alto | Deuda alta")]

cols_high <- c(
  "IF alto | Deuda media" = "#2166AC",
  "IF alto | Deuda alta"  = "#D6604D"
)

p_high <- ggplot(irf_high_if, aes(x = h, y = irf, color = label, fill = label)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20, color = NA) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = cols_high) +
  scale_fill_manual(values  = cols_high) +
  scale_x_continuous(breaks = 0:12) +
  coord_cartesian(ylim = c(-0.05, 0.30)) +
  theme_classic(base_size = 13) +
  labs(
    x        = "Horizonte (trimestres)",
    y        = "Respuesta TPM (pp)",
    title    = "IF Alto (alta integración financiera)",
    subtitle = "Deuda media vs Deuda alta",
    color    = "", fill = ""
  ) +
  theme(legend.position = "bottom")

print(p_low)
print(p_high)

ggsave("IRF_LP_IF_bajo_separado.png", p_low,  width = 7, height = 5, dpi = 300)
ggsave("IRF_LP_IF_alto_separado.png", p_high, width = 7, height = 5, dpi = 300)

# --- PANEL 2x1 ---
p_panel <- grid.arrange(
  p_low, p_high,
  ncol = 2,
  top  = "Respuesta TPM ante shock inflacionario: IF bajo vs IF alto"
)

ggsave("IRF_LP_IF_panel2x1.png", p_panel, width = 14, height = 5, dpi = 300)

cat("\nTodos los graficos guardados.\n")

############################################################
# 15. GRÁFICO DE GAMMA (efecto moderador de la deuda)
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
    
    g_res <- get_est("shock_pi:b_z")
    
    out[[h + 1]] <- data.table(
      h        = h,
      gamma    = g_res$est,
      gamma_lo = g_res$est - 1.645 * g_res$se,
      gamma_hi = g_res$est + 1.645 * g_res$se,
      label    = label
    )
  }
  rbindlist(out)
}

coefs_low_if  <- get_coefs_by_h(copy(data_low),  label = "Low IF")
coefs_high_if <- get_coefs_by_h(copy(data_high), label = "High IF")
coefs_all_if  <- rbindlist(list(coefs_low_if, coefs_high_if))

p_gamma <- ggplot(coefs_all_if, aes(x = h, y = gamma, color = label, fill = label)) +
  geom_ribbon(aes(ymin = gamma_lo, ymax = gamma_hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Low IF" = "#E41A1C", "High IF" = "#377EB8")) +
  scale_fill_manual(values  = c("Low IF" = "#E41A1C", "High IF" = "#377EB8")) +
  scale_x_continuous(breaks = 0:12) +
  theme_classic(base_size = 13) +
  labs(
    x     = "Quarter",
    y     = "γ_h (interaction shock x debt)",
    color = "", fill = ""
  ) +
  theme(legend.position = "bottom")

print(p_gamma)
ggsave("IRF_gamma_IF.png", p_gamma, width = 9, height = 5, dpi = 300)

cat("\nGráfico de gamma guardado.\n")

############################################################
# 16. TABLA ESTILO PAPER — CIRF en h=1, h=5, h=12
############################################################

# Necesitas el SE del CIRF — recalcular con se acumulado
# El SE del CIRF en h es sqrt(sum of variances) si asumes independencia
# Aproximación práctica: usar (hi - lo) / (2 * 1.645)

irf_all[, se := (hi - lo) / (2 * 1.645)]

# CIRF acumulado ya está en irf_all (sección 13)
# Recalcular SE acumulado por suma de varianzas
irf_all[, se_cirf := sqrt(cumsum(se^2)), by = label]

# Seleccionar horizontes de interés
horizontes <- c(1, 5, 12)

tabla_paper <- irf_all[h %in% horizontes, .(
  h,
  label,
  cirf   = round(cirf, 3),
  se_cirf = round(se_cirf, 3)
)]

# Formato largo: valor + (se) intercalados
tabla_paper[, valor_se := paste0(cirf, "\n(", se_cirf, ")")]

# Pivotear
tabla_wide <- dcast(tabla_paper, h ~ label, value.var = "valor_se")
tabla_wide[, Horizon := paste0(h, ifelse(h == 1, " quarter", " quarters"))]
tabla_wide[, h := NULL]
setcolorder(tabla_wide, c("Horizon", setdiff(names(tabla_wide), "Horizon")))

print(tabla_wide)
write.csv(tabla_wide, "CIRF_tabla_paper.csv", row.names = FALSE)
cat("\nTabla guardada.\n")