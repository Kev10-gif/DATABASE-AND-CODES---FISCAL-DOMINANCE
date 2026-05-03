REPLICATION PACKAGE - PANEL TAYLOR RULES / FISCAL DOMINANCE

CÓMO CORRERLO
1. Abre RStudio.
2. Abre RUN_ALL.R.
3. Presiona Source.
4. Los resultados aparecerán en la carpeta outputs/.

ARCHIVO DE DATOS
El código espera que el Excel se llame exactamente:
PANEL1_COMPLETO(1).xlsx

Este paquete incluye una copia del Excel para que pueda correr directamente.
Si reemplazas el Excel, conserva el mismo nombre o cambia file_xlsx en 00_setup_data.R.

ARCHIVOS PRINCIPALES
00_setup_data.R        Prepara la base: logs, inflación, depreciación, GDP growth, output gap HP, rezagos e interacciones.
01_table_1_advanced.R Tabla 1: advanced economies.
02_table_2_emerging.R Tabla 2: emerging economies.
03_table_3_CBI.R      Tabla 3a y 3b: CBI como moderador.
04_table_4_FI.R       Tabla 4a y 4b: financial integration como moderador.
RUN_ALL.R             Corre todo en orden y consolida resultados.

OUTPUT PRINCIPAL
outputs/ALL_TABLES_REPLICATION_RESULTS.xlsx

METODOLOGÍA CODIFICADA
- Panel FE por país.
- Variable dependiente: monetary policy rate (mpr).
- Rezago: mpr_lag1.
- Inflación: 400 * Δlog(IPC).
- Depreciación cambiaria: 400 * Δlog(NER).
- Crecimiento del PBI: 400 * Δlog(GDP).
- Output gap: HP filter sobre log(GDP), lambda = 1600.
- Deuda: GD.
- Interacción fiscal: GD * inflation.
- Moderadores: GD * inflation * CBI y GD * inflation * IF.
- Efectos de largo plazo: beta / (1 - rho).
- Phi(b): (beta_inflation + beta_interaction * b) / (1 - rho).
- Errores estándar: PCSE vía plm::vcovBK por defecto.

NOTA
El Excel usa la variable IF para financial integration. En las tablas se etiqueta como FI.
