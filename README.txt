REPLICATION PACKAGE – PANEL TAYLOR RULES / FISCAL DOMINANCE
HOW TO RUN (MAIN RESULTS)
Open RStudio.
Open RUN_ALL.R.
Click Source.
Results will be saved in the outputs/ folder.
DATA FILE

The code expects the Excel file to be named exactly:
PANEL1_COMPLETO(1).xlsx

A copy of the dataset is included in this package.

If you replace the file, either:

Keep the same name, or
Update file_xlsx inside 00_setup_data.R
MAIN FILES
00_setup_data.R → Data preparation
01_table_1_advanced.R → Advanced economies
02_table_2_emerging.R → Emerging economies
03_table_3_CBI.R → CBI results
04_table_4_FI.R → Financial integration results
RUN_ALL.R → Runs everything
MAIN OUTPUT

outputs/ALL_TABLES_REPLICATION_RESULTS.xlsx

EXTENSIONS: LOCAL PROJECTIONS
FILES
Local_Projection.R → Baseline local projections (IRFs)
LP_extension_CBI.R → IRF plotting (BVAR vs DSGE comparison)
LP_extension2.R → Extended LP results with additional splits and outputs
