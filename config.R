# config.R

# Racine du projet
tool_root     <- here::here()

# Dossiers de données
data_dir      <- file.path(tool_root, "02_Data")
precip_dir    <- file.path(data_dir, "Precipitation")
tmax_dir      <- file.path(data_dir, "Tmax")
tmin_dir      <- file.path(data_dir, "Tmin")
tmean_dir     <- file.path(data_dir, "Tmean")
metadata_dir  <- file.path(data_dir, "metadata")

# Dossier de sortie
plots_dir     <- file.path(tool_root, "03_Plots")

# Fichiers métadonnées
info_ts_file  <- file.path(metadata_dir, "00_INFO_TS_for_ta_hmax_C.csv")
station_dict  <- file.path(metadata_dir, "Stazioni_SIAS_def_33n.csv")
