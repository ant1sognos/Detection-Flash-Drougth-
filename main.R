# main.R

thisfile <- if (interactive()) {
  rstudioapi::getActiveDocumentContext()$path
} else {
  commandArgs(trailingOnly = FALSE) %>% grep("--file=", ., value = TRUE) %>% sub("--file=", "", .)
}
if (length(thisfile) == 0 || is.na(thisfile)) thisfile <- "main.R"
setwd(dirname(thisfile))

source("setup.R")
source("config.R")
source("functions/data_preparation.R")
source("functions/analysis.R")
source("functions/visualization.R")
source("functions/spatialization.R")

library(openxlsx)

sheet_name_safe <- function(x, maxlen=31) {
  x <- gsub(" ", "_", x)
  x <- iconv(x, to="ASCII//TRANSLIT")
  if (nchar(x) > maxlen) x <- substr(x, 1, maxlen)
  x
}

stations              <- get_valid_stations()

# --- DEBUG : exécuter une seule station --------------------------
#target_id <- "705"               
#stations  <- dplyr::filter(stations, id == target_id)


#if (nrow(stations) == 0) {
#  stop("L'ID de station spécifiée n'existe pas dans la table 'stations'")
#}

 #Map of stations (with coastline)
coast_shapefile_path <- file.path("..", "02_Data", "metadata", "linea_costa_ATA0708.shp")
plot_stations_map(stations, plots_dir, coast_shapefile_path)

excel_dir <- file.path(plots_dir, "Excel_Exports")
dir.create(excel_dir, showWarnings=FALSE, recursive=TRUE)
excel_file_events <- file.path(excel_dir, "flash_droughts_summary.xlsx")
excel_file_spei   <- file.path(excel_dir, "spei_tables.xlsx")


all_flash_events       <- list()
all_spei               <- list()
all_classic_droughts   <- list()
all_yearly_counts      <- list()

# --- nouveaux conteneurs pour Events_Seasonality.xlsx ------------
flash_season_list   <- list()   # FD : station / date / season
drought_season_list <- list()   # D  : station / date / season
for (i in seq_len(nrow(stations))) {
  sid         <- stations$id[i]
  station_nom <- stations$STAZIONE[i]
  station_lat <- stations$latitude[i]
  station_dir <- paste0(sid, "_", station_nom)
  station_nom_short <- sheet_name_safe(substr(station_nom, 1, 15))
  
  cat("\n=== Station", station_dir, "===\n")
  
  files  <- prepare_station_data(sid)
  P      <- somme_P(files$P)
  Tmax   <- max_T_hebdo(files$Tmax)
  Tmin   <- min_T_hebdo(files$Tmin)
  Tmean  <- moy_T_hebdo(files$Tmean)
  
  raw_dates <- readr::read_csv(files$P, show_col_types=FALSE)[[1]]
  raw_dates <- as.Date(raw_dates)
  dates     <- seq.Date(
    from       = min(raw_dates),
    by         = "week",
    length.out = length(P)
  )
  
  df <- data.frame(
    Date          = dates,
    Precipitation = P,
    Tmax          = Tmax,
    Tmin          = Tmin,
    Tmean         = Tmean
  )
  df$PET     <- SPEI::hargreaves(df$Tmin, df$Tmax, lat=station_lat, na.rm=TRUE)
  df$Deficit <- df$Precipitation - df$PET
  
  plot_before_spei(df, station_dir, station_nom)
  
  spei1 <- calcul_spei(df$Deficit, dates,  4, list(distribution="log-Logistic", fit="ub-pwm", na.rm=TRUE))
  spei3 <- calcul_spei(df$Deficit, dates, 12, list(distribution="log-Logistic", fit="ub-pwm", na.rm=TRUE))
  spei6 <- calcul_spei(df$Deficit, dates, 24, list(distribution="log-Logistic", fit="ub-pwm", na.rm=TRUE))
  

  spei_df <- detect_and_enrich_flash_droughts(spei1, dates) %>% 
    dplyr::mutate(Flash_drought = !is.na(id))
  

  # DÉTECTION DES FLASH DROUGHTS
  flash_events <- resume_flash_events(spei_df)
  
  # DÉTECTION DES SÉCHERESSES CLASSIQUES
  classic_droughts <- detect_secheresses(spei_df$SPEI_1m, spei_df$Date, seuil=-1.28, duree_min=1)
  n_fd <- nrow(flash_events)
  n_droughts <- nrow(classic_droughts)
  
  
  # Ajout dans all_classic_droughts pour résumé global
  all_classic_droughts[[station_dir]] <- classic_droughts
  
  # Ajout des colonnes au tableau événements station
  flash_events$Total_FD_Station <- n_fd
  flash_events$Total_Droughts_Station <- n_droughts
  flash_events$Ratio_FD_Over_Droughts <- if (n_droughts > 0) n_fd / n_droughts else NA_real_
  
  export_and_plot_severity(flash_events, station_dir, station_nom)
  
  ## ---- AJOUTS POUR SEASONALITY --------------------------------
  season_from_date <- function(d) {
    m <- lubridate::month(d)
    dplyr::case_when(
      m %in% c(12,1,2) ~ "Winter",
      m %in% c(3,4,5)  ~ "Spring",
      m %in% c(6,7,8)  ~ "Summer",
      TRUE             ~ "Autumn"
    )
  }
  
  # Flash-Droughts
  fd_dates <- if ("start_maturity" %in% names(flash_events))
    flash_events$start_maturity
  else flash_events$start_develop
  flash_season_list[[station_dir]] <-
    tibble(
      station_id    = sid,
      station_name  = station_nom,
      maturity_date = as.Date(fd_dates),
      season        = season_from_date(fd_dates)
    ) |> filter(!is.na(maturity_date))
  
  # Droughts classiques
  if (nrow(classic_droughts)) {
    drought_season_list[[station_dir]] <-
      tibble(
        station_id    = sid,
        station_name  = station_nom,
        maturity_date = as.Date(classic_droughts$debut),
        season        = season_from_date(classic_droughts$debut)
      ) |> filter(!is.na(maturity_date))
  }
  
  # Compilation globale

  all_flash_events[[station_dir]] <- cbind(station_id=sid, station_name=station_nom, flash_events)
  all_spei[[station_dir]] <- cbind(station_id=sid, station_name=station_nom, spei_df)
  plot_timeline_severity(flash_events, station_dir, station_nom)
  
  ## --- Excel : Résumés FD ---
  if (file.exists(excel_file_events)) {
    wb_events <- openxlsx::loadWorkbook(excel_file_events)
  } else {
    wb_events <- openxlsx::createWorkbook()
  }
  sheetname_events <- sheet_name_safe(paste0(sid, "_", station_nom_short, "_ev"), maxlen=31)
  if (sheetname_events %in% names(wb_events)) openxlsx::removeWorksheet(wb_events, sheetname_events)
  openxlsx::addWorksheet(wb_events, sheetname_events)
  openxlsx::writeData(wb_events, sheet = sheetname_events, x = flash_events)
  openxlsx::saveWorkbook(wb_events, excel_file_events, overwrite=TRUE)
  
  ## --- Excel : Tables SPEI ---
  if (file.exists(excel_file_spei)) {
    wb_spei <- openxlsx::loadWorkbook(excel_file_spei)
  } else {
    wb_spei <- openxlsx::createWorkbook()
  }
  sheetname_spei <- sheet_name_safe(paste0(sid, "_", station_nom_short, "_spei"), maxlen=31)
  if (sheetname_spei %in% names(wb_spei)) openxlsx::removeWorksheet(wb_spei, sheetname_spei)
  openxlsx::addWorksheet(wb_spei, sheetname_spei)
  openxlsx::writeData(wb_spei, sheet = sheetname_spei, x = spei_df)
  openxlsx::saveWorkbook(wb_spei, excel_file_spei, overwrite=TRUE)
  
  ## --- Graphiques ---
  spei_long <- spei_df %>%
    dplyr::select(Date, SPEI_1m) %>%
    tidyr::pivot_longer(-Date, names_to="Scale", values_to="SPEI") %>%
    dplyr::mutate(Condition = ifelse(SPEI>=0, "Wet", "Drought"))
  plot_spei_multiscale(spei_long, station_dir, station_nom)
  
  if (nrow(flash_events) > 0) {
    plot_flash_events(flash_events, spei_df, station_dir, station_nom)
  }
  plot_seasonal(spei_df, station_dir, station_nom)
  
  # Comptage annuel par station
  fd_by_year <- flash_events %>%
    dplyr::mutate(year = lubridate::year(start_develop)) %>%
    dplyr::count(year, name = "nb_flash_droughts") %>%
    dplyr::mutate(station_id = sid, station_name = station_nom)
  
  droughts_by_year <- classic_droughts %>%
    dplyr::mutate(year = lubridate::year(debut)) %>%
    dplyr::count(year, name = "nb_droughts") %>%
    dplyr::mutate(station_id = sid, station_name = station_nom)
  
  yearly_summary <- full_join(droughts_by_year, fd_by_year,
                              by = c("station_id", "station_name", "year")) %>%
    dplyr::select(station_id, station_name, year, nb_droughts, nb_flash_droughts) %>%
    dplyr::arrange(year) %>%
    replace_na(list(nb_droughts = 0, nb_flash_droughts = 0))
  
  if (!exists("all_yearly_counts")) all_yearly_counts <- list()
  all_yearly_counts[[station_dir]] <- yearly_summary
  
  
  
}

## 4) FICHIER SAISONNALITÉ 
## ----------------------------------------------------------------
season_file <- file.path(plots_dir, "Excel_Exports", "Events_Seasonality.xlsx")
wb_season   <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb_season, "Flash_Droughts")
openxlsx::writeData(
  wb_season, "Flash_Droughts",
  dplyr::bind_rows(flash_season_list)
)

openxlsx::addWorksheet(wb_season, "Droughts")
openxlsx::writeData(
  wb_season, "Droughts",
  dplyr::bind_rows(drought_season_list)
)

openxlsx::saveWorkbook(wb_season, season_file, overwrite = TRUE)
cat("✅ Classeur Events_Seasonality.xlsx créé :", season_file, "\n")

# Compilation globale (feuilles globales dans chaque Excel)
all_flash_events_df <- do.call(rbind, all_flash_events)
all_spei_df <- do.call(rbind, all_spei)

if (file.exists(excel_file_events)) {
  wb_events <- openxlsx::loadWorkbook(excel_file_events)
} else {
  wb_events <- openxlsx::createWorkbook()
}
if ("All_Stations_Events" %in% names(wb_events)) openxlsx::removeWorksheet(wb_events, "All_Stations_Events")
openxlsx::addWorksheet(wb_events, "All_Stations_Events")
openxlsx::writeData(wb_events, "All_Stations_Events", all_flash_events_df)
openxlsx::saveWorkbook(wb_events, excel_file_events, overwrite=TRUE)

if (file.exists(excel_file_spei)) {
  wb_spei <- openxlsx::loadWorkbook(excel_file_spei)
} else {
  wb_spei <- openxlsx::createWorkbook()
}
if ("All_Stations_SPEI" %in% names(wb_spei)) openxlsx::removeWorksheet(wb_spei, "All_Stations_SPEI")
openxlsx::addWorksheet(wb_spei, "All_Stations_SPEI")
openxlsx::writeData(wb_spei, "All_Stations_SPEI", all_spei_df)
openxlsx::saveWorkbook(wb_spei, excel_file_spei, overwrite=TRUE)

# ---- EXPORT FICHIER RÉCAPITULATIF SÉPARÉ ----
summary_counts <- data.frame(
  station_id   = stations$id,
  station_name = stations$STAZIONE,
  total_flash_droughts = sapply(all_flash_events, nrow),
  total_droughts = NA_integer_
)
for(i in seq_len(nrow(summary_counts))) {
  sid <- summary_counts$station_id[i]
  index <- which(names(all_classic_droughts) == paste0(sid, "_", stations$STAZIONE[i]))
  if(length(index) == 1) {
    summary_counts$total_droughts[i] <- nrow(all_classic_droughts[[index]])
  }
}

summary_counts$ratio_flash_total <- with(summary_counts, 
                                         ifelse(total_droughts > 0, total_flash_droughts / total_droughts, NA_real_)
)
summary_excel_file <- file.path(excel_dir, "Summary_FD_counts.xlsx")
openxlsx::write.xlsx(summary_counts, summary_excel_file, rowNames=FALSE)

# === Export du fichier résumé annuel ===
if (exists("all_yearly_counts")) {
  yearly_df_all <- dplyr::bind_rows(all_yearly_counts)
  yearly_file <- file.path(excel_dir, "Yearly_FD_Drought_Counts.xlsx")
  openxlsx::write.xlsx(yearly_df_all, yearly_file, rowNames = FALSE)
  cat("\n Fichier par année exporté dans :", yearly_file, "\n")
}


cat("\n=> Résumé global exporté dans :", summary_excel_file, "\n")
cat("Terminé !\n")
