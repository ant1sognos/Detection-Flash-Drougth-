# setup.R
# Installation et chargement des packages requis

required_pkgs <- c(
  "readr", "readxl", "openxlsx", "lubridate", "dplyr", "tidyr","viridis","av","purrr",
  "ggplot2", "SPEI", "xts", "zoo", "ggrepel", "scales","devtools","patchwork","fpc","concaveman","data.table","clusterCrit",
  "envalysis", "Bolstad2", "hydroTSM", "rstudioapi", "here","sf","scales","furrr","progressr","dbscan","geosphere","stringr",
  "gganimate", "transformr","gifski","ggtext"
)
for(pkg in required_pkgs){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

