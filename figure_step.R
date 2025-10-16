# =================================================================== #
#   Figure 1 – Schéma méthodologique en 4 panels (a–d)                #
# =================================================================== #

## 0. Packages -------------------------------------------------------
pkgs <- c("ggplot2","patchwork","png","grid")
install.packages(setdiff(pkgs, installed.packages()[,"Package"]),
                 dependencies = TRUE)
lapply(pkgs, library, character.only = TRUE)

## 1. Chemins & dossier Figure ---------------------------------------
project_root <- "C:/Flash_Drougth/Sicilia_data/Sicilia_V2"  
fig_dir      <- file.path(project_root, "03_Plots", "Figure")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

## 2. Paramètres modulables ------------------------------------------
# Chemin vers votre image .png (SPEI series)
spei_png <- file.path(
  project_root,
  "03_Plots","776_Marsala","FlashDroughts_Details","Marsala_FD_FD0001.png"
)


# Texte des 4 étapes (modifiable)
step_a_text <- paste0(
  "a) Agrégation des données SPEI\n",
  "(P, PET → ΔSPEI hebdo)\n",
  "– SPEI 1-mois, fenêtre mobile 4 sem."
)
step_b_text <- paste0(
  "b) Détection des points FD\n",
  "Critères : ΔSPEI ≤ –2 & SPEI ≤ –1.28\n",
  "Fenêtre développement ≥ 4 sem."
)
step_c_text <- paste0(
  "c) Clustering spatio-temporel\n",
  "ST-DBSCAN(εₛ=40 km, εₜ=7 j, MinPts=7)\n",
  "→ catalogue station-semaines"
)
step_d_text <- paste0(
  "d) Post-processing spatial\n",
  "- Empreintes hebdo (hulls)\n",
  "- Centroïdes & métriques\n",
  "- Cartographies finales"
)

## 3. Lire l'image SPEI ----------------------------------------------
spei_img  <- png::readPNG(spei_png)
spei_grob <- grid::rasterGrob(
  spei_img,
  width  = unit(1, "npc"),
  height = unit(1, "npc")
)

## 4. Créer les panels -----------------------------------------------
# Panel a: l'image SPEI
p_a <- ggplot() +
  annotation_custom(
    spei_grob,
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = Inf
  ) +
  labs(tag = "a)") +
  theme_void() +
  theme(
    plot.tag.position = c(0.02, 0.98),
    plot.tag          = element_text(size = 18, face = "bold")
  )

# Fonction générique pour panels b–d
make_panel <- function(tag, text) {
  ggplot() +
    annotate(
      "text", x = 0, y = 1, label = tag,
      hjust = 0, vjust = 1, size = 6, fontface = "bold"
    ) +
    annotate(
      "text", x = 0, y = 0.9, label = text,
      hjust = 0, vjust = 1, size = 4.5, lineheight = 1.2
    ) +
    theme_void() +
    xlim(0,1) + ylim(0,1)
}

p_b <- make_panel("b)", step_b_text)
p_c <- make_panel("c)", step_c_text)
p_d <- make_panel("d)", step_d_text)

## 5. Assemblage et export -------------------------------------------
fig1 <- (p_a + p_b) / (p_c + p_d) +
  plot_layout(guides = "collect") &
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave(
  filename = file.path(fig_dir, "Figure1_Methodology.png"),
  plot     = fig1,
  width    = 10, height = 12,
  units    = "in", dpi = 300
)
