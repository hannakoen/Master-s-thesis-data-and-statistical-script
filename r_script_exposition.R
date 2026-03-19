library(readxl)
data <- read_excel(file.choose(), sheet = "Tabelle7")
num_cols <- sapply(data, is.numeric)
data[num_cols] <- lapply(data[num_cols], function(x) ifelse(is.na(x), 0, x))
data <- as.data.frame(lapply(data, function(x) {
  if(all(x %in% c(0, NA))) {return(as.numeric(x))} else {return(x)}}))
only_na_cols <- sapply(data, function(x) all(is.na(x)))
only_na_cols
data[only_na_cols] <- lapply(data[only_na_cols], function(x) rep(0, length(x)))

#### Relative Veränderung der Exposition anhand von Expositionsindizes für die einzelnen Kategorien (Siedlung, landwirtschaftliche FLäche, Infrastrukturelemente) ####
## Pakete:
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(tidyverse)
library(patchwork)

## 1. Variablenlisten definieren: ####

# Siedlung
Siedlung_prim_aktuell_vars <- c(
  "Siedlung_Haeuser_prim_aktuell",
  "Siedlung_Haeuser_mit_Farmland_prim_aktuell",
  "Siedlung_kritische_Infra_prim_aktuell",
  "Siedlung_Tourismus_prim_aktuell",
  "Siedlung_Industrie_prim_aktuell"
)

Siedlung_prim_historisch_vars <- c(
  "Siedlung_Haeuser_prim_historisch",
  "Siedlung_Haeuser_mit_Farmland_prim_historisch",
  "Siedlung_kritische_Infra_prim_historisch",
  "Siedlung_Tourismus_prim_historisch",
  "Siedlung_Industrie_prim_historisch"
)

Siedlung_sek_aktuell_vars <- c(
  "Siedlung_Haeuser_sek_aktuell",
  "Siedlung_Haeuser_mit_Farmland_sek_aktuell",
  "Siedlung_kritische_Infra_sek_aktuell",
  "Siedlung_Tourismus_sek_aktuell",
  "Siedlung_Industrie_sek_aktuell"
)

Siedlung_sek_historisch_vars <- c(
  "Siedlung_Haeuser_sek_historisch",
  "Siedlung_Haeuser_mit_Farmland_sek_historisch",
  "Siedlung_kritische_Infra_sek_historisch",
  "Siedlung_Tourismus_sek_historisch",
  "Siedlung_Industrie_sek_historisch"
)

# Landwirtschaft
Landwirtschaft_prim_aktuell_vars <- c(
  "Landwirtschaft_Farmland_prim_aktuell",
  "Landwirtschaft_Farmland_mit_Haeusern_prim_aktuell"
)

Landwirtschaft_prim_historisch_vars <- c(
  "Landwirtschaft_Farmland_prim_historisch",
  "Landwirtschaft_Farmland_mit_Haeusern_prim_historisch"
)

Landwirtschaft_sek_aktuell_vars <- c(
  "Landwirtschaft_Farmland_sek_aktuell",
  "Landwirtschaft_Farmland_mit_Haeusern_sek_aktuell"
)

Landwirtschaft_sek_historisch_vars <- c(
  "Landwirtschaft_Farmland_sek_historisch",
  "Landwirtschaft_Farmland_mit_Haeusern_sek_historisch"
)

# Infrastruktur
Infrastruktur_prim_aktuell_vars <- c(
  "Infrastruktur_Straße_prim_aktuell",
  "Infrastruktur_Bruecke_prim_aktuell",
  "Infrastruktur_Erosion_prim_aktuell",
  "Infrastruktur_Energie_Wasser_prim_aktuell"
)

Infrastruktur_prim_historisch_vars <- c(
  "Infrastruktur_Straße_prim_historisch",
  "Infrastruktur_Bruecke_prim_historisch",
  "Infrastruktur_Erosion_prim_historisch",
  "Infrastruktur_Energie_Wasser_prim_historisch"
)

Infrastruktur_sek_aktuell_vars <- c(
  "Infrastruktur_Straße_sek_aktuell",
  "Infrastruktur_Bruecke_sek_aktuell",
  "Infrastruktur_Erosion_sek_aktuell",
  "Infrastruktur_Energie_Wasser_sek_aktuell"
)

Infrastruktur_sek_historisch_vars <- c(
  "Infrastruktur_Straße_sek_historisch",
  "Infrastruktur_Bruecke_sek_historisch",
  "Infrastruktur_Erosion_sek_historisch",
  "Infrastruktur_Energie_Wasser_sek_historisch"
)

## 2. Alle Rohvariablen sammeln: ####

alle_rohvariablen <- c(
  Siedlung_prim_aktuell_vars,
  Siedlung_prim_historisch_vars,
  Siedlung_sek_aktuell_vars,
  Siedlung_sek_historisch_vars,
  Landwirtschaft_prim_aktuell_vars,
  Landwirtschaft_prim_historisch_vars,
  Landwirtschaft_sek_aktuell_vars,
  Landwirtschaft_sek_historisch_vars,
  Infrastruktur_prim_aktuell_vars,
  Infrastruktur_prim_historisch_vars,
  Infrastruktur_sek_aktuell_vars,
  Infrastruktur_sek_historisch_vars
)

## 3. Neue Tabelle mit Metadaten und z-standardisierten Werten und Indizes erstellen: ####

data_indizes <- data %>%
  slice(1:16) %>%
  select(
    Nr.,
    GLOF.Name,
    Land,
    Bevölkerungswachstum..1960.2023..in..,
    Jahr,
    Jahr_historisch,
    Gebirge,
    Gletschersee..m..,
    all_of(alle_rohvariablen)
  ) %>%
  mutate(
    across(
      all_of(alle_rohvariablen),
      ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
      .names = "{.col}_z"
    )
  ) %>%
  mutate(
    Index_Siedlung_prim_aktuell = rowMeans(
      across(all_of(paste0(Siedlung_prim_aktuell_vars, "_z"))),
      na.rm = TRUE
    ),
    Index_Siedlung_prim_historisch = rowMeans(
      across(all_of(paste0(Siedlung_prim_historisch_vars, "_z"))),
      na.rm = TRUE
    ),
    Index_Siedlung_sek_aktuell = rowMeans(
      across(all_of(paste0(Siedlung_sek_aktuell_vars, "_z"))),
      na.rm = TRUE
    ),
    Index_Siedlung_sek_historisch = rowMeans(
      across(all_of(paste0(Siedlung_sek_historisch_vars, "_z"))),
      na.rm = TRUE
    ),
    
    Index_Landwirtschaft_prim_aktuell = rowMeans(
      across(all_of(paste0(Landwirtschaft_prim_aktuell_vars, "_z"))),
      na.rm = TRUE
    ),
    Index_Landwirtschaft_prim_historisch = rowMeans(
      across(all_of(paste0(Landwirtschaft_prim_historisch_vars, "_z"))),
      na.rm = TRUE
    ),
    Index_Landwirtschaft_sek_aktuell = rowMeans(
      across(all_of(paste0(Landwirtschaft_sek_aktuell_vars, "_z"))),
      na.rm = TRUE
    ),
    Index_Landwirtschaft_sek_historisch = rowMeans(
      across(all_of(paste0(Landwirtschaft_sek_historisch_vars, "_z"))),
      na.rm = TRUE
    ),
    
    Index_Infrastruktur_prim_aktuell = rowMeans(
      across(all_of(paste0(Infrastruktur_prim_aktuell_vars, "_z"))),
      na.rm = TRUE
    ),
    Index_Infrastruktur_prim_historisch = rowMeans(
      across(all_of(paste0(Infrastruktur_prim_historisch_vars, "_z"))),
      na.rm = TRUE
    ),
    Index_Infrastruktur_sek_aktuell = rowMeans(
      across(all_of(paste0(Infrastruktur_sek_aktuell_vars, "_z"))),
      na.rm = TRUE
    ),
    Index_Infrastruktur_sek_historisch = rowMeans(
      across(all_of(paste0(Infrastruktur_sek_historisch_vars, "_z"))),
      na.rm = TRUE
    )
  ) %>%
  select(
    Nr.,
    GLOF.Name,
    Land,
    Bevölkerungswachstum..1960.2023..in..,
    Jahr,
    Jahr_historisch,
    Gebirge,
    Gletschersee..m..,
    ends_with("_z"),
    starts_with("Index_")
  )

View(data_indizes)

## 4. Hilfsfunktion für Scatterplots: ####

plot_index_scatter <- function(df, x_var, y_var, titel, x_lab, y_lab) {
  
  plot_df <- df %>%
    transmute(
      GLOF.Name,
      Index_historisch = .data[[x_var]],
      Index_aktuell    = .data[[y_var]]
    ) %>%
    mutate(
      Veränderung = case_when(
        Index_aktuell > Index_historisch ~ "gestiegen",
        Index_aktuell < Index_historisch ~ "gesunken",
        TRUE                             ~ "gleich"
      )
    )
  
  lims <- range(
    plot_df$Index_historisch,
    plot_df$Index_aktuell,
    na.rm = TRUE
  )
  
  ggplot(
    plot_df,
    aes(
      x = Index_historisch,
      y = Index_aktuell,
      color = Veränderung,
      label = GLOF.Name
    )
  ) +
    geom_point(size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_text_repel(
      size = 3,
      family = "Times New Roman",
      segment.colour = NA,
      max.overlaps = Inf
    ) +
    scale_color_manual(
      values = c(
        "gestiegen" = "deeppink",
        "gesunken" = "darkgreen",
        "gleich"   = "grey50"
      )
    ) +
    coord_equal(xlim = lims, ylim = lims) +
    theme_minimal(base_family = "Times New Roman") +
    labs(
      title = titel,
      x = x_lab,
      y = y_lab,
      color = "Entwicklung"
    )
}

## 5. Scatterplots: primär historisch vs. aktuell: ####

# Siedlung primär
Scatterplot_Indizes_Siedlung <- plot_index_scatter(
  df    = data_indizes,
  x_var = "Index_Siedlung_prim_historisch",
  y_var = "Index_Siedlung_prim_aktuell",
  titel = "Siedlung: Historisch vs. aktuell",
  x_lab = "Historischer  primärer Expositionsindex",
  y_lab = "Aktueller  primärer Expositionsindex"
)

Scatterplot_Indizes_Siedlung

# Landwirtschaft primär
Scatterplot_Indizes_Landwirtschaft <- plot_index_scatter(
  df    = data_indizes,
  x_var = "Index_Landwirtschaft_prim_historisch",
  y_var = "Index_Landwirtschaft_prim_aktuell",
  titel = "Landwirtschaft: Historisch vs. aktuell",
  x_lab = "Historischer primärer  Expositionsindex",
  y_lab = "Aktueller primärer Expositionsindex"
)

Scatterplot_Indizes_Landwirtschaft

# Infrastruktur primär
Scatterplot_Indizes_Infrastruktur <- plot_index_scatter(
  df    = data_indizes,
  x_var = "Index_Infrastruktur_prim_historisch",
  y_var = "Index_Infrastruktur_prim_aktuell",
  titel = "Infrastruktur: Historisch vs. aktuell",
  x_lab = "Historischer primärer Expositionsindex",
  y_lab = "Aktueller primärer Expositionsindex"
)

Scatterplot_Indizes_Infrastruktur

## 6. Scatterplots: sekundär historisch vs. aktuell: ####

# Siedlung sekundär
Scatterplot_Indizes_Siedlung_sekundär <- plot_index_scatter(
  df    = data_indizes,
  x_var = "Index_Siedlung_sek_historisch",
  y_var = "Index_Siedlung_sek_aktuell",
  titel = "Siedlung: Historisch vs. aktuell",
  x_lab = "Historischer sekundärer Expositionsindex",
  y_lab = "Aktueller sekundärer Expositionsindex"
)

Scatterplot_Indizes_Siedlung_sekundär

# Landwirtschaft sekundär
Scatterplot_Indizes_Landwirtschaft_sekundär <- plot_index_scatter(
  df    = data_indizes,
  x_var = "Index_Landwirtschaft_sek_historisch",
  y_var = "Index_Landwirtschaft_sek_aktuell",
  titel = "Landwirtschaft: Historisch vs. aktuell",
  x_lab = "Historischer sekundärer Expositionsindex",
  y_lab = "Aktueller sekundärer Expositionsindex"
)

Scatterplot_Indizes_Landwirtschaft_sekundär

# Infrastruktur sekundär
Scatterplot_Indizes_Infrastruktur_sekundär <- plot_index_scatter(
  df    = data_indizes,
  x_var = "Index_Infrastruktur_sek_historisch",
  y_var = "Index_Infrastruktur_sek_aktuell",
  titel = "Infrastruktur: Historisch vs. aktuell",
  x_lab = "Historischer sekundärer Expositionsindex",
  y_lab = "Aktueller sekundärer Expositionsindex"
)

Scatterplot_Indizes_Infrastruktur_sekundär

#### Absolute Veränderung der Exposition anhand von Expositionsindizes #####

## 1. Neue Tabelle nur für Siedlung mit gemeinsamer Standardisierung für aktuell und historisch: ####

data_indizes_abs_siedlung <- data %>%
  slice(1:16) %>%
  select(
    Nr.,
    GLOF.Name,
    Land,
    Bevölkerungswachstum..1960.2023..in..,
    Jahr,
    Jahr_historisch,
    Gebirge,
    Gletschersee..m..,
    all_of(c(
      Siedlung_prim_historisch_vars,
      Siedlung_prim_aktuell_vars,
      Siedlung_sek_historisch_vars,
      Siedlung_sek_aktuell_vars
    ))
  )

View(data_indizes_abs_siedlung)

## 2. Gemeinsame z-Standardisierung für primär: ####

for (i in seq_along(Siedlung_prim_historisch_vars)) {
  
  hist_var <- Siedlung_prim_historisch_vars[i]
  akt_var  <- Siedlung_prim_aktuell_vars[i]
  
  gemeinsame_werte <- c(
    data_indizes_abs_siedlung[[hist_var]],
    data_indizes_abs_siedlung[[akt_var]]
  )
  
  mu <- mean(gemeinsame_werte, na.rm = TRUE)
  sdv <- sd(gemeinsame_werte, na.rm = TRUE)
  
  data_indizes_abs_siedlung[[paste0(hist_var, "_zabs")]] <-
    (data_indizes_abs_siedlung[[hist_var]] - mu) / sdv
  
  data_indizes_abs_siedlung[[paste0(akt_var, "_zabs")]] <-
    (data_indizes_abs_siedlung[[akt_var]] - mu) / sdv
}

## 3. Gemeinsame z-Standardisierung für sekundär: ####

for (i in seq_along(Siedlung_sek_historisch_vars)) {
  
  hist_var <- Siedlung_sek_historisch_vars[i]
  akt_var  <- Siedlung_sek_aktuell_vars[i]
  
  gemeinsame_werte <- c(
    data_indizes_abs_siedlung[[hist_var]],
    data_indizes_abs_siedlung[[akt_var]]
  )
  
  mu <- mean(gemeinsame_werte, na.rm = TRUE)
  sdv <- sd(gemeinsame_werte, na.rm = TRUE)
  
  data_indizes_abs_siedlung[[paste0(hist_var, "_zabs")]] <-
    (data_indizes_abs_siedlung[[hist_var]] - mu) / sdv
  
  data_indizes_abs_siedlung[[paste0(akt_var, "_zabs")]] <-
    (data_indizes_abs_siedlung[[akt_var]] - mu) / sdv
}

## 4. Indizes berechnen: ####

data_indizes_abs_siedlung <- data_indizes_abs_siedlung %>%
  mutate(
    Index_Siedlung_prim_historisch_abs = rowMeans(
      across(all_of(paste0(Siedlung_prim_historisch_vars, "_zabs"))),
      na.rm = TRUE
    ),
    Index_Siedlung_prim_aktuell_abs = rowMeans(
      across(all_of(paste0(Siedlung_prim_aktuell_vars, "_zabs"))),
      na.rm = TRUE
    ),
    Index_Siedlung_sek_historisch_abs = rowMeans(
      across(all_of(paste0(Siedlung_sek_historisch_vars, "_zabs"))),
      na.rm = TRUE
    ),
    Index_Siedlung_sek_aktuell_abs = rowMeans(
      across(all_of(paste0(Siedlung_sek_aktuell_vars, "_zabs"))),
      na.rm = TRUE
    )
  )

## 5. Scatterplot erstellen für primäre Siedlungsexposition: ####

df_siedlung_abs <- data_indizes_abs_siedlung %>%
  transmute(
    GLOF.Name,
    Index_historisch = Index_Siedlung_prim_historisch_abs,
    Index_aktuell    = Index_Siedlung_prim_aktuell_abs
  ) %>%
  mutate(
    Veränderung = case_when(
      Index_aktuell > Index_historisch ~ "gestiegen",
      Index_aktuell < Index_historisch ~ "gesunken",
      TRUE                             ~ "gleich"
    )
  )

lims <- range(
  df_siedlung_abs$Index_historisch,
  df_siedlung_abs$Index_aktuell,
  na.rm = TRUE
)

Scatterplot_Indizes_Siedlung_abs <- ggplot(
  df_siedlung_abs,
  aes(
    x = Index_historisch,
    y = Index_aktuell,
    color = Veränderung,
    label = GLOF.Name
  )
) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_text_repel(
    size = 3,
    family = "Times New Roman",
    segment.colour = NA,
    max.overlaps = Inf
  ) +
  scale_color_manual(values = c(
    "gestiegen" = "deeppink",
    "gesunken" = "darkgreen",
    "gleich"   = "grey50"
  )) +
  coord_equal(xlim = lims, ylim = lims) +
  theme_minimal(base_family = "Times New Roman") +
  labs(
    title = "Siedlung (absolute Standardisierung): Historisch vs. aktuell",
    x = "Historischer primärer Expositionsindex",
    y = "Aktueller primärer Expositionsindex",
    color = "Entwicklung"
  )

Scatterplot_Indizes_Siedlung_abs

## 6. Scatterplot erstellen für sekundäre Siedlingsexposition: ####

df_siedlung_sek_abs <- data_indizes_abs_siedlung %>%
  transmute(
    GLOF.Name,
    Index_historisch = Index_Siedlung_sek_historisch_abs,
    Index_aktuell    = Index_Siedlung_sek_aktuell_abs
  ) %>%
  mutate(
    Veränderung = case_when(
      Index_aktuell > Index_historisch ~ "gestiegen",
      Index_aktuell < Index_historisch ~ "gesunken",
      TRUE                             ~ "gleich"
    )
  )

lims_sek <- range(
  df_siedlung_sek_abs$Index_historisch,
  df_siedlung_sek_abs$Index_aktuell,
  na.rm = TRUE
)

Scatterplot_Indizes_Siedlung_sek_abs <- ggplot(
  df_siedlung_sek_abs,
  aes(
    x = Index_historisch,
    y = Index_aktuell,
    color = Veränderung,
    label = GLOF.Name
  )
) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_text_repel(
    size = 3,
    family = "Times New Roman",
    segment.colour = NA,
    max.overlaps = Inf
  ) +
  scale_color_manual(values = c(
    "gestiegen" = "deeppink",
    "gesunken" = "darkgreen",
    "gleich"   = "grey50"
  )) +
  coord_equal(xlim = lims_sek, ylim = lims_sek) +
  theme_minimal(base_family = "Times New Roman") +
  labs(
    title = "Sekundäre Siedlung (absolute Standardisierung): Historisch vs. aktuell",
    x = "Historischer sekundärer Expositionsindex",
    y = "Aktueller sekundärer Expositionsindex",
    color = "Entwicklung"
  )

Scatterplot_Indizes_Siedlung_sek_abs

## 7. Neue Tabelle nur für Landwirtschaft mit gemeinsamer Standardisierung für aktuell und historisch:  ####

data_indizes_abs_landwirtschaft <- data %>%
  slice(1:16) %>%
  select(
    Nr.,
    GLOF.Name,
    Land,
    Bevölkerungswachstum..1960.2023..in..,
    Jahr,
    Jahr_historisch,
    Gebirge,
    Gletschersee..m..,
    all_of(c(
      Landwirtschaft_prim_historisch_vars,
      Landwirtschaft_prim_aktuell_vars,
      Landwirtschaft_sek_historisch_vars,
      Landwirtschaft_sek_aktuell_vars
    ))
  )

## 8. Gemeinsame z-Standardisierung für primär ####

for (i in seq_along(Landwirtschaft_prim_historisch_vars)) {
  
  hist_var <- Landwirtschaft_prim_historisch_vars[i]
  akt_var  <- Landwirtschaft_prim_aktuell_vars[i]
  
  gemeinsame_werte <- c(
    data_indizes_abs_landwirtschaft[[hist_var]],
    data_indizes_abs_landwirtschaft[[akt_var]]
  )
  
  mu  <- mean(gemeinsame_werte, na.rm = TRUE)
  sdv <- sd(gemeinsame_werte, na.rm = TRUE)
  
  data_indizes_abs_landwirtschaft[[paste0(hist_var, "_zabs")]] <-
    (data_indizes_abs_landwirtschaft[[hist_var]] - mu) / sdv
  
  data_indizes_abs_landwirtschaft[[paste0(akt_var, "_zabs")]] <-
    (data_indizes_abs_landwirtschaft[[akt_var]] - mu) / sdv
}

## 9. Gemeinsame z-Standardisierung für sekundär ####

for (i in seq_along(Landwirtschaft_sek_historisch_vars)) {
  
  hist_var <- Landwirtschaft_sek_historisch_vars[i]
  akt_var  <- Landwirtschaft_sek_aktuell_vars[i]
  
  gemeinsame_werte <- c(
    data_indizes_abs_landwirtschaft[[hist_var]],
    data_indizes_abs_landwirtschaft[[akt_var]]
  )
  
  mu  <- mean(gemeinsame_werte, na.rm = TRUE)
  sdv <- sd(gemeinsame_werte, na.rm = TRUE)
  
  data_indizes_abs_landwirtschaft[[paste0(hist_var, "_zabs")]] <-
    (data_indizes_abs_landwirtschaft[[hist_var]] - mu) / sdv
  
  data_indizes_abs_landwirtschaft[[paste0(akt_var, "_zabs")]] <-
    (data_indizes_abs_landwirtschaft[[akt_var]] - mu) / sdv
}

## 10. Indizes berechnen ####

data_indizes_abs_landwirtschaft <- data_indizes_abs_landwirtschaft %>%
  mutate(
    Index_Landwirtschaft_prim_historisch_abs = rowMeans(
      across(all_of(paste0(Landwirtschaft_prim_historisch_vars, "_zabs"))),
      na.rm = TRUE
    ),
    Index_Landwirtschaft_prim_aktuell_abs = rowMeans(
      across(all_of(paste0(Landwirtschaft_prim_aktuell_vars, "_zabs"))),
      na.rm = TRUE
    ),
    Index_Landwirtschaft_sek_historisch_abs = rowMeans(
      across(all_of(paste0(Landwirtschaft_sek_historisch_vars, "_zabs"))),
      na.rm = TRUE
    ),
    Index_Landwirtschaft_sek_aktuell_abs = rowMeans(
      across(all_of(paste0(Landwirtschaft_sek_aktuell_vars, "_zabs"))),
      na.rm = TRUE
    )
  )

## 11. Scatterplot erstellen für primäre Landwirtschafts-Exposition: ####

df_landwirtschaft_abs <- data_indizes_abs_landwirtschaft %>%
  transmute(
    GLOF.Name,
    Index_historisch = Index_Landwirtschaft_prim_historisch_abs,
    Index_aktuell    = Index_Landwirtschaft_prim_aktuell_abs
  ) %>%
  mutate(
    Veränderung = case_when(
      Index_aktuell > Index_historisch ~ "gestiegen",
      Index_aktuell < Index_historisch ~ "gesunken",
      TRUE                             ~ "gleich"
    )
  )

lims_land <- range(
  df_landwirtschaft_abs$Index_historisch,
  df_landwirtschaft_abs$Index_aktuell,
  na.rm = TRUE
)

Scatterplot_Indizes_Landwirtschaft_abs <- ggplot(
  df_landwirtschaft_abs,
  aes(
    x = Index_historisch,
    y = Index_aktuell,
    color = Veränderung,
    label = GLOF.Name
  )
) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_text_repel(
    size = 3,
    family = "Times New Roman",
    segment.colour = NA,
    max.overlaps = Inf
  ) +
  scale_color_manual(values = c(
    "gestiegen" = "deeppink",
    "gesunken" = "darkgreen",
    "gleich"   = "grey50"
  )) +
  coord_equal(xlim = lims_land, ylim = lims_land) +
  theme_minimal(base_family = "Times New Roman") +
  labs(
    title = "Landwirtschaft (absolute Standardisierung): Historisch vs. aktuell",
    x = "Historischer primärer Expositionsindex",
    y = "Aktueller primärer Expositionsindex",
    color = "Entwicklung"
  )

Scatterplot_Indizes_Landwirtschaft_abs

## 12. Scatterplot erstellen für sekundäre Landwirtschafts-Exposition: ####

df_landwirtschaft_sek_abs <- data_indizes_abs_landwirtschaft %>%
  transmute(
    GLOF.Name,
    Index_historisch = Index_Landwirtschaft_sek_historisch_abs,
    Index_aktuell    = Index_Landwirtschaft_sek_aktuell_abs
  ) %>%
  mutate(
    Veränderung = case_when(
      Index_aktuell > Index_historisch ~ "gestiegen",
      Index_aktuell < Index_historisch ~ "gesunken",
      TRUE                             ~ "gleich"
    )
  )

lims_land_sek <- range(
  df_landwirtschaft_sek_abs$Index_historisch,
  df_landwirtschaft_sek_abs$Index_aktuell,
  na.rm = TRUE
)

Scatterplot_Indizes_Landwirtschaft_sek_abs <- ggplot(
  df_landwirtschaft_sek_abs,
  aes(
    x = Index_historisch,
    y = Index_aktuell,
    color = Veränderung,
    label = GLOF.Name
  )
) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_text_repel(
    size = 3,
    family = "Times New Roman",
    segment.colour = NA,
    max.overlaps = Inf
  ) +
  scale_color_manual(values = c(
    "gestiegen" = "deeppink",
    "gesunken" = "darkgreen",
    "gleich"   = "grey50"
  )) +
  coord_equal(xlim = lims_land_sek, ylim = lims_land_sek) +
  theme_minimal(base_family = "Times New Roman") +
  labs(
    title = "Sekundäre Landwirtschaft (absolute Standardisierung): Historisch vs. aktuell",
    x = "Historischer sekundärer Expositionsindex",
    y = "Aktueller sekundärer Expositionsindex",
    color = "Entwicklung"
  )

Scatterplot_Indizes_Landwirtschaft_sek_abs

## 13. Neue Tabelle nur für Infrastruktur mit gemeinsamer Standardisierung für aktuell und historisch: ####

data_indizes_abs_infrastruktur <- data %>%
  slice(1:16) %>%
  select(
    Nr.,
    GLOF.Name,
    Land,
    Bevölkerungswachstum..1960.2023..in..,
    Jahr,
    Jahr_historisch,
    Gebirge,
    Gletschersee..m..,
    all_of(c(
      Infrastruktur_prim_historisch_vars,
      Infrastruktur_prim_aktuell_vars,
      Infrastruktur_sek_historisch_vars,
      Infrastruktur_sek_aktuell_vars
    ))
  )

## 14. Gemeinsame z-Standardisierung für primär: ####

for (i in seq_along(Infrastruktur_prim_historisch_vars)) {
  
  hist_var <- Infrastruktur_prim_historisch_vars[i]
  akt_var  <- Infrastruktur_prim_aktuell_vars[i]
  
  gemeinsame_werte <- c(
    data_indizes_abs_infrastruktur[[hist_var]],
    data_indizes_abs_infrastruktur[[akt_var]]
  )
  
  mu  <- mean(gemeinsame_werte, na.rm = TRUE)
  sdv <- sd(gemeinsame_werte, na.rm = TRUE)
  
  data_indizes_abs_infrastruktur[[paste0(hist_var, "_zabs")]] <-
    (data_indizes_abs_infrastruktur[[hist_var]] - mu) / sdv
  
  data_indizes_abs_infrastruktur[[paste0(akt_var, "_zabs")]] <-
    (data_indizes_abs_infrastruktur[[akt_var]] - mu) / sdv
}

## 15. Gemeinsame z-Standardisierung für sekundär: ####

for (i in seq_along(Infrastruktur_sek_historisch_vars)) {
  
  hist_var <- Infrastruktur_sek_historisch_vars[i]
  akt_var  <- Infrastruktur_sek_aktuell_vars[i]
  
  gemeinsame_werte <- c(
    data_indizes_abs_infrastruktur[[hist_var]],
    data_indizes_abs_infrastruktur[[akt_var]]
  )
  
  mu  <- mean(gemeinsame_werte, na.rm = TRUE)
  sdv <- sd(gemeinsame_werte, na.rm = TRUE)
  
  data_indizes_abs_infrastruktur[[paste0(hist_var, "_zabs")]] <-
    (data_indizes_abs_infrastruktur[[hist_var]] - mu) / sdv
  
  data_indizes_abs_infrastruktur[[paste0(akt_var, "_zabs")]] <-
    (data_indizes_abs_infrastruktur[[akt_var]] - mu) / sdv
}

## 16. Indizes berechnen: ####

data_indizes_abs_infrastruktur <- data_indizes_abs_infrastruktur %>%
  mutate(
    Index_Infrastruktur_prim_historisch_abs = rowMeans(
      across(all_of(paste0(Infrastruktur_prim_historisch_vars, "_zabs"))),
      na.rm = TRUE
    ),
    
    Index_Infrastruktur_prim_aktuell_abs = rowMeans(
      across(all_of(paste0(Infrastruktur_prim_aktuell_vars, "_zabs"))),
      na.rm = TRUE
    ),
    
    Index_Infrastruktur_sek_historisch_abs = rowMeans(
      across(all_of(paste0(Infrastruktur_sek_historisch_vars, "_zabs"))),
      na.rm = TRUE
    ),
    
    Index_Infrastruktur_sek_aktuell_abs = rowMeans(
      across(all_of(paste0(Infrastruktur_sek_aktuell_vars, "_zabs"))),
      na.rm = TRUE
    )
  )

## 17. Scatterplot erstellen für primäre Infrastruktur-Exposition:####

df_infra_abs <- data_indizes_abs_infrastruktur %>%
  transmute(
    GLOF.Name,
    Index_historisch = Index_Infrastruktur_prim_historisch_abs,
    Index_aktuell    = Index_Infrastruktur_prim_aktuell_abs
  ) %>%
  mutate(
    Veränderung = case_when(
      Index_aktuell > Index_historisch ~ "gestiegen",
      Index_aktuell < Index_historisch ~ "gesunken",
      TRUE ~ "gleich"
    )
  )

lims <- range(
  df_infra_abs$Index_historisch,
  df_infra_abs$Index_aktuell,
  na.rm = TRUE
)

Scatterplot_Indizes_Infrastruktur_abs <- ggplot(
  df_infra_abs,
  aes(
    x = Index_historisch,
    y = Index_aktuell,
    color = Veränderung,
    label = GLOF.Name
  )
) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_text_repel(
    size = 3,
    family = "Times New Roman",
    segment.colour = NA,
    max.overlaps = Inf
  ) +
  scale_color_manual(values = c(
    "gestiegen" = "deeppink",
    "gesunken" = "darkgreen",
    "gleich"   = "grey50"
  )) +
  coord_equal(xlim = lims, ylim = lims) +
  theme_minimal(base_family = "Times New Roman") +
  labs(
    title = "Infrastruktur (absolute Standardisierung): Historisch vs. aktuell",
    x = "Historischer Expositionsindex",
    y = "Aktueller Expositionsindex",
    color = "Entwicklung"
  )

Scatterplot_Indizes_Infrastruktur_abs

## 18. Scatterplot erstellen für sekundäre Infrastruktur-Exposition: ####

df_infra_sek_abs <- data_indizes_abs_infrastruktur %>%
  transmute(
    GLOF.Name,
    Index_historisch = Index_Infrastruktur_sek_historisch_abs,
    Index_aktuell    = Index_Infrastruktur_sek_aktuell_abs
  ) %>%
  mutate(
    Veränderung = case_when(
      Index_aktuell > Index_historisch ~ "gestiegen",
      Index_aktuell < Index_historisch ~ "gesunken",
      TRUE ~ "gleich"
    )
  )

lims <- range(
  df_infra_sek_abs$Index_historisch,
  df_infra_sek_abs$Index_aktuell,
  na.rm = TRUE
)

Scatterplot_Indizes_Infrastruktur_sek_abs <- ggplot(
  df_infra_sek_abs,
  aes(
    x = Index_historisch,
    y = Index_aktuell,
    color = Veränderung,
    label = GLOF.Name
  )
) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_text_repel(
    size = 3,
    family = "Times New Roman",
    segment.colour = NA,
    max.overlaps = Inf
  ) +
  scale_color_manual(values = c(
    "gestiegen" = "deeppink",
    "gesunken" = "darkgreen",
    "gleich"   = "grey50"
  )) +
  coord_equal(xlim = lims, ylim = lims) +
  theme_minimal(base_family = "Times New Roman") +
  labs(
    title = "Sekundäre Infrastruktur (absolute Standardisierung): Historisch vs. aktuell",
    x = "Historischer sekundärer Expositionsindex",
    y = "Aktueller sekundärer Expositionsindex",
    color = "Entwicklung"
  )

Scatterplot_Indizes_Infrastruktur_sek_abs

#### Darstellung der Rohgrößen anhand von Boxplots ####
# Hier erneut Excel-Tabelle data_exposition wählen
data_statistik <- read_excel(file.choose(), sheet = "Tabelle7")
num_cols_2 <- sapply(data_statistik, is.numeric)
data_statistik[num_cols_2] <- lapply(data_statistik[num_cols_2], function(x) ifelse(is.na(x), 0, x))
data_statistik <- as.data.frame(lapply(data_statistik, function(x) {
  if(all(x %in% c(0, NA))) {return(as.numeric(x))} else {return(x)}}))
only_na_cols_statistik <- sapply(data_statistik, function(x) all(is.na(x)))
only_na_cols_statistik
data_statistik[only_na_cols_statistik] <- lapply(data_statistik[only_na_cols_statistik], function(x) rep(0, length(x)))

library(stringr)
library(scales)

## 1. Boxplot für landwirtschaftliche Fläche - Farmland mit Häusern: ####
stats_farmland_haeuser <- data_statistik %>%
  filter(GLOF.Name %in% c("Min", "Max", "Median",
                          "Interquartilsabstand_1", "Interquartilsabstand_3")) %>%
  select(
    GLOF.Name,
    Landwirtschaft_Farmland_mit_Haeusern_prim_aktuell,
    Landwirtschaft_Farmland_mit_Haeusern_prim_historisch,
    Landwirtschaft_Farmland_mit_Haeusern_sek_aktuell,
    Landwirtschaft_Farmland_mit_Haeusern_sek_historisch
  )

box_stats_farmland_haeuser <- stats_farmland_haeuser %>%
  pivot_longer(
    cols      = -GLOF.Name,
    names_to  = "Variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from  = GLOF.Name,
    values_from = value
  ) %>%
  transmute(
    Variable,
    ymin   = Min,
    lower  = Interquartilsabstand_1,
    middle = Median,
    upper  = Interquartilsabstand_3,
    ymax   = Max
  )

all_vals_farmland_haeuser <- unlist(
  box_stats_farmland_haeuser[, c("ymin","lower","middle","upper","ymax")]
)

min_pos  <- min(all_vals_farmland_haeuser[all_vals_farmland_haeuser > 0],
                na.rm = TRUE)
epsilon  <- min_pos / 10

box_stats_farmland_haeuser_log <- box_stats_farmland_haeuser %>%
  mutate(across(c(ymin, lower, middle, upper, ymax),
                ~ pmax(.x, epsilon))) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "Landwirtschaft_Farmland_mit_Haeusern_prim_aktuell",
        "Landwirtschaft_Farmland_mit_Haeusern_prim_historisch",
        "Landwirtschaft_Farmland_mit_Haeusern_sek_aktuell",
        "Landwirtschaft_Farmland_mit_Haeusern_sek_historisch"
      )
    )
  )

plot_df_farmland_haeuser <- box_stats_farmland_haeuser_log %>%
  mutate(
    Typ  = if_else(str_detect(Variable, "_prim_"), "primär", "sekundär"),
    Zeit = if_else(str_detect(Variable, "_aktuell$"), "aktuell", "historisch")
  )

ggplot(plot_df_farmland_haeuser,
       aes(x = Typ,
           ymin   = ymin,
           lower  = lower,
           middle = middle,
           upper  = upper,
           ymax   = ymax,
           fill   = Zeit)) +
  
  geom_boxplot(
    stat = "identity",
    position = position_dodge(width = 0.65),
    width = 0.55,
    color = "black"
  ) +
  
  scale_y_log10(
    breaks = log_breaks(n = 6),
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  
  annotation_logticks(sides = "l") +
  
  scale_fill_manual(
    values = c("aktuell" = "grey70", "historisch" = "grey30")
  ) +
  
  labs(
    title = "Landwirtschaft – Farmland mit Häusern: primär/sekundär, historisch/aktuell",
    x = NULL,
    y = "Exponierte landwirtschaftliche Fläche des Farmlands mit Häusern in m²  (log10-Skala)",
    fill = NULL
  ) +
  
  theme_minimal(base_family = "Times") +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    plot.title   = element_text(size = 12),
    legend.position = "top",
    legend.text = element_text(size = 11)
  ) +
  
  guides(
    fill = guide_legend(
      override.aes = list(
        size  = 1.4,
        width = 1.4
      )
    )
  )

## 2. Boxplot für landwirtschaftliche Fläche - Tierhaltung und Farmland: ####
stats_farmland <- data_statistik %>%
  filter(GLOF.Name %in% c("Min", "Max", "Median",
                          "Interquartilsabstand_1", "Interquartilsabstand_3")) %>%
  select(
    GLOF.Name,
    Landwirtschaft_Farmland_prim_aktuell,
    Landwirtschaft_Farmland_prim_historisch,
    Landwirtschaft_Farmland_sek_aktuell,
    Landwirtschaft_Farmland_sek_historisch
  )

box_stats_farmland <- stats_farmland %>%
  pivot_longer(
    cols      = -GLOF.Name,
    names_to  = "Variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from  = GLOF.Name,
    values_from = value
  ) %>%
  transmute(
    Variable,
    ymin   = Min,
    lower  = Interquartilsabstand_1,
    middle = Median,
    upper  = Interquartilsabstand_3,
    ymax   = Max
  )

all_vals_farmland <- unlist(
  box_stats_farmland[, c("ymin","lower","middle","upper","ymax")]
)

min_pos  <- min(all_vals_farmland[all_vals_farmland > 0],
                na.rm = TRUE)
epsilon  <- min_pos / 10

box_stats_farmland_log <- box_stats_farmland %>%
  mutate(across(c(ymin, lower, middle, upper, ymax),
                ~ pmax(.x, epsilon))) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "Landwirtschaft_Farmland_prim_aktuell",
        "Landwirtschaft_Farmland_prim_historisch",
        "Landwirtschaft_Farmland_sek_aktuell",
        "Landwirtschaft_Farmland_sek_historisch"
      )
    )
  )

plot_df_farmland <- box_stats_farmland_log %>%
  mutate(
    Typ  = if_else(str_detect(Variable, "_prim_"), "primär", "sekundär"),
    Zeit = if_else(str_detect(Variable, "_aktuell$"), "aktuell", "historisch")
  )

ggplot(plot_df_farmland,
       aes(x = Typ,
           ymin   = ymin,
           lower  = lower,
           middle = middle,
           upper  = upper,
           ymax   = ymax,
           fill   = Zeit)) +
  
  geom_boxplot(
    stat = "identity",
    position = position_dodge(width = 0.65),
    width = 0.55,
    color = "black"
  ) +
  
  scale_y_log10(
    breaks = log_breaks(n = 6),
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  
  annotation_logticks(sides = "l") +
  
  scale_fill_manual(
    values = c("aktuell" = "grey70", "historisch" = "grey30")
  ) +
  
  labs(
    title = "Landwirtschaft – Farmland: primär/sekundär, historisch/aktuell",
    x = NULL,
    y = "Exponierte landwirtschaftliche Fläche mit Farmland in m² (log10-Skala)",
    fill = NULL
  ) +
  
  theme_minimal(base_family = "Times") +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    plot.title   = element_text(size = 12),
    legend.position = "top",
    legend.text = element_text(size = 11)
  ) +
  
  guides(
    fill = guide_legend(
      override.aes = list(
        size  = 1.4,
        width = 1.4
      )
    )
  )


## 2. Boxplot für Siedlungsfläche - Häuser mit Farmland: ####
stats_siedlung_haeuser_farmland <- data_statistik %>%
  filter(GLOF.Name %in% c("Min", "Max", "Median",
                          "Interquartilsabstand_1", "Interquartilsabstand_3")) %>%
  select(
    GLOF.Name,
    Siedlung_Haeuser_mit_Farmland_prim_aktuell,
    Siedlung_Haeuser_mit_Farmland_prim_historisch,
    Siedlung_Haeuser_mit_Farmland_sek_aktuell,
    Siedlung_Haeuser_mit_Farmland_sek_historisch
  )

box_stats_siedlung_haeuser_farmland <- stats_siedlung_haeuser_farmland %>%
  pivot_longer(
    cols      = -GLOF.Name,
    names_to  = "Variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from  = GLOF.Name,
    values_from = value
  ) %>%
  transmute(
    Variable,
    ymin   = Min,
    lower  = Interquartilsabstand_1,
    middle = Median,
    upper  = Interquartilsabstand_3,
    ymax   = Max
  )

all_vals_siedlung_haeuser_farmland <- unlist(
  box_stats_siedlung_haeuser_farmland[, c("ymin","lower","middle","upper","ymax")]
)

min_pos  <- min(all_vals_siedlung_haeuser_farmland[
  all_vals_siedlung_haeuser_farmland > 0], na.rm = TRUE)
epsilon  <- min_pos / 10

box_stats_siedlung_haeuser_farmland_log <- box_stats_siedlung_haeuser_farmland %>%
  mutate(across(c(ymin, lower, middle, upper, ymax),
                ~ pmax(.x, epsilon))) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "Siedlung_Haeuser_mit_Farmland_prim_aktuell",
        "Siedlung_Haeuser_mit_Farmland_prim_historisch",
        "Siedlung_Haeuser_mit_Farmland_sek_aktuell",
        "Siedlung_Haeuser_mit_Farmland_sek_historisch"
      )
    )
  )

plot_df_siedlung_haeuser_farmland <- box_stats_siedlung_haeuser_farmland_log %>%
  mutate(
    Typ  = if_else(str_detect(Variable, "_prim_"), "primär", "sekundär"),
    Zeit = if_else(str_detect(Variable, "_aktuell$"), "aktuell", "historisch")
  )

ggplot(plot_df_siedlung_haeuser_farmland,
       aes(x = Typ,
           ymin   = ymin,
           lower  = lower,
           middle = middle,
           upper  = upper,
           ymax   = ymax,
           fill   = Zeit)) +
  
  geom_boxplot(
    stat = "identity",
    position = position_dodge(width = 0.65),
    width = 0.55,
    color = "black"
  ) +
  
  scale_y_log10(
    breaks = log_breaks(n = 6),
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  
  annotation_logticks(sides = "l") +
  
  scale_fill_manual(
    values = c("aktuell" = "grey70", "historisch" = "grey30")
  ) +
  
  labs(
    title = "Siedlung – Häuser mit Farmland: primär/sekundär, historisch/aktuell",
    x = NULL,
    y = "Exponierte Siedlungsfläche der Häuser mit Farmland (log10-Skala)",
    fill = NULL
  ) +
  
  theme_minimal(base_family = "Times") +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    plot.title   = element_text(size = 12),
    legend.position = "top",
    legend.text = element_text(size = 11)
  ) +
  
  guides(
    fill = guide_legend(
      override.aes = list(
        size  = 1.4,
        width = 1.4
      )
    )
  )

## 4. Boxplot für Siedlungsfläche - Häuser: ####
stats_siedlung_haeuser <- data_statistik %>%
  filter(GLOF.Name %in% c("Min", "Max", "Median",
                          "Interquartilsabstand_1", "Interquartilsabstand_3")) %>%
  select(
    GLOF.Name,
    Siedlung_Haeuser_prim_aktuell,
    Siedlung_Haeuser_prim_historisch,
    Siedlung_Haeuser_sek_aktuell,
    Siedlung_Haeuser_sek_historisch
  )

box_stats_siedlung_haeuser <- stats_siedlung_haeuser %>%
  pivot_longer(
    cols      = -GLOF.Name,
    names_to  = "Variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from  = GLOF.Name,
    values_from = value
  ) %>%
  transmute(
    Variable,
    ymin   = Min,
    lower  = Interquartilsabstand_1,
    middle = Median,
    upper  = Interquartilsabstand_3,
    ymax   = Max
  )

all_vals_siedlung_haeuser <- unlist(
  box_stats_siedlung_haeuser[, c("ymin","lower","middle","upper","ymax")]
)

min_pos  <- min(all_vals_siedlung_haeuser[
  all_vals_siedlung_haeuser > 0], na.rm = TRUE)
epsilon  <- min_pos / 10

box_stats_siedlung_haeuser_log <- box_stats_siedlung_haeuser %>%
  mutate(across(c(ymin, lower, middle, upper, ymax),
                ~ pmax(.x, epsilon))) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "Siedlung_Haeuser_prim_aktuell",
        "Siedlung_Haeuser_prim_historisch",
        "Siedlung_Haeuser_sek_aktuell",
        "Siedlung_Haeuser_sek_historisch"
      )
    )
  )

plot_df_siedlung_haeuser <- box_stats_siedlung_haeuser_log %>%
  mutate(
    Typ  = if_else(str_detect(Variable, "_prim_"), "primär", "sekundär"),
    Zeit = if_else(str_detect(Variable, "_aktuell$"), "aktuell", "historisch")
  )

ggplot(plot_df_siedlung_haeuser,
       aes(x = Typ,
           ymin   = ymin,
           lower  = lower,
           middle = middle,
           upper  = upper,
           ymax   = ymax,
           fill   = Zeit)) +
  
  geom_boxplot(
    stat = "identity",
    position = position_dodge(width = 0.65),
    width = 0.55,
    color = "black"
  ) +
  
  scale_y_log10(
    breaks = log_breaks(n = 6),
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  
  annotation_logticks(sides = "l") +
  
  scale_fill_manual(
    values = c("aktuell" = "grey70", "historisch" = "grey30")
  ) +
  
  labs(
    title = "Siedlung – Häuser: primär/sekundär, historisch/aktuell",
    x = NULL,
    y = "Exponierte Siedlungsfläche der Häuser (log10-Skala)",
    fill = NULL
  ) +
  
  theme_minimal(base_family = "Times") +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    plot.title   = element_text(size = 12),
    legend.position = "top",
    legend.text = element_text(size = 11)
  ) +
  
  guides(
    fill = guide_legend(
      override.aes = list(
        size  = 1.4,
        width = 1.4
      )
    )
  )
## 5. Boxplot für Population der landwirtschaftlichen Flächen: ####
stats_landwirtschaft_population <- data_statistik %>%
  filter(GLOF.Name %in% c("Min", "Max", "Median",
                          "Interquartilsabstand_1", "Interquartilsabstand_3")) %>%
  select(
    GLOF.Name,
    Landwirtschaft_Population_prim_aktuell,
    Landwirtschaft_Population_prim_historisch,
    Landwirtschaft_Population_sek_aktuell,
    Landwirtschaft_Population_sek_historisch
  )

box_stats_landwirtschaft_population <- stats_landwirtschaft_population %>%
  pivot_longer(
    cols      = -GLOF.Name,
    names_to  = "Variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from  = GLOF.Name,
    values_from = value
  ) %>%
  transmute(
    Variable,
    ymin   = Min,
    lower  = Interquartilsabstand_1,
    middle = Median,
    upper  = Interquartilsabstand_3,
    ymax   = Max
  )

all_vals_landwirtschaft_population <- unlist(
  box_stats_landwirtschaft_population[, c("ymin","lower","middle","upper","ymax")]
)

min_pos  <- min(all_vals_landwirtschaft_population[all_vals_landwirtschaft_population > 0],
                na.rm = TRUE)
epsilon  <- min_pos / 10

box_stats_landwirtschaft_population_log <- box_stats_landwirtschaft_population %>%
  mutate(across(c(ymin, lower, middle, upper, ymax),
                ~ pmax(.x, epsilon))) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "Landwirtschaft_Population_prim_aktuell",
        "Landwirtschaft_Population_prim_historisch",
        "Landwirtschaft_Population_sek_aktuell",
        "Landwirtschaft_Population_sek_historisch"
      )
    )
  )

plot_df_landwirtschaft_population <- box_stats_landwirtschaft_population_log %>%
  mutate(
    Typ  = if_else(str_detect(Variable, "_prim_"), "primär", "sekundär"),
    Zeit = if_else(str_detect(Variable, "_aktuell$"), "aktuell", "historisch")
  )

ggplot(plot_df_landwirtschaft_population,
       aes(x = Typ,
           ymin   = ymin,
           lower  = lower,
           middle = middle,
           upper  = upper,
           ymax   = ymax,
           fill   = Zeit)) +
  
  geom_boxplot(
    stat = "identity",
    position = position_dodge(width = 0.65),
    width = 0.55,
    color = "black"
  ) +
  
  scale_y_log10(
    breaks = log_breaks(n = 6),
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  
  annotation_logticks(sides = "l") +
  
  scale_fill_manual(
    values = c("aktuell" = "grey70", "historisch" = "grey30")
  ) +
  
  labs(
    title = "Landwirtschaft – Population: primär/sekundär, historisch/aktuell",
    x = NULL,
    y = "Exponierte Population der landwirtschaftlichen Flächen (log10-Skala)",
    fill = NULL
  ) +
  
  theme_minimal(base_family = "Times") +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    plot.title   = element_text(size = 12),
    legend.position = "top",
    legend.text = element_text(size = 11)
  ) +
  
  guides(
    fill = guide_legend(
      override.aes = list(
        size  = 1.4,
        width = 1.4
      )
    )
  )


## 6. Boxplot für Population der Siedlungsflächen: ####
stats_siedlung_population <- data_statistik %>%
  filter(GLOF.Name %in% c("Min", "Max", "Median",
                          "Interquartilsabstand_1", "Interquartilsabstand_3")) %>%
  select(
    GLOF.Name,
    Siedlung_Population_prim_aktuell,
    Siedlung_Population_prim_historisch,
    Siedlung_Population_sek_aktuell,
    Siedlung_Population_sek_historisch
  )

box_stats_siedlung_population <- stats_siedlung_population %>%
  pivot_longer(
    cols      = -GLOF.Name,
    names_to  = "Variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from  = GLOF.Name,
    values_from = value
  ) %>%
  transmute(
    Variable,
    ymin   = Min,
    lower  = Interquartilsabstand_1,
    middle = Median,
    upper  = Interquartilsabstand_3,
    ymax   = Max
  )

all_vals_siedlung_population <- unlist(
  box_stats_siedlung_population[, c("ymin","lower","middle","upper","ymax")]
)

min_pos  <- min(all_vals_siedlung_population[all_vals_siedlung_population > 0],
                na.rm = TRUE)
epsilon  <- min_pos / 10

box_stats_siedlung_population_log <- box_stats_siedlung_population %>%
  mutate(across(c(ymin, lower, middle, upper, ymax),
                ~ pmax(.x, epsilon))) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "Siedlung_Population_prim_aktuell",
        "Siedlung_Population_prim_historisch",
        "Siedlung_Population_sek_aktuell",
        "Siedlung_Population_sek_historisch"
      )
    )
  )

plot_df_siedlung_population <- box_stats_siedlung_population_log %>%
  mutate(
    Typ  = if_else(str_detect(Variable, "_prim_"), "primär", "sekundär"),
    Zeit = if_else(str_detect(Variable, "_aktuell$"), "aktuell", "historisch")
  )

ggplot(plot_df_siedlung_population,
       aes(x = Typ,
           ymin   = ymin,
           lower  = lower,
           middle = middle,
           upper  = upper,
           ymax   = ymax,
           fill   = Zeit)) +
  
  geom_boxplot(
    stat = "identity",
    position = position_dodge(width = 0.65),
    width = 0.55,
    color = "black"
  ) +
  
  scale_y_log10(
    breaks = log_breaks(n = 6),
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  
  annotation_logticks(sides = "l") +
  
  scale_fill_manual(
    values = c("aktuell" = "grey70", "historisch" = "grey30")
  ) +
  
  labs(
    title = "Siedlung – Population: primär/sekundär, historisch/aktuell",
    x = NULL,
    y = "Exponierte Population der Siedlungsflächen (log10-Skala)",
    fill = NULL
  ) +
  
  theme_minimal(base_family = "Times") +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    plot.title   = element_text(size = 12),
    legend.position = "top",
    legend.text = element_text(size = 11)   
  ) +
  
  guides(
    fill = guide_legend(
      override.aes = list(
        size  = 1.4,   
        width = 1.4    
      )
    )
  )

#### Darstellung von GLOF-Clustern und dominanten Kategorien innerhalb von Dendrogrammen ####

#install.packages("factoextra")
#install.packages("dendextend")

library(dplyr)
library(factoextra)
library(dendextend)

## 1. Datengrundlage für die Dendrogramme: ####

# Primär historisch
cluster_prim_hist <- data_indizes %>%
  select(
    GLOF.Name,
    Index_Siedlung_prim_historisch,
    Index_Landwirtschaft_prim_historisch,
    Index_Infrastruktur_prim_historisch
  )

# Primär aktuell
cluster_prim_akt <- data_indizes %>%
  select(
    GLOF.Name,
    Index_Siedlung_prim_aktuell,
    Index_Landwirtschaft_prim_aktuell,
    Index_Infrastruktur_prim_aktuell
  )

# Sekundär historisch
cluster_sek_hist <- data_indizes %>%
  select(
    GLOF.Name,
    Index_Siedlung_sek_historisch,
    Index_Landwirtschaft_sek_historisch,
    Index_Infrastruktur_sek_historisch
  )

# Sekundär aktuell
cluster_sek_akt <- data_indizes %>%
  select(
    GLOF.Name,
    Index_Siedlung_sek_aktuell,
    Index_Landwirtschaft_sek_aktuell,
    Index_Infrastruktur_sek_aktuell
  )

## 2. Funktion für Erstellung der Dendrogramme: ####

plot_cluster_dendrogramm <- function(df, titel) {
  
  mat <- df %>%
    as.data.frame()
  
  rownames(mat) <- mat$GLOF.Name
  mat <- mat[, -1]
  
  dist_mat <- dist(mat, method = "euclidean")
  hc <- hclust(dist_mat, method = "ward.D2")
  
  plot(
    hc,
    main = titel,
    ylab = "Cluster-Distanz",
    xlab = "",
    sub = "",
    cex = 0.8
  )
}

## 3. Vier Dendrogramme in einer Abbildung: #### 

par(
  mfrow = c(2, 2),
  mar = c(7, 4, 3, 2),
  family = "Times"
)

plot_cluster_dendrogramm(cluster_prim_hist, "Primär historisch")
plot_cluster_dendrogramm(cluster_sek_hist, "Sekundär historisch")
plot_cluster_dendrogramm(cluster_prim_akt,  "Primär aktuell")
plot_cluster_dendrogramm(cluster_sek_akt,   "Sekundär aktuell")




