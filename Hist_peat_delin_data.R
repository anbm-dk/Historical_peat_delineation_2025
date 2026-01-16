# Process data for new delineation of historical peatlands

library(tidyverse)
library(dplyr)
library(dbplyr)
library(magrittr)
library(terra)
library(RODBC)
library(DBI)
library(tidyr)
library(tidyterra)
library(stringr)

dir <- getwd()
root <- dirname(dir)

# Load DEM

dem <- root %>%
  paste0(., "/UDKIK_GIS/dhm2015_terraen_10m.tif") %>%
  rast()

# Open Ochre DB

db <- paste0(root, "/UDKIK_GIS/Historical_peat/Okker/Okker-kortlægning 1984/NYokkerdatabaseVer2007.mdb")

con <- odbcConnectAccess2007(db)

# tbl(con, "BOREPROFILLAG")

sqlTables(con)

ochre_samples <- sqlQuery(
  con,
  "SELECT * FROM BOREPROFILLAG"
) %>%
  tibble(
    .name_repair = "universal"
    ) %>%
  select(PROFIL.NR, LAG, DYBDEFRA, DYBDETIL, LAGTYK, MAT_KODE, MATERIALE)

ochre_samples$MATERIALE %>% table()
# Diatomegytje     Ej observeret  Fibric materiale           Finsand   Folic materiale
#            1                 1               780               466                12
#     Grovsand              Grus   Hemic materiale         Kalkgytje    Koprogen gytje
#          377                 1              2431               224              1990
#   Leret sand        Mellemsand        Sandet ler  Sapric materiale              Silt
#          903             11770               425              7973                24
#   Siltet ler Siltet leret sand          Svær ler
#         1440              2586                49

ochre_samples$MATERIALE %>% is.na() %>% sum()
# [1] 16

ochre_samples_peat <- ochre_samples %>%
  mutate(
    is_peat = MAT_KODE %in% c(23:25),
    lower_bound_top = case_when(
      DYBDETIL > 30 ~ 30,
      DYBDETIL <= 30 ~ DYBDETIL,
      .default = NA,
    ),
    lower_bound_top = case_when(
      DYBDEFRA >= 30 ~ NA,
      .default = lower_bound_top,
    ),
    cm_top = case_when(
      is.finite(lower_bound_top) ~ lower_bound_top - DYBDEFRA,
      .default = 0
      ),
    cm_peat = LAGTYK * is_peat,
    cm_peat_top = cm_top * is_peat,
    mat_missing_top = case_when(
      cm_top == 0 ~ 0,
      is.na(MATERIALE) ~ 1,
      MATERIALE == "Ej observeret" ~ 1,
      .default = 0
    )
  )

ochre_samples_peat$mat_missing_top %>% sum()
# [1] 3

ochre_samples_peat %>%
  filter(mat_missing_top == TRUE)

sum(ochre_samples_peat$is_peat)
# 11,184 samples with peat

ochre_samples_peat %>%
  group_by(PROFIL.NR) %>%
  summarise(
    sum_is_peat = sum(is_peat),
    sum_missing_mat = sum(mat_missing_top)
  ) %>%
  filter(
    sum_is_peat > 0,
    sum_missing_mat == 0
  ) %>%
  nrow()
# 6045 boreholes with peat

ochre_samples_peat_summary <- ochre_samples_peat %>%
  mutate(
    cm_to_peat = case_when(
      !is_peat ~ 9999,
      .default = DYBDEFRA
    )
  ) %>%
  group_by(PROFIL.NR) %>%
  summarise(
    cm_peat = sum(cm_peat, na.rm = TRUE),
    cm_peat_top = sum(cm_peat_top, na.rm = TRUE),
    cm_to_peat = min(cm_to_peat, na.rm = TRUE),
    depth_explored = max(DYBDETIL, na.rm = TRUE),
    sum_missing_mat = sum(mat_missing_top)
  ) %>%
  mutate(
    is_peat = as.numeric(cm_peat_top >= 30),
    buried_peat = as.numeric(cm_to_peat > 0 & cm_to_peat != 9999),
    thin_peat = as.numeric(cm_peat > 0 & is_peat == 0 & buried_peat == 0)
  ) %>%
  ungroup()

ochre_samples_peat_summary %>% as.data.frame()


# Load ochre db points

ochre_pts <- paste0(
  root,
  "/UDKIK_GIS/Historical_peat/Okker/Okker-kortlægning 1984/Okker Arcview/OkkerETRS89.shp"
  ) %>%
  vect() %>%
  left_join(
    ochre_samples_peat_summary,
    join_by(BOREPROFIL == PROFIL.NR)
  ) %>%
  filter(
    sum_missing_mat == 0
  )

plot(ochre_pts, "is_peat")

# Load wetlands

wetlands <- paste0(
  root,
  "/UDKIK_GIS/Historical_peat/Okker/Okker-kortlægning 1984/Lavbund Arcview tema/LAVBUND.SHP"
  ) %>%
  vect()

wetlands %<>% filter(LAVBUND != 9999)

# plot(wetlands, "LAVBUND")

wetlands_jutland <- wetlands %>% filter(LAVBUND != 50)

# plot(wetlands_jutland)

wetlands_jutland

wetland_area_jutland <- sum(wetlands_jutland$AREA)/10^6
# [1] 5579.443

nrow(ochre_pts) / wetland_area_jutland
# [1] 1.455163 samples per km2

nrow(ochre_pts)*45000 / wetland_area_jutland
# Generate 65482 background samples

# Review processed data

ochre_pts %>%
  as.data.frame() %>%
  summarise(
    n_top_peat    = sum(is_peat),
    n_buried_peat = sum(buried_peat),
    n_thin_peat   = sum(thin_peat)
  )
# # A tibble: 1 × 3
# n_top_peat n_buried_peat n_thin_peat
#      <dbl>         <dbl>       <dbl>
#       4098          1072         875
# Out of 8122 points

ochre_pts %>%
  as.data.frame() %>%
  filter(buried_peat == TRUE) %>%
  reframe(
    prob = seq(0.05, 0.95, 0.05),
    qs_cm_to_peat = quantile(cm_to_peat, seq(0.05, 0.95, 0.05))
  )
#     prob qs_cm_to_peat
#    <dbl>         <dbl>
#  1  0.05            15
#  2  0.1             20
#  3  0.15            20
#  4  0.2             20
#  5  0.25            30
#  6  0.3             30
#  7  0.35            30
#  8  0.4             40
#  9  0.45            40
# 10  0.5             40
# 11  0.55            50
# 12  0.6             50
# 13  0.65            60
# 14  0.7             60
# 15  0.75            70
# 16  0.8             80
# 17  0.85            90
# 18  0.9            110
# 19  0.95           140

ochre_pts %>%
  as.data.frame() %>%
  filter(buried_peat == TRUE) %>%
  summarise(mean_cm_to_peat = mean(cm_to_peat))
# mean_cm_to_peat
#         55.8722

# Finalise ochre db points

ochre_pts_processed <- ochre_pts %>%
  as.data.frame(
    geom = "XY"
  ) %>%
  rename(ID = BOREPROFIL) %>%
  mutate(source = "OchreDB") %>%
  select(
    -c(UTME, UTMN, Nr, TorveDyb, Materi)
    ) %>%
  relocate(
    any_of(c("x", "y")),
    .after = ID
    )

ochre_pts_processed

write.table(
  ochre_pts_processed,
  file = "ochre_pts_processed.txt",
  sep = ";",
  row.names = FALSE
)

saveRDS(
  ochre_pts_processed,
  file = "ochre_pts_processed.rds"
)

# Process Jupiter points

# Open Jupiter DB

db_jup <- paste0(root, "/UDKIK_GIS/Historical_peat/Jupiter/BoringJordart_20110127.mdb")

con_jup <- odbcConnectAccess2007(db_jup)

# tbl(con, "BOREPROFILLAG")

sqlTables(con_jup)

Jupiter_samples <- sqlQuery(
  con_jup,
  "SELECT * FROM tbljup1975"
) %>%
  tibble(
    .name_repair = "universal"
  )

Jupiter_samples$ROCKTYPE %>% table() %>% as.data.frame()
#      .  Freq
# 1    b  2078  # b == "brønd" ("well")
# 2  brk     1  # Brown coal
# 3  dib     1  # diabas
# 4  dit    27  # diatomite
# 5  fli     9  # flint
# 6  fyl  7038  # fyl == "fyld" ("fill")
# 7  gne    26  # Gneiss
# 8  gos    89  # gravel and stones
# 9  gra    44  # granite
# 10 grs  1192  # gravel
# 11 gyt  1190  # gyttja
# 12 het    39  # heterolith
# 13 hus    10  # humous substance
# 14 kal   535  # Limestone
# 15 kon    12  # conkretion
# 16 kri   160  # Limestone
# 17 lej     1  # Clay-ironstone
# 18 ler 25360  # clay
# 19 mul 20692  # "mull" (topsoil)
# 20 plr    15  # Plant remains
# 21 san 41361  # sand
# 22 sat    27  # sandstone
# 23 sil  1410  # silt
# 24 sit     5  # siltstone
# 25 ska     8  # shells
# 26 ski     1  # shale
# 27 sog  1253  # sand and gravel
# 28 ste   153  # cobble (gravel)
# 29 tør  1100  # peat
# 30 tuf     1  # tuff

# Drop boreholes containing "b" or "fyl" as they do not give information
# on parent materials (i.e. not confirmed negatives for peat).

Jupiter_samples$ROCKTYPE %>% is.na() %>% sum()
# [1] 0  # No missing rocktypes.

Jupiter_samples_peat <- Jupiter_samples %>%
  mutate(
    nr_new = str_squish(nr),
    is_peat = as.numeric(ROCKTYPE == "tør"),
    DYBDEFRA = TOP*100,
    DYBDETIL = BOTTOM*100,
    LAGTYK = DYBDETIL - DYBDEFRA,
    lower_bound_top = case_when(
      DYBDETIL > 30 ~ 30,
      DYBDETIL <= 30 ~ DYBDETIL,
      .default = NA,
    ),
    lower_bound_top = case_when(
      DYBDEFRA >= 30 ~ NA,
      .default = lower_bound_top,
    ),
    cm_top = case_when(
      is.finite(lower_bound_top) ~ lower_bound_top - DYBDEFRA,
      .default = 0
    ),
    cm_peat = LAGTYK * is_peat,
    cm_peat_top = cm_top * is_peat,
    technic = as.numeric(ROCKTYPE %in% c("b", "fyl"))
  )

Jupiter_samples_peat %>%
  filter(!is.finite(DYBDETIL)) %>%
  as.data.frame()

sum(Jupiter_samples_peat$is_peat)
# 1100 samples with peat

sum(Jupiter_samples_peat$technic)
# 9116 samples with "b" or "fyl"

Jupiter_samples_peat %>%
  filter(technic == 1)

Jupiter_samples_peat %>%
  group_by(nr) %>%
  summarise(
    sum_is_peat = sum(is_peat)
  ) %>%
  filter(
    sum_is_peat > 0
  ) %>%
  nrow()
# 993 boreholes with peat at some depth

Jupiter_samples_peat %>%
  group_by(nr) %>%
  summarise(
    sum = sum(technic)
  ) %>%
  filter(
    sum > 0
  ) %>%
  nrow()
# 7245 boreholes with "b" or "fyl".

Jupiter_samples_peat %>%
  group_by(nr) %>%
  summarise(
    sum1 = sum(technic),
    sum2 = sum(is_peat)
  ) %>%
  filter(
    sum1 > 0 & sum2 > 0
  ) %>%
  nrow()
# 188 boreholes with peat as well as technical layers

Jupiter_samples_peat_summary <- Jupiter_samples_peat %>%
  mutate(
    cm_to_peat = case_when(
      !is_peat ~ 9999,
      .default = DYBDEFRA
    )
  ) %>%
  group_by(nr_new) %>%
  summarise(
    cm_peat = sum(cm_peat, na.rm = TRUE),
    cm_peat_top = sum(cm_peat_top, na.rm = TRUE),
    cm_to_peat = min(cm_to_peat, na.rm = TRUE),
    # x_mean = mean(X, na.rm = TRUE),
    # y_mean = mean(Y, na.rm = TRUE),
    # x_sd = sd(X, na.rm = TRUE),
    # y_sd = sd(Y, na.rm = TRUE),
    has_technic = sum(technic),
    depth_explored = max(DYBDETIL, na.rm = TRUE),
    missing_depth = sum(!is.finite(DYBDETIL))
  ) %>%
  mutate(
    is_peat = as.numeric(cm_peat_top >= 30),
    buried_peat = as.numeric(cm_to_peat > 0 & cm_to_peat != 9999),
    thin_peat = as.numeric(cm_peat > 0 & is_peat == 0 & buried_peat == 0)
  ) %>%
  ungroup()

Jupiter_samples_peat_summary %>% as.data.frame()

Jupiter_samples_peat_summary %>%
  filter(has_technic > 0)

Jupiter_samples_peat_summary %>%
  filter(missing_depth > 0)

Jupiter_samples_peat_summary %>%
  summarise(
    n_top_peat    = sum(is_peat),
    n_buried_peat = sum(buried_peat),
    n_thin_peat   = sum(thin_peat)
  )
# # A tibble: 1 × 3
#     n_top_peat n_buried_peat n_thin_peat
#          <dbl>         <dbl>       <dbl>
#   1        369           603          21
# Out of 49395 points

Jupiter_samples_peat_summary %>%
  filter(buried_peat == TRUE) %>%
  reframe(
    prob = seq(0.05, 0.95, 0.05),
    qs_cm_to_peat = quantile(cm_to_peat, seq(0.05, 0.95, 0.05))
  )
#     prob qs_cm_to_peat
#    <dbl>         <dbl>
#  1  0.05          20
#  2  0.1           30
#  3  0.15          42.9
#  4  0.2           50
#  5  0.25          60
#  6  0.3           60
#  7  0.35          70
#  8  0.4           80
#  9  0.45          90
# 10  0.5          100
# 11  0.55         100
# 12  0.6          110
# 13  0.65         120
# 14  0.7          130
# 15  0.75         140
# 16  0.8          150
# 17  0.85         160
# 18  0.9          170
# 19  0.95         180

Jupiter_samples_peat_summary %>%
  filter(buried_peat == TRUE) %>%
  summarise(mean_cm_to_peat = mean(cm_to_peat))
# mean_cm_to_peat
#           <dbl>
#            99.6


# Jupiter_samples_peat_summary %>% filter(
#   x_sd > 0 | y_sd > 0
# )
# A tibble: 0 × 8
# ℹ 8 variables: nr <chr>, cm_peat <dbl>, cm_peat_top <dbl>, x_mean <dbl>, y_mean <dbl>, x_sd <dbl>, y_sd <dbl>,
#   is_peat <dbl>

# Load Jupiter points

paste0(root, "/UDKIK_GIS/Historical_peat/Jupiter/") %>%
  list.files(pattern = "\\.shp$",
             full.names = TRUE) %>%
  lapply(vect)

Jupiter_pts <- paste0(root, "/UDKIK_GIS/Historical_peat/Jupiter/JupNyNy.shp") %>%
  vect() %>%
  mutate(
    nr_new = str_squish(nr)
  ) %>%
  distinct(nr_new, .keep_all = TRUE) %>%
  left_join(
    Jupiter_samples_peat_summary,
    join_by(nr_new == nr_new)
  )

Jupiter_pts

plot(Jupiter_pts, "is_peat")

# There are a lot of points located in the ocean, so I will filter them using
# the DEM

Jupiter_pts_dem <- Jupiter_pts %>%
  terra::extract(
    x = dem,
    y = .,
    ID = FALSE,
    bind = TRUE
  ) %>%
  filter(
    is.finite(dhm2015_terraen_10m)
    )

Jupiter_pts_dem

plot(Jupiter_pts_dem, "is_peat")


# Remove boreholes with technic layers and missing depth information

Jupiter_pts_dem_nontechnic <- Jupiter_pts_dem %>%
  filter(
    has_technic == 0,
    missing_depth == 0
    )

Jupiter_pts_dem_nontechnic

plot(Jupiter_pts_dem_nontechnic, "is_peat")

Jupiter_pts_dem_nontechnic %>%
  summarise(
    n_top_peat    = sum(is_peat),
    n_buried_peat = sum(buried_peat),
    n_thin_peat   = sum(thin_peat)
  )
# names       : n_top_peat n_buried_peat n_thin_peat
# type        :      <num>         <num>       <num>
# values      :        368           400          21
# Out of 40,892 points
# Generate an equal number of background samples

Jupiter_pts_dem_nontechnic %>%
  values() %>%
  filter(buried_peat == TRUE) %>%
  reframe(
    prob = seq(0.05, 0.95, 0.05),
    qs_cm_to_peat = quantile(cm_to_peat, seq(0.05, 0.95, 0.05))
  )
#    prob qs_cm_to_peat
# 1  0.05            20
# 2  0.10            30
# 3  0.15            40
# 4  0.20            50
# 5  0.25            50
# 6  0.30            50
# 7  0.35            60
# 8  0.40            70
# 9  0.45            80
# 10 0.50            90
# 11 0.55           100
# 12 0.60           100
# 13 0.65           110
# 14 0.70           120
# 15 0.75           130
# 16 0.80           140
# 17 0.85           150
# 18 0.90           160
# 19 0.95           180

Jupiter_pts_dem_nontechnic %>%
  values() %>%
  filter(buried_peat == TRUE) %>%
  summarise(mean_cm_to_peat = mean(cm_to_peat))
# mean_cm_to_peat
#           <dbl>
#            92.05

# Finalize

Jupiter_pts_processed <- Jupiter_pts_dem_nontechnic %>%
  select(-c(X, Y)) %>%
  as.data.frame(
    geom = "XY"
  ) %>%
  rename(ID = nr_new) %>%
  mutate(source = "Jupiter") %>%
  select(
    c(ID, x, y, cm_peat, cm_peat_top, cm_to_peat, depth_explored, is_peat, buried_peat,
      thin_peat, source)
  ) %>%
  relocate(
    any_of(c("x", "y")),
    .after = ID
  )

Jupiter_pts_processed

write.table(
  Jupiter_pts_processed,
  file = "Jupiter_pts_processed.txt",
  sep = ";",
  row.names = FALSE
)

saveRDS(
  Jupiter_pts_processed,
  file = "Jupiter_pts_processed.rds"
)


# END
