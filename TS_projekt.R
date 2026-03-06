############################################################
# 0) Paket (installera separat vid behov)
############################################################
install.packages(c(
  "tidyverse",
  "lubridate",
  "tsibble",
  "fable",
  "feasts",
  "tseries",
  "forecast"
))


library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(feasts)
library(tseries)
library(forecast) 


############################################################
# 1) Hjälpfunktioner
############################################################

#--- robust kolumnidentifiering för Eurostat-format ---
detect_time_col <- function(df) {
  nms <- toupper(names(df))
  if ("TIME" %in% nms) return(names(df)[match("TIME", nms)])
  if ("TIME_PERIOD" %in% nms) return(names(df)[match("TIME_PERIOD", nms)])
  stop("Hittar ej TIME eller TIME_PERIOD.")
}

detect_value_col <- function(df) {
  nms <- toupper(names(df))
  if ("OBS_VALUE" %in% nms) return(names(df)[match("OBS_VALUE", nms)])
  if ("VALUE" %in% nms) return(names(df)[match("VALUE", nms)])
  stop("Hittar ej OBS_VALUE eller VALUE.")
}

parse_eurostat_date <- function(x) {
  # hanterar "YYYY-MM" och "YYYY-MM-DD"
  ifelse(grepl("^\\d{4}-\\d{2}$", x),
         ymd(paste0(x, "-01")),
         suppressWarnings(ymd(x))) %>% as.Date()
}

#--- läs + filtrera HICP (CP00) för valda länder ---
load_hicp <- function(data_path, countries = c("SE","DE"), coicop = "CP00", years_back = 15) {
  df <- read.csv(data_path)
  names(df) <- toupper(names(df))
  
  time_col <- detect_time_col(df)
  val_col  <- detect_value_col(df)
  
  df <- df %>%
    mutate(
      DATE = parse_eurostat_date(.data[[time_col]]),
      HICP_INDEX = as.numeric(.data[[val_col]])
    )
  
  d <- df %>%
    filter(!is.na(DATE), !is.na(HICP_INDEX)) %>%
    filter(GEO %in% countries)
  
  # COICOP-filter om finns
  if ("COICOP" %in% names(d)) {
    d <- d %>% filter(toupper(COICOP) == toupper(coicop))
  }
  
  # Välj vanligaste UNIT om finns (så ni inte blandar enheter)
  if ("UNIT" %in% names(d)) {
    chosen_unit <- d %>% count(UNIT, sort = TRUE) %>% slice(1) %>% pull(UNIT)
    d <- d %>% filter(UNIT == chosen_unit)
  }
  
  # ta senaste years_back år
  last_date <- max(d$DATE, na.rm = TRUE)
  from_date <- last_date %m-% years(years_back)
  
  d %>%
    filter(DATE >= from_date) %>%
    select(GEO, DATE, HICP_INDEX) %>%
    arrange(GEO, DATE)
}

#--- bygg tsibble + inflationserier ---
make_tsibble <- function(plot_df, n_months = 180) {
  plot_df %>%
    group_by(GEO) %>%
    slice_tail(n = n_months) %>%    # exakt lika lång serie per land
    ungroup() %>%
    mutate(
      ym = yearmonth(DATE),
      log_index = log(HICP_INDEX)
    ) %>%
    select(GEO, ym, HICP_INDEX, log_index) %>%
    as_tsibble(key = GEO, index = ym) %>%
    group_by_key() %>%
    mutate(
      infl_mom = 100 * (log_index - lag(log_index)),
      infl_yoy = 100 * (log_index - lag(log_index, 12))
    ) %>%
    ungroup()
}

#--- split train/validation/test enligt N=180 och H ---
split_sets <- function(hicp_ts, H = 12, N = 180) {
  test_len  <- 3 * H
  valid_len <- 3 * H
  train_len <- N - test_len - valid_len
  if (train_len <= 0) stop("För kort serie för vald H. Minska H eller split-längder.")
  
  hicp_split <- hicp_ts %>%
    group_by_key() %>%
    mutate(
      rn = row_number(),
      set = case_when(
        rn <= train_len ~ "train",
        rn <= train_len + valid_len ~ "validation",
        TRUE ~ "test"
      )
    ) %>%
    ungroup() %>%
    select(-rn)
  
  list(
    hicp_split = hicp_split,
    train_len = train_len,
    valid_len = valid_len,
    test_len  = test_len
  )
}

#--- stationaritetstabell på train ---
stationarity_checks <- function(train_data) {
  train_data %>%
    as_tibble() %>%
    filter(!is.na(infl_mom)) %>%
    group_by(GEO) %>%
    summarise(
      n = n(),
      ADF_p  = if (n() >= 20) adf.test(infl_mom)$p.value  else NA_real_,
      KPSS_p = if (n() >= 20) kpss.test(infl_mom)$p.value else NA_real_,
      .groups = "drop"
    )
}

############################################################
# 2) Ladda + skapa serier
############################################################
data_path <- "X:/732G52/estat_prc_hicp_midx_filtered_en.csv"

plot_df <- load_hicp(data_path, countries = c("SE","DE"), coicop = "CP00", years_back = 15)
hicp    <- make_tsibble(plot_df, n_months = 180)

############################################################
# 3) Datakapitlet: grafer
############################################################




###########HICP för både länderna (separat)
country_name  <- c(SE = "Sverige",  DE = "Tyskland")
country_color <- c(DE = "#F8766D",  SE = "#00BFC4")  # samma som i din bild

p_se <- ggplot(filter(hicp, GEO == "SE"), aes(x = ym, y = HICP_INDEX, colour = GEO)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = country_color, labels = country_name) +
  labs(title = "HICP (HIKP) – indexnivå: Sverige", x = "Tid", y = "Index (basår enligt Eurostat)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

p_de <- ggplot(filter(hicp, GEO == "DE"), aes(x = ym, y = HICP_INDEX, colour = GEO)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = country_color, labels = country_name) +
  labs(title = "HICP (HIKP) – indexnivå: Tyskland", x = "Tid", y = "Index (basår enligt Eurostat)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

p_se
p_de



######################inflation för både länderna
# Färger som i din första figur
country_color <- c(DE = "#F8766D", SE = "#00BFC4")
country_name  <- c(DE = "Tyskland", SE = "Sverige")

# 1) Behåll bara månadsinflation och ta bort NA (första månaden per land blir NA)
infl_mom_df <- hicp %>%
  as_tibble() %>%                 # funkar även om hicp är tsibble
  select(GEO, ym, infl_mom) %>%
  filter(!is.na(infl_mom))

# (valfritt) samma y-axel i båda figurerna
y_rng <- range(infl_mom_df$infl_mom, na.rm = TRUE)

# 2) Separata plots
p_se <- infl_mom_df %>%
  filter(GEO == "SE") %>%
  ggplot(aes(x = ym, y = infl_mom)) +
  geom_line(linewidth = 0.6, color = country_color["SE"]) +
  labs(
    title = "Inflationstakt (transformerad från indexnivå)",
    subtitle = "Månatlig inflation (100*Δlog index) – Sverige",
    x = "Tid", y = "Procent"
  ) +
  coord_cartesian(ylim = y_rng) +   # ta bort raden om du vill ha “auto-zoom”
  theme_bw()

p_de <- infl_mom_df %>%
  filter(GEO == "DE") %>%
  ggplot(aes(x = ym, y = infl_mom)) +
  geom_line(linewidth = 0.6, color = country_color["DE"]) +
  labs(
    title = "Inflationstakt (transformerad från indexnivå)",
    subtitle = "Månatlig inflation (100*Δlog index) – Tyskland",
    x = "Tid", y = "Procent"
  ) +
  coord_cartesian(ylim = y_rng) +   # ta bort raden om du vill ha “auto-zoom”
  theme_bw()

p_se
p_de












# Säsongsgrafer (kör bara om ggtime finns)
if (requireNamespace("ggtime", quietly = TRUE)) {
  hicp %>%
    filter(!is.na(infl_mom)) %>%
    gg_season(infl_mom, labels = "both") +
    labs(title = "Säsongsmönster: Månatlig inflation (Δlog)")
  
  hicp %>%
    filter(!is.na(infl_mom)) %>%
    ggtime::gg_subseries(infl_mom) +
    labs(title = "Subseries: Månatlig inflation (Δlog)")
}

############################################################
# 4) Split: train/validation/test
############################################################
H <- 12
sp <- split_sets(hicp, H = H, N = 180)
hicp_split <- sp$hicp_split
train_len  <- sp$train_len

train_data <- hicp_split %>% filter(set == "train")
valid_data <- hicp_split %>% filter(set == "validation")
test_data  <- hicp_split %>% filter(set == "test")








############graf för uppdelning för data

#tabell
library(dplyr)
library(lubridate)
library(knitr)

hicp_split_tbl <- hicp_split %>%
  as_tibble() %>%
  mutate(
    set = factor(set, levels = c("train","validation","test"))
  )

split_table <- hicp_split_tbl %>%
  group_by(GEO, set) %>%
  summarise(
    start = min(ym),
    end   = max(ym),
    n_obs = n(),
    n_infl_nonNA = sum(!is.na(infl_mom)),  # bra att visa när ni modellerar inflation
    .groups = "drop"
  ) %>%
  arrange(GEO, set) %>%
  mutate(
    start = as.character(start),
    end   = as.character(end)
  )

kable(split_table,
      col.names = c("Land", "Delmängd", "Start", "Slut", "Antal obs", "Antal obs (infl_mom ej NA)"))




#plott

library(dplyr)
library(ggplot2)

split_counts <- hicp_split %>%
  as_tibble() %>%
  mutate(set = factor(set, levels = c("train","validation","test"))) %>%
  group_by(GEO, set) %>%
  summarise(n_obs = n(), .groups = "drop")

set_colors <- c(train = "#4C78A8", validation = "#F58518", test = "#54A24B")

ggplot(split_counts, aes(x = set, y = n_obs, fill = set)) +
  geom_col(width = 0.7) +
  facet_wrap(~GEO, ncol = 1) +
  scale_fill_manual(values = set_colors) +
  labs(
    title = "Uppdelning av tidsserier i träning, validering och test",
    x = "Delmängd (train/validation/test)",
    y = "Antal observationer",
    fill = "Delmängd"
  ) +
  theme_bw()




############################################################
# 5) Tolkbar modell: STL + TSLM (på inflation: infl_mom)
############################################################

# STL-dekomposition på månadsinflation
hicp %>%
  filter(!is.na(infl_mom)) %>%
  model(stl = STL(infl_mom)) %>%
  components() %>%
  autoplot() +
  labs(title = "STL-dekomposition: Månatlig inflation (infl_mom)")

# TSLM på månadsinflation (endast train+validation)
reg_fit_infl <- hicp_split %>%
  filter(set != "test", !is.na(infl_mom)) %>%
  model(tslm = TSLM(infl_mom ~ trend() + season()))

report(reg_fit_infl %>% filter(GEO == "DE") %>% select(tslm))
report(reg_fit_infl %>% filter(GEO == "SE") %>% select(tslm))








############################################################
# 6) ARIMA/SARIMA: identifiering + kandidater
############################################################
diff_suggestions <- train_data %>%
  filter(!is.na(infl_mom)) %>%
  group_by(GEO) %>%
  summarise(
    nd_infl_mom  = ndiffs(infl_mom),
    nsd_infl_mom = nsdiffs(infl_mom, m = 12),
    .groups = "drop"
  )

plot_arima_id <- function(country_code) {
  train_data %>%
    filter(GEO == country_code, !is.na(infl_mom)) %>%
    gg_tsdisplay(infl_mom, plot_type = "partial", lag_max = 48) +
    labs(title = paste0("ARIMA-identifikation (train): infl_mom – ", country_code))
}

plot_arima_id("SE")
plot_arima_id("DE")

# Manuella specs (uppdatera efter ACF/PACF)
manual_spec <- tibble(
  GEO = c("SE","DE"),
  p = c(3, 0), d = c(0, 0), q = c(0, 1),
  P = c(1, 1), D = c(0, 0), Q = c(0, 0)
)
















############################################################
# 7) CV-funktion per land (train+validation) – INFLATION
############################################################
run_cv_one_country <- function(country_code, hicp_split, manual_spec, H = 12) {
  
  # 1) Bygg train+validation för landet (på infl_mom) + behåll set för att räkna init korrekt
  tv0 <- hicp_split %>%
    filter(
      GEO == country_code,
      set %in% c("train","validation"),
      !is.na(infl_mom)
    ) %>%
    select(GEO, ym, infl_mom, set) %>%
    as_tsibble(key = GEO, index = ym)
  
  # 2) Effektiv train-längd efter NA-filter (infl_mom har NA i början)
  train_len_eff <- sum(tv0$set == "train")
  
  # 3) Data som modelleras (utan set-kolumn)
  tv <- tv0 %>% select(GEO, ym, infl_mom)
  
  # 4) Rolling-origin CV: starta vid slutet av train (korrekt init)
  tv_cv <- tv %>% stretch_tsibble(.init = train_len_eff, .step = 1)
  
  # 5) Hämta manuella ARIMA-specar för landet
  ms <- manual_spec %>% filter(GEO == country_code)
  if (nrow(ms) != 1) stop("manual_spec måste ha exakt 1 rad för landet: ", country_code)
  
  p <- ms$p[[1]]; d <- ms$d[[1]]; q <- ms$q[[1]]
  P <- ms$P[[1]]; D <- ms$D[[1]]; Q <- ms$Q[[1]]
  
  # 6) Fit modeller på varje CV-fönster
  fits_cv <- tv_cv %>%
    model(
      ETS          = ETS(infl_mom),
      ARIMA_auto   = ARIMA(infl_mom),
      ARIMA_manual = ARIMA(infl_mom ~ pdq(p, d, q) + PDQ(P, D, Q)),
      Reg_ARIMA    = ARIMA(infl_mom ~ trend() + season())
    )
  
  # 7) Faktiska värden för att matcha forecast mot actual
  actual_tbl <- tv %>%
    as_tibble() %>%
    transmute(ym, actual = infl_mom) %>%
    distinct(ym, .keep_all = TRUE)
  
  # 8) Prognoser + matchning med actual
  fc_cv <- fits_cv %>%
    forecast(h = H) %>%
    as_tibble() %>%
    group_by(.id, .model) %>%
    arrange(ym, .by_group = TRUE) %>%
    mutate(h = row_number()) %>%
    ungroup() %>%
    left_join(actual_tbl, by = "ym") %>%
    filter(!is.na(actual))
  
  # 9) CV-mått per horisont
  cv_metrics <- fc_cv %>%
    mutate(err = actual - .mean) %>%
    group_by(.model, h) %>%
    summarise(
      RMSE = sqrt(mean(err^2)),
      MAE  = mean(abs(err)),
      .groups = "drop"
    ) %>%
    mutate(GEO = country_code)
  
  # 10) Välj bästa modell (lägsta medel-RMSE över horisonter)
  best_model <- cv_metrics %>%
    group_by(.model) %>%
    summarise(mean_RMSE = mean(RMSE), .groups = "drop") %>%
    arrange(mean_RMSE) %>%
    slice(1) %>%
    pull(.model)
  
  # 11) Fit slutligt på train+validation (för fitted-vs-actual och residualer)
  fit_train_valid <- tv %>%
    model(
      ETS          = ETS(infl_mom),
      ARIMA_auto   = ARIMA(infl_mom),
      ARIMA_manual = ARIMA(infl_mom ~ pdq(p, d, q) + PDQ(P, D, Q)),
      Reg_ARIMA    = ARIMA(infl_mom ~ trend() + season())
    )
  
  list(
    country = country_code,
    train_len_eff = train_len_eff,
    cv_metrics = cv_metrics,
    best_model = best_model,
    fit_train_valid = fit_train_valid
  )
}

# Kör för SE och DE
cv_SE <- run_cv_one_country("SE", hicp_split, manual_spec, H = H)
cv_DE <- run_cv_one_country("DE", hicp_split, manual_spec, H = H)

# Samla och plotta CV-resultat
cv_all <- bind_rows(cv_SE$cv_metrics, cv_DE$cv_metrics)

ggplot(cv_all, aes(x = h, y = RMSE, colour = .model)) +
  geom_line() +
  facet_wrap(~GEO, ncol = 1) +
  labs(
    title = "CV (train+validation): RMSE per prognoshorisont – infl_mom",
    x = "Horisont (månader)",
    y = "RMSE"
  )

ggplot(cv_all, aes(x = h, y = MAE, colour = .model)) +
  geom_line() +
  facet_wrap(~GEO, ncol = 1) +
  labs(
    title = "CV (train+validation): MAE per prognoshorisont – infl_mom",
    x = "Horisont (månader)",
    y = "MAE"
  )

cat("Bästa modell (SE):", cv_SE$best_model, "\n")
cat("Bästa modell (DE):", cv_DE$best_model, "\n")









############################################################
# 8) Fitted vs actual (train+validation)
############################################################
plot_fitted_vs_actual <- function(fit_obj, country_code) {
  aug <- augment(fit_obj) %>%
    as_tibble() %>%
    mutate(GEO = country_code) %>%
    select(GEO, ym, .model, actual = infl_mom, fitted = .fitted)
  
  aug_long <- aug %>%
    pivot_longer(c(actual, fitted), names_to = "type", values_to = "value")
  
  ggplot(aug_long, aes(x = ym, y = value, linetype = type)) +
    geom_line() +
    facet_wrap(~.model, ncol = 1, scales = "free_y") +
    labs(title = paste0("Train+validation: fitted vs actual (", country_code, ")"),
         x = "Tid", y = "Månatlig inflation (100*Δlog index)", linetype = "")
}

plot_fitted_vs_actual(cv_SE$fit_train_valid, "SE")
plot_fitted_vs_actual(cv_DE$fit_train_valid, "DE")

############################################################
# 9) Residualdiagnostik (bästa modellen)
############################################################
resid_diag <- function(fit_obj, country_code, model_name) {
  fit_obj %>%
    select(all_of(model_name)) %>%
    gg_tsresiduals() +
    labs(title = paste0("Residualdiagnostik: ", country_code, " (", model_name, ")"))
}

resid_diag(cv_SE$fit_train_valid, "SE", cv_SE$best_model)
resid_diag(cv_DE$fit_train_valid, "DE", cv_DE$best_model)

############################################################
# 10) Test-utvärdering (endast bästa modellen) – INFLATION
############################################################
run_test_eval_one_country <- function(country_code, hicp_split, manual_spec, best_model_name, H = 12) {
  
  # 1) Full serie (train+validation+test) för landet, på infl_mom
  #    OBS: filtrerar bort NA i infl_mom (första månaden per land)
  full <- hicp_split %>%
    filter(GEO == country_code, !is.na(infl_mom)) %>%
    select(GEO, ym, infl_mom, set) %>%
    as_tsibble(key = GEO, index = ym)
  
  # 2) Init = antal obs i train+validation (EFTER NA-filter)
  init_full_eff <- sum(full$set %in% c("train", "validation"))
  
  # Rolling-origin CV som börjar efter train+validation
  full_cv <- full %>%
    stretch_tsibble(.init = init_full_eff, .step = 1)
  
  # 3) Manuella ARIMA-specar
  ms <- manual_spec %>% filter(GEO == country_code)
  if (nrow(ms) != 1) stop("manual_spec måste ha exakt 1 rad för landet: ", country_code)
  
  p <- ms$p[[1]]; d <- ms$d[[1]]; q <- ms$q[[1]]
  P <- ms$P[[1]]; D <- ms$D[[1]]; Q <- ms$Q[[1]]
  
  # 4) Fit modeller på varje rullande fönster och behåll bara bästa modellen
  fits_best <- full_cv %>%
    model(
      ETS          = ETS(infl_mom),
      ARIMA_auto   = ARIMA(infl_mom),
      ARIMA_manual = ARIMA(infl_mom ~ pdq(p, d, q) + PDQ(P, D, Q)),
      Reg_ARIMA    = ARIMA(infl_mom ~ trend() + season())
    ) %>%
    select(all_of(best_model_name))
  
  # 5) Tabell med faktiska värden + set (för att filtrera till test)
  actual_tbl <- full %>%
    as_tibble() %>%
    transmute(ym, actual = infl_mom, set) %>%
    distinct(ym, .keep_all = TRUE)
  
  # 6) Prognoser + matchning mot actual, men bara där actual ligger i TEST
  fc <- fits_best %>%
    forecast(h = H) %>%
    as_tibble() %>%
    group_by(.id, .model) %>%
    arrange(ym, .by_group = TRUE) %>%
    mutate(h = row_number()) %>%
    ungroup() %>%
    left_join(actual_tbl, by = "ym") %>%
    filter(set == "test", !is.na(actual))
  
  # 7) Test-mått per horisont
  test_metrics <- fc %>%
    mutate(err = actual - .mean) %>%
    group_by(.model, h) %>%
    summarise(
      RMSE = sqrt(mean(err^2)),
      MAE  = mean(abs(err)),
      .groups = "drop"
    ) %>%
    mutate(GEO = country_code)
  
  list(fc = fc, test_metrics = test_metrics)
}

# Kör test för SE/DE med respektive bästa modell från steg 7
test_SE <- run_test_eval_one_country("SE", hicp_split, manual_spec, cv_SE$best_model, H = H)
test_DE <- run_test_eval_one_country("DE", hicp_split, manual_spec, cv_DE$best_model, H = H)

test_all <- bind_rows(test_SE$test_metrics, test_DE$test_metrics)

ggplot(test_all, aes(x = h, y = RMSE, colour = .model)) +
  geom_line() +
  facet_wrap(~GEO, ncol = 1) +
  labs(title = "TEST (bästa modell): RMSE per prognoshorisont – infl_mom",
       x = "Horisont (månader)", y = "RMSE")

ggplot(test_all, aes(x = h, y = MAE, colour = .model)) +
  geom_line() +
  facet_wrap(~GEO, ncol = 1) +
  labs(title = "TEST (bästa modell): MAE per prognoshorisont – infl_mom",
       x = "Horisont (månader)", y = "MAE")

############################################################
# 11) Slutprognos: skatta om bästa modellen på ALL data och prognostisera H steg
############################################################
forecast_future_one_country <- function(country_code, hicp_ts, manual_spec, best_model_name, H = 12) {
  
  full <- hicp_ts %>%
    filter(GEO == country_code, !is.na(infl_mom)) %>%
    select(GEO, ym, infl_mom) %>%
    as_tsibble(key = GEO, index = ym)
  
  ms <- manual_spec %>% filter(GEO == country_code)
  
  fit_full <- full %>%
    model(
      ETS          = ETS(infl_mom),
      ARIMA_auto   = ARIMA(infl_mom),
      ARIMA_manual = ARIMA(infl_mom ~ pdq(ms$p, ms$d, ms$q) + PDQ(ms$P, ms$D, ms$Q)),
      Reg_ARIMA    = ARIMA(infl_mom ~ trend() + season())
    ) %>%
    select(all_of(best_model_name))
  
  fc <- fit_full %>% forecast(h = H)
  
  autoplot(fc) +
    autolayer(full, infl_mom) +
    labs(
      title = paste0("Slutprognos (", country_code, "): ", best_model_name, " på infl_mom"),
      x = "Tid",
      y = "Månatlig inflation (100*Δlog index)"
    )
}

forecast_future_one_country("SE", hicp, manual_spec, cv_SE$best_model, H = H)
forecast_future_one_country("DE", hicp, manual_spec, cv_DE$best_model, H = H)

