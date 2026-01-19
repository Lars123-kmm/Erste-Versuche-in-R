# ============================================================
# PAKETE & GRUNDEINSTELLUNGEN
# ============================================================

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(scales)
library(forecast)
library(tseries)

theme_set(theme_minimal(base_size = 12))

base_path <- "Datenpfad"


# ============================================================
# DATEIPFADE + CHECKS
# ============================================================

mil_path  <- file.path(base_path, "Military_spend.csv")
tech_path <- file.path(base_path, "Technological innovations.csv")
env_path  <- file.path(base_path, "environmental_data.csv")

cat("\n--- Datei-Check ---\n")
if (!file.exists(mil_path))  stop("Fehlt: ", mil_path)
if (!file.exists(tech_path)) stop("Fehlt: ", tech_path)
if (!file.exists(env_path))  stop("Fehlt: ", env_path)
cat("Alle Dateien gefunden.\n")


# ============================================================
# ROBUSTES NUMERIC-PARSING (kann auch 2.03E+06 / Tausendertrennzeichen)
# ============================================================

to_num <- function(x) {
  x <- as.character(x)
  x <- gsub("\\s+", "", x)   # Leerzeichen raus
  x <- gsub(",", "", x)      # Tausendertrennzeichen raus
  suppressWarnings(as.numeric(x))  # kann scientific notation
}


# ============================================================
# DATEN EINLESEN
# ============================================================

cat("\n--- Lese Military Spending ---\n")
mil_raw <- read_csv(mil_path, show_col_types = FALSE) |> clean_names()
print(head(mil_raw, 3))
cat("Spalten:", paste(names(mil_raw), collapse = ", "), "\n")

cat("\n--- Lese Technological Innovations ---\n")
tech_raw <- read_csv(tech_path, show_col_types = FALSE) |> clean_names()
print(head(tech_raw, 3))
cat("Spalten:", paste(names(tech_raw), collapse = ", "), "\n")

cat("\n--- Lese Environmental Data ---\n")
env_raw <- read_csv(env_path, show_col_types = FALSE) |> clean_names()
print(head(env_raw, 3))
cat("Spalten:", paste(names(env_raw), collapse = ", "), "\n")


# ============================================================
# SPALTEN-CHECKS
# ============================================================

if (!("year" %in% names(mil_raw)))  stop("Military: Spalte 'year' fehlt.")
if (!("military_spending" %in% names(mil_raw))) stop("Military: Spalte 'military_spending' fehlt.")

if (!("year" %in% names(tech_raw))) stop("Tech: Spalte 'year' fehlt.")
if (!("technological_innovations" %in% names(tech_raw))) stop("Tech: Spalte 'technological_innovations' fehlt.")

if (!("year" %in% names(env_raw))) stop("Env: Spalte 'year' fehlt.")
if (!("annual_co2_emissions_per_capita" %in% names(env_raw))) {
  stop("Env: Spalte 'annual_co2_emissions_per_capita' fehlt. Spalten sind: ",
       paste(names(env_raw), collapse = ", "))
}


# ============================================================
# DATEN AUFBEREITEN (inkl. Konsolen-Outputs)
# ============================================================

cat("\n--- Bereite Military Spending auf ---\n")
mil <- mil_raw |>
  transmute(
    year = as.integer(year),
    military_spending = to_num(military_spending)
  ) |>
  filter(!is.na(year)) |>
  distinct(year, .keep_all = TRUE) |>
  arrange(year)

print(summary(mil))
cat("Jahre:", min(mil$year), "-", max(mil$year), "\n")
cat("Letzte Zeilen:\n"); print(tail(mil, 5))

cat("\n--- Bereite Technological Innovations auf ---\n")
tech <- tech_raw |>
  transmute(
    year = as.integer(year),
    technological_innovations = to_num(technological_innovations)
  ) |>
  filter(!is.na(year)) |>
  distinct(year, .keep_all = TRUE) |>
  arrange(year)

print(summary(tech))
cat("Jahre:", min(tech$year), "-", max(tech$year), "\n")
cat("Letzte Zeilen:\n"); print(tail(tech, 5))

cat("\n--- Bereite CO2-Daten auf ---\n")
env <- env_raw |>
  transmute(
    year = as.integer(year),
    co2_per_capita = to_num(annual_co2_emissions_per_capita)
  ) |>
  filter(!is.na(year)) |>
  distinct(year, .keep_all = TRUE) |>
  arrange(year)

print(summary(env))
cat("Jahre:", min(env$year), "-", max(env$year), "\n")
cat("Letzte Zeilen:\n"); print(tail(env, 5))


# ============================================================
# QUICK SANITY CHECKS (sichtbar, aber nicht überladen)
# ============================================================

cat("\n--- Sanity Checks ---\n")

mil_last <- tail(mil$military_spending, 1)
mil_prev <- tail(mil$military_spending, 2)[1]
if (is.finite(mil_last) && is.finite(mil_prev) && mil_last < 0.2 * mil_prev) {
  warning("Military: letzter Wert ist massiv kleiner als der vorherige -> Daten/Parsing prüfen.")
}

co2_last <- tail(env$co2_per_capita, 1)
co2_prev <- tail(env$co2_per_capita, 2)[1]
if (is.finite(co2_last) && is.finite(co2_prev) && co2_last < 0.5 * co2_prev) {
  warning("CO2: letzter Wert ist stark kleiner als der vorherige -> Daten/Parsing prüfen.")
}

cat("OK.\n")


# ============================================================
# EINZELPLOTS (HISTORISCH)
# ============================================================

cat("\n--- Erzeuge Einzelplots (historisch) ---\n")

p_mil <- ggplot(mil, aes(year, military_spending)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(title = "Military spending over time", x = "Year", y = "Military spending")

p_tech <- ggplot(tech, aes(year, technological_innovations)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(title = "Technological innovations over time", x = "Year", y = "Technological innovations")

p_env <- ggplot(env, aes(year, co2_per_capita)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  labs(title = "CO2 emissions per capita over time", x = "Year", y = "CO2 per capita")

print(p_mil)
print(p_tech)
print(p_env)


# ============================================================
# DATEN ZUSAMMENFÜHREN + CHECK
# ============================================================

cat("\n--- Kombiniere Datensätze ---\n")

combined <- mil |>
  full_join(tech, by = "year") |>
  full_join(env,  by = "year") |>
  arrange(year)

cat("Zeilen gesamt:", nrow(combined), "\n")
cat(
  "Gemeinsame Jahre (alle 3 Variablen):",
  sum(complete.cases(combined[, c("military_spending", "technological_innovations", "co2_per_capita")])),
  "\n"
)
print(head(combined, 5))


# ============================================================
# LONG-FORMAT
# ============================================================

combined_long <- combined |>
  pivot_longer(
    cols = c(military_spending, technological_innovations, co2_per_capita),
    names_to = "series",
    values_to = "value"
  ) |>
  mutate(
    series = recode(
      series,
      military_spending = "Military spending",
      technological_innovations = "Technological innovations",
      co2_per_capita = "CO2 per capita"
    )
  )


# ============================================================
# KOMBIPLOT (FACETS, KEINE VERZERRUNG)
# ============================================================

cat("\n--- Kombiplot (Facets) ---\n")

p_facet <- ggplot(combined_long, aes(year, value)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  facet_wrap(~ series, scales = "free_y", ncol = 1) +
  labs(title = "Trends over time", x = "Year", y = NULL)

print(p_facet)


# ============================================================
# INDEX = 100 (TRENDVERGLEICH) + KONSOLEN-OUTPUT
# ============================================================

cat("\n--- Index-Berechnung (base = 100) ---\n")

indexed <- combined_long |>
  group_by(series) |>
  arrange(year) |>
  mutate(
    base_year  = first(year[!is.na(value)]),
    base_value = first(value[!is.na(value)]),
    index_100  = 100 * value / base_value
  ) |>
  ungroup()

indexed |>
  group_by(series) |>
  summarise(
    base_year = first(base_year),
    base_value = first(base_value),
    n_obs = sum(!is.na(value))
  ) |>
  print()

p_index <- ggplot(indexed, aes(year, index_100, color = series)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  labs(title = "Trend comparison (base = 100)", x = "Year", y = "Index", color = "Series")

print(p_index)


# ============================================================
# ZEITREIHEN ERSTELLEN (NA-SAFE)
# ============================================================

cat("\n--- Erzeuge Zeitreihen ---\n")

mil_ts_data  <- mil  |> filter(!is.na(military_spending))
tech_ts_data <- tech |> filter(!is.na(technological_innovations))
env_ts_data  <- env  |> filter(!is.na(co2_per_capita))

if (nrow(mil_ts_data) < 10)  warning("Military: sehr wenige Beobachtungen für ARIMA.")
if (nrow(tech_ts_data) < 10) warning("Tech: sehr wenige Beobachtungen für ARIMA.")
if (nrow(env_ts_data) < 10)  warning("CO2: sehr wenige Beobachtungen für ARIMA.")

ts_mil <- ts(mil_ts_data$military_spending, start = min(mil_ts_data$year), frequency = 1)
ts_tech <- ts(tech_ts_data$technological_innovations, start = min(tech_ts_data$year), frequency = 1)
ts_co2 <- ts(env_ts_data$co2_per_capita, start = min(env_ts_data$year), frequency = 1)

cat("Zeitreihen erstellt.\n")


# ============================================================
# STATIONARITÄT (ADF-TEST) + WARNUNG BEI KURZER REIHE
# ============================================================

if (length(ts_mil) < 20)  warning("ADF-Test Military: sehr kurze Zeitreihe (<20 Beobachtungen).")
if (length(ts_tech) < 20) warning("ADF-Test Tech: sehr kurze Zeitreihe (<20 Beobachtungen).")
if (length(ts_co2) < 20)  warning("ADF-Test CO2: sehr kurze Zeitreihe (<20 Beobachtungen).")

cat("\n--- ADF-Test: Military spending ---\n")
print(adf.test(ts_mil))

cat("\n--- ADF-Test: Technological innovations ---\n")
print(adf.test(ts_tech))

cat("\n--- ADF-Test: CO2 per capita ---\n")
print(adf.test(ts_co2))


# ============================================================
# ARIMA-MODELLE
# ============================================================

cat("\n--- ARIMA: Military spending ---\n")
fit_mil <- auto.arima(ts_mil, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
print(summary(fit_mil))
cat("Gewähltes Modell (p,d,q):", paste(arimaorder(fit_mil), collapse = ","), "\n")

cat("\n--- ARIMA: Technological innovations ---\n")
fit_tech <- auto.arima(ts_tech, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
print(summary(fit_tech))
cat("Gewähltes Modell (p,d,q):", paste(arimaorder(fit_tech), collapse = ","), "\n")

cat("\n--- ARIMA: CO2 per capita ---\n")
fit_co2 <- auto.arima(ts_co2, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
print(summary(fit_co2))
cat("Gewähltes Modell (p,d,q):", paste(arimaorder(fit_co2), collapse = ","), "\n")


# ------------------------------------------------------------
# RESIDUEN-DIAGNOSE
# ------------------------------------------------------------

cat("\n--- Residuen-Check: Military ---\n")
checkresiduals(fit_mil)

cat("\n--- Residuen-Check: Tech ---\n")
checkresiduals(fit_tech)

cat("\n--- Residuen-Check: CO2 ---\n")
checkresiduals(fit_co2)


# ============================================================
# FORECAST (10 JAHRE) + KONSOLEN-OUTPUT
# ============================================================

cat("\n--- Forecasts (10 Jahre) ---\n")

fc_mil <- forecast(fit_mil, h = 10)
fc_tech <- forecast(fit_tech, h = 10)
fc_co2 <- forecast(fit_co2, h = 10)

cat("\nMilitary spending forecast:\n")
print(fc_mil)
cat("Forecast-Jahre:", start(fc_mil$mean), "bis", end(fc_mil$mean), "\n")

cat("\nTechnological innovations forecast:\n")
print(fc_tech)
cat("Forecast-Jahre:", start(fc_tech$mean), "bis", end(fc_tech$mean), "\n")

cat("\nCO2 per capita forecast:\n")
print(fc_co2)
cat("Forecast-Jahre:", start(fc_co2$mean), "bis", end(fc_co2$mean), "\n")

p_fc_mil <- autoplot(fc_mil) +
  labs(title = "Forecast: Military spending", x = "Year", y = "Military spending")

p_fc_tech <- autoplot(fc_tech) +
  labs(title = "Forecast: Technological innovations", x = "Year", y = "Technological innovations")

p_fc_co2 <- autoplot(fc_co2) +
  labs(title = "Forecast: CO2 per capita", x = "Year", y = "CO2 per capita")

print(p_fc_mil)
print(p_fc_tech)
print(p_fc_co2)

# ============================================================
# VAR / GRANGER – ZUSAMMENHANGSANALYSE (korrekt)
# Voraussetzung: combined existiert bereits und enthält:
# year, military_spending, technological_innovations, co2_per_capita
# ============================================================

library(vars)

cat("\n--- VAR/Granger: baue gemeinsames Sample ---\n")

var_df <- combined |>
  select(year, military_spending, technological_innovations, co2_per_capita) |>
  filter(
    !is.na(military_spending),
    !is.na(technological_innovations),
    !is.na(co2_per_capita)
  ) |>
  arrange(year)

cat("Gemeinsame Beobachtungen:", nrow(var_df), "\n")
cat("Jahre:", min(var_df$year), "-", max(var_df$year), "\n")
print(head(var_df, 5))

if (nrow(var_df) < 20) {
  warning("VAR/Granger: Wenige gemeinsame Beobachtungen (<20). Ergebnisse vorsichtig interpretieren.")
}

# ------------------------------------------------------------
# Stationarisierung (Log-Differenzen wenn alles > 0, sonst Differenzen)
# ------------------------------------------------------------

cat("\n--- VAR/Granger: Stationarisierung ---\n")

all_positive <- all(
  var_df$military_spending > 0,
  var_df$technological_innovations > 0,
  var_df$co2_per_capita > 0
)

if (all_positive) {
  cat("Nutze Log-Differenzen (dlog).\n")
  
  Y <- var_df |>
    transmute(
      d_mil  = diff(log(military_spending)),
      d_tech = diff(log(technological_innovations)),
      d_co2  = diff(log(co2_per_capita))
    ) |>
    as.data.frame()
  
  years_used <- var_df$year[-1]
  
} else {
  cat("Werte nicht strikt >0. Nutze einfache Differenzen.\n")
  
  Y <- var_df |>
    transmute(
      d_mil  = diff(military_spending),
      d_tech = diff(technological_innovations),
      d_co2  = diff(co2_per_capita)
    ) |>
    as.data.frame()
  
  years_used <- var_df$year[-1]
}

cat("Stationarisierte Daten (kurz):\n")
print(summary(Y))
cat("NA je Spalte:\n")
print(colSums(is.na(Y)))

# NA raus (sollte normalerweise 0 sein)
Y <- Y |>
  as_tibble() |>
  drop_na() |>
  as.data.frame()

cat("Beobachtungen nach drop_na:", nrow(Y), "\n")

if (nrow(Y) < 15) stop("Zu wenige Beobachtungen nach Stationarisierung für VAR.")


# ------------------------------------------------------------
# Lag-Auswahl
# ------------------------------------------------------------

cat("\n--- VAR: Lag-Auswahl (AIC/HQ/SC/FPE) ---\n")

max_lag <- min(10, floor(nrow(Y) / 5))
if (max_lag < 1) stop("Zu wenige Beobachtungen für Lag-Auswahl.")

lag_sel <- VARselect(Y, lag.max = max_lag, type = "const")
print(lag_sel$selection)

# konservativ: SC(n) (BIC)
p <- as.integer(lag_sel$selection["SC(n)"])
if (is.na(p) || p < 1) p <- 1

cat("Gewählter Lag p =", p, "\n")


# ------------------------------------------------------------
# VAR schätzen + Summary
# ------------------------------------------------------------

cat("\n--- VAR: Schätzung ---\n")
var_fit <- VAR(Y, p = p, type = "const")
print(summary(var_fit))


# ------------------------------------------------------------
# Diagnostik: Serial correlation + Stabilität
# ------------------------------------------------------------

cat("\n--- VAR: Diagnostik ---\n")

cat("\nSerial correlation (Portmanteau):\n")
print(serial.test(var_fit, lags.pt = 16, type = "PT.asymptotic"))

cat("\nStabilität (Roots):\n")
roots_vals <- roots(var_fit)
print(roots_vals)

if (any(Mod(roots_vals) >= 1)) {
  warning("VAR ist nicht stabil (mindestens eine Root >= 1). Lag/Transformation prüfen.")
} else {
  cat("VAR stabil (alle Roots < 1).\n")
}


# ------------------------------------------------------------
# Granger-Causality (gerichtete Vorhersagekraft)
# ------------------------------------------------------------

cat("\n--- Granger-Causality Tests ---\n")

cat("\nH0: d_tech verursacht d_mil NICHT (Granger)\n")
print(causality(var_fit, cause = "d_tech")$Granger)

cat("\nH0: d_co2 verursacht d_mil NICHT (Granger)\n")
print(causality(var_fit, cause = "d_co2")$Granger)

cat("\nH0: d_mil verursacht d_tech NICHT (Granger)\n")
print(causality(var_fit, cause = "d_mil")$Granger)

cat("\nH0: d_co2 verursacht d_tech NICHT (Granger)\n")
print(causality(var_fit, cause = "d_co2")$Granger)

cat("\nH0: d_mil verursacht d_co2 NICHT (Granger)\n")
print(causality(var_fit, cause = "d_mil")$Granger)

cat("\nH0: d_tech verursacht d_co2 NICHT (Granger)\n")
print(causality(var_fit, cause = "d_tech")$Granger)


# ------------------------------------------------------------
# Optional: IRF (Impuls-Antwort-Funktionen)
# ------------------------------------------------------------

cat("\n--- IRF (optional) ---\n")

irf_mil <- irf(var_fit, impulse = "d_mil", response = c("d_tech", "d_co2"),
               n.ahead = 10, boot = TRUE)
plot(irf_mil)

irf_tech <- irf(var_fit, impulse = "d_tech", response = c("d_mil", "d_co2"),
                n.ahead = 10, boot = TRUE)
plot(irf_tech)

irf_co2 <- irf(var_fit, impulse = "d_co2", response = c("d_mil", "d_tech"),
               n.ahead = 10, boot = TRUE)
plot(irf_co2)

cat("\n--- VAR/Granger fertig ---\n")


# ============================================================
# EXPORT
# ============================================================

cat("\n--- Exportiere Plots ---\n")

out_dir <- file.path(base_path, "plots")
dir.create(out_dir, showWarnings = FALSE)

if (!dir.exists(out_dir)) stop("Konnte Output-Ordner nicht erstellen: ", out_dir)
cat("Output-Ordner:", out_dir, "\n")

ggsave(file.path(out_dir, "01_military.png"), p_mil, width = 12, height = 4.5, dpi = 200)
ggsave(file.path(out_dir, "02_tech.png"),     p_tech, width = 12, height = 4.5, dpi = 200)
ggsave(file.path(out_dir, "03_co2.png"),      p_env,  width = 12, height = 4.5, dpi = 200)
ggsave(file.path(out_dir, "04_combined_facets.png"), p_facet, width = 12, height = 9, dpi = 200)
ggsave(file.path(out_dir, "05_combined_index.png"),  p_index, width = 12, height = 5.5, dpi = 200)

ggsave(file.path(out_dir, "06_forecast_military.png"), p_fc_mil, width = 12, height = 5.5, dpi = 200)
ggsave(file.path(out_dir, "07_forecast_tech.png"),     p_fc_tech, width = 12, height = 5.5, dpi = 200)
ggsave(file.path(out_dir, "08_forecast_co2.png"),      p_fc_co2, width = 12, height = 5.5, dpi = 200)


# ============================================================
# ABSCHLUSS-CHECK
# ============================================================

cat("\n--- Abschluss-Check ---\n")
cat("Military NA:", sum(is.na(mil$military_spending)), "\n")
cat("Tech NA:", sum(is.na(tech$technological_innovations)), "\n")
cat("CO2 NA:", sum(is.na(env$co2_per_capita)), "\n")
cat("Alle Analysen abgeschlossen.\n")

cat("\n--- FERTIG ---\n")
