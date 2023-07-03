library(fpp3)
library(latex2exp)
library(data.table)
library(knitr)
library(ggpubr)

## Cross validation

## -----------------Step 1: Data preparing-------------------
rm(list = ls(all = TRUE))

# Get hourly visits dataset from CSV file
vis_data <-
  readr::read_csv("open_hourly_visits.csv") %>% as_tsibble(index = time)

# Divide training sets and test sets of cross-validation
rl_init <- 24 * 827
rl_step <- 24 * 30
h_step <- 24 * 30

vis_training <- vis_data %>%
  filter_index("2020-01-23 00" ~ "2023-04-23 23") %>%
  stretch_tsibble(.init = rl_init, .step = rl_step)

## ---------------------Step 2: Forecasting--------------------

# Fit models
# 4 combinations,  3 models,  1 benchmark
com_fit <- vis_training %>%
  model(
    snaive = SNAIVE(visits ~ lag("day")),
    sarima = ARIMA(visits),
    nnetar = NNETAR(visits, MaxNWts = 84581),
    stl_arima = decomposition_model(
      STL(
        visits ~
          season(period = 24) +
          season(period = 24 * 7) +
          season(period = 24 * 365.25),
        robust = TRUE
      ),
      ARIMA(season_adjust ~ PDQ(0, 0, 0))
    )
  ) %>%
  mutate(
    combination_1 = (stl_arima + sarima) / 2,
    combination_2 = (stl_arima + nnetar) / 2,
    combination_3 = (sarima + nnetar) / 2,
    combination_4 = (nnetar + sarima + stl_arima) / 3
  )

# Do the forecasting
com_fc <- com_fit %>% fabletools::forecast(h = h_step)

# Calculate the accuracy
com_acc <- fabletools::accuracy(com_fc, vis_data)
com_acc

## ---------------------Step 3: Compare results--------------------

# Get forecast horizon accuracy of cross-validation
fc_acc_by_model <- function(model_name) {
  fc <- com_fc %>%
    filter(.model == model_name) %>%
    group_by(.id) %>%
    mutate(h = row_number() %/% 24 + 1) %>%
    ungroup() %>%
    as_fable(response = "visits", distribution = visits)
  
  return(fc %>%
           fabletools::accuracy(vis_data, by = c("h", ".model")) %>%
           filter(h < 31))
}

model_names <- c(
  "combination_1",
  "combination_2",
  "combination_3",
  "combination_4",
  "stl_arima",
  "snaive",
  "sarima",
  "nnetar"
)

for (m in model_names) {
  if (exists("horizon_acc")) {
    horizon_acc <- horizon_acc %>% add_row(fc_acc_by_model(m))
  } else {
    horizon_acc <- fc_acc_by_model(m)
  }
}

horizon_acc <- horizon_acc %>%
  mutate(
    model = recode(
      .model,
      snaive = "SNaïve",
      sarima = "SARIMA",
      stl_arima = "STLF",
      nnetar = "NNAR",
      combination_1 = "Hybrid SARIMA-STLF",
      combination_2 = "Hybrid NNAR-STLF",
      combination_3 = "Hybrid SARIMA-NNAR",
      combination_4 = "Hybrid SARIMA-NNAR-STLF"
    )
  )

# Plot MAE for forecast horizon accuracy
pdf("cv_mae.pdf")
horizon_acc %>%
  ggplot(aes(
    x = h,
    y = MAE,
    colour = model,
    shape = model
  )) +
  geom_point(size = 2.5) +
  xlab("Forecast horizon (day)") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 10, 0, "pt"),
    legend.box.margin = margin(0, 0, 0, 0, "pt"),
    legend.key.size = unit(15, "pt"),
    legend.text = element_text(size = 8)
  ) +
  theme(plot.margin = unit(c(20, 20, 10, 10), "pt")) +
  scale_shape_manual(values = c(17, 2, 17, 2, 3, 20, 3, 20))
dev.off()

# Plot RMSE for forecast horizon accuracy
pdf("cv_rmse.pdf")
horizon_acc %>%
  ggplot(aes(
    x = h,
    y = RMSE,
    colour = model,
    shape = model
  )) +
  geom_point(size = 2.5) +
  xlab("Forecast horizon (day)") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 10, 0, "pt"),
    legend.box.margin = margin(0, 0, 0, 0, "pt"),
    legend.key.size = unit(15, "pt"),
    legend.text = element_text(size = 8)
  ) +
  theme(plot.margin = unit(c(20, 20, 10, 10), "pt")) +
  scale_shape_manual(values = c(17, 2, 17, 2, 3, 20, 3, 20))
dev.off()

# Statistics of forecast accuracy by month
month_acc <- com_fc %>%
  mutate(m = month(time)) %>%
  as_fable(response = "visits", distribution = visits) %>%
  fabletools::accuracy(vis_data, by = c("m", ".model"))

# Plot boxplot
box_data <- data.frame(month = month_acc$m,
                       RMSE = month_acc$RMSE,
                       MAE = month_acc$MAE)

pdf("month_rmse_boxplot.pdf")
boxplot(
  box_data$RMSE ~ box_data$m,
  boxwex = 0.8,
  col = rgb(0.2, 0.3, 0.8, 0.5),
  ylab = "RMSE",
  xlab = "Month"
)
dev.off()

pdf("month_mae_boxplot.pdf")
boxplot(
  box_data$MAE ~ box_data$m,
  boxwex = 0.8,
  col = rgb(0.2, 0.3, 0.8, 0.5),
  ylab = "MAE",
  xlab = "Month"
)
dev.off()

# Get forecast accuracy for the next few days
acc_of_consequent_days <- function(model_name, h_day) {
  fc <- com_fc %>%
    filter(.model == model_name) %>%
    group_by(.id) %>%
    mutate(h = row_number() %/% 24 + 1) %>%
    ungroup() %>%
    as_fable(response = "visits", distribution = visits)
  
  return(fc %>% filter(h <= h_day) %>%
           fabletools::accuracy(vis_data))
}

for (m in model_names) {
  if (exists("acc_15_days")) {
    acc_15_days <-
      acc_15_days %>% add_row(acc_of_consequent_days(m, 15))
  } else {
    acc_15_days <- acc_of_consequent_days(m, 15)
  }
}

# Accuracy metrics results for the next 15 days
acc_15_days %>%
  mutate(
    model = recode(
      .model,
      snaive = "SNaïve",
      sarima = "SARIMA",
      stl_arima = "STLF",
      nnetar = "NNAR",
      combination_1 = "Hybrid SARIMA-STLF",
      combination_2 = "Hybrid NNAR-STLF",
      combination_3 = "Hybrid SARIMA-NNAR",
      combination_4 = "Hybrid SARIMA-NNAR-STLF"
    )
  ) %>% select(.model, model, MAE, RMSE) %>% arrange(RMSE)
