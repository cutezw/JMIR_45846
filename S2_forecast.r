library(fpp3)
library(latex2exp)
library(data.table)
library(knitr)

library(ggpattern)

## Compare results of different models for 15-day forecasting

## -----------------Step 1: Data preparing-------------------
rm(list = ls(all = TRUE))

# Get hourly visits dataset from CSV file
vis_data <-
  readr::read_csv("open_hourly_visits.csv") %>% as_tsibble(index = time)

# Divide data into training set and test set
vis_training <- vis_data %>%
  filter_index("2020-01-23 00" ~ "2023-04-23 23")

h_step <- 24 * 15

training_tail <- vis_training %>% tail(1)
future_begin <- training_tail$time + hours(1)
future_end <- training_tail$time + hours(h_step)
vis_future <- vis_data %>%
  filter_index(as.character(future_begin) ~ as.character(future_end))

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

# Get models estimates and summary measures
com_fit %>% select(snaive) %>% report()
com_fit %>% select(sarima) %>% report()
com_fit %>% select(nnetar) %>% report()
com_fit %>% select(stl_arima) %>% report()

com_fit %>% select(sarima) %>% coef()
com_fit %>% select(sarima) %>% str()
com_fit %>% select(stl_arima) %>% str()

# Do the forecasting
com_fc <- com_fit %>% fabletools::forecast(h = h_step)

# Calculate the accuracy
com_acc <- fabletools::accuracy(com_fc, vis_data)
com_acc

## ---------------------Step 3: Compare results--------------------

# Plot the forecast results
fig_map <-
  tibble(
    m_name = c(
      "snaive",
      "sarima",
      "nnetar",
      "stl_arima",
      "combination_1",
      "combination_2",
      "combination_3",
      "combination_4"
    ),
    t_name = c(
      "SNaïve",
      "SARIMA",
      "NNAR",
      "STLF",
      "Hybrid SARIMA-STLF",
      "Hybrid NNAR-STLF",
      "Hybrid SARIMA-NNAR",
      "Hybrid SARIMA-NNAR-STLF"
    )
  )

for (i in seq(nrow(fig_map))) {
  model_fc <- com_fc %>% filter(.model == fig_map[i,]$m_name)
  vis_future$fc_visits <- model_fc$.mean
  
  pdf(
    paste("results_", fig_map[i,]$t_name, ".pdf", sep = ""),
    width = 7,
    height = 5
  )
  ggplot(vis_future, aes(x = time)) +
    geom_line(aes(y = visits, color = "Observation")) +
    geom_line(aes(
      y = fc_visits,
      color = paste(fig_map[i,]$t_name, "Prediction", sep = " ")
    )) +
    scale_color_manual(
      '',
      values = c("#1cdf3cc8", "#1d55e1c7"),
      breaks = c(paste(fig_map[i,]$t_name, "Prediction", sep = " "), "Observation")
    ) +
    labs(x = 'Hour', y = 'Visits') +
    theme(
      legend.position = c(0, 1),
      legend.justification = c("left", "top"),
      legend.key = element_blank(),
      legend.title = element_blank(),
      legend.background = element_rect(
        fill = "white",
        colour = "gray",
        linewidth = 0.1
      ),
      legend.margin = margin(t = -0.2, unit = 'cm')
    )
  dev.off()
  
  Sys.sleep(3)
}

# Accuracy metrics for all models
# RMSE, MAE
acc_data <- com_acc %>% mutate(
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
) %>% select(.model, model, RMSE, MAE)

acc_data %>% arrange(RMSE)
acc_data %>% arrange(MAE)

tiff(
  filename = "acc_bar.tiff",
  width = 20,
  height = 15,
  units = "cm",
  res = 300
)
acc_data %>%
  pivot_longer(cols = RMSE:MAE,
               names_to = "measure",
               values_to = "value") %>%
  ggplot(mapping = aes(x = .model, y = value, fill = model)) +
  geom_col_pattern(
    aes(pattern_type = model),
    pattern = "magick",
    pattern_fill = "#f8734ada",
    fill = "white",
    pattern_scale = 2,
    colour = "#f57650f3",
    linewidth = 0.05,
    pattern_density = 2
  ) +
  scale_pattern_type_discrete(
    choices = c(
      "gray50",
      "hs_bdiagonal",
      "hs_horizontal",
      "hs_diagcross",
      "hs_vertical",
      "gray100",
      "hs_fdiagonal",
      "gray0"
    )
  ) +
  facet_wrap(~ `measure`) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  geom_text(
    aes(label = round(value, 2)),
    vjust = 0,
    color = "black",
    size = 2.8
  ) +
  theme(aspect.ratio = 1) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 15, 0, "pt"),
    legend.box.margin = margin(0, 0, 0, 0, "pt"),
    legend.key.size = unit(20, "pt"),
    legend.text = element_text(size = 9)
  ) +
  theme(plot.margin = unit(c(10, 0, 0, 0), "pt"))
dev.off()