library(fpp3)
library(data.table)
library(knitr)
library(readr)


## Exploratory Data Analysis

rm(list = ls(all = TRUE))

# Get hourly visits dataset from CSV file
vis_data <-
  readr::read_csv("open_hourly_visits.csv") %>% as_tsibble(index = time)

# The time point of the changed policy
policy_interrupt <-
  vis_data %>% filter(time == as.Date("2022-12-19"))

# Plot time plot of the dataset
pdf("time_plot.pdf", width = 7, height = 5)

ggplot(vis_data, aes(x = time, y = visits)) + geom_line() + labs(y = "Hourly visits", x =
                                                                   "Year") +
  geom_vline(
    xintercept = as.numeric(vis_data$time[policy_interrupt$...1]),
    linetype = "dashed",
    color = "red",
    linewidth = 0.5
  )
dev.off()

# Plot multi-seasonal patterns of the time series
# daily pattern
pdf("daily_pattern.pdf", width = 7, height = 5)
vis_data %>% gg_season(visits, period = "day") +
  theme(legend.position = "none") +
  labs(y = "Hourly visits", x = "Hour of the day")
dev.off()

# weekly pattern
pdf("weekly_pattern.pdf", width = 7, height = 5)
vis_data %>% gg_season(visits, period = "week") +
  theme(legend.position = "none") +
  labs(y = "Hourly visits", x = "Hour of the week")
dev.off()

# yearly pattern
pdf("yearly_pattern.pdf", width = 8, height = 5)
vis_data %>% gg_season(visits, period = "year") +
  labs(y = "Hourly visits", x = "Hour of the year")
dev.off()

# Plot ACF and PACF analysis of the time series
pdf("acf.pdf")
vis_data %>% gg_tsdisplay(visits, lag_max = 48, plot_type = "partial")
dev.off()

pdf("acf_dif.pdf")
vis_data %>% gg_tsdisplay(difference(visits, 24),
                          lag_max = 48,
                          plot_type = "partial")
dev.off()

# Plot the STL decomposition of the time series
pdf("stl.pdf")
vis_decomp <- vis_data %>%
  model(STL(
    visits ~ season(period = 24) +
      season(period = 24 * 7) +
      season(period = 24 * 365.25),
    robust = TRUE
  )) %>%
  components()
vis_decomp %>%
  autoplot() + labs(x = "Hour of the year")
dev.off()
