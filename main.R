library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)

breaks <- c(0.5625, 1.125, 2.25, 4.5, 9)

bin.risk.levels <- function (data) {
  risk.levels <- cut(
    data$weekly_rate,
    breaks = c(-1, breaks, Inf),
    labels = c(
      "Low (< 0.56)",
      "Medium (< 1.13)",
      "High (< 2.25)",
      "Very High (< 4.5)",
      "Extremely High (< 9)",
      "Ludicrously High (> 9)")
  )
  levels(risk.levels) <- c(levels(risk.levels), "Incomplete")
  data %>%
    mutate(risk_level = risk.levels)
}

read <- function() {
  read_csv("Weekly_Rates_of_Laboratory-Confirmed_COVID-19_Hospitalizations_from_the_COVID-NET_Surveillance_System.csv") %>%
    filter(
      State == "Oregon"
      & `AgeCategory_Legend` == "All"
      & `Sex_Label` == "All"
      & `Race_Label` == "All"
    ) %>%
    transmute(
      date = as.Date(`_WeekendDate`, "%m/%d/%Y"),
      weekly_rate = WeeklyRate
    ) %>%
    arrange(date) %>%
    bin.risk.levels()
}

extrapolate <- function (data, num.weeks = 4) {
  last.date <- max(filter(data, date < max(data$date))$date)
  valid.data <- filter(data, !is.na(weekly_rate))
  last.month <- valid.data[(nrow(valid.data) - 1):(nrow(valid.data) - num.weeks),] %>%
    mutate(log_weekly_rate = log(weekly_rate))
  model <- lm(log_weekly_rate ~ date, data = last.month)
  b <- model$coefficients[[1]]
  m <- model$coefficients[[2]]
  tibble(
    date = last.date + c(0, 7, 14, 21, 28),
    weekly_rate = NA,
    risk_level = NA
  ) %>%
    mutate(extrapolated = ifelse(
      date < last.month$date[[1]],
      NA,
      exp(m * as.numeric(date) + b))) %>%
    mutate(extrapolated = ifelse(extrapolated > max(valid.data$weekly_rate), NA, extrapolated))
}

plot <- function (data, future, log_scale) {
  # Last data point is incomplete
  data <- cbind(data)
  data[(nrow(data) - 1):nrow(data),]$risk_level = "Incomplete"
  complete.data <- data[1:(nrow(data) - 2),]
  data %>%
    ggplot(aes(x = date, y = weekly_rate, col = risk_level)) +
    geom_hline(yintercept = c(0, breaks), color = "#bbbbbb") +
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
    scale_y_continuous(trans = ifelse(log_scale, "log10", "identity")) +
    scale_color_manual(
      name = "Risk level (cases/100k)",
      values = c("#a4c96f", "#f0c300", "#ff8000", "#e13220", "#930d6e", "black", "#7f7f7f")
    ) +
    geom_line(data = complete.data, color = "black") +
    geom_point() +
    geom_line(data = future, aes(y = extrapolated), color = "black", linetype = "dashed") +
    ggtitle(label = 'Rates of laboratory-confirmed COVID-19-associated hospitalization', sub = 'From CDC COVID-NET surveillance data in Oregon') +
    xlab('Week ending') +
    ylab(paste('Weekly cases per 100k people', ifelse(log_scale, "(log scale)", ""))) +
    theme(
      axis.text.x = element_text(angle = 315, hjust = 0)
    )
}

main <- function (argv = c()) {
  log_scale <- "--log" %in% argv
  data <- read()
  last_dates <- data %>%
    filter(!is.na(weekly_rate)) %>%
    group_by(risk_level) %>%
    summarise(last_date = max(date))
  print(last_dates)
  future <- extrapolate(data)
  plot(data, future, log_scale)
}
