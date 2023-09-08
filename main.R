library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)

breaks <- c(0, 0.5625, 1.125, 2.25, 4.5)

bin.risk.levels <- function (data) {
  data %>%
    mutate(risk_level = cut(
      data$weekly_rate,
      breaks = c(breaks, Inf),
      labels = c(
        "Low (< 0.56)",
        "Medium (< 1.13)",
        "High (< 2.25)",
        "Very High (< 4.5)",
        "Extremely High (> 4.5)")
    ))
}

read <- function () {
  read_csv("COVID-19Surveillance_All_Data.csv") %>%
    filter(
      CATCHMENT == 'Oregon'
      & `AGE CATEGORY` == 'Overall'
      & SEX == 'Overall'
      & RACE == 'Overall'
      & `WEEKLY RATE` != 'null'
    ) %>%
    transmute(
      date = as.Date(paste0(`MMWR-YEAR`, str_pad(`MMWR-WEEK` - 1, 2, pad = "0"), "0"), "%Y%U%w") + 6 + 7,
      `WEEKLY RATE`,
      weekly_rate = as.numeric(`WEEKLY RATE`)
    ) %>%
    bin.risk.levels()
}

plot <- function (data, log_scale) {
  data %>%
    ggplot(aes(x = date, y = weekly_rate, col = risk_level)) +
    geom_hline(yintercept = breaks) +
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
    scale_y_continuous(trans = ifelse(log_scale, "log10", "identity")) +
    scale_color_manual(
      name = "Risk level (cases/100k)",
      values = c("#a4c96f", "#f0c300", "#ff8000", "#e13220", "#930d6e")
    ) +
    geom_line(color = "black") +
    geom_point() +
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
    group_by(risk_level) %>%
    summarise(last_date = max(date))
  print(last_dates)
  plot(data, log_scale)
}
