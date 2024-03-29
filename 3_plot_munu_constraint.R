suppressMessages({
	library(readr)
	library(tidyr)
	library(purrr)
	library(dplyr)
	library(ggplot2)
	library(tikzDevice)
})

input_data <- commandArgs(trailingOnly = TRUE)  # .csv.gz files

df <- read_csv(input_data)

df <- df %>%
  mutate(type = ifelse(is.na(row), "nu", ifelse(is.na(col), "mu", "munu")))

meas_names <- c("cv", "mean_cv_row", "mean_cv_col", "cv_mean_row",
                "cv_mean_col", "X-squared", "correlation_row",
                "correlation_col")
meas_lab <- c("CV", "row CV", "col CV", "$\\overline{\\mu}$ CV",
              "$\\overline{\\nu}$ CV", "$\\chi^2$", "row corr", "col corr")
type_lab <- c("$\\overline{\\mu}$", "$\\overline{\\nu}$",
              "$\\overline{\\mu}\\overline{\\nu}$")

p <- df %>%
  gather(measure, value, meas_names) %>%
  filter(measure != "X-squared") %>%
  mutate(coef = ifelse(is.na(row), col, row)) %>%
  mutate(type = factor(type, levels = c("mu", "nu", "munu"), labels = type_lab)) %>%
  mutate(measure = factor(measure, levels = meas_names, labels = meas_lab)) %>%
  ggplot(aes(x = factor(coef), y = value)) +
  geom_boxplot(outlier.size = 0.8) +
  scale_x_discrete(name = "CV") +
  scale_y_continuous(name = NULL) +
  facet_grid(measure ~ type, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

tikz(file = "plots/3_plot_munu_constraint.tex", width = 5, height = 6.5)
print(p)
dev.off()

