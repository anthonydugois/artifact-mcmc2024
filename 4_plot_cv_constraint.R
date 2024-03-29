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

meas_names <- rev(c("cv", "mean_cv_row", "mean_cv_col", "correlation_row",
                    "correlation_col"))
meas_lab <- rev(c("CV", "row CV", "col CV", "row corr", "col corr"))

coefficient <- c(seq(from = 0, to = 1.5, by = 0.3), "\\mathrm{NA}")

p <- df %>%
  gather(measure, value, meas_names) %>%
  mutate(measure = factor(measure, levels = meas_names, labels = meas_lab)) %>%
  mutate(row = ifelse(is.na(row), "\\mathrm{NA}", row), col = ifelse(is.na(col), "\\mathrm{NA}", col)) %>%
  mutate(row = factor(row, levels = coefficient, labels = paste0("$\\mathrm{CV}_{\\overline{\\mu}}=", coefficient, "$"))) %>%
  mutate(col = factor(col, levels = coefficient, labels = paste0("$\\mathrm{CV}_{\\overline{\\nu}}=", coefficient, "$"))) %>%
  ggplot(aes(x = measure, y = value)) +
  geom_boxplot(outlier.size = 1) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL) +
  facet_grid(row ~ col) +
  coord_flip(ylim = c(-0.2, 1.5))

tikz(file = "plots/4_plot_cv_constraint.tex", width = 7, height = 6)
print(p)
dev.off()
