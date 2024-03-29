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

na <- "\\mathrm{NA}"
coefficient_row <- c(0, 0, 0, 1.5, 1.5, 1.5, na, na, na)
coefficient_col <- c(0, 1.5, na, 0, 1.5, na, 0, 1.5, na)

param_names <- paste(coefficient_row, coefficient_col, sep = ".")
param_lab <- paste0("$\\mathrm{CV}_{\\overline{\\mu}}=", coefficient_row, ",\\mathrm{CV}_{\\overline{\\nu}}=", coefficient_col, "$")
algo_names <- c("bal_suff", "LPT", "EFT")
algo_lab <- c("BalSuff", "HLPT", "EFT")

p <- df %>%
  mutate(min_mks = pmin(bal_suff, LPT, EFT)) %>%
  mutate(row = ifelse(is.na(row), na, row), col = ifelse(is.na(col), na, col)) %>%
  mutate(param = factor(interaction(row, col), levels = param_names, labels = param_lab)) %>%
  gather(algo, value, algo_names) %>%
  mutate(algo = factor(algo, levels = algo_names, labels = algo_lab)) %>%
  ggplot(aes(x = algo, y = value / min_mks)) +
  geom_boxplot(outlier.size = 1) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Relative makespan") +
  facet_wrap(~ param)

tikz(file = "plots/5_plot_heuristics.tex", width = 5, height = 4)
print(p)
dev.off()

