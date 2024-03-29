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
coefficient_rowcol <- c(0, 0, 0, 1.5, 1.5, 1.5, na, na, na)
coefficient_bound <- c(0, 0.75, 1, 0, 0.75, 1, 0, 0.75, 1)

param_names <- paste(coefficient_rowcol, coefficient_bound, sep = ".")
param_lab <- paste0("$\\mathrm{CV}=", coefficient_rowcol, ",\\lambda=", coefficient_bound, "$")
algo_names <- c("bal_suff", "LPT", "EFT")
algo_lab <- c("BalSuff", "HLPT", "EFT")

d <- df %>%
  mutate(min_mks = pmin(bal_suff, LPT, EFT)) %>%
  mutate(rowcol = ifelse(is.na(rowcol), na, rowcol), bound = ifelse(is.na(bound), na, bound)) %>%
  mutate(param = factor(interaction(rowcol, bound), levels = param_names, labels = param_lab)) %>%
  gather(algo, value, algo_names) %>%
  mutate(algo = factor(algo, levels = algo_names, labels = algo_lab))

p <- d %>%
  ggplot(aes(x = algo, y = value / min_mks)) +
  geom_boxplot(outlier.size = 1) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Relative makespan") +
  facet_wrap(~ param)

tikz(file = "plots/6_plot_heuristics_bounded.tex", width = 5, height = 4)
print(p)
dev.off()

p <- d %>%
  filter( (rowcol == 1.5 & bound == 0)  |
          (rowcol == 0 & bound == 0.75) |
          (rowcol == na & bound == 1) ) %>%
  ggplot(aes(x = algo, y = value / min_mks)) +
  geom_boxplot(outlier.size = 1) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Relative makespan") +
  facet_wrap(~ param)

tikz(file = "plots/6_plot_heuristics_bounded_paper.tex", width = 5, height = 1.75)
print(p)
dev.off()

