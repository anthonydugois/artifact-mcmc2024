suppressMessages({
	library(readr)
	library(dplyr)
	library(ggplot2)
	library(scales)
	library(zoo)
	library(tikzDevice)
})

source("plotting.R")

CV <- function(n, N, S) {
	sqrt(S / n - N * N / (n * n)) / (N / n)
}

input_data <- commandArgs(trailingOnly = TRUE)  # .csv.gz files

data <- read_csv(input_data, col_types = "iidd")

n_value <- 10
N_value <- 100

sm_median <- data %>%
    filter(N == N_value, count > 0) %>%
    mutate(median = my_rollmedian(count, 15))

S_min <- N_value^2 / n_value
S_max <- N_value^2

# We also keep the NA to show them in the final plot (except for
# even/odd sums of squares). Could have been done with 'dist' column
sm_median_NA <- data %>%
    filter(N == N_value & S >= S_min & S <= S_max) %>%
    group_by(cum_sum = cumsum(count)) %>%
    filter(row_number() == 3) %>% # avoid inserting NA for each even/odd S
    ungroup() %>%
    select(-cum_sum) %>%
    mutate(median = NA)

sm_median <- sm_median %>%
    rbind(sm_median_NA) %>%
    arrange(S)

sm_min_max <- data %>%
    filter(N == N_value, count > 0) %>%
    mutate(min = my_rollquantile(count, 15, 0),
           max = my_rollquantile(count, 15, 1))

pal <- viridis_pal()(10)

g <- ggplot(mapping = aes(x = CV(n_value, N_value, S))) %+%
    geom_ribbon(data = sm_min_max %>% filter(approx_point(S, min, prec = 0.001) | approx_point(S, max, prec = 0.001)),
                mapping = aes(ymin = min, ymax = max), fill = pal[[1]], alpha = 0.3) %+%
    geom_line(data = sm_median %>% filter(approx_point(S, median, prec = 0.001)),
              mapping = aes(y = median), size = 0.75, colour = pal[[1]]) %+%
    scale_x_continuous(name = "CV", n.breaks = 6) %+%
    scale_y_continuous(name = "Number of solutions", n.breaks = 6) %+%
    geom_vline(xintercept = c(0.5, 1.5))

tikz(file = "plots/1_plot_cv_distribution.tex", width = 3.25, height = 1.75)
print(g)
dev.off()

print("Proportion of vectors with 0.5 <= CV <= 1.5")
total_count <- data %>%
  filter(N == N_value) %>%
  summarise(sum = sum(count))
center_count <- data %>%
  filter(N == N_value, CV(n_value, N_value, S) >= 0.5, CV(n_value, N_value, S) <= 1.5) %>%
  summarise(sum = sum(count))
print(center_count / total_count)
print("Proportion of vectors with CV > 2")
right_count <- data %>%
  filter(N == N_value, CV(n_value, N_value, S) >= 2) %>%
  summarise(sum = sum(count))
print(right_count / total_count)

#eps <- 0.01 # 0.00000001
#ratios <- data %>%
#	filter(N <= N_value) %>%
#    group_by(N) %>%
#    #filter(dist > eps * (N * N - N * N / n_value)) %>%
#    filter(dist > max(1, eps * (N * N - N * N / n_value))) %>%
#    slice_min(dist, with_ties = FALSE) %>%
#    mutate(ratio = S / (N * N))
#
#g <- ggplot(ratios, mapping = aes(x = N, y = ratio)) %+%
#    geom_point(size = 0.8, shape = 3, colour = pal[[1]]) %+%
#    scale_x_continuous(name = "N") %+%
#    scale_y_continuous(name = "Safety rate ($\\varepsilon=0.01$)", breaks = seq(0, 1, by = 0.2)) %+%
#    coord_cartesian(ylim = c(0, 1))
#
#tikz(file = "plots/1_plot_cv_safety.tex", width = 3.25, height = 1.75)
#print(g)
#dev.off()

data_CV <- data %>%
  filter(!is.na(dist)) %>%
  mutate(CV = CV(n_value, N, S)) %>%
  mutate(next_CV_diff = CV(n_value, N, S + dist + 1) - CV)

result <- list()
for (curr_N in 2:100) {
  data_CV_curr_N <- data_CV %>% filter(N == curr_N)
  for (curr_CV in seq(0, 3, 0.04))
    result[[length(result) + 1]] <- c(N = curr_N, CV = curr_CV, data_CV_curr_N %>%
                     filter(CV <= curr_CV) %>%
                     arrange(desc(CV)) %>%
                     slice(1) %>%
                     select(next_CV_diff) %>%
                     unlist())
}

g <- bind_rows(result) %>%
  ggplot(aes(x = CV, y = N, fill = next_CV_diff)) +
  geom_tile() +
  scale_fill_viridis_b("CV difference\nwith closest vector",
                       breaks = c(0.001, 0.01, 0.1, 0.999999999999),
                       trans = "log", na.value = "white",
                       labels = scales::label_number(drop0trailing=TRUE)) +
  scale_y_continuous("$N$") +
  theme(legend.position = "bottom", legend.key.width = unit(0.75, "cm"))

tikz(file = "plots/1_plot_cv_safety.tex", width = 3.25, height = 3.5)
print(g)
dev.off()
