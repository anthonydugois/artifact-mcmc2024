suppressMessages({
	library(readr)
	library(dplyr)
	library(ggplot2)
	library(tikzDevice)
})

CV_levels <- c(0.5, 1.0, 1.5)
CV_labels <- c(0.5, 1.0, 1.5)

input_data <- commandArgs(trailingOnly = TRUE)  # .csv.gz files

data <- read_csv(input_data, col_types = "iiddiiidd") %>%
	mutate(n = factor(n), N = factor(N), CV = factor(CV, levels = CV_levels, labels = CV_labels))

g <- ggplot(data %>% filter(norm_frequency > 0),
		    mapping = aes(x = value, y = norm_frequency)) %+%
    geom_col(mapping = aes(fill = CV), position = "identity", alpha = 1,
             colour = "black", size = 0.1, width = 1) %+%
    geom_point(mapping = aes(shape = CV), size = 1) %+%
    scale_x_continuous(name = "Vector value", breaks = c(1, 10, 20, 30, 40, 50)) %+%
    scale_y_continuous(name = "Normalized frequency", breaks = seq(0, 1, 0.2)) %+%
    scale_fill_viridis_d(name = "CV") %+%
    scale_shape_manual(name = "CV", values = c(1, 4, 17)) %+%
    theme(legend.position = "bottom")

tikz(file = "plots/2_plot_value_distribution.tex", width = 3.25, height = 2.25)
print(g)
dev.off()
