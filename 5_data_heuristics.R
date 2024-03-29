suppressMessages({
	library(dplyr)
	library(reticulate)
})

source("heuristics.R")
source("initialization.R")
source("mcmc.R")

use_python("/home/adugois/.pyenv/versions/3.9.6/bin/python")

np <- import("numpy")
mv <- import("munu_var")

input_data <- commandArgs(trailingOnly = TRUE)  # .npy file

data <- np$load(input_data)

iterations <- 50000
number_vectors <- 30L
size_row <- 20L
size_col <- 10L
alpha_row <- 1L
alpha_col <- 2L
sum_all_elements <- 400L
weight <- 10
err_CV <- 0.01

coefficient_row <- c(0, 0, 0, 1.5, 1.5, 1.5, NA, NA, NA)
coefficient_col <- c(0, 1.5, NA, 0, 1.5, NA, 0, 1.5, NA)
param <- data.frame(row = coefficient_row, col = coefficient_col)

mat_min_ones <- matrix(1, nrow = size_row, ncol = size_col)
result <- list()

set.seed(1)

for (i in seq_len(nrow(param))) {
	print(paste(i, "/", nrow(param)))

	row_sum <- weight * (if (is.na(param$row[i])) mv$random_vecs(data, size_row, sum_all_elements, alpha_row, sum_all_elements, number_vectors, 2L * i)
					 else mv$random_vecs_cv(data, size_row, sum_all_elements, param$row[i], err_CV, err_CV, alpha_row, sum_all_elements, number_vectors, 2L * i))

	col_sum <- weight * (if (is.na(param$col[i])) mv$random_vecs(data, size_col, sum_all_elements, alpha_col, sum_all_elements, number_vectors, 2L * i + 1L)
					 else mv$random_vecs_cv(data, size_col, sum_all_elements, param$col[i], err_CV, err_CV, alpha_col, sum_all_elements, number_vectors, 2L * i + 1L))

	for (j in 1:nrow(row_sum)) {
    	print(paste(" ", j, "/", nrow(row_sum)))
    	mat <- generate_matrix_prop(row_sum = row_sum[j,], col_sum = col_sum[j,], mat_min = mat_min_ones)
		res <- mcmc(mat, iterations, final_meas = measures, mat_min = mat_min_ones)
		bal_suff <- balance_sufferage(res$mat)
		LPT <- LPT_unrelated(res$mat)
		EFT <- MinMin(res$mat)
		result[[length(result) + 1]] <- c(res$final, instance = j, param[i, c("row", "col")], bal_suff = bal_suff, LPT = LPT, EFT = EFT)
	}
}

df <- bind_rows(result)

write.csv(df, gzfile("__data__/__results__5_data_heuristics__.csv.gz"))
 
