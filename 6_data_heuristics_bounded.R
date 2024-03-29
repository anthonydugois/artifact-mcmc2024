suppressMessages({
	library(dplyr)
	library(reticulate)
})

source("heuristics.R")
source("initialization.R")
source("mcmc.R")

use_python("/home/adugois/munu_var/venv/bin/python")

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

coefficient_rowcol <- c(0, 1.5, NA)
coefficient_bound <- c(0, 0.75, 1)
# coefficient_rowcol <- c(1.5)
# coefficient_bound <- c(1)
param <- expand.grid(rowcol = coefficient_rowcol, bound = coefficient_bound)

mat_min_ones <- matrix(1, nrow = size_row, ncol = size_col)
result <- list()

set.seed(1)

for (i in seq_len(nrow(param))) {
	print(paste(i, "/", nrow(param)))

	row_sum <- weight * (if (is.na(param$rowcol[i])) mv$random_vecs(data, size_row, sum_all_elements, alpha_row, sum_all_elements, number_vectors, 2L * i)
					 else mv$random_vecs_cv(data, size_row, sum_all_elements, param$rowcol[i], err_CV, err_CV, alpha_row, sum_all_elements, number_vectors, 2L * i))

	col_sum <- weight * (if (is.na(param$rowcol[i])) mv$random_vecs(data, size_col, sum_all_elements, alpha_col, sum_all_elements, number_vectors, 2L * i + 1L)
					 else mv$random_vecs_cv(data, size_col, sum_all_elements, param$rowcol[i], err_CV, err_CV, alpha_col, sum_all_elements, number_vectors, 2L * i + 1L))

	for (j in 1:nrow(row_sum)) {
    	print(paste(" ", j, "/", nrow(row_sum)))

		mat_prop_real <- row_sum[j,] %*% t(col_sum[j,]) / (weight * sum_all_elements)
		b <- param$bound[i]

		mat_min <- pmax(mat_min_ones, floor(b * mat_prop_real))
		mat_min <- fix_mat_min(row_sum[j,], col_sum[j,], mat_min)

		mat_max <- ceiling(mat_prop_real / b)
		mat_max <- fix_mat_max(row_sum[j,], col_sum[j,], mat_min, mat_max) + 1

    	mat <- generate_matrix_prop(row_sum = row_sum[j,], col_sum = col_sum[j,], mat_min = mat_min, mat_max = mat_max)

		res <- mcmc(mat, iterations, final_meas = measures, mat_min = mat_min, mat_max = mat_max)

		bal_suff <- balance_sufferage(res$mat)
		LPT <- LPT_unrelated(res$mat)
		EFT <- MinMin(res$mat)

		result[[length(result) + 1]] <- c(res$final, instance = j, param[i, c("rowcol", "bound")], bal_suff = bal_suff, LPT = LPT, EFT = EFT)
	}
}

df <- bind_rows(result)

write.csv(df, gzfile("__data__/__results__6_data_heuristics_bounded__.csv.gz"))
 
