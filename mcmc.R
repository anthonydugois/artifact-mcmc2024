correlation <- function(mat) {
    corr <- suppressWarnings(cor(mat))
    if (sum(!is.na(corr)) == nrow(corr))
      return(NA)
    (sum(corr, na.rm = TRUE) - nrow(corr)) / (sum(!is.na(corr)) - nrow(corr))
}

mean_cv_row <- function(mat) {
  mean(apply(mat, 1, sd) / apply(mat, 1, mean))
}

mean_cv_col <- function(mat) {
  mean(apply(mat, 2, sd) / apply(mat, 2, mean))
}

cv_mean_row <- function(mat) {
  sums <- rowSums(mat)
  sd(sums) / mean(sums)
}

cv_mean_col <- function(mat) {
  sums <- colSums(mat)
  sd(sums) / mean(sums)
}

measures <- c(
  function(x) c(cv = sd(x) / mean(x)),
  function(x) suppressWarnings(chisq.test(x)$statistic),
  function(x) c(correlation_row = correlation(t(x))),
  function(x) c(correlation_col = correlation(x)),
  function(x) c(mean_cv_row = mean_cv_row(x)),
  function(x) c(mean_cv_col = mean_cv_col(x)),
  function(x) c(cv_mean_row = cv_mean_row(x)),
  function(x) c(cv_mean_col = cv_mean_col(x))
)

# TODO what if iterations == 0?
mcmc <- function(mat, iterations,
                 mat_min = 0 * mat, mat_max = sum(mat) + 0 * mat,
                 intermediate_meas = NULL, final_meas = intermediate_meas) {
  library(dplyr)
  library(purrr)
  intermediate <- list()
  intermediate[[1]] <- c(iter = 0, k = NA, a = NA, b = NA,
                         flatten(map(intermediate_meas, ~.(mat))))
  for (iter in 1:iterations) {
    AA <- sample(nrow(mat), 2)
    BB <- sample(ncol(mat), 2)
    sousmat <- mat[AA, BB]
    sousmat_min <- mat_min[AA, BB]
    sousmat_max <- mat_max[AA, BB]
    a <- max((sousmat_min[1,1] - sousmat[1,1]),
             (sousmat[1,2] - sousmat_max[1,2]),
             (sousmat[2,1] - sousmat_max[2,1]),
             (sousmat_min[2,2] - sousmat[2,2]))
    b <- min((sousmat_max[1,1] - sousmat[1,1]),
             (sousmat[1,2] - sousmat_min[1,2]),
             (sousmat[2,1] - sousmat_min[2,1]),
             (sousmat_max[2,2] - sousmat[2,2]))
    k <- sample(a:b, size=1)
    sousmat[1,1] <- sousmat[1,1] + k
    sousmat[1,2] <- sousmat[1,2] - k
    sousmat[2,1] <- sousmat[2,1] - k
    sousmat[2,2] <- sousmat[2,2] + k
    mat[AA, BB] <- sousmat
    intermediate[[iter + 1]] <- c(iter = iter, k = k, a = a, b = b,
                                  flatten(map(intermediate_meas, ~.(mat))))
  }
  list(mat = mat,
       inter = bind_rows(intermediate),
       final = flatten(map(final_meas, ~.(mat))))
}
