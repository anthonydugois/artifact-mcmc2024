generate_matrix_homo <- function(row_sum, col_sum = row_sum) {
  library(dplyr)
  library(purrr)
  stopifnot(sum(row_sum) == sum(col_sum))
  stopifnot(any(c(row_sum, col_sum) >= 0))
  mat_max <- pmin(row_sum %*% t(rep(1, length(col_sum))),
                  rep(1, length(row_sum)) %*% t(col_sum))
  mat <- row_sum %*% t(col_sum) * 0
  while (sum(col_sum) != 0) {
    transpose <- FALSE
    if (max(row_sum) / length(col_sum) < max(col_sum) / length(row_sum)) {
      transpose <- TRUE
      mat <- t(mat)
      tmp <- col_sum
      col_sum <- row_sum
      row_sum <- tmp
    }
    i <- which.max(row_sum)
    order_j <- order(col_sum)
    for (k in seq_along(order_j)) {
      j <- order_j[k]
      d <- min(col_sum[j], round(row_sum[i] / (length(col_sum) - k + 1)))
      mat[i,j] <- mat[i,j] + d
      row_sum[i] <- row_sum[i] - d
      col_sum[j] <- col_sum[j] - d
    }
    if (transpose) {
      mat <- t(mat)
      tmp <- col_sum
      col_sum <- row_sum
      row_sum <- tmp
    }
  }
  stopifnot(all(c(row_sum, col_sum) == 0))
  stopifnot(!any(mat < 0 | mat > mat_max))
  mat
}

generate_matrix_hetero <- function(row_sum, col_sum = row_sum) {
  library(dplyr)
  library(purrr)
  stopifnot(sum(row_sum) == sum(col_sum))
  stopifnot(any(c(row_sum, col_sum) >= 0))
  mat_max <- pmin(row_sum %*% t(rep(1, length(col_sum))),
                  rep(1, length(row_sum)) %*% t(col_sum))
  mat <- row_sum %*% t(col_sum) * 0
  row_sum <- row_sum - rowSums(mat)
  col_sum <- col_sum - colSums(mat)
  index <- expand.grid(i = seq_along(row_sum), j = seq_along(col_sum))
  # Shuffle indexes to allow different max to be chosen at each call of this
  # function (in case of equality)
  index <- index[sample(1:nrow(index)),]
  while (sum(col_sum) != 0) {
    # Remove indexes that should not be increased
    index <- filter(index, row_sum[i] != 0 & col_sum[j] != 0)
    D <- pmin(row_sum %*% t(rep(1, length(col_sum))),
             rep(1, length(row_sum)) %*% t(col_sum))
    mat_long <- map2_dbl(index[,1], index[,2], ~ D[.x,.y])
    i <- order(mat_long, decreasing = TRUE)[1]
    d <- D[index[i,1],index[i,2]]
    mat[index[i,1],index[i,2]] <- mat[index[i,1],index[i,2]] + d
    row_sum[index[i,1]] <- row_sum[index[i,1]] - d
    col_sum[index[i,2]] <- col_sum[index[i,2]] - d
  }
  stopifnot(all(c(row_sum, col_sum) == 0))
  stopifnot(!any(mat < 0 | mat > mat_max))
  mat
}

generate_matrix_prop <- function(row_sum, col_sum = row_sum,
                                 mat_min = NULL, mat_max = NULL) {
  stopifnot(sum(row_sum) == sum(col_sum))
  stopifnot(any(c(row_sum, col_sum) >= 0))
  if (is.null(mat_min))
    mat_min <- 0 * row_sum %*% t(col_sum)
  if (is.null(mat_max))
    mat_max <- pmin(row_sum %*% t(rep(1, length(col_sum))),
                    rep(1, length(row_sum)) %*% t(col_sum))
  stopifnot(all(row_sum >= rowSums(mat_min)))
  stopifnot(all(col_sum >= colSums(mat_min)))
  stopifnot(all(row_sum <= rowSums(mat_max)))
  stopifnot(all(col_sum <= colSums(mat_max)))
  stopifnot(check_matrix_constraint(row_sum, col_sum, mat_min, mat_max))
  mat <- round(row_sum %*% t(col_sum) / sum(col_sum))
  mat <- pmax(mat, mat_min)
  mat <- pmin(mat, mat_max)
  row_sum <- row_sum - rowSums(mat)
  col_sum <- col_sum - colSums(mat)
  while (sum(abs(c(row_sum, col_sum))) != 0) {
    i <- sample(seq_along(row_sum), 1)
    j <- sample(seq_along(col_sum), 1)
    d <- 0
    if (mat[i,j] < mat_max[i,j] && (row_sum[i] > 0 || col_sum[j] > 0))
      d <- 1
    if (mat[i,j] > mat_min[i,j] && (row_sum[i] < 0 || col_sum[j] < 0))
      d <- -1
    mat[i,j] <- mat[i,j] + d
    row_sum[i] <- row_sum[i] - d
    col_sum[j] <- col_sum[j] - d
  }
  stopifnot(all(c(row_sum, col_sum) == 0))
  stopifnot(!any(mat < mat_min | mat > mat_max))
  mat
}

check_matrix_constraint <- function(row_sum, col_sum, mat_min, mat_max) {
  mat <- mat_min * NA
  mat[row_sum == rowSums(mat_min),] <- mat_min[row_sum == rowSums(mat_min),]
  mat[,col_sum == colSums(mat_min)] <- mat_min[,col_sum == colSums(mat_min)]
  mat[is.na(mat)] <- mat_max[is.na(mat)]
  if (!all(colSums(mat) >= col_sum) || !all(rowSums(mat) >= row_sum))
    return(FALSE)
  mat <- mat_max * NA
  mat[row_sum == rowSums(mat_max),] <- mat_max[row_sum == rowSums(mat_max),]
  mat[,col_sum == colSums(mat_max)] <- mat_max[,col_sum == colSums(mat_max)]
  mat[is.na(mat)] <- mat_min[is.na(mat)]
  if (!all(colSums(mat) <= col_sum) || !all(rowSums(mat) <= row_sum))
    return(FALSE)
  TRUE
}

fix_mat_max <- function(row_sum, col_sum, mat_min, mat_max) {
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  repeat {
    mat <- mat_min * NA
    mat[row_sum == rowSums(mat_min),] <- mat_min[row_sum == rowSums(mat_min),]
    mat[,col_sum == colSums(mat_min)] <- mat_min[,col_sum == colSums(mat_min)]
    completed <- mat
    completed[is.na(mat)] <- mat_max[is.na(mat)]
    col_sat <- colSums(completed) < col_sum
    row_sat <- rowSums(completed) < row_sum
    if (any(col_sat)) {
      j <- resample(which(col_sat), 1)
      i <- resample(which(is.na(mat[,j])), 1)
      mat_max[i,j] <- mat_max[i,j] + 1
    } else if (any(row_sat)) {
      i <- resample(which(row_sat), 1)
      j <- resample(which(is.na(mat[i,])), 1)
      mat_max[i,j] <- mat_max[i,j] + 1
    } else
      return(mat_max)
  }
}

fix_mat_min <- function(row_sum, col_sum, mat_min) {
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  repeat {
    row_sat <- rowSums(mat_min) > row_sum
    col_sat <- colSums(mat_min) > col_sum
    if (any(row_sat)) {
      i <- resample(which(row_sat), 1)
      j <- resample(which(mat_min[i,] > 1), 1)
    } else if (any(col_sat)) {
      j <- resample(which(col_sat), 1)
      i <- resample(which(mat_min[,j] > 1), 1)
    } else
      return(mat_min)
    mat_min[i,j] <- mat_min[i,j] - 1
  }
}
