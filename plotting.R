# Rolling median with smaller adaptative window for head and tail
# values
my_rollmedian <- function(x, n) {
  stopifnot(n %% 2 == 1)
  require(zoo)
  require(purrr)

  my_rollapply(x, n, function(x) median(x, na.rm = TRUE))
}

# Rolling quantile with smaller adaptative window for head and tail
# values
my_rollquantile <- function(x, n, prob) {
  stopifnot(n %% 2 == 1)
  require(zoo)
  require(purrr)

  my_rollapply(x, n, function(x) quantile(x, probs = prob, na.rm = TRUE))
}

# Rolling quantile with smaller adaptative window for head and tail
# values
my_rollapply <- function(x, n, f) {
  stopifnot(n %% 2 == 1)
  require(zoo)
  require(purrr)

  first <- map_dbl(seq_len((n - 1) / 2), ~ f(head(x, 2 * . - 1)))
  last <- map_dbl(seq_len((n - 1) / 2), ~ f(tail(x, 2 * . - 1)))
  c(first, rollapply(x, n, f), rev(last))
}

# Simple approximation of a curve defined with x and y
approx_point <- function(x, y, prec = 0.01, log = FALSE) {
    stopifnot(length(x) == length(y))

    res <- rep(TRUE, length(x))
    y[is.na(y)] <- Inf

    if (length(x) <= 2) {
        return(res)
    }

    if (log) {
        y <- log(y)
    }

    abs_prec <- prec * diff(range(y[is.finite(y)]))
    prev_x <- x[1]
    prev_y <- y[1]
    cum_err <- 0

    for (i in 2:(length(x) - 1)) {
        x1 <- prev_x
        x2 <- x[i]
        x3 <- x[i + 1]
        y1 <- prev_y
        y2 <- y[i]
        y3 <- y[i + 1]

        # Determine if current point must be kept
        keep <- TRUE

        if (is.finite(y1) && is.finite(y2) && is.finite(y3)) {
            if (x3 == x1) {
                keep <- y2 != y1
            } else {
                err <- (x2 - x1) / (x3 - x1) * (y3 - y1) - (y2 - y1)
                cum_err <- cum_err + err

                if (abs(err) <= abs_prec && abs(cum_err) <= abs_prec) {
                    keep <- FALSE
                }
            }
        }

        # Keep point or not
        if (keep) {
            prev_x <- x2
            prev_y <- y2
            cum_err <- 0
        } else {
            res[i] <- FALSE
        }
    }

    res
}
