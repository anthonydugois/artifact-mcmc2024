compute_alloc <- function(costs, alloc) {
  max(sapply(1:ncol(costs), function(j) sum(costs[alloc == j,j])))
}

balance_sufferage <- function(costs) {
  compute_alloc(costs, balance_sufferage_alloc(costs))
}

balance_sufferage_alloc <- function(costs) {
  # Compute sufferage matrix
  S <- t(sapply(1:nrow(costs), function(i) { costs[i,] - min(costs[i,]) }))
  # Compute default allocation that minimizes sufferage
  allocation <- sapply(1:nrow(costs), function(i) which.min(S[i,])[1])
  alloc <- allocation
  # Transform as an appropriate structure
  proc <- sapply(1:ncol(costs),
                 function(p) list(proc = p, tasks = which(allocation == p)),
                 simplify = FALSE)
  # Compute ready times
  RT <- sapply(proc, function(p) sum(costs[p$tasks,p$p]))
  repeat {
    pmax <- which.max(RT)[1]
    suff_min <- max(S) + 1
    task <- NULL
    target <- NULL
    for (i in proc[[pmax]]$tasks) {
      for (p in which(1:ncol(costs) != pmax)) {
        suff <- S[i,p]
        cmax <- max(RT[pmax] - costs[i,pmax], RT[p] + costs[i,p])
        if (cmax < RT[pmax] && suff < suff_min) {
          suff_min <- suff
          task <- i
          target <- p
        }
      }
    }
    if (is.null(task) || is.null(target)) {
      return(alloc)
    }
    proc[[target]]$tasks <- c(proc[[target]]$tasks, task)
    proc[[pmax]]$tasks <- proc[[pmax]]$tasks[proc[[pmax]]$tasks != task]
    RT[target] <- RT[target] + costs[task,target]
    alloc[task] <- target
    RT[pmax] <- RT[pmax] - costs[task,pmax]
  }
}

# Min-Min, actually the same as EFT but differently written, not satisfied :-(
MinMin <- function(costs) {
  compute_alloc(costs, MinMin_alloc(costs))
}

MinMin_alloc <- function(costs) {
  RT <- rep(0, ncol(costs))
  index <- 1:nrow(costs)
  alloc <- numeric(nrow(costs))
  while (length(index) != 0) {
    minct <- NULL
    minp <- NULL
    # compute the set of min completion time
    for (i in 1:length(index)) {
      cti <- RT + costs[index[i],]
      minct[i] <- min(cti)
      minp[i] <- which.min(cti)
    }
    n <- which.min(minct)
    efj <- index[n]
    efp <- minp[n]
    RT[efp] <- RT[efp] + costs[efj,efp]
    alloc[efj] <- efp
    index <- index[index != efj]
  }
  alloc
}

LPT_unrelated <- function(costs, func=min) {
  compute_alloc(costs, LPT_unrelated_alloc(costs, func))
}

LPT_unrelated_alloc <- function(costs, func = min) {
  RT <- rep(0, ncol(costs))
  mean_costs <- apply(costs, 1, func)
  index <- order(mean_costs, decreasing = TRUE)
  alloc <- numeric(nrow(costs))
  for (i in index) {
    efp <- 1
    for (j in 1:ncol(costs))
      if (RT[j] + costs[i,j] < RT[efp] + costs[i,efp])
        efp <- j
    RT[efp] <- RT[efp] + costs[i,efp]
    alloc[i] <- efp
  }
  alloc
}
