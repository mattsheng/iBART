dataprocessing <- function(data) {
  data$X <- as.matrix(data$X)

  # Remove X cols with duplicated data
  temp <- round(data$X, digits = 6)
  dup_idx <- duplicated(temp, MARGIN = 2)
  data$X <- as.matrix(data$X[, !dup_idx])
  data$head <- data$head[!dup_idx]
  if (!is.null(data$unit)) data$unit <- data$unit[!dup_idx]

  # Remove columns with -Inf, Inf
  inf_idx <- apply(data$X, 2, function(x) any(abs(x) == Inf))
  data$X <- as.matrix(data$X[, !inf_idx])
  data$head <- data$head[!inf_idx]
  if (!is.null(data$unit)) data$unit <- data$unit[!inf_idx]

  # Remove columns without variability
  s <- apply(data$X, 2, function(x) sd(x))
  no_var_idx <- (s == 0)
  data$X <- as.matrix(data$X[, !no_var_idx])
  data$head <- data$head[!no_var_idx]
  if (!is.null(data$unit)) data$unit <- data$unit[!no_var_idx]
  colnames(data$X) <- data$head

  return(data)
}


##### Binary Operations #####
ADD <- function(dat) {
  X <- dat$X
  head_in <- dat$head
  dimen_in <- dat$unit

  n <- nrow(X)
  p <- ncol(X)
  X_tmp <- matrix(0, nrow = n, ncol = choose(p, 2))
  head <- c()
  unit <- list()
  count <- 1

  if (is.null(dimen_in)) {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        X_tmp[, count] <- X[, i] + X[, j]
        head[count] <- paste0("(", head_in[i], "+", head_in[j], ")")
        count <- count + 1
      }
    }
    unit <- NULL
  } else {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        dimen_i <- dimen_in[[i]]
        dimen_j <- dimen_in[[j]]
        dimen_i <- dimen_i[order(names(dimen_i))]
        dimen_j <- dimen_j[order(names(dimen_j))]
        if (setequal(names(dimen_i), names(dimen_j)) && isTRUE(dimen_i == dimen_j)) {
          X_tmp[, count] <- X[, i] + X[, j]
          head[count] <- paste0("(", head_in[i], "+", head_in[j], ")")
          unit[[count]] <- dimen_i
        } else {
          head[count] <- "empty"
          unit[[count]] <- "empty"
        }
        count <- count + 1
      }
    }

    # Remove non-physical descriptors
    idx_empty <- (head == "empty")
    X_tmp <- as.matrix(X_tmp[, !idx_empty])
    head <- head[!idx_empty]
    unit <- unit[!idx_empty]
  }

  data_out <- list(X = as.matrix(X_tmp),
                   head = head,
                   unit = unit)
  return(data_out)
}

MINUS <- function(dat) {
  X <- dat$X
  head_in <- dat$head
  dimen_in <- dat$unit

  n <- nrow(X)
  p <- ncol(X)
  X_tmp <- matrix(0, nrow = n, ncol = choose(p, 2))
  head <- c()
  unit <- list()
  count <- 1

  if (is.null(dimen_in)) {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        X_tmp[, count] <- X[, i] - X[, j]
        head[count] <- paste0("(", head_in[i], "-", head_in[j], ")")
        count <- count + 1
      }
    }
    unit <- NULL
  } else {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        dimen_i <- dimen_in[[i]]
        dimen_j <- dimen_in[[j]]
        dimen_i <- dimen_i[order(names(dimen_i))]
        dimen_j <- dimen_j[order(names(dimen_j))]
        if (setequal(names(dimen_i), names(dimen_j)) && isTRUE(dimen_i == dimen_j)) {
          X_tmp[, count] <- X[, i] - X[, j]
          head[count] <- paste0("(", head_in[i], "-", head_in[j], ")")
          unit[[count]] <- dimen_i
        } else {
          head[count] <- "empty"
          unit[[count]] <- "empty"
        }
        count <- count + 1
      }
    }

    # Remove non-physical descriptors
    idx_empty <- (head == "empty")
    X_tmp <- as.matrix(X_tmp[, !idx_empty])
    head <- head[!idx_empty]
    unit <- unit[!idx_empty]
  }

  data_out <- list(X = as.matrix(X_tmp),
                   head = head,
                   unit = unit)
  return(data_out)
}

MULTI <- function(dat) {
  X <- dat$X
  head_in <- dat$head
  dimen_in <- dat$unit

  n <- nrow(X)
  p <- ncol(X)
  X_tmp <- matrix(0, nrow = n, ncol = choose(p, 2))
  head <- c()
  unit <- list()
  count <- 1

  if (is.null(dimen_in)) {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        X_tmp[, count] <- X[, i] * X[, j]
        head[count] <- paste0("(", head_in[i], "*", head_in[j], ")")
        count <- count + 1
      }
    }
    unit <- NULL
  } else {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        X_tmp[, count] <- X[, i] * X[, j]
        head[count] <- paste0("(", head_in[i], "*", head_in[j], ")")

        dimen_i <- dimen_in[[i]]
        dimen_j <- dimen_in[[j]]
        dimen_i <- dimen_i[order(names(dimen_i))]
        dimen_j <- dimen_j[order(names(dimen_j))]
        names <- union(names(dimen_i), names(dimen_j))
        names_i <- setdiff(names, names(dimen_i))
        names_j <- setdiff(names, names(dimen_j))
        if (length(names_i) > 0) {
          dimen_i[names_i] <- 0
          dimen_i <- dimen_i[order(names(dimen_i))]
        }
        if (length(names_j) > 0) {
          dimen_j[names_j] <- 0
          dimen_j <- dimen_j[order(names(dimen_j))]
        }
        unit[[count]] <- dimen_i + dimen_j
        count <- count + 1
      }
    }
  }

  data_out <- list(X = as.matrix(X_tmp),
                   head = head,
                   unit = unit)
  return(data_out)
}

DIVD <- function(dat) {
  X <- dat$X
  head_in <- dat$head
  dimen_in <- dat$unit

  n <- nrow(X)
  p <- ncol(X)
  X_tmp_1 <- matrix(0, nrow = n, ncol = choose(p, 2))
  X_tmp_2 <- matrix(0, nrow = n, ncol = choose(p, 2))
  head_1 <- head_2 <- c()
  dimen_1 <- dimen_2 <- list()
  count <- 1

  if (is.null(dimen_in)) {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        if (all(X[, j] != 0)) {
          X_tmp_1[, count] <- X[, i] / X[, j]
          head_1[count] <- paste0("(", head_in[i], "/", head_in[j], ")")
        } else {
          head_1[count] <- "empty"
        }
        if (all(X[, i] != 0)) {
          X_tmp_2[, count] <- X[, j] / X[, i]
          head_2[count] <- paste0("(", head_in[j], "/", head_in[i], ")")
        } else {
          head_2[count] <- "empty"
        }
        count <- count + 1
      }
    }
    idx_empty_1 <- (head_1 == "empty")
    idx_empty_2 <- (head_2 == "empty")

    # Remove non-physical descriptors
    X_tmp_1 <- as.matrix(X_tmp_1[, !idx_empty_1])
    head_1 <- head_1[!idx_empty_1]
    X_tmp_2 <- as.matrix(X_tmp_2[, !idx_empty_2])
    head_2 <- head_2[!idx_empty_2]

    dimen_1 <- dimen_2 <- NULL
  } else {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        dimen_i <- dimen_in[[i]]
        dimen_j <- dimen_in[[j]]
        dimen_i <- dimen_i[order(names(dimen_i))]
        dimen_j <- dimen_j[order(names(dimen_j))]

        if (all(X[, j] != 0)) {
          X_tmp_1[, count] <- X[, i] / X[, j]
          head_1[count] <- paste0("(", head_in[i], "/", head_in[j], ")")

          names <- union(names(dimen_i), names(dimen_j))
          names_i <- setdiff(names, names(dimen_i))
          names_j <- setdiff(names, names(dimen_j))
          if (length(names_i) > 0) {
            dimen_i[names_i] <- 0
            dimen_i <- dimen_i[order(names(dimen_i))]
          }
          if (length(names_j) > 0) {
            dimen_j[names_j] <- 0
            dimen_j <- dimen_j[order(names(dimen_j))]
          }
          dimen_1[[count]] <- dimen_i - dimen_j
        } else {
          head_1[count] <- "empty"
          dimen_1[[count]] <- "empty"
        }

        if (all(X[, i] != 0)) {
          X_tmp_2[, count] <- X[, j] / X[, i]
          head_2[count] <- paste0("(", head_in[j], "/", head_in[i], ")")

          names <- union(names(dimen_i), names(dimen_j))
          names_i <- setdiff(names, names(dimen_i))
          names_j <- setdiff(names, names(dimen_j))
          if (length(names_i) > 0) {
            dimen_i[names_i] <- 0
            dimen_i <- dimen_i[order(names(dimen_i))]
          }
          if (length(names_j) > 0) {
            dimen_j[names_j] <- 0
            dimen_j <- dimen_j[order(names(dimen_j))]
          }
          dimen_2[[count]] <- dimen_j - dimen_i
        } else {
          head_2[count] <- "empty"
          dimen_2[[count]] <- "empty"
        }
        count <- count + 1
      }
    }

    idx_empty_1 <- (head_1 == "empty")
    idx_empty_2 <- (head_2 == "empty")

    # Remove non-physical descriptors
    X_tmp_1 <- as.matrix(X_tmp_1[, !idx_empty_1])
    head_1 <- head_1[!idx_empty_1]
    dimen_1 <- dimen_1[!idx_empty_1]

    X_tmp_2 <- as.matrix(X_tmp_2[, !idx_empty_2])
    head_2 <- head_2[!idx_empty_2]
    dimen_2 <- dimen_2[!idx_empty_2]
  }

  X_out <- cbind(X_tmp_1, X_tmp_2)
  head_out <- c(head_1, head_2)
  dimen_out <- c(dimen_1, dimen_2)

  data_out <- list(X = as.matrix(X_out),
                   head = head_out,
                   unit = dimen_out)
  return(data_out)
}

MINUS_ABS <- function(dat) {
  X <- dat$X
  head_in <- dat$head
  dimen_in <- dat$unit

  n <- nrow(X)
  p <- ncol(X)
  X_tmp <- matrix(0, nrow = n, ncol = choose(p, 2))
  head <- c()
  unit <- list()
  count <- 1

  if (is.null(dimen_in)) {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        X_tmp[, count] <- abs(X[, i] - X[, j])
        head[count] <- paste0("|", head_in[i], "-", head_in[j], "|")
        count <- count + 1
      }
    }
    unit <- NULL
  } else {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        dimen_i <- dimen_in[[i]]
        dimen_j <- dimen_in[[j]]
        dimen_i <- dimen_i[order(names(dimen_i))]
        dimen_j <- dimen_j[order(names(dimen_j))]
        if (setequal(names(dimen_i), names(dimen_j)) && setequal(dimen_i, dimen_j)) {
          X_tmp[, count] <- abs(X[, i] - X[, j])
          head[count] <- paste0("|", head_in[i], "-", head_in[j], "|")
          unit[[count]] <- dimen_i
        } else {
          head[count] <- "empty"
          unit[[count]] <- "empty"
        }
        count <- count + 1
      }
    }

    # Remove non-physical descriptors
    idx_empty <- (head == "empty")
    X_tmp <- as.matrix(X_tmp[, !idx_empty])
    head <- head[!idx_empty]
    unit <- unit[!idx_empty]
  }

  data_out <- list(X = as.matrix(X_tmp),
                   head = head,
                   unit = unit)
  return(data_out)
}

binary <- function(data, sin_cos) {
  p <- ncol(data$X)
  if (p < 2) {
    message("X has less than 2 columns. Need at least 2 columns to perform binary operations!")
    data$error <- TRUE
    return(data)
  } else {
    # Binary operations
    data_add <- ADD(data)
    data_minus <- MINUS(data)
    data_multi <- MULTI(data)
    data_divd <- DIVD(data)

    dat_tmp <- list()
    dat_tmp$X <- cbind(data_add$X, data_minus$X, data_multi$X, data_divd$X)
    dat_tmp$head <- c(data_add$head, data_minus$head, data_multi$head, data_divd$head)
    dat_tmp$unit <- c(data_add$unit, data_minus$unit, data_multi$unit, data_divd$unit)

    # Combine datasets
    if (sin_cos == FALSE) {
      data_abs <- ABS(dat_tmp)
      data$X <- cbind(dat_tmp$X, data_abs$X)
      data$head <- c(dat_tmp$head, data_abs$head)
      data$unit <- c(dat_tmp$unit, data_abs$unit)
    } else{
      data_abs_minus <- MINUS_ABS(data)
      data$X <- cbind(dat_tmp$X, data_abs_minus$X)
      data$head <- c(dat_tmp$head, data_abs_minus$head)
      data$unit <- c(dat_tmp$unit, data_abs_minus$unit)
    }
    colnames(data$X) <- unname(data$head)

    # Remove redundant descriptors
    data <- dataprocessing(data)
    return(data)
  }
}

##### Unary Operations #####
ABS <- function(dat) {
  dat$X <- apply(dat$X, 2, function(x) abs(x))
  dat$head <- unname(sapply(dat$head, function(x) paste0("abs(", x, ")")))

  data_out <- list(X = dat$X,
                   head = dat$head,
                   unit = dat$unit)
  return(data_out)
}

SQRT <- function(dat, apply_pos_opt_on_neg_x) {
  # Record which columns has negative values
  neg_col <- apply(dat$X, 2, function(x) any(x < 0))

  if (apply_pos_opt_on_neg_x) {
    dat$X <- suppressWarnings(apply(dat$X, 2, function(x) sqrt(abs(x))))
    dat$head <- unname(sapply(dat$head, function(x) paste0(x, "^0.5")))
    if (any(neg_col)) dat$head[neg_col] <- unname(sapply(dat$head[neg_col], function(x) paste0("abs(", x, ")^0.5")))
    if (!is.null(dat$unit)) dat$unit <- lapply(dat$unit, function(x) 0*x)
  } else {
    if (sum(neg_col) == ncol(dat$X)) {
      return(list(X = NULL, head = NULL, unit = NULL))
    } else {
      dat$X <- suppressWarnings(apply(as.matrix(dat$X[, !neg_col]), 2, function(x) sqrt(abs(x))))
      dat$head <- unname(sapply(dat$head[!neg_col], function(x) paste0(x, "^0.5")))
      if (!is.null(dat$unit)) dat$unit <- dat$unit[!neg_col]
    }
  }
  dat$X <- as.matrix(dat$X)
  data_out <- list(X = dat$X,
                   head = dat$head,
                   unit = dat$unit)
  return(data_out)
}

INV <- function(dat) {
  # Remove columns containing 0
  zero_idx <- apply(dat$X, 2, function(x) any(x == 0))

  dat$X <- suppressWarnings(apply(as.matrix(dat$X[, !zero_idx]), 2, function(x) x^(-1)))
  dat$head <- unname(sapply(dat$head[!zero_idx], function(x) paste0(x, "^(-1)")))
  if (!is.null(dat$unit)) dat$unit <- lapply(dat$unit[!zero_idx], function(x) -x)

  dat$X <- as.matrix(dat$X)
  data_out <- list(X = dat$X,
                   head = dat$head,
                   unit = dat$unit)
  return(data_out)
}

SQRE <- function(dat) {
  dat$X <- apply(dat$X, 2, function(x) x^2)
  dat$head <- unname(sapply(dat$head, function(x) paste0(x, "^2")))
  if (!is.null(dat$unit)) dat$unit <- lapply(dat$unit, function(x) 2 * x)

  data_out <- list(X = dat$X,
                   head = dat$head,
                   unit = dat$unit)
  return(data_out)
}

LOG <- function(dat, apply_pos_opt_on_neg_x) {
  # Record which columns has negative values
  neg_col <- apply(dat$X, 2, function(x) any(x < 0))

  if (apply_pos_opt_on_neg_x) {
    dat$X <- suppressWarnings(apply(dat$X, 2, function(x) log(abs(x))))
    dat$head <- unname(sapply(dat$head, function(x) paste0("log(", x, ")")))
    if (any(neg_col)) dat$head[neg_col] <- unname(sapply(dat$head[neg_col], function(x) paste0("log(abs(", x, "))")))
    if (!is.null(dat$unit)) dat$unit <- lapply(dat$unit, function(x) 0*x)
  } else {
    if (sum(neg_col) == ncol(dat$X)) {
      return(list(X = NULL, head = NULL, unit = NULL))
    } else {
      dat$X <- suppressWarnings(apply(as.matrix(dat$X[, !neg_col]), 2, function(x) log(abs(x))))
      dat$head <- unname(sapply(dat$head[!neg_col], function(x) paste0("log(", x, ")")))
      if (!is.null(dat$unit)) dat$unit <- dat$unit[!neg_col]
    }
  }

  # Remove NAs in case of log(0)
  NA_col <- apply(as.matrix(dat$X), 2, anyNA)
  dat$X <- as.matrix(dat$X[, !NA_col])
  dat$head <- dat$head[!NA_col]
  if (!is.null(dat$unit)) dat$unit <- dat$unit[!NA_col]

  data_out <- list(X = dat$X,
                   head = dat$head,
                   unit = dat$unit)
  return(data_out)
}

EXP <- function(dat) {
  dat$X <- apply(dat$X, 2, function(x) exp(x))
  dat$head <- unname(sapply(dat$head, function(x) paste0("exp(", x, ")")))
  if (!is.null(dat$unit)) dat$unit <- lapply(dat$unit, function(x) 0*x)

  data_out <- list(X = dat$X,
                   head = dat$head,
                   unit = dat$unit)
  return(data_out)
}

SIN <- function(dat) {
  dat$X <- apply(dat$X, 2, function(x) sin(pi * x))
  dat$head <- unname(sapply(dat$head, function(x) paste0("sin(pi*", x, ")")))
  if (!is.null(dat$unit)) dat$unit <- lapply(dat$unit, function(x) 0*x)

  data_out <- list(X = dat$X,
                   head = dat$head,
                   unit = dat$unit)
  return(data_out)
}

COS <- function(dat) {
  dat$X <- apply(dat$X, 2, function(x) cos(pi * x))
  dat$head <- unname(sapply(dat$head, function(x) paste0("cos(pi*", x, ")")))
  if (!is.null(dat$unit)) dat$unit <- lapply(dat$unit, function(x) 0*x)

  data_out <- list(X = dat$X,
                   head = dat$head,
                   unit = dat$unit)
  return(data_out)
}

unary <- function(data, sin_cos, apply_pos_opt_on_neg_x) {
  p <- ncol(data$X)
  if (p < 1) {
    stop("X has zero column. Need at least 1 column to perform unary operations!")
  } else {
    # unary operations
    data_abs <- ABS(data)
    data_sqrt <- SQRT(data, apply_pos_opt_on_neg_x)
    data_inv <- INV(data)
    data_sqre <- SQRE(data)
    data_log <- LOG(data, apply_pos_opt_on_neg_x)
    data_exp <- EXP(data)

    if(sin_cos == TRUE){
      data_sin <- SIN(data)
      data_cos <- COS(data)

      # Combine datasets
      data$X <- cbind(data$X, data_sqrt$X, data_sqre$X,
                      data_log$X, data_exp$X, data_sin$X,
                      data_cos$X, data_inv$X, data_abs$X)
      data$head <- c(data$head, data_sqrt$head, data_sqre$head,
                     data_log$head, data_exp$head, data_sin$head,
                     data_cos$head, data_inv$head, data_abs$head)
      data$unit <- c(data$unit, data_sqrt$unit, data_sqre$unit,
                      data_log$unit, data_exp$unit, data_sin$unit,
                      data_cos$unit, data_inv$unit, data_abs$unit)
    } else{
      # Combine datasets
      data$X <- cbind(data$X, data_abs$X, data_sqrt$X,
                      data_inv$X, data_sqre$X, data_log$X,
                      data_exp$X)
      data$head <- c(data$head, data_abs$head, data_sqrt$head,
                     data_inv$head, data_sqre$head, data_log$head,
                     data_exp$head)
      data$unit <- c(data$unit, data_abs$unit, data_sqrt$unit,
                      data_inv$unit, data_sqre$unit, data_log$unit,
                      data_exp$unit)
    }

    colnames(data$X) <- unname(data$head)

    # Remove redundant descriptors
    data <- dataprocessing(data)
    return(data)
  }
}
