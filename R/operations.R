dataprocessing <- function(dat) {
  X <- as.matrix(dat$X)
  head <- dat$head
  dimen <- dat$dimen

  # Remove X cols with duplicated data
  temp <- round(X, digits = 6)
  dup_idx <- duplicated(temp, MARGIN = 2)
  X <- as.matrix(X[, !dup_idx])
  head <- head[!dup_idx]
  if (is.null(dimen)) {
    dimen <- NULL
  } else {
    dimen <- dimen[!dup_idx]
  }

  # Remove columns with -Inf, Inf
  inf_idx <- apply(X, 2, function(x) any(abs(x) == Inf))
  X <- as.matrix(X[, !inf_idx])
  head <- head[!inf_idx]
  if (is.null(dimen)) {
    dimen <- NULL
  } else {
    dimen <- dimen[!inf_idx]
  }

  # Remove columns without variability
  s <- apply(X, 2, function(x) sd(x))
  no_var_idx <- (s == 0)
  X <- as.matrix(X[, !no_var_idx])
  head <- head[!no_var_idx]
  if (is.null(dimen)) {
    dimen <- NULL
  } else {
    dimen <- dimen[!no_var_idx]
  }

  results <- list(X = X,
                  head = head,
                  dimen = dimen)
  return(results)
}


##### Binary Operations #####
ADD <- function(dat) {
  X <- as.matrix(dat$X)
  head_in <- dat$head
  dimen_in <- dat$dimen

  n <- nrow(X)
  p <- ncol(X)
  X_tmp <- matrix(0, nrow = n, ncol = choose(p, 2))
  head <- c()
  dimen <- list()
  count <- 1

  if (is.null(dimen_in)) {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        X_tmp[, count] <- X[, i] + X[, j]
        head[count] <- paste0("(", head_in[i], "+", head_in[j], ")")
        count <- count + 1
      }
    }
    dimen <- NULL
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
          dimen[[count]] <- dimen_i
        } else {
          head[count] <- "empty"
          dimen[[count]] <- "empty"
        }
        count <- count + 1
      }
    }

    # Remove non-physical descriptors
    idx_empty <- (head == "empty")
    X_tmp <- as.matrix(X_tmp[, !idx_empty])
    head <- head[!idx_empty]
    dimen <- dimen[!idx_empty]
  }

  data_out <- list(X = X_tmp,
                   head = head,
                   dimen = dimen)
  return(data_out)
}

MINUS <- function(dat) {
  X <- as.matrix(dat$X)
  head_in <- dat$head
  dimen_in <- dat$dimen

  n <- nrow(X)
  p <- ncol(X)
  X_tmp <- matrix(0, nrow = n, ncol = choose(p, 2))
  head <- c()
  dimen <- list()
  count <- 1

  if (is.null(dimen_in)) {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        X_tmp[, count] <- X[, i] - X[, j]
        head[count] <- paste0("(", head_in[i], "-", head_in[j], ")")
        count <- count + 1
      }
    }
    dimen <- NULL
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
          dimen[[count]] <- dimen_i
        } else {
          head[count] <- "empty"
          dimen[[count]] <- "empty"
        }
        count <- count + 1
      }
    }

    # Remove non-physical descriptors
    idx_empty <- (head == "empty")
    X_tmp <- as.matrix(X_tmp[, !idx_empty])
    head <- head[!idx_empty]
    dimen <- dimen[!idx_empty]
  }

  data_out <- list(X = X_tmp,
                   head = head,
                   dimen = dimen)
  return(data_out)
}

MULTI <- function(dat) {
  X <- as.matrix(dat$X)
  head_in <- dat$head
  dimen_in <- dat$dimen

  n <- nrow(X)
  p <- ncol(X)
  X_tmp <- matrix(0, nrow = n, ncol = choose(p, 2))
  head <- c()
  dimen <- list()
  count <- 1

  if (is.null(dimen_in)) {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        X_tmp[, count] <- X[, i] * X[, j]
        head[count] <- paste0("(", head_in[i], "*", head_in[j], ")")
        count <- count + 1
      }
    }
    dimen <- NULL
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
        dimen[[count]] <- dimen_i + dimen_j
        count <- count + 1
      }
    }
  }

  data_out <- list(X = X_tmp,
                   head = head,
                   dimen = dimen)
  return(data_out)
}

DIVD <- function(dat) {
  X <- as.matrix(dat$X)
  head_in <- dat$head
  dimen_in <- dat$dimen

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

  data_out <- list(X = X_out,
                   head = head_out,
                   dimen = dimen_out)
  return(data_out)
}

MINUS_ABS <- function(dat) {
  X <- as.matrix(dat$X)
  head_in <- dat$head
  dimen_in <- dat$dimen

  n <- nrow(X)
  p <- ncol(X)
  X_tmp <- matrix(0, nrow = n, ncol = choose(p, 2))
  head <- c()
  dimen <- list()
  count <- 1

  if (is.null(dimen_in)) {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        X_tmp[, count] <- abs(X[, i] - X[, j])
        head[count] <- paste0("|", head_in[i], "-", head_in[j], "|")
        count <- count + 1
      }
    }
    dimen <- NULL
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
          dimen[[count]] <- dimen_i
        } else {
          head[count] <- "empty"
          dimen[[count]] <- "empty"
        }
        count <- count + 1
      }
    }

    # Remove non-physical descriptors
    idx_empty <- (head == "empty")
    X_tmp <- as.matrix(X_tmp[, !idx_empty])
    head <- head[!idx_empty]
    dimen <- dimen[!idx_empty]
  }

  data_out <- list(X = X_tmp,
                   head = head,
                   dimen = dimen)
  return(data_out)
}

binary <- function(data, sin_cos) {
  p <- ncol(as.matrix(data$X))
  if (p < 2) {
    stop("X has less than 2 columns. Need at least 2 columns to perform binary operations!")
  } else {
    # Binary operations
    data_add <- ADD(data)
    data_minus <- MINUS(data)
    data_multi <- MULTI(data)
    data_divd <- DIVD(data)
    # data_abs_minus = MINUS_ABS(data)

    X <- cbind(data_add$X, data_minus$X, data_multi$X, data_divd$X)
    head <- c(data_add$head, data_minus$head, data_multi$head, data_divd$head)
    dimen <- c(data_add$dimen, data_minus$dimen, data_multi$dimen, data_divd$dimen)
    Phi <- list(X = X, head = unname(head), dimen = dimen)

    # Combine datasets
    if(sin_cos == FALSE){
      data_abs <- ABS(Phi)
      X <- cbind(X, data_abs$X)
      head <- c(head, data_abs$head)
      dimen <- c(dimen, data_abs$dimen)
    } else{
      data_abs_minus <- MINUS_ABS(data)
      X <- cbind(X, data_abs_minus$X)
      head <- c(head, data_abs_minus$head)
      dimen <- c(dimen, data_abs_minus$dimen)
    }
    colnames(X) <- unname(head)
    Phi <- list(X = X, head = colnames(X), dimen = dimen)

    # Remove redundant descriptors
    Phi <- dataprocessing(Phi)
    return(Phi)
  }
}

##### Unary Operations #####
ABS <- function(dat) {
  X <- as.matrix(dat$X)
  head_in <- dat$head
  dimen_in <- dat$dimen
  p <- ncol(X)

  X_tmp <- apply(X, 2, function(x) abs(x))
  head <- unname(sapply(head_in, function(x) paste0("abs(", x, ")")))
  dimen <- dimen_in

  data_out <- list(X = X_tmp,
                   head = head,
                   dimen = dimen)
  return(data_out)
}

SQRT <- function(dat, apply_pos_opt_on_neg_x) {
  X <- as.matrix(dat$X)
  head_in <- dat$head
  dimen_in <- dat$dimen
  p <- ncol(X)

  X_tmp <- suppressWarnings(apply(X, 2, function(x) sqrt(abs(x))))
  head <- unname(sapply(head_in, function(x) paste0(x, "^0.5")))
  if (is.null(dimen_in)) {
    dimen <- NULL
  } else {
    dimen <- lapply(dimen_in, function(x) x / 2)
  }

  neg_col <- apply(X, 2, function(x) any(x < 0))
  if (any(neg_col) == TRUE) {
    if (apply_pos_opt_on_neg_x == TRUE){
      idx <- which(neg_col)
      head[idx] <- unname(sapply(idx, function(x) paste0("abs(", head_in[x], ")^0.5")))
    } else {
      X_tmp <- as.matrix(X_tmp[, !neg_col])
      head <- head[!neg_col]
      if (is.null(dimen_in)) {
        dimen <- NULL
      } else {
        dimen <- dimen[!neg_col]
      }
    }
  }

  NA_col <- apply(X_tmp, 2, anyNA)
  X_tmp <- as.matrix(X_tmp[, !NA_col])
  head <- head[!NA_col]
  if (is.null(dimen_in)) {
    dimen <- NULL
  } else {
    dimen <- dimen[!NA_col]
  }

  data_out <- list(X = X_tmp,
                   head = head,
                   dimen = dimen)
  return(data_out)
}

INV <- function(dat) {
  X <- as.matrix(dat$X)
  head_in <- dat$head
  dimen_in <- dat$dimen
  p <- ncol(X)

  X_tmp <- suppressWarnings(apply(X, 2, function(x) x^(-1)))
  head <- unname(sapply(head_in, function(x) paste0(x, "^(-1)")))
  if (is.null(dimen_in)) {
    dimen <- NULL
  } else {
    dimen <- lapply(dimen_in, function(x) -x)
  }

  NA_col <- apply(X_tmp, 2, anyNA)
  X_tmp <- as.matrix(X_tmp[, !NA_col])
  head <- head[!NA_col]
  if (is.null(dimen_in)) {
    dimen <- NULL
  } else {
    dimen <- dimen[!NA_col]
  }

  data_out <- list(X = X_tmp,
                   head = head,
                   dimen = dimen)
  return(data_out)
}

SQRE <- function(dat) {
  X <- as.matrix(dat$X)
  head_in <- dat$head
  dimen_in <- dat$dimen
  p <- ncol(X)

  X_tmp <- apply(X, 2, function(x) x^2)
  head <- unname(sapply(head_in, function(x) paste0(x, "^2")))
  if (is.null(dimen_in)) {
    dimen <- NULL
  } else {
    dimen <- lapply(dimen_in, function(x) 2 * x)
  }

  data_out <- list(X = X_tmp,
                   head = head,
                   dimen = dimen)
  return(data_out)
}

LOG <- function(dat, apply_pos_opt_on_neg_x) {
  X <- as.matrix(dat$X)
  head_in <- dat$head
  dimen_in <- dat$dimen
  p <- ncol(X)

  X_tmp <- suppressWarnings(apply(X, 2, function(x) log(abs(x))))
  head <- unname(sapply(head_in, function(x) paste0("log(", x, ")")))
  if (is.null(dimen_in)) {
    dimen <- NULL
  } else {
    dimen <- lapply(dimen_in, function(x) 0*x)
  }

  neg_col <- apply(X, 2, function(x) any(x < 0))
  if (any(neg_col) == TRUE) {
    if (apply_pos_opt_on_neg_x == TRUE){
      idx <- which(neg_col)
      head[idx] <- unname(sapply(idx, function(x) paste0("log(abs(", head_in[x], "))")))
    } else {
      X_tmp <- as.matrix(X_tmp[, !neg_col])
      head <- head[!neg_col]
      if (is.null(dimen_in)) {
        dimen <- NULL
      } else {
        dimen <- dimen[!neg_col]
      }
    }
  }

  NA_col <- apply(X_tmp, 2, anyNA)
  X_tmp <- as.matrix(X_tmp[, !NA_col])
  head <- head[!NA_col]
  if (is.null(dimen_in)) {
    dimen <- NULL
  } else {
    dimen <- dimen[!NA_col]
  }

  data_out <- list(X = X_tmp,
                   head = head,
                   dimen = dimen)
  return(data_out)
}

EXP <- function(dat) {
  X <- as.matrix(dat$X)
  head_in <- dat$head
  dimen_in <- dat$dimen
  p <- ncol(X)

  X_tmp <- apply(X, 2, function(x) exp(x))
  head <- unname(sapply(head_in, function(x) paste0("exp(", x, ")")))
  if (is.null(dimen_in)) {
    dimen <- NULL
  } else {
    dimen <- lapply(dimen_in, function(x) 0*x)
  }

  data_out <- list(X = X_tmp,
                   head = head,
                   dimen = dimen)
  return(data_out)
}

SIN <- function(dat) {
  X <- as.matrix(dat$X)
  head_in <- dat$head
  dimen_in <- dat$dimen
  p <- ncol(X)

  X_tmp <- apply(X, 2, function(x) sin(pi * x))
  head <- unname(sapply(head_in, function(x) paste0("sin(pi*", x, ")")))
  if (is.null(dimen_in)) {
    dimen <- NULL
  } else {
    dimen <- lapply(dimen_in, function(x) 0*x)
  }

  data_out <- list(X = X_tmp,
                   head = head,
                   dimen = dimen)
  return(data_out)
}

COS <- function(dat) {
  X <- as.matrix(dat$X)
  head_in <- dat$head
  dimen_in <- dat$dimen
  p <- ncol(X)

  X_tmp <- apply(X, 2, function(x) cos(pi * x))
  head <- unname(sapply(head_in, function(x) paste0("cos(pi*", x, ")")))
  if (is.null(dimen_in)) {
    dimen <- NULL
  } else {
    dimen <- lapply(dimen_in, function(x) 0*x)
  }

  data_out <- list(X = X_tmp,
                   head = head,
                   dimen = dimen)
  return(data_out)
}

unary <- function(data, sin_cos, apply_pos_opt_on_neg_x) {
  p <- ncol(as.matrix(data$X))
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
      X <- cbind(data$X, data_sqrt$X, data_sqre$X,
                 data_log$X, data_exp$X, data_sin$X,
                 data_cos$X, data_inv$X, data_abs$X)
      head <- c(data$head, data_sqrt$head, data_sqre$head,
                data_log$head, data_exp$head, data_sin$head,
                data_cos$head, data_inv$head, data_abs$head)
      dimen <- c(data$dimen, data_sqrt$dimen, data_sqre$dimen,
                 data_log$dimen, data_exp$dimen, data_sin$dimen,
                 data_cos$dimen, data_inv$dimen, data_abs$dimen)
    } else{
      # Combine datasets
      X <- cbind(data$X, data_abs$X, data_sqrt$X,
                 data_inv$X, data_sqre$X, data_log$X,
                 data_exp$X)
      head <- c(data$head, data_abs$head, data_sqrt$head,
                data_inv$head, data_sqre$head, data_log$head,
                data_exp$head)
      dimen <- c(data$dimen, data_abs$dimen, data_sqrt$dimen,
                 data_inv$dimen, data_sqre$dimen, data_log$dimen,
                 data_exp$dimen)
    }

    colnames(X) <- unname(head)
    Phi <- list(X = X, head = colnames(X), dimen = dimen)

    # Remove redundant descriptors
    Phi <- dataprocessing(Phi)
    return(Phi)
  }
}
