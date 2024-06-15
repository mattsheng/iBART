##### Binary Operations #####
ADD <- function(dat) {
  if (is.null(dat$unit)) {
    p <- ncol(dat$X)
    df_out <- list(X = matrix(0, nrow = nrow(dat$X), ncol = choose(p, 2)),
                   unit = NULL,
                   name = c())
    count <- 1

    for (i in 1:(p-1)) {
      for (j in (i+1):p) {
        df_out$X[, count] <- dat$X[, i] + dat$X[, j]
        df_out$name[count] <- paste0("(", dat$name[i], "+", dat$name[j], ")")
        count <- count + 1
      }
    }
    # colnames(df_out$X) <- df_out$name
    return(df_out)

  } else {
    # 1. Only perform + on variables with the same unit: X_unit + X_unit
    # 2. Can perform X_unit + X_const
    # 3. Can perform X_unitless + X_unitless

    # Find X_unitless
    X_unitless <- NULL
    name_unitless <- c()
    unitless_col <- apply(dat$unit, 2, function(x) all(x == 0))
    p_unitless <- sum(unitless_col)

    if (p_unitless > 0) {
      X_unitless <- as.matrix(dat$X[, unitless_col])
      name_unitless <- dat$name[unitless_col]

      # Find X_const
      const_col <- apply(X_unitless, 2, function(x) length(unique(x)) == 1)
      p_const <- sum(const_col)
      if (p_const > 0) {
        X_const <- as.matrix(X_unitless[, const_col])
        name_const <- dat$name[const_col]
      }
    } else {
      X_const <- NULL
      p_const <- 0
    }

    # Find unique units
    unit_var <- as.matrix(dat$unit[, !unitless_col])
    unit_unique <- as.matrix(unique(unit_var, MARGIN = 2))

    # Count number of X_unit + X_unit
    unit_var_str <- apply(unit_var, 2, paste, collapse = ",")
    p_var <- as.vector(table(unit_var_str))
    p_1 <- sapply(p_var, function(x) if (x > 1) choose(x, 2) else 0)
    p_1 <- sum(p_1)

    # Count number of X_unit + X_const
    p_2 <- sum(p_var * p_const)

    # Count number of X_unitless + X_unitless
    p_3 <- if (p_unitless > 1) choose(p_unitless, 2) else 0

    p_total <- p_1 + p_2 + p_3

    # Initialize df_out to store results
    df_out <- list(X = matrix(nrow = nrow(dat$X), ncol = p_total),
                   unit = NULL,
                   name = c())
    count <- 1

    # Iterative over unique units
    for (i in 1:ncol(unit_unique)) {
      unit_tmp <- unit_unique[, i]
      unit_id <- apply(dat$unit, 2, function(x) all(x == unit_tmp))
      X_tmp <- as.matrix(dat$X[, unit_id])
      name_tmp <- dat$name[unit_id]
      p_tmp <- ncol(X_tmp)

      #################
      # X_unit + X_unit
      #################
      # Only + if there are at least 2 columns
      if (p_tmp > 1) {
        for (j in 1:(p_tmp-1)) {
          for (k in (j+1):p_tmp) {
            df_out$X[, count] <- X_tmp[, j] + X_tmp[, k]
            df_out$name[count] <- paste0("(", name_tmp[j], "+", name_tmp[k], ")")
            count <- count + 1
          }
        }
        p_tmp2 <- choose(p_tmp, 2)
        df_out$unit <- cbind(df_out$unit,
                             matrix(rep(unit_tmp, p_tmp2), ncol = p_tmp2))
      }
      ##################
      # X_unit + X_const
      ##################
      # Only + if there are at least 1 column in both X_tmp and X_const
      if ((p_const > 0) & (p_tmp > 0)) {
        for (j in 1:p_tmp) {
          for (k in 1:p_const){
            df_out$X[, count] <- X_tmp[, j] + X_const[, k]
            df_out$name[count] <- paste0("(", name_tmp[j], "+", name_const[k], ")")
            count <- count + 1
          }
        }
        p_tmp3 <- p_tmp * p_const
        df_out$unit <- cbind(df_out$unit,
                             matrix(rep(unit_tmp, p_tmp3), ncol = p_tmp3))
      }
    }

    #########################
    # X_unitless + X_unitless
    #########################
    if (p_unitless > 1) {
      for (i in 1:(p_unitless-1)) {
        for (j in (i+1):p_unitless) {
          df_out$X[, count] <- X_unitless[, i] + X_unitless[, j]
          df_out$name[count] <- paste0("(", name_unitless[i], "+", name_unitless[j], ")")
          count <- count + 1
        }
      }
      df_out$unit <- cbind(df_out$unit,
                           matrix(0, nrow = nrow(df_out$unit), ncol = choose(p_unitless, 2)))
    }
    # colnames(df_out$X) <- df_out$name
    return(df_out)
  }
}

MINUS <- function(dat) {
  if (is.null(dat$unit)) {
    p <- ncol(dat$X)
    df_out <- list(X = matrix(0, nrow = nrow(dat$X), ncol = choose(p, 2)),
                   unit = NULL,
                   name = c())
    count <- 1

    for (i in 1:(p-1)) {
      for (j in (i+1):p) {
        df_out$X[, count] <- dat$X[, i] - dat$X[, j]
        df_out$name[count] <- paste0("(", dat$name[i], "-", dat$name[j], ")")
        count <- count + 1
      }
    }
    # colnames(df_out$X) <- df_out$name
    return(df_out)

  } else {
    # 1. Only perform - on variables with the same unit: X_unit - X_unit
    # 2. Can perform X_unit - X_const
    # 3. Can perform X_unitless - X_unitless

    # Find X_unitless
    X_unitless <- NULL
    name_unitless <- c()
    unitless_col <- apply(dat$unit, 2, function(x) all(x == 0))
    p_unitless <- sum(unitless_col)

    if (p_unitless > 0) {
      X_unitless <- as.matrix(dat$X[, unitless_col])
      name_unitless <- dat$name[unitless_col]

      # Find X_const
      const_col <- apply(X_unitless, 2, function(x) length(unique(x)) == 1)
      p_const <- sum(const_col)
      if (p_const > 0) {
        X_const <- as.matrix(X_unitless[, const_col])
        name_const <- dat$name[const_col]
      }
    } else {
      X_const <- NULL
      p_const <- 0
    }

    # Find unique units
    unit_var <- as.matrix(dat$unit[, !unitless_col])
    unit_unique <- as.matrix(unique(unit_var, MARGIN = 2))

    # Count number of X_unit - X_unit
    unit_var_str <- apply(unit_var, 2, paste, collapse = ",")
    p_var <- as.vector(table(unit_var_str))
    p_1 <- sapply(p_var, function(x) if (x > 1) choose(x, 2) else 0)
    p_1 <- sum(p_1)

    # Count number of X_unit - X_const
    p_2 <- sum(p_var * p_const)

    # Count number of X_unitless - X_unitless
    p_3 <- if (p_unitless > 1) choose(p_unitless, 2) else 0

    p_total <- p_1 + p_2 + p_3

    # Initialize df_out to store results
    df_out <- list(X = matrix(nrow = nrow(dat$X), ncol = p_total),
                   unit = NULL,
                   name = c())
    count <- 1

    # Iterative over unique units
    for (i in 1:ncol(unit_unique)) {
      unit_tmp <- unit_unique[, i]
      unit_id <- apply(dat$unit, 2, function(x) all(x == unit_tmp))
      X_tmp <- as.matrix(dat$X[, unit_id])
      name_tmp <- dat$name[unit_id]
      p_tmp <- ncol(X_tmp)

      #################
      # X_unit - X_unit
      #################
      # Only - if there are at least 2 columns
      if (p_tmp > 1) {
        for (j in 1:(p_tmp-1)) {
          for (k in (j+1):p_tmp) {
            df_out$X[, count] <- X_tmp[, j] - X_tmp[, k]
            df_out$name[count] <- paste0("(", name_tmp[j], "-", name_tmp[k], ")")
            count <- count + 1
          }
        }
        p_tmp2 <- choose(p_tmp, 2)
        df_out$unit <- cbind(df_out$unit,
                             matrix(rep(unit_tmp, p_tmp2), ncol = p_tmp2))
      }
      ##################
      # X_unit - X_const
      ##################
      # Only - if there are at least 1 column in both X_tmp and X_const
      if ((p_const > 0) & (p_tmp > 0)) {
        for (j in 1:p_tmp) {
          for (k in 1:p_const){
            df_out$X[, count] <- X_tmp[, j] - X_const[, k]
            df_out$name[count] <- paste0("(", name_tmp[j], "-", name_const[k], ")")
            count <- count + 1
          }
        }
        p_tmp3 <- p_tmp * p_const
        df_out$unit <- cbind(df_out$unit,
                             matrix(rep(unit_tmp, p_tmp3), ncol = p_tmp3))
      }
    }

    #########################
    # X_unitless - X_unitless
    #########################
    if (p_unitless > 1) {
      for (i in 1:(p_unitless-1)) {
        for (j in (i+1):p_unitless) {
          df_out$X[, count] <- X_unitless[, i] - X_unitless[, j]
          df_out$name[count] <- paste0("(", name_unitless[i], "-", name_unitless[j], ")")
          count <- count + 1
        }
      }
      df_out$unit <- cbind(df_out$unit,
                           matrix(0, nrow = nrow(df_out$unit), ncol = choose(p_unitless, 2)))
    }
    # colnames(df_out$X) <- df_out$name
    return(df_out)
  }
}

MULTI <- function(dat) {
  p <- ncol(dat$X)
  df_out <- list(X = matrix(0, nrow = nrow(dat$X), ncol = choose(p, 2)),
                 unit = if (is.null(dat$unit)) NULL else matrix(0, nrow = nrow(dat$unit), ncol = choose(p, 2)),
                 name = c())
  count <- 1
  if (is.null(dat$unit)) {
    for (i in 1:(p-1)) {
      for (j in (i+1):p) {
        df_out$X[, count] <- dat$X[, i] * dat$X[, j]
        df_out$name[count] <- paste0("(", dat$name[i], "*", dat$name[j], ")")
        count <- count + 1
      }
    }
  } else {
    for (i in 1:(p-1)) {
      for (j in (i+1):p) {
        df_out$X[, count] <- dat$X[, i] * dat$X[, j]
        df_out$unit[, count] <- dat$unit[, i] + dat$unit[, j]
        df_out$name[count] <- paste0("(", dat$name[i], "*", dat$name[j], ")")
        count <- count + 1
      }
    }
  }
  # colnames(df_out$X) <- df_out$name
  return(df_out)
}

DIVD <- function(dat) {
  # Find columns that have 0
  # Put nonzero columns first, then zero columns
  p <- ncol(dat$X)
  col_nonzero <- which(apply(dat$X, 2, function(x) all(x != 0)))
  col_zero <- setdiff(1:p, col_nonzero)
  dat$X <- dat$X[, c(col_nonzero, col_zero)]
  dat$unit <- dat$unit[, c(col_nonzero, col_zero)]
  dat$name <- dat$name[c(col_nonzero, col_zero)]

  # Record which are nonzero and which are zero
  p_nonzero <- length(col_nonzero)
  p_zero <- ncol(dat$X) - p_nonzero
  p_total <- p_nonzero * (p_nonzero - 1) + p_nonzero * p_zero
  if (p_total == 0) {
    cat("There is no column that is strictly nonzero. Cannot perform division.")
    return(list(X = NULL, unit = NULL, name = c()))
  }

  df_out <- list(X = matrix(nrow = nrow(dat$X), ncol = p_total),
                 unit = if (is.null(dat$unit)) NULL else matrix(nrow = nrow(dat$unit), ncol = p_total),
                 name = c())
  count <- 1

  if (is.null(dat$unit)) {
    #### Operation 1: on X.no.zero ###
    if (p_nonzero > 1) {
      for (i in 1:(p_nonzero - 1)) {
        for (j in (i + 1):p_nonzero) {
          df_out$X[, count] <- dat$X[, i] / dat$X[, j]
          df_out$name[count] <- paste0("(", dat$name[i], "/", dat$name[j], ")")
          count <- count + 1
          df_out$X[, count] <- dat$X[, j] / dat$X[, i]
          df_out$name[count] <- paste0("(", dat$name[j], "/", dat$name[i], ")")
          count <- count + 1
        }
      }
    }

    #### Operation 2: on X.some.zero ####
    if ((p_nonzero > 0) & (p_zero > 0)) {
      for (i in (p_nonzero + 1):p) {
        for (j in 1:p_nonzero) {
          df_out$X[, count] <- dat$X[, i] / dat$X[, j]
          df_out$name[count] <- paste0("(", dat$name[i], "/", dat$name[j], ")")
          count <- count + 1
        }
      }
    }
  } else {
    #### Operation 1: on X.no.zero ###
    if (p_nonzero > 1) {
      for (i in 1:(p_nonzero - 1)) {
        for (j in (i + 1):p_nonzero) {
          df_out$X[, count] <- dat$X[, i] / dat$X[, j]
          df_out$unit[, count] <- dat$unit[, i] - dat$unit[, j]
          df_out$name[count] <- paste0("(", dat$name[i], "/", dat$name[j], ")")
          count <- count + 1
          df_out$X[, count] <- dat$X[, j] / dat$X[, i]
          df_out$unit[, count] <- dat$unit[, j] - dat$unit[, i]
          df_out$name[count] <- paste0("(", dat$name[j], "/", dat$name[i], ")")
          count <- count + 1
        }
      }
    }

    #### Operation 2: on X.some.zero ####
    if ((p_nonzero > 0) & (p_zero > 0)) {
      for (i in (p_nonzero + 1):p) {
        for (j in 1:p_nonzero) {
          df_out$X[, count] <- dat$X[, i] / dat$X[, j]
          df_out$unit[, count] <- dat$unit[, i] - dat$unit[, j]
          df_out$name[count] <- paste0("(", dat$name[i], "/", dat$name[j], ")")
          count <- count + 1
        }
      }
    }
  }

  # colnames(df_out$X) <- df_out$name
  return(df_out)
}

binary <- function(data, sin_cos) {
  p <- ncol(data$X)
  if (p < 2) {
    message("X has less than 2 columns. Need at least 2 columns to perform binary operations!")
    return(data)
  } else {

    data_binary <- data

    # ADD
    data_tmp <- ADD(data)
    data_binary$X <- data_tmp$X
    data_binary$unit <- data_tmp$unit
    data_binary$name <- data_tmp$name
    data_tmp <- NULL

    # MINUS
    data_minus <- MINUS(data)
    data_binary$X <- cbind(data_binary$X, data_minus$X)
    data_binary$unit <- cbind(data_binary$unit, data_minus$unit)
    data_binary$name <- c(data_binary$name, data_minus$name)

    # MULTI
    data_tmp <- MULTI(data)
    data_binary$X <- cbind(data_binary$X, data_tmp$X)
    data_binary$unit <- cbind(data_binary$unit, data_tmp$unit)
    data_binary$name <- c(data_binary$name, data_tmp$name)
    data_tmp <- NULL

    # DIVD
    data_tmp <- DIVD(data)
    data_binary$X <- cbind(data_binary$X, data_tmp$X)
    data_binary$unit <- cbind(data_binary$unit, data_tmp$unit)
    data_binary$name <- c(data_binary$name, data_tmp$name)
    data_tmp <- NULL

    # MINUS_ABS
    data_tmp <- ABS(data_minus)
    data_minus <- NULL
    data_binary$X <- cbind(data_binary$X, data_tmp$X)
    data_binary$unit <- cbind(data_binary$unit, data_tmp$unit)
    data_binary$name <- c(data_binary$name, data_tmp$name)
    data_tmp <- NULL

    # Remove redundant descriptors
    data_binary$name <- unname(data_binary$name)
    colnames(data_binary$X) <- data_binary$name
    data_binary <- dataprocessing(data_binary)

    cat(c(paste0("Finished building X.binary... Initial p = ", p, "; New p = ", ncol(data_binary$X)), "\n"))
    return(data_binary)
  }
}

##### Unary Operations #####
ABS <- function(dat) {
  dat$X <- abs(dat$X)
  dat$name <- unname(sapply(dat$name, function(x) paste0("abs(", x, ")")))
  # colnames(dat$X) <- dat$name
  return(dat)
}

SQRT <- function(dat, apply_pos_opt_on_neg_x) {
  # Record which columns has negative values
  neg_col <- apply(dat$X, 2, function(x) any(x < 0))

  if (apply_pos_opt_on_neg_x) {
    dat$X <- sqrt(abs(dat$X))
    if (any(neg_col)) dat$name[neg_col] <- unname(sapply(dat$name[neg_col], function(x) paste0("abs(", x, ")")))
    dat$name <- unname(sapply(dat$name, function(x) paste0("sqrt(", x, ")")))
    if (!is.null(dat$unit)) {
      dat$unit[,] <- 0
    }
  } else {
    if (sum(neg_col) == ncol(dat$X)) return(list(X = NULL, unit = NULL, name = NULL))

    # Apply sqrt on non-negative columns
    dat$X <- as.matrix(sqrt(dat$X[, !neg_col]))
    dat$name <- unname(sapply(dat$name[!neg_col], function(x) paste0("sqrt(", x, ")")))
    if (!is.null(dat$unit)) {
      dat$unit <- matrix(0, nrow = nrow(dat$unit), ncol = ncol(dat$X))
    }
  }
  # colnames(dat$X) <- dat$name
  return(dat)
}

INV <- function(dat) {
  # Remove columns containing 0
  zero_idx <- apply(dat$X, 2, function(x) any(x == 0))
  if (sum(zero_idx) == ncol(dat$X)) return(list(X = NULL, unit = NULL, name = c()))

  # Calculate 1/X for non-zero columns
  dat$X <- as.matrix(1 / dat$X[, !zero_idx])
  dat$name <- unname(sapply(dat$name[!zero_idx], function(x) paste0("(1/", x, ")")))
  if (!is.null(dat$unit)) {
    dat$unit <- as.matrix(-dat$unit[, !zero_idx])
  }
  # colnames(dat$X) <- dat$name
  return(dat)
}

SQRE <- function(dat) {
  dat$X <- dat$X^2
  dat$name <- unname(sapply(dat$name, function(x) paste0("(", x, ")^2")))
  if (!is.null(dat$unit)) {
    dat$unit <- 2 * dat$unit
  }
  # colnames(dat$X) <- dat$name
  return(dat)
}

LOG <- function(dat, apply_pos_opt_on_neg_x) {
  # Remove columns containing 0
  zero_col <- apply(dat$X, 2, function(x) any(x == 0))
  dat$X <- as.matrix(dat$X[, !zero_col])
  dat$name <- dat$name[!zero_col]
  dat$unit <- if (is.null(dat$unit)) NULL else as.matrix(dat$unit[, !zero_col])

  # Record which columns has negative values
  neg_col <- apply(dat$X, 2, function(x) any(x < 0))

  if (apply_pos_opt_on_neg_x & any(neg_col)) {
    dat$X <- log(abs(dat$X))
    dat$name[neg_col] <- unname(sapply(dat$name[neg_col], function(x) paste0("abs(", x, ")")))
    dat$name <- unname(sapply(dat$name, function(x) paste0("log(", x, ")")))
    if (!is.null(dat$unit)) {
      dat$unit[,] <- 0
    }
  } else {
    if (sum(neg_col) == ncol(dat$X)) return(list(X = NULL, unit = NULL, name = NULL))

    # Apply log to positive columns
    dat$X <- as.matrix(log(dat$X[, !neg_col]))
    dat$name <- unname(sapply(dat$name[!neg_col], function(x) paste0("log(", x, ")")))
    if (!is.null(dat$unit)) {
      dat$unit <- matrix(0, nrow = nrow(dat$unit), ncol = ncol(dat$X))
    }
  }
  # colnames(dat$X) <- dat$name
  return(dat)
}

EXP <- function(dat) {
  dat$X <- exp(dat$X)
  dat$name <- unname(sapply(dat$name, function(x) paste0("exp(", x, ")")))
  if (!is.null(dat$unit)) {
    dat$unit[,] <- 0
  }
  # colnames(dat$X) <- dat$name
  return(dat)
}

SIN <- function(dat) {
  dat$X <- sin(pi * dat$X)
  dat$name <- unname(sapply(dat$name, function(x) paste0("sin(pi*", x, ")")))
  if (!is.null(dat$unit)) {
    dat$unit[,] <- 0
  }
  # colnames(dat$X) <- dat$name
  return(dat)
}

COS <- function(dat) {
  dat$X <- cos(pi * dat$X)
  dat$name <- unname(sapply(dat$name, function(x) paste0("cos(pi*", x, ")")))
  if (!is.null(dat$unit)) {
    dat$unit[,] <- 0
  }
  # colnames(dat$X) <- dat$name
  return(dat)
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

    if (sin_cos) {
      data_sin <- SIN(data)
      data_cos <- COS(data)

      # Combine datasets
      data$X <- cbind(data$X, data_sqrt$X, data_sqre$X,
                      data_log$X, data_exp$X, data_sin$X,
                      data_cos$X, data_inv$X, data_abs$X)
      data$unit <- cbind(data$unit, data_sqrt$unit, data_sqre$unit,
                         data_log$unit, data_exp$unit, data_sin$unit,
                         data_cos$unit, data_inv$unit, data_abs$unit)
      data$name <- unname(c(data$name, data_sqrt$name, data_sqre$name,
                            data_log$name, data_exp$name, data_sin$name,
                            data_cos$name, data_inv$name, data_abs$name))
    } else {
      # Combine datasets
      data$X <- cbind(data$X, data_sqrt$X, data_sqre$X,
                      data_log$X, data_exp$X,
                      data_inv$X, data_abs$X)
      data$unit <- cbind(data$unit, data_sqrt$unit, data_sqre$unit,
                         data_log$unit, data_exp$unit,
                         data_inv$unit, data_abs$unit)
      data$name <- unname(c(data$name, data_sqrt$name, data_sqre$name,
                            data_log$name, data_exp$name,
                            data_inv$name, data_abs$name))
    }

    colnames(data$X) <- data$name

    # Remove redundant descriptors
    data <- dataprocessing(data)

    cat(c(paste0("Building X.unary... Initial p = ", p, "; New p = ", ncol(data$X)), "\n"))
    return(data)
  }
}
