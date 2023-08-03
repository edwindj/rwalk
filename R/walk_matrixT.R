library(data.table)
library(Matrix)

node_matrixT <- function(nodes, edges){
  n <- nodes[, .( id = factor(id)
                , type = factor(type)
                , weight
                )]

  n_id <- nrow(n)


  n_id <- nrow(n)
  n_types <- nlevels(n$type)

  lev_n <- levels(n$id)
  N <- length(lev_n)

  X <- matrix(0
              , nrow = n_id
              , ncol = n_types
              , dimnames = list(id = n$id, exposed_to = levels(n$type))
  )

  D <- X

  type_missing <- is.na(n$type)
  if (any(type_missing)){
    n$weight[type_missing] <- 0
    n_m <- n[!is.na(type), .(n = .N, w = sum(weight, na.rm = TRUE)), by = .(type)]
    n_m[, w_fraction:= w/sum(w)]
    n_m[, expected := w_fraction * n_id]
    n_m[, w_missing:= expected - n]

    # TODO check if w_missing is not negative...
    if (any(n_m$w_missing < 0)){
      warning("Weight ratio is in conflict with the found number of records.
w_missing, the number of missing records for that 'category' should be non-negative'.
Please fix the weights.
")

      print(n_m)
      n_m[ w_missing < 0, w_missing := 0]
    }

    n_m[, f_w := w_missing/sum(w_missing)]
    n_m
  }

  D[cbind(seq_len(n_id), n$type)] <- 1

  X_step <- X

  e <- edges[, .( from = match(from, lev_n)
                , to = match(to, lev_n)
                , weight
                )]

  M <- sparseMatrix( i = e$to
                   , j = e$from
                   , x = e$weight
                   , dims = c(N, N)
                   , dimnames = list(from=lev_n, to=lev_n)
                   )
  list( D = D
      , M = M
      , X = X
      , X_step = X_step
  )
}


#' @export
#' @param edges data.frame with edges: from,to,weight
#' @param nodes data.frame with nodes: id, weight
#' @param alpha stopping probability
#' @param max_steps `integer` with the maximum number of steps
#' @param tolerance `numeric`
#' @import data.table
rwalk_matrixT <- function( edges
                        , nodes
                        , alpha = 0.85
                        , max_steps = ceiling(log(tolerance)/log(alpha))
                        , tolerance = 1e-5
                        , verbose = TRUE
){
  if (verbose){
    message("settings:")
    print(list(alpha = alpha, max_steps = max_steps, tolerance = tolerance))
  }
  l <- node_matrixT(nodes, edges)

  for (step in seq_len(max_steps)){
    if (verbose) message("## step: ", step)
    l <- rstep_matrixT( l = l
                     , alpha = alpha
                     , step = step
                     )


    if (verbose){
      l |> print()
      message("##\n")
    }

    if (l$max_diff < tolerance){
      if (verbose) message("Stopped at step ",step,", seems converged")
      break
    }
  }

  exposure <- cbind( id = rownames(l$X)
                   , l$X |> as.matrix() |> as.data.table()
  )


  list(
    X = l$X |> as.matrix(),
    exposure = exposure,
    D = l$D,
    max_diff = l$max_diff,
    steps = l$step
  )
}

rstep_matrixT <- function(alpha = 0.85, l, step = 1){
  # current D

  D_step <- Matrix::crossprod(l$M, l$D)# |> as.matrix()

  X <- l$X + (1-alpha) * D_step
  D <- alpha * D_step

  max_diff <- (1-alpha) * max(D_step)

  list( X = X
      , D = D
      , M = l$M
      , max_diff = max_diff
      , step = step
      )
}

sparse_to_dt <- function(X){
  s <- Matrix::summary(X) |> as.data.frame()
  setDT(s)

  # faster way of creating a factor
  nid <- seq_len(nrow(X))
  levels(nid) <- rownames(X)
  class(nid) <- "factor"

  ntype <- seq_len(ncol(X))
  levels(ntype) <- colnames(X)
  class(ntype) <- "factor"

  d <- s[, .( id = nid[i]
            , exposed_to = ntype[j]
            , exposure = x)
            ]
  setkey(d, id, exposed_to)
  d
}
