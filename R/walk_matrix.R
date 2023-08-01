library(data.table)
library(Matrix)

node_matrix <- function(nodes, edges){
  n <- nodes[, .(id = factor(id), type = factor(type), weight)]
  lev_n <- levels(n$id)

  D <- sparseMatrix( n$id
                   , n$type
                   , x = n$weight
                   , dims = c(nlevels(n$id), nlevels(n$type))
                   , dimnames = list(levels(n$id), levels(n$type)))

  X <- sparseMatrix( i = integer()
                   , j = integer()
                   , x = double()
                   , dims = c(nlevels(n$id), nlevels(n$type))
                   , dimnames = list(levels(n$id), levels(n$type))
                   )

  X_step <- X

  e <- edges[, .( from = factor(from, levels=lev_n)
                , to = factor(to, levels=lev_n)
                , weight
                )]

  M <- sparseMatrix( i = e$to
                   , j = e$from
                   , x = e$weight
                   , dimnames = list(to=lev_n, from=lev_n)
                   )
  list( D = D
      , M = M
      , X = X
      , X_step = X_step
  )
}

plot_graph <- function(e){
  e |> igraph::graph_from_data_frame() |> plot()
}

#' @export
#' @param edges data.frame with edges: from,to,weight
#' @param nodes data.frame with nodes: id, weight
#' @param alpha stopping probability
#' @param max_steps `integer` with the maximum number of steps
#' @param tolerance `numeric`
#' @import data.table
rwalk_matrix <- function( edges
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
  l <- node_matrix(nodes, edges)

  for (step in seq_len(max_steps)){
    if (verbose) message("## step: ", step)
    l <- rstep_matrix( l = l
                     , alpha = alpha
                     , step = step
                     )


    if (verbose){
      l |> print()
      message("##\n")
    }

    if (max(l$X_step, na.rm = TRUE) < tolerance){
      if (verbose) message("Stopped at step ",step,", seems converged")
      break
    }
  }

  exposure_long <- l$X |> sparse_to_dt()
  exposure <- l$X |> as.matrix() |> as.data.table()


  list(
    X = l$X,
    exposure = exposure,
    exposure_long = exposure_long,
    D = l$D,
    steps = l$step
  )
}

rstep_matrix <- function(alpha = 0.85, l, step = 1){
  # current D

  D_step <- l$M %*% l$D

  X <- l$X + (1-alpha)*D_step
  D <- alpha * D_step

  list( X = X
      , D = D
      , M = l$M
      , X_step  = (1-alpha)*D_step
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
