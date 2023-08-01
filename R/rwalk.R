node_stat_dt <- function(nodes){
  d <- data.table::CJ( id = nodes$id
         , exposed_to = nodes$type
         , exposure   = 0
         , unique     = TRUE
         )


  d$delta <- nodes[ .( d$id)
                     , ifelse(type == d$exposed_to, weight, 0)
                     , on=.(id)
                     ]
  d
}

plot_graph <- function(e){
  e |> igraph::graph_from_data_frame() |> plot()
}

show_fractions <- function(n){
  f <- n[, .(f = (exposure/sum(exposure)) |> round(2), exposed_to)
         , by = .(id)
         ]
  message("n:")
  print(n)
  message("fractions:")
  dcast(f, id ~ exposed_to, value.var = "f", fill = 0)
}

#' @export
#' @param edges data.frame with edges: from,to,weight
#' @param nodes data.frame with nodes: id, weight
#' @param alpha stopping probability
#' @param max_steps `integer` with the maximum number of steps
#' @param tolerance `numeric`
#' @import data.table
rwalk <- function( edges
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

  e <- edges
  n <- node_stat_dt(nodes)
  for (step in seq_len(max_steps)){
    if (verbose) message("## step: ", step)
    n <- rstep_dt( e = e
                 , n = n
                 , alpha = alpha
                 , step = step
    )


    if (verbose){
      n |> show_fractions() |> print()
      message("##\n")
    }

    if (max(n$delta, na.rm = TRUE) < tolerance){
      if (verbose) message("Stopped at step ",step,", seems converged")
      break
    }


  }
  n[, f := round(exposure/sum(exposure), 2), by = .(id)]
  exposure = dcast(n, id ~ exposed_to, sum, value.var = "exposure")
  fraction = dcast(n, id ~ exposed_to, sum, value.var = "f")
  exposure_avg = n[, .(exposure = mean(exposure) |> round(2)), by = .(exposed_to)]
  list( n = n
      , exposure = exposure
      , fraction = fraction
      , exposure_avg = exposure_avg
      )
}

rstep_dt <- function(alpha = 0.85, e, n, step = 1){
  e_w <- n[ e
          , .( from # we calculate this for the from node
             , exposed_to
             , delta_old = delta
             , delta = i.weight * delta
             , weight = i.weight * weight
             # , exposure
             , to
             )
          , on = .(id = to) # but we aggregate for the to node
          , by = .EACHI
          , nomatch = NA
          ]
  e_w
  n_w <- e_w[
            , .( delta = sum(delta, na.rm=TRUE)
               , delta_old = first(delta_old)
               , weight = sum(weight, na.rm=TRUE)
               , n = .N
               )
            , by = .(id = from, exposed_to)
            ]

  n_w$exposure <- n[n_w[,.(id, exposed_to)], exposure, on=.(id, exposed_to)]
  n_w[is.na(delta), delta := 0]

  n_w[, exposure := exposure + (1-alpha)*delta]
  n_w[, delta := alpha*delta]
  n_w
}
