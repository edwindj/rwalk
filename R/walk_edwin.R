library(data.table)

node_stat <- function(nodes){
  d <- CJ( id = nodes$id
         , exposed_to = nodes$type
         , exposure = 0
         , unique = TRUE
         )
  
  d$delta <- nodes[ .(d$id)
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

rstep_edwin <- function(alpha = 0.85, e, n, step = 1){
  e_w <- n[ e
          , .( id = from # we calculate this for the from node
             , exposed_to
             , delta_old = delta
             , delta = i.weight * delta
             , weight
             # , exposure
             , to
             )
          , on = .(id = to) # but we aggregate for the to node
          , nomatch = NA
          ]
  e_w
  n_w <- e_w[
            , .( delta = sum(delta)
               # , exposure = first(exposure)
               , n = .N
               )
            , by = .(id, exposed_to)
            ]
  
  n_w$exposure <- n[n_w[,.(id, exposed_to)], exposure, on=.(id, exposed_to)]
  
  n_w[, exposure := exposure + (1-alpha)*delta]
  n_w[, delta := alpha*delta]
  n_w
}

nodes <- fread("data/d1_nodes.csv")
edges <- fread("data/d1_edges.csv")

# nodes <- fread("data/d3_nodes.csv")
# edges <- fread("data/d3_edges.csv")

edges[, weight := weight/sum(weight), by  = .(from)]

n <- node_stat(nodes)
e <- edges

tol <- 1e-5

for (step in 1:20){
  message("## step: ", step)
  n <- rstep_edwin( e = e
            , n = n
            , alpha = 0.4
            , step = step
            )
  
  if (max(n$delta) < tol){
    message("Stopped, seems converged")
    break
  }
  n |> show_fractions() |> print()
  message("##\n")
}
 