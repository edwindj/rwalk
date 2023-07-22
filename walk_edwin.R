library(data.table)

node_stat <- function(nodes){
  d <- CJ(id = nodes$id, exposed_to = nodes$type, unique = TRUE)
  d$exposure <- nodes[ .(d$id)
                     , ifelse(type == d$exposed_to, weight, 0)
                     , on=.(id)
                     ]
  d
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
             , exposure = (1-alpha) * weight * exposure 
             )
          , on = .(id = to) # but we aggregate for the to node
          , nomatch = NA
          ]
  n_w <- e_w[
            , .( exposure = sum(exposure)
               , n = .N
               )
            , by = .(id, exposed_to)
            ]
  
  # n_w[, exposure := exposure/sum(exposure), by = .(id)]
  ns <- n[ .(n_w[, .(id, exposed_to)])
         , .(old = exposure, old_n=n)
         , on =.(id, exposed_to)
         ]
  n_w$old <- ns$old
  n_w$old_n <- ns$old_n
  
  if (step > 1){
    n_w[, exposure := exposure + old]
    n_w[, n := n * old_n]
  }
  n_w
  
  # if (step > 1){
  # } 
  # n_w[, .(n = sum(n)), by = .(id, exposed_to)]
}

nodes <- fread("data/d1_nodes.csv")
edges <- fread("data/d1_edges.csv")

# nodes <- fread("data/d3_nodes.csv")
# edges <- fread("data/d3_edges.csv")

edges[, weight := weight/sum(weight), by  = .(to)]

n <- node_stat(nodes)
e <- edges

for (step in 1:20){
  message("## step: ", step)
  n <- rstep_edwin( e = e
            , n = n
            , alpha = 0.4
            , step = step
            )
  n |> show_fractions() |> print()
  message("##\n")
}
