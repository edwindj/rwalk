library(data.table)
node_stat <- function(nodes){
  d <- CJ(id = nodes$id, exposed_to = nodes$type, unique = TRUE)
  d$n <- nodes[.(d$id), ifelse(type == d$exposed_to, weight, 0), on=.(id)]
  d[, nsum := 0]
  d
}

show_fractions <- function(n){
  f <- n[, .(f = (nsum/sum(nsum)) |> round(2), exposed_to)
         , by = .(id)
         ]
  print(n)
  dcast(f, id ~ exposed_to, value.var = "f", fill = 0)
}

rstep_jan <- function(alpha = 0.85, e, n, step = 1){
  e_w <- n[ e
          , .( id = from # we calculate this for the from node
             , exposed_to
             , n = i.weight * n # skip alpha for next step (more efficient)
             )
          , on = .(id = to) # but we aggregate for the to node
          , nomatch = NA
          ]
  n_w <- e_w[
           , .( n = sum(n)
              # , nsum = first(nsum) + (1-alpha) * sum(n) 
              )
           , by = .(id, exposed_to)
          ]
  setkey(n, id, exposed_to)
  n_w$nsum <- n[(n_w[,.(id, exposed_to)]),n]
  
  n_w[, n := alpha * n]
  n_w
  # if (step > 1){
  # } 
  # n_w[, .(n = sum(n)), by = .(id, exposed_to)]
}

nodes <- fread("data/d1_nodes.csv")
edges <- fread("data/d1_edges.csv")

# nodes <- fread("data/d3_nodes.csv")
# edges <- fread("data/d3_edges.csv")

edges[, weight := weight/sum(weight), by  = .(from)]

n <- node_stat(nodes)
e <- edges

for (step in 1:10){
  message("step: ", step)
  n <- rstep_jan( e = e
            , n = n
            , alpha = 0.4
            , step = step
            )
  n |> show_fractions() |> print()
}
