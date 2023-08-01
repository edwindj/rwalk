library(data.table)

get_nodes <- function(x){
  id <- c(x$from, x$to) |> unique()
  nodes <- data.table(
    id = id,
    type = substr(id, 1,1),
    weight = 1
  )
  nodes[type == "M", type := NA_character_]
  nodes
}

# ----- super simple
d <-
  "from, to, weight
A1,B1,1.
B1,A1,1
" |> fread()
d[, weight := weight/sum(weight), by = .(from)]
d

d |> fwrite("data-raw/d0_edges.csv")
d |> get_nodes() |> fwrite("data-raw/d0_nodes.csv")

# ---- simple

d <-
"from, to, weight
A1,A2,1.
A2,A1,1
A1,B1,1
B1,A1,1
B1,A2,1
A2,B1,1
" |> fread()
d[, weight := weight/sum(weight), by = .(from)]
d

d |> fwrite("data-raw/d1_edges.csv")
d |> get_nodes() |> fwrite("data-raw/d1_nodes.csv")

d2 <-
  "from, to, weight
A1,A2,1
A2,A1,1
A1,B1,1
B1,A1,1
B2,B1,1
B1,B2,1
A3,B1,1
B1,A3,1
" |> fread()

d2[, weight := weight/sum(weight)]
d2 |> fwrite("data-raw/d2_edges.csv")
d2 |> get_nodes() |> fwrite("data-raw/d2_nodes.csv")


d3 <-
"from, to, weight
A1,B1,1.
B1,A1,1
M1,B1,1
B1,M1,1
" |> fread()

d3[, weight := weight/sum(weight), by = .(from)]
d3 |> fwrite("data-raw/d3_edges.csv")
d3 |> get_nodes() |> fwrite("data-raw/d3_nodes.csv")


# ---- large

set.seed(1)
N <- 40
M <- floor(sqrt(N)) * N

tpe <- sample(c("A", "B"), size = N, replace = TRUE, prob=c(.4, .6))
id <- factor(paste0(tpe, seq_len(N)))
edges <- data.table( from = sample(id, size = M, replace = TRUE)
                   , to = sample(id, size = M, replace = TRUE)
                   , weight = 1
                   ) |>
  unique()

edges <- edges[from != to, ]
edges <- rbind(edges, edges[,.(from = to, to = from, weight)])
edges[, weight := weight/sum(weight), by = .(from)]

nodes <- data.table( id = id
                   , type = tpe
                   , weight = 1
                   )
e <- edges
(w <- rwalk(e, nodes, alpha = 0.4))
w$exposure
