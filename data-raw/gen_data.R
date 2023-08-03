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
N <- 4e5

250 *  N
M <- floor(log(N)) * N
n_types <- seq_len(3)

tpe <- sample(LETTERS[n_types], size = N, replace = TRUE, prob=n_types)
id <- factor(paste0(tpe, seq_len(N)))
edges <- data.table( from = sample(id, size = M, replace = TRUE)
                   , to = sample(id, size = M, replace = TRUE)
                   , weight = 1
                   )
# |>
#   unique()

edges <- edges[from != to, ] |> unique()
edges <- rbind(edges, edges[,.(from = to, to = from, weight)])
edges[, weight := weight/sum(weight), by = .(from)]

nodes <- data.table( id = id
                   , type = tpe
                   , weight = 1
                   )

edges |>
  fwrite(sprintf("data-raw/d_%#d_edges.csv", N))

nodes |>
  fwrite(sprintf("data-raw/d_%#d_nodes.csv", N))


# system.time({
#   w <- rwalk(edges, nodes, alpha = 0.4, verbose = TRUE, max_steps = 2)
# })


system.time({
  w2 <- rwalk_matrix(edges, nodes, alpha = 0.4, verbose = FALSE)
})

system.time({
  w3 <- rwalk_matrixT(edges, nodes, alpha = 0.4, verbose = FALSE)
})

nas <- sample(N, 0.05 * N)
nodes$type[nas] <- NA_character_

system.time({
  w3 <- rwalk_matrixT(edges, nodes, alpha = 0.4, verbose = FALSE)
})

w3$exposure
