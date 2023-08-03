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

nodes[, p := runif(.N)]
nodes[type == "A", ntype := ifelse(p < 0.06, NA_character_, type)]
nodes[type == "B", ntype := ifelse(p < 0.04, NA_character_, type)]
nodes[type == "C", ntype := ifelse(p < 0.02, NA_character_, type)]

nodes[is.na(ntype), weight := 0]
nodes[, weight := .N/sum(weight), by = .(type)]
nodes[, type := ntype]

system.time({
  w3 <- rwalk_matrixT(edges, nodes, alpha = 0.4, verbose = FALSE)
})

library(ggplot2)

w3$exposure |>
  melt(id.vars = "id", variable.name = "exposed_to") |>
  ggplot() +
  geom_freqpoly(aes(x = value, col = exposed_to), binwidth=0.01)

w3$exposure |>
  melt(id.vars = "id", variable.name = "exposed_to") |>
  ggplot() +
  geom_histogram(aes(x = value, fill = exposed_to), binwidth=0.01) +
  facet_wrap(~exposed_to)

d <- w3$exposure |>
  melt(id.vars = "id", variable.name = "exposed_to")

d[exposed_to != "V4", .(sv = value / sum(value), value, exposed_to), by = .(id)] |>
  ggplot() +
  geom_freqpoly(aes(x = sv, col = exposed_to), binwidth=0.01)

d[exposed_to != "V4", .(sv = value / sum(value), value, exposed_to), by = .(id)][
  , .(sv = 6* mean(sv)), by = .(exposed_to)
]
