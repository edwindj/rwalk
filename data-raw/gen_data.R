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
A1,B1,1
B1,A1,1
" |> fread()
d[, weight := weight/sum(weight), by = .(from)]
d

d |> fwrite("data/d0_edges.csv")
d |> get_nodes() |> fwrite("data/d0_nodes.csv")

# ---- simple

d <-
"from, to, weight
A1,A2,1
A2,A1,1
A1,B1,1
B1,A1,1
" |> fread()
d[, weight := weight/sum(weight)]
d

d |> fwrite("data/d1_edges.csv")
d |> get_nodes() |> fwrite("data/d1_nodes.csv")

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
d2 |> fwrite("data/d2_edges.csv")
d2 |> get_nodes() |> fwrite("data/d2_nodes.csv")


d3 <-
"from, to, weight
A1,B1,1
B1,A1,1
M1,B1,1
B1,M1,1
" |> fread()

d3[, weight := weight/sum(weight)]
d3 |> fwrite("data/d3_edges.csv")
d3 |> get_nodes() |> fwrite("data/d3_nodes.csv")
