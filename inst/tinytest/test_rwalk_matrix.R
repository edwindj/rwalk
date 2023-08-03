library(tinytest)
library(data.table)
library(matrix)
library(rwalk)

edges <- fread("./data-raw/d0_edges.csv")
nodes <- fread("./data-raw/d0_nodes.csv")

l <- rwalk:::node_matrix(nodes, edges)
expect_equal(names(l), c("D", "M", "X", "X_step"))

expect_equal( as.matrix(l$D)
            , matrix(c(1,0,0,1), ncol = 2, dimnames = list(c("A1", "B1"), c("A", "B")))
            )

expect_equal( as.matrix(l$M)
            , matrix(c(0,1,1,0), ncol = 2, dimnames = list(to = c("A1", "B1"), from=c("A1", "B1")))
            )


# Placeholder with simple test

# one step
rw <- rwalk_matrix(edges, nodes, alpha = 0.4, max_steps = 1, verbose = FALSE)
expect_equal(rw$exposure$type |> as.character(), c("B", "A"))
expect_equal(rw$exposure$exposure, c(0.6, 0.6))

rw <- rwalk_matrix(edges, nodes, alpha = 0.99, verbose = TRUE)
rw
expect_equal(rw$exposure_avg$exposure, rep(0.5, 2))
rw$fraction

rw <- rwalk(edges, nodes, alpha = 0.9, verbose = FALSE)
expect_equal(rw$exposure_avg$exposure, rep(0.5, 2))
expect_equal(rw$n$f, c(0.47, .53, .53, .47))


edges <- fread("./data-raw/d1_edges.csv")
nodes <- fread("./data-raw/d1_nodes.csv")

rw <- rwalk(edges, nodes, alpha = 0.4, max_steps = 1, verbose = FALSE)
expect_equal(rw$exposure$A, c(0.3, 0.3, 0.6))
expect_equal(rw$exposure$B, c(0.3, 0.3, 0.0))

rw <- rwalk(edges, nodes, alpha = 0.4, verbose = FALSE)
expect_equal(rw$exposure_avg$exposure, c(0.67, 0.33))
