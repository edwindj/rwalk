library(tinytest)
library(data.table)
library(rwalk)

edges <- fread("./data-raw/d0_edges.csv")
nodes <- fread("./data-raw/d0_nodes.csv")

n <- rwalk:::node_stat_dt(nodes)
expect_equal_to_reference(n, file = "d0_node_stat_dt.rds")
# Placeholder with simple test

# one step
rw <- rwalk(edges, nodes, alpha = 0.4, max_steps = 1, verbose = FALSE)
expect_equal(rw$exposure$A, c(0.0, 0.6))
expect_equal(rw$exposure$B, c(0.6, 0.0))

rw <- rwalk(edges, nodes, alpha = 0.4, verbose = FALSE)
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
