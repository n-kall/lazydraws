# lazydraws

Inspect selected posterior draws without loading a whole fit into R. Runtime
dependencies are **duckdb**, **DBI**, and **posterior**.

```r
library("posterior")
library("lazydraws")

drw <- load_draws("draws.parquet") # initiates a connection but does not load into memory

drw |>
  subset_draws(variable = c("mu", "theta"), chain = 1:2) |> # loads the selection into memory
  summarise_draws()

close_draws(drw) # closes the connection
```

`load_draws()` returns an S3 object of class `lazydraws`, holding a DuckDB
connection and a view over the Parquet file. `nchains()`, `nvariables()`, `ndraws()`, `variables()` access metadata from the `lazydraws` object.

`subset_draws()` will query the database and return a `draws_df` into memory,
which can be used with `posterior` functions.

The source stays open and reusable after subsetting. `close_draws()` closes its
connection.

Installation:

```r
pak::pkg_install("n-kall/lazydraws")
```
