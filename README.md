# lazydraws

Inspect selected posterior draws without loading a whole fit into R. Runtime
dependencies are **duckdb**, **DBI**, and **posterior**.

```r
library("posterior")
library("lazydraws")

drw <- load_draws("draws.parquet")

drw |>
  subset_draws(variable = c("mu", "theta"), chain = 1:2) |>
  summarise_draws()
```

`load_draws()` returns an S3 object of class `lazydraws`, holding a DuckDB
connection and a view over the Parquet file. Parameter values stay on disk.
`nchains()`, `niterations()`, `ndraws()`, `nvariables()`, and `variables()` use
cached metadata. `chain_ids()`, `iteration_ids()`, and `draw_ids()` query the
corresponding index columns.

`subset_draws()` queries the selected draws and returns an in-memory `draws_df`
for use with `posterior` functions. With no selectors, it reads all draws.

Operations such as `mutate_variables()`, `thin_draws()`, `summarise_draws()`,
and draws-format conversions require in-memory draws. Calling them directly on
a `lazydraws` object gives an error explaining that you should subset first:

```r
drw |>
  subset_draws(variable = c("mu", "theta")) |>
  mutate_variables(theta_diff = `theta[1]` - `theta[2]`)
```

The source stays open and reusable after subsetting. Its connection is cleaned
up automatically when the last `lazydraws` object referring to it is
garbage-collected, or when R exits. Use `close_draws(drw)` if you want to release
the connection immediately; already returned draws remain usable.

Installation:

```r
pak::pkg_install("n-kall/lazydraws")
```
