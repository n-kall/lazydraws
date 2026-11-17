# lazydraws

Inspect selected posterior draws without loading a whole fit into R. Runtime
dependencies are **duckdb**, **DBI**, and **posterior**.

```r
# On the cluster, while the original CmdStan CSV files still exist:
lazydraws::cmdstanr_to_parquet_draws(fit, "draws.parquet")

# Transfer draws.parquet to your local machine:
library(lazydraws)
library(posterior)

drw <- load_draws("draws.parquet")
drw |>
  subset_draws(variable = c("mu", "theta"), chain = 1:2) |>
  summarise_draws()

# Each call executes a query and returns an ordinary draws_df:
x <- subset_draws(drw, iteration = 100:200)
y <- subset_draws(drw, draw = 1:100) # merges chains
all_draws <- subset_draws(drw)       # reads all draws

close_draws(drw)
```

`load_draws()` returns an S3 object of class `lazydraws`, holding a DuckDB
connection and a view over the Parquet file. Only the schema is inspected at
load time. `posterior::subset_draws()` dispatches to `subset_draws.lazydraws()`,
executes the selection, and returns a `draws_df` containing the selected values.
No collection step is needed. Further subsets of that result use posterior's
ordinary in-memory methods.

The source stays open and reusable after subsetting. `close_draws()` closes its
connection; previously returned draws remain usable. Automatic cleanup is a
fallback when the last object holding the connection is garbage collected.
Reopen the file in another R session rather than saving the live connection.

Variable expansion, regex matching, exclusion, duplicate selection, draw merging,
and index repair follow `posterior::subset_draws()`. Row selection first reads
all draw index columns into R, applies posterior's rules, and fetches selected
values through a DuckDB join. Thus memory is needed for the original indices
plus selected parameter values, but unselected parameter values stay out of R.

Export supports completed MCMC fits with matching chain lengths and plain CSV
output. Warmup is excluded by default; sampler diagnostics are included. The
Parquet file stores scalar parameter names and `.chain`, `.iteration`, `.draw`,
not a serialized fit. Conversion does not call `fit$draws()`. It uses streaming
queries, with defaults of two DuckDB threads and a 1 GB engine memory limit;
this is not a hard cap on total process memory. DuckDB can spill supported
operations to R's temporary directory, which should be on disk.

Installation from a local checkout:

```r
install.packages(c("duckdb", "DBI", "posterior"))
install.packages("/path/to/lazydraws", repos = NULL, type = "source")
```
