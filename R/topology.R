# Fixed topology and resource-pool construction.

NODE <- c(Source = 1L, Collection = 2L, Transfer_A = 3L, Transfer_B = 4L,
          Stage1_a = 5L, Stage1_b = 6L, Stage2 = 7L, Stage3 = 8L)

make_leg_pools <- function(V, leg_origins) {
  pools <- list()
  add <- function(leg, origins, vehicles, kind) {
    if (vehicles < 1L) return(invisible(NULL))
    pools[[length(pools) + 1L]] <<- list(
      id = length(pools) + 1L, leg = as.integer(leg),
      origins = as.integer(origins), vehicles = as.integer(vehicles),
      kind = kind)
  }
  for (leg in seq_along(leg_origins)) {
    origins <- leg_origins[[leg]]
    if (length(origins) == 1L) {
      add(leg, origins, V[leg], "dedicated")
    } else {
      per_origin <- V[leg] %/% length(origins)
      for (origin in origins) add(leg, origin, per_origin, "dedicated")
      remainder <- V[leg] - per_origin * length(origins)
      add(leg, origins, remainder, "flex")
    }
  }
  pools
}
