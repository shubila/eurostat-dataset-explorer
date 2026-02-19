# ============================================================
# 00_globals.R
# General utility helpers (no Shiny dependency)
# ============================================================

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !is.na(a)) a else b
}
