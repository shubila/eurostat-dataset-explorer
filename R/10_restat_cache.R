# ============================================================
# 10_restat_cache.R
# TOC loading with in-memory caching
# ============================================================

# load_toc_once <- local({
#   toc <- NULL
#   
#   function() {
#     if (is.null(toc)) {
#       toc <<- tryCatch(
#         get_eurostat_toc(
#           cache = TRUE,
#           update_cache = FALSE,
#           verbose = FALSE
#         ),
#         error = function(e) {
#           message("TOC load failed: ", e$message)
#           NULL
#         }
#       )
#     }
#     toc
#   }
# })

load_toc_once <- local({
  toc <- NULL
  function() {
    if (is.null(toc)) {
      toc <<- tryCatch(
        get_eurostat_toc(
          mode = "txt",          # ✅ faster than xml
          cache = TRUE,
          update_cache = FALSE,
          verbose = FALSE
        ),
        error = function(e) {
          message("TOC load failed: ", e$message)
          NULL
        }
      )
    }
    toc
  }
})
