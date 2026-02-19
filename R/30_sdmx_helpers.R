# ============================================================
# 30_sdmx_helpers.R
# SDMX helpers (dimension detection + CSV handling)
# NOTE:
# - TIME is NOT derived from DSD.
# - Time filtering is handled ONLY via startPeriod/endPeriod.
# ============================================================

# ---- Dimension detection ----

get_dim <- function(dims, name) {
  hit <- grep(paste0("^", name, "$"), dims, ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) NULL else hit[1]
}

get_geo_dim  <- function(dims) get_dim(dims, "geo")
get_freq_dim <- function(dims) get_dim(dims, "freq")

# We keep this only for dimension detection,
# NOT for time inference.
get_time_dim <- function(dims) {
  get_dim(dims, "time_period") %||% get_dim(dims, "time")
}

# ---- TOC-based time normalization ----
# Used ONLY to adapt TOC dataEnd to freq (A/Q/M)
normalize_toc_end_for_freq <- function(toc_end, freq_code) {
  
  if (is.null(toc_end) || !nzchar(toc_end)) return(NULL)
  
  freq_code <- toupper(freq_code %||% "")
  year <- sub("^([0-9]{4}).*$", "\\1", toc_end)
  
  if (freq_code == "A") return(year)
  
  if (freq_code == "Q") {
    if (grepl("Q[1-4]$", toc_end)) return(toc_end)
    return(paste0(year, "-Q4"))
  }
  
  if (freq_code == "M") {
    if (grepl("M[0-9]{2}$", toc_end) || grepl("-[0-9]{2}$", toc_end))
      return(toc_end)
    return(paste0(year, "M12"))
  }
  
  toc_end
}

# ---- SDMX CSV handling ----

fetch_sdmx_csv <- function(url, max_lines = Inf) {
  tf <- tempfile(fileext = ".csv")
  utils::download.file(url, tf, quiet = TRUE, mode = "wb")
  
  con <- file(tf, open = "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  
  readLines(con, n = max_lines)
}

parse_csv_lines <- function(lines) {
  tf <- tempfile(fileext = ".csv")
  writeLines(lines, tf, useBytes = TRUE)
  utils::read.csv(tf, stringsAsFactors = FALSE, check.names = FALSE)
}

prev_period <- function(freq_code, period) {
  freq_code <- toupper(freq_code %||% "")
  
  # A: YYYY
  if (freq_code == "A") {
    y <- suppressWarnings(as.integer(sub("^([0-9]{4}).*$", "\\1", period)))
    if (is.na(y)) return(NULL)
    return(as.character(y - 1L))
  }
  
  # Q: YYYY-Qn (accept YYYYQn too)
  if (freq_code == "Q") {
    m <- regexec("^(\\d{4})-?Q([1-4])$", period, perl = TRUE)
    mm <- regmatches(period, m)[[1]]
    if (length(mm) != 3) return(NULL)
    y <- as.integer(mm[2]); q <- as.integer(mm[3])
    if (q > 1) return(paste0(y, "-Q", q - 1L))
    return(paste0(y - 1L, "-Q4"))
  }
  
  # M: YYYY-MM or YYYYMmm
  if (freq_code == "M") {
    m <- regexec("^(\\d{4})[-M]?(\\d{2})$", period, perl = TRUE)
    mm <- regmatches(period, m)[[1]]
    if (length(mm) != 3) return(NULL)
    y <- as.integer(mm[2]); mo <- as.integer(mm[3])
    if (mo > 1) return(sprintf("%04d-%02d", y, mo - 1L))
    return(sprintf("%04d-12", y - 1L))
  }
  
  NULL
}

