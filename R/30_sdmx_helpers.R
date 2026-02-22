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
# normalize_toc_end_for_freq <- function(toc_end, freq_code) {
#   
#   if (is.null(toc_end) || !nzchar(toc_end)) return(NULL)
#   
#   freq_code <- toupper(freq_code %||% "")
#   year <- sub("^([0-9]{4}).*$", "\\1", toc_end)
#   
#   if (freq_code == "A") return(year)
#   
#   if (freq_code == "Q") {
#     if (grepl("Q[1-4]$", toc_end)) return(toc_end)
#     return(paste0(year, "-Q4"))
#   }
#   
#   if (freq_code == "M") {
#     if (grepl("M[0-9]{2}$", toc_end) || grepl("-[0-9]{2}$", toc_end))
#       return(toc_end)
#     return(paste0(year, "M12"))
#   }
#   
#   toc_end
# }

normalize_toc_end_for_freq <- function(toc_end, freq_code) {
  if (is.null(toc_end) || !nzchar(toc_end)) return(NULL)
  
  freq_code <- toupper(freq_code %||% "")
  year <- sub("^([0-9]{4}).*$", "\\1", toc_end)
  
  # A: return year only
  if (freq_code == "A") return(year)
  
  # Q: prefer explicit quarter; otherwise assume Q4
  if (freq_code == "Q") {
    # accept YYYY-Qn or YYYYnQn variants
    if (grepl("^(\\d{4})-?Q[1-4]$", toc_end)) return(gsub("^(\\d{4})Q", "\\1-Q", toc_end))
    return(paste0(year, "-Q4"))
  }
  
  # M: prefer explicit month; otherwise assume December
  if (freq_code == "M") {
    # accept YYYY-MM or YYYYMmm or YYYYmMM
    if (grepl("^\\d{4}-\\d{2}$", toc_end)) return(toc_end)
    if (grepl("^\\d{4}[-]?M\\d{2}$", toc_end, ignore.case = TRUE)) {
      # normalize to YYYY-MM
      y <- sub("^([0-9]{4}).*$", "\\1", toc_end)
      m <- sub("^\\d{4}[-]?M(\\d{2}).*$", "\\1", toc_end, ignore.case = TRUE)
      return(paste0(y, "-", m))
    }
    return(paste0(year, "-12"))
  }
  
  # S (semiannual): prefer explicit half; otherwise assume second half
  # Accept YYYY-S1 / YYYY-S2 or YYYY-H1 / YYYY-H2 (normalize to YYYY-Sn)
  if (freq_code == "S") {
    if (grepl("^\\d{4}-[SH][12]$", toc_end, ignore.case = TRUE)) {
      y <- sub("^([0-9]{4}).*$", "\\1", toc_end)
      h <- sub("^\\d{4}-[SH]([12])$", "\\1", toc_end, ignore.case = TRUE)
      return(paste0(y, "-S", h))
    }
    if (grepl("^\\d{4}[SH][12]$", toc_end, ignore.case = TRUE)) {
      y <- sub("^([0-9]{4}).*$", "\\1", toc_end)
      h <- sub("^\\d{4}[SH]([12])$", "\\1", toc_end, ignore.case = TRUE)
      return(paste0(y, "-S", h))
    }
    return(paste0(year, "-S2"))
  }
  
  # W (weekly): prefer explicit week; otherwise assume W52
  # Accept YYYY-Www or YYYYWww (normalize to YYYY-Www)
  if (freq_code == "W") {
    m <- regexec("^(\\d{4})-?W(\\d{2})$", toc_end, perl = TRUE, ignore.case = TRUE)
    mm <- regmatches(toc_end, m)[[1]]
    if (length(mm) == 3) return(paste0(mm[2], "-W", mm[3]))
    return(paste0(year, "-W52"))
  }
  
  # D (daily): prefer explicit date; otherwise assume Dec 31
  if (freq_code == "D") {
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", toc_end)) return(toc_end)
    return(paste0(year, "-12-31"))
  }
  
  # default: return as-is
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
  
  # Q: YYYY-Qn or YYYYnQn
  if (freq_code == "Q") {
    m <- regexec("^(\\d{4})-?Q([1-4])$", period, perl = TRUE)
    mm <- regmatches(period, m)[[1]]
    if (length(mm) != 3) return(NULL)
    y <- as.integer(mm[2]); q <- as.integer(mm[3])
    if (q > 1) return(paste0(y, "-Q", q - 1L))
    return(paste0(y - 1L, "-Q4"))
  }
  
  # M: YYYY-MM (also accept YYYYMmm / YYYYmMM)
  if (freq_code == "M") {
    # normalize to YYYY-MM first
    if (grepl("^\\d{4}[-]?M\\d{2}$", period, ignore.case = TRUE)) {
      y <- sub("^([0-9]{4}).*$", "\\1", period)
      m2 <- sub("^\\d{4}[-]?M(\\d{2}).*$", "\\1", period, ignore.case = TRUE)
      period <- paste0(y, "-", m2)
    }
    m <- regexec("^(\\d{4})-(\\d{2})$", period, perl = TRUE)
    mm <- regmatches(period, m)[[1]]
    if (length(mm) != 3) return(NULL)
    y <- as.integer(mm[2]); mo <- as.integer(mm[3])
    if (mo > 1) return(sprintf("%04d-%02d", y, mo - 1L))
    return(sprintf("%04d-12", y - 1L))
  }
  
  # S: YYYY-S1/S2 (accept H too; normalize to S)
  if (freq_code == "S") {
    m <- regexec("^(\\d{4})-?[SH]([12])$", period, perl = TRUE, ignore.case = TRUE)
    mm <- regmatches(period, m)[[1]]
    if (length(mm) != 3) return(NULL)
    y <- as.integer(mm[2]); h <- as.integer(mm[3])
    if (h == 2) return(paste0(y, "-S1"))
    return(paste0(y - 1L, "-S2"))
  }
  
  # W: YYYY-Www or YYYYWww
  if (freq_code == "W") {
    m <- regexec("^(\\d{4})-?W(\\d{2})$", period, perl = TRUE, ignore.case = TRUE)
    mm <- regmatches(period, m)[[1]]
    if (length(mm) != 3) return(NULL)
    y <- as.integer(mm[2]); w <- as.integer(mm[3])
    if (w > 1) return(sprintf("%04d-W%02d", y, w - 1L))
    return(sprintf("%04d-W52", y - 1L))  # pragmatic (ISO can be 53 sometimes)
  }
  
  # D: YYYY-MM-DD
  if (freq_code == "D") {
    d <- suppressWarnings(as.Date(period))
    if (is.na(d)) return(NULL)
    return(format(d - 1, "%Y-%m-%d"))
  }
  
  NULL
}
