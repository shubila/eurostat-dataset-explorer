# tools/context_extract_docs.R

# install.packages(c("pdftools","stringr","xml2","rvest")) if needed
# (stringr already used in your code)

doc_is_url <- function(x) {
  grepl("^https?://", x, ignore.case = TRUE)
}

html_to_text <- function(path_or_url) {
  if (!requireNamespace("xml2", quietly = TRUE) ||
      !requireNamespace("rvest", quietly = TRUE)) {
    stop("Packages 'xml2' and 'rvest' are required for HTML parsing.")
  }
  
  doc <- if (doc_is_url(path_or_url)) {
    xml2::read_html(path_or_url)
  } else {
    xml2::read_html(path_or_url)
  }
  
  # rvest::html_text2 gives nicer spacing than html_text
  rvest::html_text2(doc)
}

pdf_to_text <- function(pdf_path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required for PDF parsing.")
  }
  txt <- pdftools::pdf_text(pdf_path)
  paste(txt, collapse = "\n")
}

doc_to_text <- function(path_or_url) {
  ext <- tolower(tools::file_ext(path_or_url))
  
  if (doc_is_url(path_or_url)) {
    # If it's a URL, treat as HTML by default unless it ends with .pdf
    if (grepl("\\.pdf($|\\?)", path_or_url, ignore.case = TRUE)) {
      stop("PDF URLs are not supported directly here. Download first to a local file.")
    }
    return(html_to_text(path_or_url))
  }
  
  if (!file.exists(path_or_url)) {
    stop("Document not found: ", path_or_url)
  }
  
  if (ext %in% c("pdf")) return(pdf_to_text(path_or_url))
  if (ext %in% c("html", "htm")) return(html_to_text(path_or_url))
  
  stop("Unsupported document type: .", ext, " (supported: pdf, html, htm)")
}

doc_to_chunks <- function(path_or_url, chunk_chars = 2000) {
  txt <- doc_to_text(path_or_url)
  
  # Normalize spacing
  txt <- stringr::str_replace_all(txt, "[ \t]+", " ")
  txt <- stringr::str_replace_all(txt, "\n{2,}", "\n")
  txt <- trimws(txt)
  
  if (!nzchar(txt)) return(character(0))
  
  n <- nchar(txt)
  starts <- seq(1, n, by = chunk_chars)
  vapply(starts, function(s) {
    substr(txt, s, min(n, s + chunk_chars - 1))
  }, character(1))
}
