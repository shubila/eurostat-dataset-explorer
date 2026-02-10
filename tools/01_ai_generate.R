# tools/ai_generate.R
# ------------------------------------------------------------
# AI prompt runner with:
# - include_paths: inject current project files (e.g. app.R) verbatim
# - doc_paths: add grounded context from PDF/HTML (local or URL)
# - retrieval_query + k: select most relevant chunks ("RAG-lite")
# - verbose logging + dry_run
# ------------------------------------------------------------

# install.packages(c("ellmer","pdftools","stringr","xml2","rvest")) if needed

# ---- helpers ------------------------------------------------

doc_is_url <- function(x) grepl("^https?://", x, ignore.case = TRUE)

pdf_to_text <- function(pdf_path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required for PDF parsing.")
  }
  paste(pdftools::pdf_text(pdf_path), collapse = "\n")
}

html_to_text <- function(path_or_url) {
  if (!requireNamespace("xml2", quietly = TRUE) ||
      !requireNamespace("rvest", quietly = TRUE)) {
    stop("Packages 'xml2' and 'rvest' are required for HTML parsing.")
  }
  doc <- xml2::read_html(path_or_url)
  rvest::html_text2(doc)
}

doc_to_text <- function(path_or_url) {
  ext <- tolower(tools::file_ext(path_or_url))
  
  if (doc_is_url(path_or_url)) {
    # Treat URLs as HTML by default (download PDF URLs manually first)
    if (grepl("\\.pdf($|\\?)", path_or_url, ignore.case = TRUE)) {
      stop("PDF URLs not supported directly. Download first to a local file.")
    }
    return(html_to_text(path_or_url))
  }
  
  if (!file.exists(path_or_url)) stop("Document not found: ", path_or_url)
  
  if (ext == "pdf") return(pdf_to_text(path_or_url))
  if (ext %in% c("html", "htm")) return(html_to_text(path_or_url))
  
  stop("Unsupported document type: .", ext, " (supported: pdf, html, htm)")
}

doc_to_chunks <- function(path_or_url, chunk_chars = 2000) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required for chunking/cleanup.")
  }
  
  txt <- doc_to_text(path_or_url)
  
  # normalize spacing
  txt <- stringr::str_replace_all(txt, "[ \t]+", " ")
  txt <- stringr::str_replace_all(txt, "\n{2,}", "\n")
  txt <- trimws(txt)
  
  if (!nzchar(txt)) return(character(0))
  
  n <- nchar(txt)
  starts <- seq(1, n, by = chunk_chars)
  vapply(starts, function(s) substr(txt, s, min(n, s + chunk_chars - 1)), character(1))
}

retrieve_chunks_keyword <- function(chunks, query, k = 6) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required for retrieval scoring.")
  }
  
  terms <- tolower(unlist(strsplit(query, "\\W+")))
  terms <- terms[nchar(terms) > 2]
  if (length(terms) == 0 || length(chunks) == 0) return(character())
  
  scores <- vapply(chunks, function(ch) {
    ch_l <- tolower(ch)
    sum(vapply(terms, function(t) stringr::str_count(ch_l, stringr::fixed(t)), integer(1)))
  }, integer(1))
  
  ord <- order(scores, decreasing = TRUE)
  top_idx <- ord[seq_len(min(k, length(ord)))]
  chunks[top_idx][scores[top_idx] > 0]
}

# ---- main function ------------------------------------------

generate_from_prompt <- function(prompt_path,
                                 model = "gpt-4.1-mini",
                                 include_paths = NULL,
                                 doc_paths = NULL,
                                 retrieval_query = NULL,
                                 k = 6,
                                 dry_run = FALSE,
                                 verbose = TRUE) {
  
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required.")
  }
  if (!file.exists(prompt_path)) stop("Prompt file does not exist: ", prompt_path)
  
  # 1) Task prompt (markdown spec)
  task_prompt <- paste(readLines(prompt_path, warn = FALSE), collapse = "\n")
  
  # 2) Include current files verbatim (e.g. app.R)
  included <- ""
  if (!is.null(include_paths)) {
    if (is.character(include_paths)) include_paths <- as.list(include_paths)
    include_paths <- unlist(include_paths)
    
    for (p in include_paths) {
      if (!file.exists(p)) stop("Include file does not exist: ", p)
      code <- paste(readLines(p, warn = FALSE), collapse = "\n")
      included <- paste0(
        included,
        "\n\n---\n\n# CURRENT FILE: ", p, "\n```r\n", code, "\n```"
      )
      if (verbose) cat(sprintf("[include] added %s (%d chars)\n", p, nchar(code)))
    }
  }
  
  # 3) Grounded context from docs (PDF/HTML)
  context <- ""
  if (!is.null(doc_paths)) {
    if (is.character(doc_paths)) doc_paths <- as.list(doc_paths)
    doc_paths <- unlist(doc_paths)
    
    if (is.null(retrieval_query)) retrieval_query <- task_prompt
    
    for (d in doc_paths) {
      chunks <- doc_to_chunks(d, chunk_chars = 2000)
      top <- retrieve_chunks_keyword(chunks, retrieval_query, k = k)
      
      if (verbose) {
        cat(sprintf("[context] %s: %d chunks → %d selected\n",
                    basename(d), length(chunks), length(top)))
      }
      
      # Optional: fallback so context is never empty (uncomment if you want)
      # if (length(top) == 0 && length(chunks) > 0) {
      #   top <- chunks[1]
      #   if (verbose) cat("[context] fallback: using first chunk\n")
      # }
      
      if (length(top) > 0) {
        context <- paste0(
          context,
          "\n\n### GROUNDED CONTEXT FROM: ", basename(d), "\n",
          paste0("---- CHUNK ----\n", top, collapse = "\n\n")
        )
      }
    }
  }
  
  # 4) Assemble final prompt (order: docs → current code → task)
  full_prompt <- paste0(
    context,
    "\n\n",
    included,
    "\n\n### TASK PROMPT\n",
    task_prompt
  )
  
  if (dry_run) {
    cat("\n----- FULL PROMPT (DRY RUN) -----\n\n")
    cat(full_prompt)
    cat("\n\n----- END DRY RUN -----\n")
    return(invisible(full_prompt))
  }
  
  # 5) Call model
  chat <- ellmer::chat_openai(model = model)
  response <- chat$chat(full_prompt)
  
  cat("\n----- AI OUTPUT BEGIN -----\n\n", response, "\n\n----- AI OUTPUT END -----\n")
  invisible(response)
}
