# scripts/context_extract_pdf.R
library(pdftools)
library(stringr)

pdf_to_chunks <- function(pdf_path, chunk_chars = 2000) {
  txt <- pdftools::pdf_text(pdf_path)
  txt <- paste(txt, collapse = "\n")
  txt <- str_replace_all(txt, "[ \t]+", " ")
  txt <- str_replace_all(txt, "\n{2,}", "\n")
  
  # split into approx chunk_chars blocks
  n <- nchar(txt)
  starts <- seq(1, n, by = chunk_chars)
  chunks <- vapply(starts, function(s) {
    substr(txt, s, min(n, s + chunk_chars - 1))
  }, character(1))
  
  chunks
}

retrieve_chunks_keyword <- function(chunks, query, k = 6) {
  terms <- tolower(unlist(strsplit(query, "\\W+")))
  terms <- terms[nchar(terms) > 2]
  if (length(terms) == 0) return(character())
  
  scores <- vapply(chunks, function(ch) {
    ch_l <- tolower(ch)
    sum(vapply(terms, function(t) str_count(ch_l, fixed(t)), integer(1)))
  }, integer(1))
  
  ord <- order(scores, decreasing = TRUE)
  top <- ord[seq_len(min(k, length(ord)))]
  chunks[top][scores[top] > 0]
}
