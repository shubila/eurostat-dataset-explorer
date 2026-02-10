# at top
# install.packages(c("pdftools","stringr")) if needed


generate_from_prompt <- function(prompt_path,
                                 model = "gpt-4.1-mini",
                                 pdf_paths = NULL,
                                 retrieval_query = NULL,
                                 k = 6,
                                 dry_run = FALSE,
                                 verbose = TRUE) {
  
  prompt <- paste(readLines(prompt_path, warn = FALSE), collapse = "\n")
  
  context <- ""
  if (!is.null(pdf_paths)) {
    if (is.null(retrieval_query)) retrieval_query <- prompt
    
    for (pdf in pdf_paths) {
      chunks <- pdf_to_chunks(pdf, chunk_chars = 2000)
      top <- retrieve_chunks_keyword(chunks, retrieval_query, k = k)
      
      if (verbose) {
        cat(sprintf(
          "[context] %s: %d chunks → %d selected\n",
          basename(pdf),
          length(chunks),
          length(top)
        ))
      }
      
      if (length(top) > 0) {
        context <- paste0(
          context,
          "\n\n### GROUNDED CONTEXT FROM: ", basename(pdf), "\n",
          paste0("---- CHUNK ----\n", top, collapse = "\n\n")
        )
      }
    }
  }
  
  full_prompt <- paste0(context, "\n\n### TASK PROMPT\n", prompt)
  
  if (dry_run) {
    cat("\n----- FULL PROMPT (DRY RUN) -----\n\n", full_prompt, "\n")
    return(invisible(full_prompt))
  }
  
  
  chat <- chat_openai(model = model)
  response <- chat$chat(full_prompt)
  
  cat("\n----- AI OUTPUT BEGIN -----\n\n", response, "\n\n----- AI OUTPUT END -----\n")
  invisible(response)
}
