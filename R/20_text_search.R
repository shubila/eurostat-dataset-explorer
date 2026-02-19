# ============================================================
# 20_text_search.R
# AND / OR grouped search logic for TOC
# ============================================================

match_query_groups <- function(text, query, ignore.case = TRUE) {
  
  query <- trimws(query)
  if (!nzchar(query)) return(rep(FALSE, length(text)))
  
  query <- gsub("\\s*\\|\\s*", "|", query)
  or_groups <- strsplit(query, "\\|")[[1]]
  or_groups <- trimws(or_groups)
  or_groups <- or_groups[nzchar(or_groups)]
  
  if (length(or_groups) == 0) return(rep(FALSE, length(text)))
  
  group_hits <- lapply(or_groups, function(g) {
    terms <- strsplit(g, "\\s+")[[1]]
    terms <- terms[nzchar(terms)]
    
    Reduce(`&`, lapply(terms, function(t) {
      grepl(t, text, ignore.case = ignore.case)
    }))
  })
  
  Reduce(`|`, group_hits)
}
