# scripts/_run_prompt_temp.R
# -----------------------------------------
# Temporary runner for AI prompt execution
# Safe to edit, rerun, or delete
# -----------------------------------------

# Load ellmer
library(ellmer)

# Load all tool functions
tool_files <- sort(list.files("tools", pattern = "\\.R$", full.names = TRUE))

invisible(lapply(tool_files, source))

response <- generate_from_prompt(
  "prompts/00_explorer_mvp.md",
  pdf_paths = "misc/restatapi.pdf",
  retrieval_query = "get_eurostat_toc search_eurostat_toc get_eurostat_dsd search_eurostat_dsd DT shiny datatable",
  k = 4,
  dry_run = FALSE
)




