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

# response <- generate_from_prompt(
#   "prompts/00_explorer_mvp.md",
#   pdf_paths = "misc/restatapi.pdf",
#   retrieval_query = "get_eurostat_toc search_eurostat_toc get_eurostat_dsd search_eurostat_dsd DT shiny datatable",
#   k = 4,
#   dry_run = FALSE
# )


# response_qb <- generate_from_prompt(
#   prompt_path = "prompts/01_query_builder.md",
#   include_paths = "app.R",
#   model = "gpt-4.1-mini",
#   pdf_paths = "misc/data_query.pdf",
#   retrieval_query = "SDMX 2.1 data query series key dimension order filter CSV Eurostat dissemination api",
#   k = 6,
#   verbose = TRUE,
#   dry_run = TRUE   # <-- först alltid
# )

# response_qb  <- generate_from_prompt(
#   prompt_path = "prompts/01_query_builder.md",
#   include_paths = "app.R",
#   pdf_paths = "misc/data_query.pdf",
#   retrieval_query = "SDMX 2.1 data query series key dimension order filter CSV Eurostat dissemination api",
#   k = 6,
#   dry_run = FALSE,
#   verbose = TRUE
# )


# source("tools/ai_generate.R")

resp <- generate_from_prompt(
  prompt_path   = "prompts/01_query_builder.md",
  include_paths = "misc/app_bk.R",
  doc_paths     = "misc/api_dataquery.html",   # or a local html / URL
  retrieval_query = "sdmx 2.1 data url query key dimension order format csv time filter",
  k = 6,
  dry_run = FALSE,
  verbose = TRUE
)



