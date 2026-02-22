
# ============================================================
# 1) Packages + global options
#    Purpose:
#    - Load required packages (fail fast if missing)
#    - Set global runtime options that affect HTTP calls / caching
#    - Keep everything here "non-reactive" (no input/output)
# ============================================================

# ---- Packages ----
# Core Shiny framework + widgets/tables
library(shiny)
library(DT)

# Eurostat SDMX client (REST + cache handling)
library(restatapi)

# ---- Global options ----
# Increase timeout for Eurostat API calls (TOC/DSD/data can be slow).
# This is a global R option affecting download.file(), httr, etc.
options(timeout = 120)

# restatapi specific:
# - Keep cores = 1 to avoid parallel issues on some systems/servers.
# - Disable auto-cache updates to prevent long startup delays.
options(restatapi_cores = 1)
options(restatapi_update = FALSE)

# (Optional) Recommended for reproducible printing / less noise
# options(stringsAsFactors = FALSE)  # not needed in modern R, but harmless
# options(scipen = 999)              # avoid scientific notation in prints/tables

# ---- Optional maintenance utilities (manual use) ----
# Clear restatapi cache if you suspect stale metadata (run manually, not in app)
# clean_restatapi_cache(verbose = TRUE)

# Load restatapi config explicitly (rarely needed; only if you want to override defaults)
# load_cfg(parallel = FALSE, verbose = TRUE)

# ---- App rule: time handling ----
# IMPORTANT:
# Time filtering is applied ONLY via startPeriod/endPeriod derived from the TOC (dataStart/dataEnd).
# We do NOT infer latest periods from DSD time codes. If a time dimension exists in DSD, it is treated
# like any other dimension filter (part of the SDMX key), but start/endPeriod always come from TOC.


# ---- Load helpers ----
# We load helper files in lexical order (00_, 10_, 20_...) to ensure dependencies
# are defined before use.
# ---- Load helpers ----
helper_files <- sort(list.files("R", pattern = "\\.R$", full.names = TRUE))

for (f in helper_files) {
  tryCatch(
    source(f, local = FALSE),
    error = function(e) stop("Failed to source helper file: ", f, "\n", conditionMessage(e))
  )
}


# ============================================================
# 3) UI
#    Purpose:
#    - Define layout and inputs/outputs only
#    - No business logic here
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$script(src = "clipboard.js")
  ),
  
  titlePanel("Eurostat Dataset Explorer (restatapi) – v1.0"),
  
  sidebarLayout(
    
    # ----------------------------
    # Sidebar: controls / actions
    # ----------------------------
    sidebarPanel(
      
      # --- Dataset search ---
      h4("Dataset search"),
      textInput("search", "Search datasets by keyword:", value = ""),
      
      fluidRow(
        column(6, actionButton("btn_search", "Search", width = "100%")),
        column(6, actionButton("btn_clear_all", "Reset", width = "100%", class = "btn-warning"))
      ),
      
      tags$div(
        style = "margin-top: 6px; color: #666; font-size: 0.85em;",
        "Search tips: ",
        tags$code("space = AND"), ", ",
        tags$code("| = OR"),
        tags$br(),
        tags$span("Example: "),
        tags$code("bop financial|iip")
      ),
      
      hr(),
      
      # --- Dataset selection status (rendered from server) ---
      uiOutput("selected_dataset_ui"),
      actionButton("btn_load_dsd", "Load Structure"),
      
      hr(),
      
      # --- Filters ---
      h4("Filters"),
      uiOutput("filters_ui"),
      
      hr(),
      
      # --- Query builder ---
      h4("SDMX 2.1 Query Builder"),
      actionButton("btn_generate_query", "Generate query"),
      uiOutput("query_builder_ui"),
      
      # --- Extraction estimate  ---
      textInput("txt_final_url", "Final SDMX Data URL:", "", width = "100%"),
      # --- Extraction estimate  ---
      h4("Estimate"),
      verbatimTextOutput("txt_estimate"),
      
      actionButton("btn_copy_query", "Copy query to clipboard"),
      
      hr(),
      hr(),
      # --- Labels section ---
      h4("Labels helper (R)"),
      helpText("Generate R code you can paste into another project (no local files needed)."),
      selectInput("lbl_lang", "Label language:", choices = c("en","fr","de"), selected = "en"),
      fluidRow(
        column(6, actionButton("btn_lbl_generate", "Generate", width = "100%")),
        column(6, actionButton("btn_lbl_copy", "Copy", width = "100%"))
      ),
      textAreaInput("txt_lbl_code", NULL, value = "Click Generate to create a ready-to-paste helper…",
                    width = "100%", height = "220px"),
      
      # --- Data actions ---
      h4("Data"),
      actionButton("btn_preview_data", "Preview (first rows)"),
      downloadButton("btn_download_data", "Download SDMX-CSV")
    ),
    
    
    # ----------------------------
    # Main panel: results / tables
    # ----------------------------
    mainPanel(
      
      # --- Search results ---
      h4("Search Results"),
      DTOutput("tbl_toc"),
      
      hr(),
      
      # --- DSD exploration ---
      h4("Dataset Structure: Dimensions"),
      uiOutput("dims_overview_ui"),
      uiOutput("dim_selection_ui"),
      DTOutput("tbl_codes"),
      uiOutput("message_ui"),

      hr(),
      
      # --- Preview area ---
      uiOutput("preview_ui"),
      verbatimTextOutput("txt_data_status")
    )
  )
)
# ============================================================
# 4) Server
#    Purpose:
#    - Reactive logic
#    - Data flow management
#    - API interaction
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------
  # 4.1 Reactive state container
  # ----------------------------------------
  
  rv <- reactiveValues(
    toc = NULL,              # Eurostat TOC
    dataset_id = NULL,       # Selected dataset
    toc_start = NULL,        # dataStart from TOC
    toc_end = NULL,          # dataEnd from TOC
    dsd = NULL,              # Dataset structure
    dim_filters = list(),    # Dimension filters
    base_url = NULL,
    final_url = NULL,
    
  )
  
  search_df <- reactiveVal(data.frame())
  preview_df <- reactiveVal(NULL)
  
  
  
  
  # ----------------------------------------
  # 4.2 Load TOC (once per session)
  # ----------------------------------------
  
  # observe({
  #   if (!is.null(rv$toc)) return()
  #   
  #   withProgress(message = "Loading Eurostat catalogue (TOC)…", value = 0, {
  #     incProgress(0.3, detail = "Reading cache / downloading…")
  #     df <- load_toc_once()
  #     incProgress(0.9, detail = "Finalizing…")
  #     rv$toc <- df
  #     incProgress(1)
  #   })
  # })
  observe({
    if (!is.null(rv$toc)) return()
    
    withProgress(message = "Loading Eurostat catalogue (TOC)…", value = 0, {
      
      incProgress(0.3, detail = "Reading cache / downloading…")
      df <- load_toc_once()
      
      incProgress(0.6, detail = "Filtering invalid TOC entries…")
      
      # Remove datasets with missing or empty dataEnd
      df <- df[
        !is.na(df$dataEnd) &
          nzchar(trimws(df$dataEnd)),
      ]
      
      incProgress(0.9, detail = "Finalizing…")
      rv$toc <- df
      
      incProgress(1)
    })
  })
  
  
  # ----------------------------------------
  # 4.3 Search logic
  # ----------------------------------------
  
  observeEvent(input$btn_search, {
    req(rv$toc)
    
    pattern <- trimws(input$search)
    if (!nzchar(pattern)) {
      search_df(data.frame())
      return()
    }
    
    haystack <- paste(rv$toc$code, rv$toc$title, sep = " | ")
    hits <- match_query_groups(haystack, pattern, ignore.case = TRUE)
    
    search_df(rv$toc[hits, , drop = FALSE])
  })
  
  
  
  
  # ----------------------------------------
  # 4.4 Render search results
  # ----------------------------------------
  
  output$tbl_toc <- renderDT({
    df <- search_df()
    
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        data.frame(Message = "No search results"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    
    datatable(
      df[, c("title", "code", "dataStart", "dataEnd")],
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE),
      colnames = c("Title", "Code", "Start", "End")
    )
  })
  
  
  
  # ----------------------------------------
  # 4.5 Dataset selection
  # ----------------------------------------
  
  observeEvent(input$tbl_toc_rows_selected, {
    
    rows <- input$tbl_toc_rows_selected
    df <- search_df()
    
    if (length(rows) == 1 && nrow(df) >= rows) {
      
      rv$dataset_id <- df$code[rows]
      rv$toc_start  <- df$dataStart[rows]
      rv$toc_end    <- df$dataEnd[rows]
      
      # Reset structure & query state
      rv$dsd <- NULL
      rv$dim_filters <- list()
      rv$base_url <- NULL
      rv$final_url <- NULL
      
      
    } else {
      
      rv$dataset_id <- NULL
      rv$toc_start  <- NULL
      rv$toc_end    <- NULL
      rv$dsd <- NULL
      rv$dim_filters <- list()
      rv$base_url <- NULL
      rv$final_url <- NULL
      
    }
  })
  
  
  # ----------------------------------------
  # 4.6 Selected dataset UI
  # ----------------------------------------
  
  output$selected_dataset_ui <- renderUI({
    
    if (is.null(rv$dataset_id)) {
      span(style = "color:red;", "No dataset selected")
    } else {
      tagList(
        strong("Selected dataset ID: "),
        code(rv$dataset_id),
        tags$br(),
        tags$small(paste("TOC period:", rv$toc_start, "→", rv$toc_end))
      )
    }
  })
  
  # ----------------------------------------
  # 4.7 Reset logic
  # ----------------------------------------
  
  observeEvent(input$btn_clear_all, {
    
    # 1) Reset state
    rv$dataset_id <- NULL
    rv$toc_start  <- NULL
    rv$toc_end    <- NULL
    rv$dsd        <- NULL
    rv$dim_filters <- list()
    rv$base_url   <- NULL
    rv$final_url  <- NULL
    
    
    # 2) Reset UI inputs
    updateTextInput(session, "search", value = "")
    updateTextInput(session, "txt_final_url", value = "")
    updateTextAreaInput(session, "txt_lbl_code", value = "")
    
    
    
    # 3) Clear DT selection + DT internal search + table contents
    proxy <- DT::dataTableProxy("tbl_toc")
    DT::selectRows(proxy, NULL)
    DT::clearSearch(proxy)
    DT::replaceData(proxy, data.frame(), resetPaging = TRUE)
    search_df(data.frame())
    
    # 4) Clear preview
    preview_df(NULL)
    output$txt_data_status <- renderText("")
    
    showNotification("Reset: ready for a new search.", type = "message")
  })
  
  # ----------------------------------------
  # 4.8 Load DSD (Structure)
  # ----------------------------------------
  
  observeEvent(input$btn_load_dsd, {
    
    req(rv$dataset_id)
    
    withProgress(message = "Loading dataset structure (DSD)…", value = 0, {
      
      incProgress(0.4, detail = "Requesting DSD…")
      
      dsd_dt <- tryCatch(
        get_eurostat_dsd(rv$dataset_id, lang = "en"),
        error = function(e) {
          showNotification(
            paste("Failed to load DSD:", e$message),
            type = "error"
          )
          return(NULL)
        }
      )
      
      if (is.null(dsd_dt) || nrow(dsd_dt) == 0) {
        showNotification("DSD returned empty result.", type = "warning")
        return()
      }
      
      incProgress(0.8, detail = "Storing structure…")
      
      rv$dsd <- dsd_dt
      
      incProgress(1)
    })
  })
  
  
  
  # ----------------------------------------
  # 4.9 DSD dimensions overview container (View)
  # ----------------------------------------
  # Purpose:
  # - Show a compact "metadata box" with DSD dimension order + size.
  # - This block only defines the layout and places DTOutput().
  # - The table itself is rendered in 4.10.
  
  output$dims_overview_ui <- renderUI({
    req(rv$dsd)

    tags$div(
      style = "margin-top: 10px;",
      tags$strong("Available dimensions (DSD order):"),
      tags$div(
        style = "text-align: left;",
        tags$div(
          style = paste(
            "margin-top: 8px;",
            "margin-bottom: 18px;",
            "display: inline-block;",
            "padding: 10px 12px;",
            "border: 1px solid #e6e6e6;",
            "border-radius: 6px;",
            "background: #fafafa;"
          ),
          DT::DTOutput("tbl_dims_overview")   # <-- HÄR
        )
      )
    )

  })

  
  
  # ----------------------------------------
  # 4.10 DSD dimensions overview table (View)
  # ----------------------------------------
  # Purpose:
  # - Render the overview DT with:
  #   - DSD order (Position)
  #   - Dimension name
  #   - Number of categories (codes) per dimension
  # - This is a read-only metadata table (no paging/search/sorting).
  
  output$tbl_dims_overview <- DT::renderDT({
    req(rv$dsd)
    
    dims <- unique(rv$dsd$concept)
    
    dim_df <- data.frame(
      Position   = as.character(seq_along(dims)),
      Dimension  = dims,
      Categories = as.character(sapply(dims, function(d) sum(rv$dsd$concept == d))),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(
      dim_df,
      class = "compact",
      selection = "single",
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "45px", targets = 0),
          list(width = "160px", targets = 1),
          list(width = "70px", targets = 2),
          list(className = "dt-left", targets = "_all")
        )
      ),
      rownames = FALSE
    )
  }, server = TRUE)
  
  # ----------------------------------------
  # 4.11 Overview click -> select dimension (Controller)
  # ----------------------------------------
  # Purpose:
  # - When the user clicks a row in the overview table,
  #   update the dimension dropdown ("sel_dim") to match.
  # - This couples the overview and the code list for smoother UX.
  
  
  observeEvent(input$tbl_dims_overview_rows_selected, {
    req(rv$dsd)
    
    i <- input$tbl_dims_overview_rows_selected
    if (length(i) != 1) return()
    
    dims <- unique(rv$dsd$concept)
    selected_dim <- dims[i]
    
    updateSelectInput(session, "sel_dim", selected = selected_dim)
  })
  
  
  # ----------------------------------------
  # 4.12 Dimension selector (View)
  # ----------------------------------------
  # Purpose:
  # - Provide a dropdown for choosing a dimension (DSD order).
  # - Selection drives the code table in 4.13.
  # - No state changes here; pure UI.
  
  output$dim_selection_ui <- renderUI({
    req(rv$dsd)
    
    dims <- unique(rv$dsd$concept)
    
    selectInput(
      inputId = "sel_dim",
      label   = "Select dimension:",
      choices = dims,
      selected = NULL
    )
  })
  
  # ----------------------------------------
  # 4.13 Codes for selected dimension (View)
  # ----------------------------------------
  # Purpose:
  # - Display code + label list for the selected dimension.
  # - Useful for understanding what values can be used in filters / SDMX keys.
  
  output$tbl_codes <- renderDT({
    req(rv$dsd)
    req(input$sel_dim)
    
    dim_table <- rv$dsd[rv$dsd$concept == input$sel_dim, c("code", "name")]
    dim_table <- dim_table[order(dim_table$code), , drop = FALSE]
    
    if (nrow(dim_table) == 0) {
      return(datatable(
        data.frame(Message = "No codes found for selected dimension"),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    
    datatable(
      dim_table,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      colnames = c("Code", "Label")
    )
  }, server = TRUE)
  
  # ----------------------------------------
  # 4.14 Dimension filters UI (View)
  # ----------------------------------------
  # Purpose:
  # - Render one filter input per DSD dimension, in DSD order.
  # - Empty selection means: "all values" for that dimension.
  # - freq is required (single-select). geo is required (>=1) if present.
  output$filters_ui <- renderUI({
    req(rv$dsd)
    
    dims <- unique(rv$dsd$concept)  # keep DSD order
    
    tagList(
      tags$div(
        style = "margin-bottom:8px; color:#666; font-size:0.85em;",
        "Leave empty = all values"
      ),
      
      lapply(dims, function(d) {
        choices <- unique(rv$dsd$code[rv$dsd$concept == d])
        
        # Default: freq picks Q if present, else first available
        default_sel <- if (identical(d, "freq")) {
          if ("Q" %in% choices) "Q" else choices[1]
        } else {
          NULL
        }
        
        label_txt <- if (identical(d, "freq")) {
          "freq (required: choose 1)"
        } else if (identical(d, "geo")) {
          "geo (required: choose ≥1)"
        } else {
          paste0("Filter: ", d)
        }
        
        is_single <- identical(d, "freq")
        
        selectizeInput(
          inputId  = paste0("flt_", d),
          label    = label_txt,
          choices  = choices,
          selected = default_sel,
          multiple = !is_single,
          options  = list(
            plugins  = if (is_single) NULL else list("remove_button"),
            maxItems = if (is_single) 1 else NULL
          )
        )
      })
    )
  })
  
  # ----------------------------------------
  # 4.15 Collect selected filters (Controller)
  # ----------------------------------------
  # Purpose:
  # - Read all flt_* inputs and store them as a named list (DSD order).
  observe({
    req(rv$dsd)
    
    dims <- unique(rv$dsd$concept)
    
    filters <- setNames(vector("list", length(dims)), dims)
    for (d in dims) {
      x <- input[[paste0("flt_", d)]]
      filters[[d]] <- if (!is.null(x) && length(x) > 0) as.character(x) else character(0)
    }
    
    rv$dim_filters <- filters
  })
  
  # ----------------------------------------
  # 4.16 Estimate max size (no API calls)
  # ----------------------------------------
  # Purpose:
  # - Upper bound estimate: product of selected counts per dimension
  # - Useful guardrail for preview/download.
  max_rows_est <- reactive({
    req(rv$dsd)
    
    dims <- unique(rv$dsd$concept)
    
    n_used <- vapply(dims, function(d) {
      sel <- rv$dim_filters[[d]]
      if (!is.null(sel) && length(sel) > 0) length(sel) else sum(rv$dsd$concept == d)
    }, numeric(1))
    
    prod(n_used)
  })
  
  output$txt_estimate <- renderText({
    req(rv$dsd)
    
    dims <- unique(rv$dsd$concept)
    n_used <- vapply(dims, function(d) {
      sel <- rv$dim_filters[[d]]
      if (!is.null(sel) && length(sel) > 0) length(sel) else sum(rv$dsd$concept == d)
    }, numeric(1))
    
    max_rows <- prod(n_used)
    formatted_max <- format(max_rows, scientific = FALSE, big.mark = " ")
    
    breakdown <- paste0(dims, "=", n_used, collapse = " × ")
    
    paste0(
      "Max rows (cartesian upper bound): ", formatted_max, "\n",
      breakdown, "\n",
      "Note: actual rows are usually lower (not all combinations exist)."
    )
  })
  
  # Guard thresholds
  MAX_PREVIEW_ROWS   <- 2000
  MAX_DOWNLOAD_ROWS  <- 1000000
  
  guard_query_size <- function(limit, label = "this action") {
    est <- max_rows_est()
    if (is.null(est) || !is.finite(est)) return(TRUE)
    
    if (est > limit) {
      showNotification(
        paste0(
          "Blocked ", label, ": estimated max rows = ",
          format(est, scientific = FALSE, big.mark = " "),
          " (limit = ", format(limit, scientific = FALSE, big.mark = " "), ").\n",
          "Add filters to reduce the query."
        ),
        type = "error", duration = 10
      )
      return(FALSE)
    }
    TRUE
  }
  
  # ----------------------------------------
  # 4.17 Generate query URL (Controller)
  # ----------------------------------------
  # Purpose:
  # - Build SDMX 2.1 data URL from:
  #   - dataset_id
  #   - SDMX key from filters (DSD order)
  #   - startPeriod/endPeriod: single latest period derived from TOC end + freq
  observeEvent(input$btn_generate_query, {
    req(rv$dataset_id, rv$dsd)
    
    dims <- unique(rv$dsd$concept)
    freq_dim <- get_freq_dim(dims)
    geo_dim  <- get_geo_dim(dims)
    
    filters <- rv$dim_filters
    
    # Require freq if present
    freq_code <- NULL
    if (!is.null(freq_dim)) {
      sel_freq <- filters[[freq_dim]]
      if (is.null(sel_freq) || length(sel_freq) < 1) {
        showNotification("Please choose a value for freq before generating the URL.", type = "warning")
        return()
      }
      if (length(sel_freq) > 1) {
        showNotification("Please choose exactly 1 value for freq.", type = "warning")
        return()
      }
      freq_code <- sel_freq[1]
    }
    
    # Require geo if present
    if (!is.null(geo_dim)) {
      sel_geo <- filters[[geo_dim]]
      if (is.null(sel_geo) || length(sel_geo) < 1) {
        showNotification("Please choose at least 1 geo before generating the URL.", type = "warning")
        return()
      }
    }
    
    # SDMX key segments (DSD order)
    segments <- vapply(dims, function(d) {
      vals <- filters[[d]]
      if (is.null(vals) || length(vals) == 0) "" else paste(vals, collapse = "+")
    }, character(1))
    
    key_string <- paste(segments, collapse = ".")
    
    base_url <- paste0(
      "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
      rv$dataset_id
    )
    
    # Single latest period from TOC end adjusted by freq
    latest_period <- normalize_toc_end_for_freq(rv$toc_end, freq_code)
    
    # Fallback chain (no API calls)
    if (is.null(latest_period) || !nzchar(latest_period)) latest_period <- rv$toc_end
    if (is.null(latest_period) || !nzchar(latest_period)) latest_period <- rv$toc_start
    
    params <- c("format=SDMX-CSV")
    if (!is.null(latest_period) && nzchar(latest_period)) {
      params <- c(params,
                  paste0("startPeriod=", latest_period),
                  paste0("endPeriod=", latest_period))
    }
    
    final_url <- paste0(base_url, "/", key_string, "?", paste(params, collapse = "&"))
    
    rv$base_url  <- base_url
    rv$final_url <- final_url
    updateTextInput(session, "txt_final_url", value = final_url)
    
    showNotification(paste0("Query generated (", latest_period, ")."), type = "message")
  })
  
  # ----------------------------------------
  # 4.18 Copy query URL (Controller)
  # ----------------------------------------
  observeEvent(input$btn_copy_query, {
    url <- isolate(input$txt_final_url)
    if (is.null(url) || !nzchar(url)) {
      showNotification("No URL to copy yet.", type = "warning")
      return()
    }
    
    session$sendCustomMessage("copyToClipboard", list(
      text = url,
      inputId = "txt_final_url"
    ))
    
    showNotification("Copied (or attempted). If it failed: click the URL field and press Ctrl+C.", type = "message")
  })
  
  # ----------------------------------------
  # 4.19 Preview data with fallback (Controller)
  # ----------------------------------------
  observeEvent(input$btn_preview_data, {
    req(rv$final_url)
    
    if (!guard_query_size(MAX_PREVIEW_ROWS, "preview")) return()
    
    output$txt_data_status <- renderText("Fetching preview…")
    
    preview_n  <- 50
    max_tries  <- 10L
    
    sep <- if (grepl("\\?", rv$final_url)) "&" else "?"
    preview_url <- paste0(rv$final_url, sep, "firstNObservations=", preview_n)
    
    # Determine freq_code once for fallback stepping A/Q/M
    freq_code <- NULL
    if (!is.null(rv$dsd) && length(rv$dim_filters) > 0) {
      dims <- unique(rv$dsd$concept)
      freq_dim <- get_freq_dim(dims)
      if (!is.null(freq_dim)) {
        sel <- rv$dim_filters[[freq_dim]]
        if (!is.null(sel) && length(sel) >= 1) freq_code <- sel[1]
      }
    }
    
    tryCatch({
      withProgress(message = "Downloading preview…", value = 0, {
        
        incProgress(0.25, detail = "Requesting SDMX-CSV…")
        lines <- fetch_sdmx_csv(preview_url, max_lines = preview_n + 5)
        
        incProgress(0.55, detail = "Parsing…")
        df <- parse_csv_lines(lines)
        
        # Fallback: if 0 rows, step back in time by freq
        if (nrow(df) == 0) {
          incProgress(0.70, detail = "No rows. Trying fallback periods…")
          
          current_period <- sub(".*[?&]startPeriod=([^&]+).*", "\\1", rv$final_url)
          tried <- 0L
          
          repeat {
            if (nrow(df) > 0 || tried >= max_tries) break
            tried <- tried + 1L
            
            prev <- prev_period(freq_code, current_period)
            if (is.null(prev) || !nzchar(prev)) break
            
            preview_url2 <- sub("startPeriod=[^&]+", paste0("startPeriod=", prev), preview_url)
            preview_url2 <- sub("endPeriod=[^&]+",   paste0("endPeriod=", prev),   preview_url2)
            
            lines2 <- fetch_sdmx_csv(preview_url2, max_lines = preview_n + 5)
            df2 <- parse_csv_lines(lines2)
            
            if (nrow(df2) > 0) {
              df <- df2
              current_period <- prev
              
              new_final <- sub("startPeriod=[^&]+", paste0("startPeriod=", current_period), rv$final_url)
              new_final <- sub("endPeriod=[^&]+",   paste0("endPeriod=", current_period),   new_final)
              
              rv$final_url <- new_final
              updateTextInput(session, "txt_final_url", value = new_final)
              
              showNotification(
                paste0("No data for latest period. Fell back to ", current_period, "."),
                type = "warning", duration = 6
              )
              break
            }
            
            current_period <- prev
          }
        }
        
        incProgress(0.95, detail = "Finalizing…")
        preview_df(df)
        incProgress(1, detail = "Done")
      })
      
      output$txt_data_status <- renderText(
        sprintf("Preview loaded: %d rows, %d columns", nrow(preview_df()), ncol(preview_df()))
      )
      
    }, error = function(e) {
      preview_df(NULL)
      output$txt_data_status <- renderText(paste("Preview failed:", e$message))
      showNotification(paste0("Preview failed: ", e$message), type = "error")
    })
  })
  
  # ----------------------------------------
  # 4.20 Render preview table (View)
  # ----------------------------------------
  output$preview_ui <- renderUI({
    df <- preview_df()
    if (is.null(df)) return(NULL)
    
    tagList(
      h4("Data Preview"),
      DTOutput("tbl_preview")
    )
  })
  
  output$tbl_preview <- renderDT({
    df <- preview_df()
    req(df)
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  }, server = TRUE)
  
  # ----------------------------------------
  # 4.21 Download (Controller)
  # ----------------------------------------
  output$btn_download_data <- downloadHandler(
    filename = function() {
      id <- rv$dataset_id %||% "eurostat"
      paste0(id, "_sdmx.csv")
    },
    content = function(file) {
      req(rv$final_url)
      if (!guard_query_size(MAX_DOWNLOAD_ROWS, "download")) {
        stop("Download blocked due to estimated query size. Add more filters and try again.")
      }
      utils::download.file(rv$final_url, file, quiet = TRUE, mode = "wb")
    }
  )
  
  # ----------------------------------------
  # 4.22 Labels helper (Controller)
  # ----------------------------------------
  observeEvent(input$btn_lbl_generate, {
    req(rv$dsd)
    
    dims <- unique(rv$dsd$concept)
    lang <- input$lbl_lang %||% "en"
    
    code_txt <- build_labels_helper_code(dims = dims, lang = lang, agency = "ESTAT")
    updateTextAreaInput(session, "txt_lbl_code", value = code_txt)
    
    showNotification("Labels helper generated.", type = "message", duration = 3)
  })
  
  observeEvent(input$btn_lbl_copy, {
    txt <- input$txt_lbl_code
    req(txt)
    
    session$sendCustomMessage("copyToClipboard", list(
      text = txt,
      inputId = "txt_lbl_code"
    ))
    
    showNotification("Copied (or attempted).", type = "message", duration = 3)
  })
  
  
  
}
shinyApp(ui, server)