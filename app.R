library(shiny)
library(DT)
library(restatapi)

# Helper: Load TOC once and cache in memory
load_toc_once <- local({
  toc <- NULL
  function() {
    if (is.null(toc)) {
      # Use verbose=FALSE and cache=TRUE default
      toc <<- get_eurostat_toc(cache = TRUE, verbose = FALSE)
    }
    toc
  }
})

ui <- fluidPage(
  titlePanel("Eurostat Dataset Explorer (restatapi)"),
  sidebarLayout(
    sidebarPanel(
      textInput("search", "Search Datasets by keyword:", value = ""),
      fluidRow(
        column(
          width = 6,
          actionButton("btn_search", "Search", width = "100%")
        ),
        column(
          width = 6,
          actionButton("btn_clear_all", "Reset", width = "100%", class = "btn-warning")
        )
      ),
      tags$div(
        style = "margin-top: 6px; color: #666; font-size: 0.85em;",
        "Search tips: ",
        tags$code("space = AND"),
        ", ",
        tags$code("| = OR"),
        tags$br(),
        tags$span("Example: "),
        tags$code("bop financial|iip")
      ),
      hr(),
      uiOutput("selected_dataset_ui"),
      actionButton("btn_load_dsd", "Load Structure"),
      hr(),
      h4("SDMX 2.1 Query Builder"),
      actionButton("btn_generate_base_query", "Generate base query"),
      uiOutput("query_builder_ui"),
      textInput("txt_final_url", "Final SDMX Data URL:", "", width = "100%"),
      actionButton("btn_copy_query", "Copy query to clipboard"),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
          var text = message.text || '';
          var inputId = message.inputId || 'txt_final_url';
        
          function fallbackCopy() {
            var el = document.getElementById(inputId);
            if (!el) { alert('Copy failed: input not found'); return; }
            el.focus();
            el.select();
            try {
              var ok = document.execCommand('copy');
              if (ok) {
                if (message.notifyId) Shiny.setInputValue(message.notifyId, 'Copied!', {priority: 'event'});
                else alert('Copied to clipboard');
              } else {
                alert('Copy failed (execCommand returned false).');
              }
            } catch (e) {
              alert('Copy failed: ' + e);
            }
          }
        
          // Try modern Clipboard API first
          if (navigator.clipboard && window.isSecureContext) {
            navigator.clipboard.writeText(text).then(function() {
              if (message.notifyId) Shiny.setInputValue(message.notifyId, 'Copied!', {priority: 'event'});
              else alert('Copied to clipboard');
            }).catch(function() {
              fallbackCopy();
            });
          } else {
            fallbackCopy();
          }
        });
        "))
    ),
    
    mainPanel(
      h4("Search Results"),
      DTOutput("tbl_toc"),
      hr(),
      h4("Dataset Structure: Dimensions"),
      uiOutput("dim_selection_ui"),
      DTOutput("tbl_codes"),
      uiOutput("message_ui")
    )
  )
)

server <- function(input, output, session) {
  # Cache TOC once per session
  toc <- reactive({
    load_toc_once()
  })
  # ------------------------------------------------------------
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
      if (length(terms) == 0) return(rep(FALSE, length(text)))
      Reduce(`&`, lapply(terms, function(t) {
        grepl(t, text, ignore.case = ignore.case)
      }))
    })
    Reduce(`|`, group_hits)
  }
  # Dataset search logic
  search_results <- eventReactive(input$btn_search, {
    req(toc())
    df <- toc()
    pattern <- trimws(input$search)
    if (!nzchar(pattern)) return(data.frame())
    haystack <- paste(df$code, df$title, sep = " | ")
    hits <- match_query_groups(haystack, pattern, ignore.case = TRUE)
    res <- df[hits, , drop = FALSE]
    if (nrow(res) == 0) {
      showNotification(
        "No datasets found. Tip: space = AND, | = OR (grouped). Example: bop financial|iip",
        type = "warning"
      )
    }
    res
  })
  
  output$tbl_toc <- renderDT({
    df <- search_results()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No search results"), options = list(dom = 't'), rownames = FALSE))
    }
    datatable(df[, c("title", "code", "dataStart", "dataEnd", "shortDescription")],
              selection = "single",
              rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE),
              colnames = c("Title", "Code", "Start", "End", "Description"))
  }, server = TRUE)
  
  rv <- reactiveValues(
    dataset_id = NULL,
    dsd = NULL,
    dim_selected = NULL,
    # For query building: dimension filters (named list code vectors)
    dim_filters = NULL,
    time_from = NULL,
    time_to = NULL,
    base_url = NULL,
    final_url = NULL
  )
  
  # observeEvent(input$btn_clear_all, {
  #   # If DSD exists, clear filters first (so inputs exist)
  #   if (!is.null(rv$dsd)) {
  #     dims <- unique(rv$dsd$concept)
  #     for (d in dims) {
  #       id <- paste0("filter_", d)
  #       if (!is.null(input[[id]])) {
  #         updateSelectizeInput(session, id, selected = character(0), server = TRUE)
  #       }
  #     }
  #   }
  #   
  #   # Now reset everything
  #   rv$dataset_id <- NULL
  #   rv$dsd <- NULL
  #   rv$dim_selected <- NULL
  #   rv$dim_filters <- NULL
  #   rv$time_from <- NULL
  #   rv$time_to <- NULL
  #   rv$base_url <- NULL
  #   rv$final_url <- NULL
  #   
  #   updateTextInput(session, "search", value = "")
  #   updateTextInput(session, "txt_final_url", value = "")
  #   
  #   showNotification("Cleared. Ready for a new search.", type = "message")
  # })
  observeEvent(input$btn_clear_all, {
    # Reset reactive values
    rv$dataset_id <- NULL
    rv$dsd <- NULL
    rv$dim_selected <- NULL
    
    # Query builder state
    rv$dim_filters <- NULL
    rv$time_from <- NULL
    rv$time_to <- NULL
    rv$base_url <- NULL
    rv$final_url <- NULL
    
    # Clear UI inputs
    updateTextInput(session, "search", value = "")
    updateTextInput(session, "txt_final_url", value = "")
    
    # Clear dimension selector (it’s rendered via renderUI, so resetting rv$dim_selected is enough,
    # but this does not hurt if it exists)
    # updateSelectInput(session, "sel_dim", selected = character(0))
    
    # Clear filter inputs (they exist only when UI is rendered)
    if (!is.null(rv$dsd)) {
      dims <- unique(rv$dsd$concept)
      for (d in dims) {
        id <- paste0("filter_", d)
        if (!is.null(input[[id]])) {
          updateSelectizeInput(session, id, selected = character(0), server = TRUE)
        }
      }
    }
    
    showNotification("Cleared. Ready for a new search.", type = "message")
  })
  
  observeEvent(input$tbl_toc_rows_selected, {
    rows <- input$tbl_toc_rows_selected
    df <- search_results()
    if (length(rows) == 1 && nrow(df) >= rows) {
      rv$dataset_id <- df$code[rows]
      rv$dsd <- NULL
      rv$dim_selected <- NULL
      
      # Reset query builder facets on new dataset selection
      rv$dim_filters <- NULL
      rv$time_from <- NULL
      rv$time_to <- NULL
      rv$base_url <- NULL
      rv$final_url <- NULL
      updateTextInput(session, "txt_final_url", value = "")
    } else {
      rv$dataset_id <- NULL
      rv$dsd <- NULL
      rv$dim_selected <- NULL
      rv$dim_filters <- NULL
      rv$time_from <- NULL
      rv$time_to <- NULL
      rv$base_url <- NULL
      rv$final_url <- NULL
      updateTextInput(session, "txt_final_url", value = "")
    }
  })
  
  output$selected_dataset_ui <- renderUI({
    if (is.null(rv$dataset_id)) {
      span(style = "color: red;", "No dataset selected")
    } else {
      tagList(
        strong("Selected dataset ID:"), code(rv$dataset_id)
      )
    }
  })
  
  observeEvent(input$btn_load_dsd, {
    if (is.null(rv$dataset_id)) {
      showNotification("Please select a dataset before loading structure.", type = "error")
      return()
    }
    
    tryCatch({
      dsd_dt <- get_eurostat_dsd(rv$dataset_id, lang = "en")
      needed <- c("concept", "code", "name")
      if (!all(needed %in% names(dsd_dt)) || nrow(dsd_dt) == 0) {
        showNotification("DSD loaded but has unexpected format (missing concept/code/name).", type = "warning")
        rv$dsd <- NULL
        rv$dim_selected <- NULL
        return()
      }
      
      rv$dsd <- dsd_dt
      rv$dim_selected <- NULL
      
      # Reset query builder state on new DSD load
      rv$dim_filters <- NULL
      rv$time_from <- NULL
      rv$time_to <- NULL
      rv$base_url <- NULL
      rv$final_url <- NULL
      updateTextInput(session, "txt_final_url", value = "")
      
      showNotification("DSD loaded.", type = "message")
      
    }, error = function(e) {
      showNotification(paste0("Failed to load DSD: ", e$message), type = "error")
      rv$dsd <- NULL
      rv$dim_selected <- NULL
      rv$dim_filters <- NULL
      rv$time_from <- NULL
      rv$time_to <- NULL
      rv$base_url <- NULL
      rv$final_url <- NULL
      updateTextInput(session, "txt_final_url", value = "")
    })
  })
  
  output$dim_selection_ui <- renderUI({
    if (is.null(rv$dsd)) {
      span(style = "color: red;", "Structure not loaded")
    } else {
      dims <- unique(rv$dsd$concept)
      selectInput("sel_dim", "Select Dimension", choices = dims, selected = rv$dim_selected)
    }
  })
  
  observeEvent(input$sel_dim, {
    rv$dim_selected <- input$sel_dim
  })
  
  output$tbl_codes <- renderDT({
    if (is.null(rv$dsd)) {
      return(datatable(data.frame(Message = "Structure not loaded"), options = list(dom = 't'), rownames = FALSE))
    }
    if (is.null(rv$dim_selected)) {
      return(datatable(data.frame(Message = "No dimension selected"), options = list(dom = 't'), rownames = FALSE))
    }
    
    dim_table <- rv$dsd[rv$dsd$concept == rv$dim_selected, c("code", "name")]
    dim_table <- dim_table[order(dim_table$code), , drop = FALSE]
    
    if (nrow(dim_table) == 0) {
      return(datatable(data.frame(Message = "No codes found for selected dimension"), options = list(dom = 't'), rownames = FALSE))
    }
    
    datatable(dim_table, options = list(pageLength = 10, scrollX = TRUE), colnames = c("Code", "Label"))
  }, server = TRUE)
  
  output$message_ui <- renderUI({
    if (is.null(rv$dataset_id)) {
      span(style = "color: red;", "Please select a dataset from search results.")
    } else if (is.null(rv$dsd)) {
      span(style = "color: red;", "Structure not loaded. Click 'Load Structure'.")
    } else if (is.null(rv$dim_selected)) {
      span(style = "color: gray;", "Select a dimension to see codes and labels.")
    } else {
      NULL
    }
  })
  
  # ---------------------------
  # SDMX Query Builder UI
  # ---------------------------
  
  # Render filter ui: one multi-select per dimension, plus time filter
  output$query_builder_ui <- renderUI({
    req(rv$dsd, rv$dataset_id)
    
    dims <- unique(rv$dsd$concept)
    # For each dim, get all codes sorted
    dim_choices <- lapply(dims, function(d) {
      codes <- sort(unique(rv$dsd$code[rv$dsd$concept == d]))
      codes
    })
    names(dim_choices) <- dims
    
    # For each dim get current selected filters (if any)
    current_filters <- rv$dim_filters
    if (is.null(current_filters)) {
      current_filters <- vector("list", length(dims))
      names(current_filters) <- dims
    }
    
    ui_list <- lapply(dims, function(d) {
      selectizeInput(
        inputId = paste0("filter_", d),
        label = paste0("Filter Dimension: ", d),
        choices = dim_choices[[d]],
        selected = current_filters[[d]],
        multiple = TRUE,
        options = list(plugins = list('remove_button'), maxItems = NULL)
      )
    })
    
    # Time filter: text inputs FROM and TO (both optional)
    # We'll infer time dimension name, or fallback to "time"
    time_dim_candidates <- grep("^time$", dims, ignore.case = TRUE, value = TRUE)
    time_dim <- if (length(time_dim_candidates) == 1) time_dim_candidates else NULL
    
    if (!is.null(time_dim)) {
      ui_list <- c(
        ui_list,
        tagList(
          tags$hr(),
          tags$strong("Optional time filter (FROM:TO)"),
          fluidRow(
            column(6,
                   textInput("time_from", paste0("FROM (", time_dim, ")"), value = rv$time_from)
            ),
            column(6,
                   textInput("time_to", paste0("TO (", time_dim, ")"), value = rv$time_to)
            )
          )
        )
      )
    }
    
    do.call(tagList, ui_list)
  })
  
  # Observe filter inputs to update reactive rv$dim_filters
  observe({
    req(rv$dsd, rv$dataset_id)
    dims <- unique(rv$dsd$concept)
    filters <- list()
    for (d in dims) {
      flt <- input[[paste0("filter_", d)]]
      if (!is.null(flt) && length(flt) > 0 && nzchar(flt[1])) {
        filters[[d]] <- as.character(flt)
      } else {
        filters[[d]] <- character(0)
      }
    }
    rv$dim_filters <- filters
    
    # Time filter
    time_dim_candidates <- grep("^time$", dims, ignore.case = TRUE, value = TRUE)
    time_dim <- if (length(time_dim_candidates) == 1) time_dim_candidates else NULL
    
    if (!is.null(time_dim)) {
      rv$time_from <- if (!is.null(input$time_from)) input$time_from else NULL
      rv$time_to <- if (!is.null(input$time_to)) input$time_to else NULL
    } else {
      rv$time_from <- NULL
      rv$time_to <- NULL
    }
  })
  
  # Helper: Build base URL (unfiltered)
  build_base_url <- reactive({
    req(rv$dataset_id)
    # Compose base SDMX 2.1 REST data URL for Eurostat
    # Example:
    # https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/{flowRef}
    paste0("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/", rv$dataset_id)
  })
  
  # Button: Generate base query (unfiltered)
  observeEvent(input$btn_generate_base_query, {
    req(rv$dataset_id)
    rv$base_url <- build_base_url()
    # Unfiltered query for SDMX-CSV format
    base_url_w_format <- paste0(rv$base_url, "?format=SDMX-CSV")
    rv$final_url <- base_url_w_format
    updateTextInput(session, "txt_final_url", value = rv$final_url)
  })
  
  # Build full URL with filters and time, format=SDMX-CSV default
  build_full_url <- reactive({
    req(rv$base_url)
    dims <- unique(rv$dsd$concept)
    
    # Build SDMX key for dimensions, per order
    # Each dimension segment:
    # - if no filters: ""
    # - if filters: filter values separated by +
    # If no filters, empty string will be treated as default "all codes"
    segments <- character(length(dims))
    names(segments) <- dims
    
    for (d in dims) {
      vals <- rv$dim_filters[[d]]
      if (length(vals) == 0) {
        # empty means all codes for that dimension -> empty string segment
        segments[d] <- ""
      } else {
        # join selected codes by '+'
        segments[d] <- paste0(vals, collapse = "+")
      }
    }
    
    # Handle time dimension filtering if present
    time_dim_candidates <- grep("^time$", dims, ignore.case = TRUE, value = TRUE)
    time_dim <- if (length(time_dim_candidates) == 1) time_dim_candidates else NULL
    
    # If time dimension present and time_from or time_to defined, filter time codes accordingly:
    if (!is.null(time_dim)) {
      time_vals <- rv$dim_filters[[time_dim]]
      if (!is.null(rv$time_from) && nzchar(rv$time_from)) {
        time_from <- rv$time_from
      } else {
        time_from <- NULL
      }
      if (!is.null(rv$time_to) && nzchar(rv$time_to)) {
        time_to <- rv$time_to
      } else {
        time_to <- NULL
      }
      
      if (!is.null(time_from) || !is.null(time_to)) {
        # Get all time codes in DSD for time_dim, sorted
        all_time_codes <- sort(unique(rv$dsd$code[rv$dsd$concept == time_dim]))
        # Subset by FROM/TO bounds (string compare)
        # If time_from NULL => min, if time_to NULL => max
        min_time <- if (!is.null(time_from) && nzchar(time_from)) time_from else all_time_codes[1]
        max_time <- if (!is.null(time_to) && nzchar(time_to)) time_to else all_time_codes[length(all_time_codes)]
        
        # Filter time codes within [min_time, max_time]
        filtered_times <- all_time_codes[all_time_codes >= min_time & all_time_codes <= max_time]
        
        # If user selected specific time codes too, intersect with filtered_times
        if (length(time_vals) > 0) {
          filtered_times <- intersect(filtered_times, time_vals)
        }
        if (length(filtered_times) == 0) {
          # fallback to empty (means all, but here user filtered empty time set)
          segments[time_dim] <- ""
        } else {
          segments[time_dim] <- paste(filtered_times, collapse = "+")
        }
      }
    }
    
    # Compose SDMX key string: segments separated by '.'
    key_string <- paste(segments, collapse = ".")
    
    # Build final URL
    # Format default to SDMX-CSV to meet requirement
    url <- paste0(rv$base_url, "/", key_string, "?format=SDMX-CSV")
    url
  })
  
  # Auto-update final_url when filters or base_url change
  observe({
    req(rv$base_url)
    full_url <- build_full_url()
    rv$final_url <- full_url
    updateTextInput(session, "txt_final_url", value = full_url)
  })
  
  # Copy query to clipboard
  observeEvent(input$btn_copy_query, {
    req(rv$final_url)
    session$sendCustomMessage("copyToClipboard", rv$final_url)
  })
}

shinyApp(ui, server)