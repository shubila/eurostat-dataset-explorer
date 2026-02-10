
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
      actionButton("btn_search", "Search"),
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
      actionButton("btn_load_dsd", "Load Structure")
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
  # match_query_groups()
  #
  # Implements simple, predictable search semantics for users:
  #
  #   - SPACE  = AND
  #   - "|"    = OR (with grouping)
  #
  # Examples:
  #   "bop iip"              -> bop AND iip
  #   "bop|iip"              -> bop OR iip
  #   "bop financial|iip"    -> (bop AND financial) OR iip
  #   "bop financial|bop iip"-> (bop AND financial) OR (bop AND iip)
  #
  # The function returns a logical vector aligned with `text`,
  # suitable for direct row subsetting.
  #
  # Design goals:
  #   - No regex knowledge required for users
  #   - Deterministic behavior
  #   - Works with expert abbreviations and free text
  # ------------------------------------------------------------
  match_query_groups <- function(text, query, ignore.case = TRUE) {
    query <- trimws(query)
    if (!nzchar(query)) return(rep(FALSE, length(text)))
    
    # Normalize OR operator spacing
    query <- gsub("\\s*\\|\\s*", "|", query)
    
    # Split into OR groups
    or_groups <- strsplit(query, "\\|")[[1]]
    or_groups <- trimws(or_groups)
    or_groups <- or_groups[nzchar(or_groups)]
    if (length(or_groups) == 0) return(rep(FALSE, length(text)))
    
    # Evaluate each OR group as an AND-combination of terms
    group_hits <- lapply(or_groups, function(g) {
      terms <- strsplit(g, "\\s+")[[1]]
      terms <- terms[nzchar(terms)]
      if (length(terms) == 0) return(rep(FALSE, length(text)))
      
      Reduce(`&`, lapply(terms, function(t) {
        grepl(t, text, ignore.case = ignore.case)
      }))
    })
    
    # Combine OR groups
    Reduce(`|`, group_hits)
  }
  # ------------------------------------------------------------
  # Dataset search logic
  #
  # - Uses match_query_groups() to provide:
  #     space = AND
  #     "|"   = OR (grouped)
  #
  # - Searches over a combined "haystack" consisting of:
  #     dataset code + dataset title
  #
  # This allows:
  #   - Expert-style searches (e.g. "bop_iip")
  #   - Natural-language searches (e.g. "government finance")
  #   - Mixed queries (e.g. "bop iip|finance")
  #
  # The result is a filtered TOC data.frame suitable
  # for display in a DT::datatable.
  # ------------------------------------------------------------
  search_results <- eventReactive(input$btn_search, {
    req(toc())
    df <- toc()
    
    pattern <- trimws(input$search)
    if (!nzchar(pattern)) return(data.frame())
    
    # Search surface: combine machine-readable code and human-readable title
    haystack <- paste(df$code, df$title, sep = " | ")
    hits <- match_query_groups(haystack, pattern, ignore.case = TRUE)
    # hits <- match_query_groups(df$title, pattern, ignore.case = TRUE)
    
    res <- df[hits, , drop = FALSE]
    
    if (nrow(res) == 0) {
      showNotification(
        "No datasets found. Tip: space = AND, | = OR (grouped). Example: bop financial|iip",
        type = "warning"
      )
    }
    
    res
  })
  
  # Show search results in DT
  output$tbl_toc <- renderDT({
    df <- search_results()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No search results"), options = list(dom = 't'), rownames = FALSE))
    }
    # Show only relevant columns: title, code, dataStart, dataEnd, shortDescription
    datatable(df[, c("title", "code", "dataStart", "dataEnd", "shortDescription")],
              selection = "single",
              rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE),
              colnames = c("Title", "Code", "Start", "End", "Description"))
  }, server = TRUE)

  # Reactive val(s) to hold active dataset_id and loaded DSD
  rv <- reactiveValues(
    dataset_id = NULL,
    dsd = NULL,
    dim_selected = NULL
  )

  # Update dataset_id when user selects row in TOC table
  observeEvent(input$tbl_toc_rows_selected, {
    rows <- input$tbl_toc_rows_selected
    df <- search_results()
    if (length(rows) == 1 && nrow(df) >= rows) {
      rv$dataset_id <- df$code[rows]
      rv$dsd <- NULL
      rv$dim_selected <- NULL
    } else {
      rv$dataset_id <- NULL
      rv$dsd <- NULL
      rv$dim_selected <- NULL
    }
  })

  # Show which dataset is selected
  output$selected_dataset_ui <- renderUI({
    if (is.null(rv$dataset_id)) {
      span(style = "color: red;", "No dataset selected")
    } else {
      tagList(
        strong("Selected dataset ID:"), code(rv$dataset_id)
      )
    }
  })

  # Load DSD on button click
  observeEvent(input$btn_load_dsd, {
    if (is.null(rv$dataset_id)) {
      showNotification("Please select a dataset before loading structure.", type = "error")
      return()
    }
    
    tryCatch({
      dsd_dt <- get_eurostat_dsd(rv$dataset_id, lang = "en")
      
      # Expect long table with concept/code/name
      needed <- c("concept", "code", "name")
      if (!all(needed %in% names(dsd_dt)) || nrow(dsd_dt) == 0) {
        showNotification("DSD loaded but has unexpected format (missing concept/code/name).", type = "warning")
        rv$dsd <- NULL
        rv$dim_selected <- NULL
        return()
      }
      
      rv$dsd <- dsd_dt
      rv$dim_selected <- NULL
      showNotification("DSD loaded.", type = "message")
      
    }, error = function(e) {
      showNotification(paste0("Failed to load DSD: ", e$message), type = "error")
      rv$dsd <- NULL
      rv$dim_selected <- NULL
    })
  })
  

  # Show UI to select dimension after DSD is loaded
  output$dim_selection_ui <- renderUI({
    if (is.null(rv$dsd)) {
      span(style = "color: red;", "Structure not loaded")
    } else {
      # Keep DSD order – important for query building and SDMX semantics
      dims <- unique(rv$dsd$concept)
      selectInput("sel_dim", "Select Dimension", choices = dims, selected = rv$dim_selected)
    }
  })
  
  # Track dimension selection input, update reactive val
  observeEvent(input$sel_dim, {
    rv$dim_selected <- input$sel_dim
  })

  # Show codes/labels of the selected dimension
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
  

  # Show user messages below codes table
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
}

shinyApp(ui, server)

