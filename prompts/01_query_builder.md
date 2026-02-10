# Task
Extend the existing Shiny app with a SDMX 2.1 Query Builder.

## Preconditions
- A dataset is selected
- Its DSD is already loaded

## Requirements
- Do NOT use restatapi for data queries
- Build SDMX 2.1 data URLs manually
- Default format MUST be `format=SDMX-CSV` 
- Do NOT filter time via the SDMX series key 
- Time filtering MUST use query parameters `startPeriod` and `endPeriod` (case-sensitive) 

## Features
1. Button: "Generate base query"
   - Builds an unfiltered SDMX data URL
2. Query builder UI
   - One filter per DSD dimension (multi-select)
   - Preserve DSD dimension order exactly as in the DSD
   - Unfiltered dimensions must be represented as empty positions in the SDMX series key
3. Optional time filter (FROM:TO)
   - Implemented via `startPeriod` / `endPeriod`
4. Show final URL in a text box
5. Button: "Copy query"

## Clipboard requirement (IMPORTANT)
The "Copy query" button must work even when the Clipboard API is blocked (e.g. RStudio Viewer).
Implement a robust JS handler:
1) Try `navigator.clipboard.writeText(text)`
2) On failure, fallback to `document.execCommand("copy")` with a temporary textarea
3) If that fails, show a Shiny modal with the URL pre-filled and selected, with instructions to press Ctrl+C.
Do NOT use external JS libraries.

## Output
- Only provide the Shiny code needed
- Integrate with existing reactiveValues (`dataset_id`, `dsd`)

