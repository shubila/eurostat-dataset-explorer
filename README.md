---
editor_options: 
  markdown: 
    wrap: 72
---

# Eurostat Dataset Explorer

A lightweight **R Shiny application** for exploring the structure of
Eurostat datasets using the official **restatapi** package.

This app is designed as a **developer-oriented explorer**, allowing
users to: - search Eurostat datasets - inspect dataset structures
(DSD) - explore dimensions and their codes/labels interactively

------------------------------------------------------------------------

## Features

🔎 1. Dataset Discovery

Search the full Eurostat catalogue (TOC)

Inspect dataset metadata (start/end period)

Explore DSD dimensions and code lists

🧱 2. SDMX 2.1 Query Builder

Build valid SDMX 2.1 data URLs

Enforce required dimensions (e.g. freq, geo)

Automatic time handling based on TOC metadata

Guardrails against overly large queries

👀 3. Data Validation via Preview

Preview first observations directly from the API

Automatic time fallback (A / Q / M) if latest period has no data

Clear status messages and updated working URL

📦 4. Exportable Output (Portable Assets)

The app produces reusable components that can be copied into other projects:

SDMX Data URL
→ Can be used directly in R, Python, or other tools

Validated time period
→ Ensures the selected period actually contains data

**R Labels Helper Script**
- Dynamically downloads codelists (TSV)
- Merges codes with labels
- No local files required
------------------------------------------------------------------------

## Project Structure

``` text
eurostat-dataset-explorer/
├─ app.R
├─ R/
├─ www/
├─ tools/
├─ scripts/
├─ prompts/
└─ misc/
```


**Production files:**  
- `app.R`
- `R/`
- `www/`


> Everything else = development tooling

------------------------------------------------------------------------

## Requirements

-   R (≥ 4.1 recommended)
-   Packages:
    -   shiny
    -   DT
    -   restatapi

Install required packages:

\`\`\`r install.packages(c("shiny", "DT", "restatapi"))
