# Eurostat Dataset Explorer

A lightweight **R Shiny application** for exploring the structure of Eurostat datasets
using the official **restatapi** package.

This app is designed as a **developer-oriented explorer**, allowing users to:
- search Eurostat datasets
- inspect dataset structures (DSD)
- explore dimensions and their codes/labels interactively



---

## Features

- 🔍 Search Eurostat datasets by keyword
- 📋 Browse dataset metadata (TOC)
- 🧱 Load and inspect dataset structures (DSD)
- 🧭 Explore dimensions and their codes interactively
- ⚡ Caches metadata per session for efficiency

No data download or analysis is performed in this version.

---

## Project Structure

eurostat-dataset-explorer/
├─ app.R # Production entry point (Shiny app)
├─ R/ # Production helpers (used by app.R)
├─ tools/ # Development tools (AI / prompt / PDF helpers)
├─ scripts/ # Dev workflows & experimental scripts
├─ prompts/ # LLM prompts used for code generation
├─ misc/ # External resources (e.g. PDFs)


**Design principle**

> Root + `R/` = production  
> Everything else = development tooling

---

## Requirements

- R (≥ 4.1 recommended)
- Packages:
  - shiny
  - DT
  - restatapi

Install required packages:

```r
install.packages(c("shiny", "DT", "restatapi"))
