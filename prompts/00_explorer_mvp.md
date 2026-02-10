# Task
Create a minimal R Shiny application (single-file `app.R`) that explores
Eurostat datasets using the **restatapi** package.

This app is a **developer-oriented dataset explorer**, not an analytical app.

---

# Purpose (MVP)
The application must allow a user to:

1. Search Eurostat datasets by keyword
2. Select a dataset from search results
3. Load and inspect the dataset structure (DSD)
4. Explore dimensions and their codes/labels interactively

No data download is required in this MVP.

---

# Functional Requirements

## Dataset search
- Load the Eurostat table of contents (TOC) once per session
- Provide a text input for keyword search
- Use `search_eurostat_toc()` to filter datasets
- Display results in a `DT::datatable`
- Selecting a row sets the active `dataset_id`

## Dataset structure (DSD)
- A button **“Load Structure”** triggers:
  - `get_eurostat_dsd(dataset_id, lang = "en")`
- Display:
  - a list of dimension names
  - selecting a dimension shows its codes and labels in a paginated table

---

# UI Requirements
- Use base **shiny**
- Use **DT** for all tabular outputs
- Layout can be simple (sidebar + main panel)
- Show clear user messages when:
  - no dataset is selected
  - structure is not yet loaded
  - an API call fails

---

# Technical Constraints (IMPORTANT)
- Use ONLY the following packages:
  - shiny
  - DT
  - restatapi
- Do NOT hardcode API URLs
- Do NOT invent functions not documented in restatapi
- Cache TOC and DSD objects in memory (reactive values)
- Keep code readable and modular (small helper functions allowed inside app.R)
- If uncertain about an argument name, stop with a helpful error rather than guessing.

---

# Output
Return ONLY the full content of `app.R`.

Do NOT include explanations or commentary.
