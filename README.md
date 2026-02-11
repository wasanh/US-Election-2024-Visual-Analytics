# US-Election-2024-Visual-Analytics
Interactive R-Shiny dashboard for 2024 US Election county-level analysis. Quantifying political shifts through the lens of income paradoxes, urban density, and demographic realignment using ggplot2, plotly, and tidycensus

## 🔗 Live Demo
View the interactive Shiny Dashboard here: [https://wasanalhabahbeh.shinyapps.io/2024USELECTION/](https://wasanalhabahbeh.shinyapps.io/2024USELECTION/)

## 📊 Project Overview
This project moves beyond standard red/blue maps to identify "breakage points" where traditional political alignments failed. Using county-level data and 2023 ACS census metrics, we analyzed:
- **The Education Dam:** How a 35% Bachelor’s degree threshold insulated counties from flipping.
- **The Suburban Collapse:** Why 88% of party flips occurred in urban and suburban zones rather than rural areas.
- **The Hispanic Realignment:** Quantifying the shift in diverse demographic strongholds.

## 🛠️ Technical Stack
- **Language:** R
- **Framework:** Shiny
- **Libraries:** `ggplot2`, `plotly`, `tidycensus`, `usmap`, `tidyverse`

## 📁 Repository Content
- `app.R`: The core logic for the interactive dashboard.
- `clean_election_data.csv`: The processed dataset merging election results with socioeconomic variables.
- `2024-US-Election-Quantifying-Political-Breakage.pdf`: The full analytical report.

## 🚀 How to Run Locally
1. Clone this repository.
2. Ensure you have R and the following libraries installed: `shiny`, `tidyverse`, `plotly`, `usmap`.
3. Open `app.R` and click **"Run App"** in RStudio.
