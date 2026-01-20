# Web-Scraping-and-Data-Analysis-Using-R-

This project performs end-to-end web scraping and analysis of journal articles published in *Human Genomics*. Article-level metadata is extracted, cleaned, and analyzed to study publication trends, keyword distributions, and author contributions.

## Data Source
- Journal: *Human Genomics*
- Pages scraped: 23+
- Data collected includes titles, authors, publication dates, abstracts, keywords, and corresponding author details.

## Tools & Libraries
- R
- rvest
- dplyr
- jsonlite
- lubridate
- ggplot2
- tidyr
- stringr
- readxl
- writexl

## Methodology
- Scraped article metadata from multiple journal listing pages and individual article pages.
- Cleaned and standardized data by removing duplicates, handling missing values, and applying robust date parsing.
- Extracted temporal features (year, month) and normalized keywords for analysis.
- Visualized publication trends, keyword distributions, and author contribution patterns.

## How to Run
1. Install required R packages listed above.
2. Run the R script

