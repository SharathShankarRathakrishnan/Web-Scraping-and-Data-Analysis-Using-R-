library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readxl)
library(writexl)
library(rvest)
library(lubridate)
library(jsonlite)


#######################################################
#                     WEB SCRAPING                    # 
#######################################################

df_all_articles <- data.frame()

df_only_year <- data.frame()

year_to_extract <- 2024
year_to_extract <- readline(prompt = "Enter the year to extract: ")


###########################
###########################
### EXECUTE TILL HERE FOR USER INPUT THEN EXECUTE THE REST ####
###########################
###########################

year_to_extract <- as.numeric(year_to_extract)
page_num <- 1

repeat {
  
  url <- paste0("https://humgenomics.biomedcentral.com/articles?searchType=journalSearch&sort=PubDate&page=", page_num)
  webpage <- read_html(url)
  
  titles <- html_nodes(webpage, "h3 a") %>% html_text()
  titles <- titles[-1]
  
  publish <- html_nodes(webpage, "span[itemprop='datePublished']") %>%
    html_text(trim = TRUE) %>%
    dmy()
  
  year_publish <- as.numeric(format(publish, "%Y"))
  
  authors <- html_nodes(webpage, 'span.c-listing__authors-list') %>%
    html_text(trim = TRUE)
  
  each_article_urls <- html_nodes(webpage, "h3 a") %>%
    html_attr("href") %>%
    .[-1]
  
  each_article_urls <- paste0("https://humgenomics.biomedcentral.com", each_article_urls)
  
  for (j in 1:length(each_article_urls)) {
    article_page <- read_html(each_article_urls[j])
    
    abstract <- article_page %>%
      html_node('meta[name="citation_abstract"]') %>%
      html_attr("content")
    
    corr_author_names <- article_page %>%
      html_nodes('p#corresponding-author-list a') %>% html_text()
    
    corr_author_emails <- article_page %>%
      html_nodes('p#corresponding-author-list a') %>%
      html_attr('href') %>%
      sub('mailto:', '', .)
    
    corr_author_combined <- paste(corr_author_names, collapse = "; ")
    corr_email_combined <- paste(corr_author_emails, collapse = "; ")
    
    keywords <- article_page %>%
      html_nodes('ul.c-article-subject-list li span') %>%
      html_text(trim = TRUE)
    
    if (length(keywords) == 0) {
      keywords <- NA
    } else {
      keywords <- paste(keywords, collapse = "; ")
    }
    
    article_row <- data.frame(
      Title = titles[j],
      Authors = authors[j],
      Correspondence_Author = corr_author_combined,
      Correspondence_Author_Email = corr_email_combined,
      Publish_Date = ifelse(length(publish) >= j, publish[j], NA),
      Abstract = abstract,
      Keywords = keywords,
      stringsAsFactors = FALSE
    )
    
    
    
    df_all_articles <- rbind(df_all_articles, article_row)
    
    if (year_publish[j] == year_to_extract) {
      df_only_year <- rbind(df_only_year, article_row)
    }
    
    cat("Scraped article", nrow(df_all_articles), ":", titles[j], "\n")
  }
  
  cat("Completed page", page_num, "\n")
  Sys.sleep(3)
  
  next_exists <- length(html_nodes(webpage, '[data-test="next-page"]')) > 0
  if (!next_exists) break
  
  page_num <- page_num + 1
}


write_xlsx(df_all_articles, "humangenomics_articles_scraped_check.xlsx")
write_xlsx(df_only_year, "humangenomics_articles_scraped_for_year.xlsx")



###############################################################
#                   DATA CLEANING                 #
###############################################################

df_publish_date_fix <- df_only_year %>%
  mutate(
    Publish_Date = as.character(Publish_Date),
    
    Publish_Date = case_when(
      str_detect(Publish_Date, "^\\d{2}/\\d{2}/\\d{4}$") ~ dmy(Publish_Date),
      str_detect(Publish_Date, "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(Publish_Date),
      str_detect(Publish_Date, "^[0-9]{1,2} [A-Za-z]+ [0-9]{4}$") ~ dmy(Publish_Date), 
      TRUE ~ NA_Date_
    )
  )


df <- df_publish_date_fix

df_clean <- df %>% 
  mutate(across(where(is.character), ~ str_squish(iconv(., from = "", to = "UTF-8")))) %>%
  
  # Remove rows with missing or blank Title/Authors
  filter(!is.na(Title) & Title != "",
         !is.na(Authors) & Authors != "") %>%
  
  # Standardize text fields (case normalization)
  mutate(
    Title = str_to_title(Title),                   # Convert titles to Title Case
    Authors = str_to_title(Authors),               # Standardize author names 
    Correspondence_Author = str_to_title(Correspondence_Author),
    Correspondence_Author_Email = str_to_lower(Correspondence_Author_Email),
    Keywords = str_to_lower(Keywords)
  ) %>%
  
  
  
  # Remove duplicates (case-insensitive)
  distinct(
    across(c(Title, Authors, Publish_Date), ~ str_to_lower(.)), .keep_all = TRUE
  ) %>%
  
  #  Detect or remove corrupted data
  filter(
    !str_detect(Title, regex("^[[:punct:][:space:]]*$")),
    !str_detect(Authors, regex("^[[:punct:][:space:]]*$")),
    !str_detect(Title, regex("error|undefined|null|n/a|missing", ignore_case = TRUE)),
    !str_detect(Authors, regex("error|undefined|null|n/a|missing", ignore_case = TRUE))
  )

# check how many rows were removed
cat("Rows before:", nrow(df), "\n")
cat("Rows after cleaning:", nrow(df_clean), "\n")

# Save cleaned data as xlsx
write_xlsx(df_clean, "cleaned_data.xlsx")


###############################################################
#                    DATA ANALYSIS                            #
###############################################################

df_clean <- df_clean %>%
  mutate(
    Publish_Date = as.Date(Publish_Date),
    Year = format(Publish_Date, "%Y"),
    Month = format(Publish_Date, "%B"),
    Month = factor(Month, levels = month.name)
  )


#                ARTICLES PER MONTH                            #


df_only_year$Publish_Date <- as.Date(df_only_year$Publish_Date)
df_only_year$Month <- month(df_only_year$Publish_Date, label = TRUE, abbr = FALSE)

articles_by_month <- df_only_year %>%
  filter(!is.na(Month)) %>%
  group_by(Month) %>%
  summarise(Total_Articles = n()) %>%
  arrange(match(Month, month.name))

print(articles_by_month)



#               KEYWORD CATEGORY DISTRIBUTION                 #


keywords_df <- df_clean %>%
  filter(!is.na(Keywords)) %>%
  separate_rows(Keywords, sep = ";|,") %>%
  mutate(Keywords = str_trim(Keywords)) %>%
  group_by(Keywords) %>%
  summarise(Frequency = n(), .groups = "drop")

keywords_df_cat <- keywords_df %>%
  mutate(Category = case_when(
    str_detect(Keywords, "gene|genomic|dna|mutation") ~ "Genomics",
    str_detect(Keywords, "cancer|tumor|oncology") ~ "Cancer Research",
    str_detect(Keywords, "microbiome|bacteria|microbial") ~ "Microbiome",
    str_detect(Keywords, "bioinformatics|machine learning|algorithm") ~ "Computational",
    TRUE ~ "Other"
  )) %>%
  group_by(Category) %>%
  summarise(Total = sum(Frequency))


#                 TOP CORRESPONDING AUTHORS                   #


top_corr_authors <- df_clean %>%
  filter(!is.na(Correspondence_Author) & Correspondence_Author != "") %>%
  group_by(Correspondence_Author) %>%
  summarise(Total_Articles = n()) %>%
  arrange(desc(Total_Articles))

top5_auth <- head(top_corr_authors, 5)


#                       VISUALIZATIONS                         #


# 1) KEYWORD CATEGORY DISTRIBUTION
ggplot(keywords_df_cat, aes(x = reorder(Category, Total), y = Total)) +
  geom_segment(aes(xend = Category, y = 0, yend = Total),
               linewidth = 1.2, color = "#3498DB") +
  geom_point(size = 4, color = "#1A5276") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Keyword Category Distribution",
    x = "Category",
    y = "Frequency"
  )


# 2) MONTHLY PUBLICATION TREND
ggplot(articles_by_month, aes(x = Month, y = Total_Articles, group = 1)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Article Publication Trend by Month",
    x = "Month",
    y = "Number of Articles"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 3) TOP 5 CORRESPONDING AUTHORS
ggplot(top5_auth, aes(x = reorder(Correspondence_Author, Total_Articles),
                      y = Total_Articles)) +
  geom_col(fill = "#1ABC9C") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 5 Most Contributing Corresponding Authors",
    x = "Author",
    y = "Articles Count"
  )

cat("\nData Analysis Completed & Visualizations Generated!\n")
