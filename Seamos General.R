# Load libraries
library(xml2)
library(rvest)
library(httr)
library(stringr)

# 1. HARDCODE THE KNOWN BLOG URLS DIRECTLY
# This completely bypasses the broken sidebar/link extraction step
absolute_urls <- c(
  "https://meloseamoss.com",
  "https://meloseamoss.com",
  "https://meloseamoss.com"
 
)

browser_headers <- add_headers(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
  `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
)

# Pre-allocate data vectors
titles       <- c()
authors      <- c()
dates        <- c()
full_content <- c()
valid_urls   <- c()

cat(sprintf("[Engine Status]: Directly connecting to %d targeted articles...\n\n", length(absolute_urls)))

# 2. Loop through each post URL directly
for (post_url in absolute_urls) {
  
  cat(sprintf("Connecting to: %s\n", post_url))
  post_resp <- GET(post_url, browser_headers)
  
  if (status_code(post_resp) != 200) {
    cat(sprintf("   [Error]: Could not reach page. Status Code: %d\n", status_code(post_resp)))
    next
  }
  
  # Parse the HTML code
  post_page <- read_html(post_resp)
  
  # STEP 1: EXTRACT TITLE 
  title <- post_page %>% html_element("h1.entry-title, h1") %>% html_text2()
  if (is.na(title) || title == "") title <- "Untitled Article"
  
  # STEP 2: EXTRACT METADATA 
  author <- post_page %>% html_element(".entry-author, .author, .byline") %>% html_text2()
  if (is.na(author) || author == "") author <- "Seamoss"
  
  date_posted <- post_page %>% html_element(".entry-date, time, .posted-on") %>% html_text2()
  if (is.na(date_posted) || date_posted == "") date_posted <- "2025"
  
  #  STEP 3: EXTRACT PARAGRAPHS 
 
  paragraphs <- post_page %>% 
    html_elements(".entry-content p, article p, p") %>% 
    html_text2()
  
  # Clean up empty noise rows
  paragraphs <- paragraphs[paragraphs != ""]
  
  if (length(paragraphs) > 0) {
    combined_text <- paste(paragraphs, collapse = "\n\n")
    # Clean up standard bottom footer text blocks
    combined_text <- str_split(combined_text, "Leave a Reply")[[1]][1]
  } else {
    combined_text <- "Text content could not be resolved structurally."
  }
  
  # Append to vectors
  titles       <- c(titles, title)
  authors      <- c(authors, author)
  dates        <- c(dates, date_posted)
  full_content <- c(full_content, combined_text)
  valid_urls   <- c(valid_urls, post_url)
  
  cat(sprintf("   [Success]: Scraped content for '%s'\n\n", title))
  Sys.sleep(1.5) # Polite scraping delay
}

# 3. Build Data Frame Matrix
blog_dataset <- data.frame(
  Title       = titles,
  Author      = authors,
  Date_Posted = dates,
  URL         = valid_urls,
  Content     = full_content,
  stringsAsFactors = FALSE
)

# Output summary to verify data population
cat("\n--- DIRECT COUPLING EXTRACTION SUMMARY ---\n")
print(blog_dataset)

# Export locally
write.csv(blog_dataset, "melo_seamoss_direct_blogs.csv", row.names = FALSE)

