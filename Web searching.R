library(rvest)
library(httr)

# Function to perform web scraping
scrape_website <- function(url_file, keywords_file, visited_urls = character(), counter = 1, visited_paragraphs = character(), output_file, all_paragraphs_found, max_urls) {
  # Check if all matching paragraphs have been found or if the maximum URL limit has been reached
  if (all_paragraphs_found || length(visited_urls) >= max_urls) {
    return()  # Stop processing if all paragraphs have been found or if maximum URLs reached
  }
  
  # Read the URL file
  urls <- readLines(url_file, warn = FALSE)
  urls <- urls[nchar(trimws(urls)) > 0]  # Remove empty lines
  
  # Read all the keywords and concatenate them into a single regular expression pattern
  keywords <- readLines(keywords_file, warn = FALSE)
  keywords <- keywords[nchar(trimws(keywords)) > 0]  # Remove empty lines
  keyword_pattern <- paste0(keywords, collapse = "|")
  
  for (url in urls) {
    # Check if the URL has already been visited
    if (url %in% visited_urls) {
      next_url <- TRUE  # Set flag to skip the URL
    } else {
      next_url <- FALSE  # Set flag to process the URL
    }
    
    # Move this if statement inside the loop to fix the error
    if (next_url) {
      next  # Skip the URL if it has already been visited
    }
    
    # Add the current URL to the visited URLs
    visited_urls <- c(visited_urls, url)
    # Store the current URL for display
    current_url <- url
    
    # Retrieve the webpage content
    response <- tryCatch(
      GET(url),
      error = function(e) {
        cat("Error retrieving webpage:", url, "\n")
        next  # Skip the current URL if an error occurs during retrieval
      }
    )
    webpage <- content(response, as = "text", encoding = "UTF-8")
    
    # Parse the webpage
    webpage_parsed <- tryCatch(
      read_html(webpage),
      error = function(e) {
        cat("Error parsing webpage:", url, "\n")
        next  # Skip the current URL if an error occurs during parsing
      }
    )
    
    # Extract the data you need from the webpage
    # For example, let's extract all the paragraphs on the page
    paragraphs <- tryCatch(
      html_nodes(webpage_parsed, "p") %>% html_text(),
      error = function(e) {
        cat("Error extracting paragraphs from webpage:", url, "\n")
        return(character())  # Return an empty character vector on error
      }
    )
    
    # Extract subtitles (if any) on the page
    subtitles <- tryCatch(
      html_nodes(webpage_parsed, "h2") %>% html_text(),
      error = function(e) {
        cat("Error extracting subtitles from webpage:", url, "\n")
        return(character())  # Return an empty character vector on error
      }
    )
    
    # Combine paragraphs and subtitles for keyword search
    content_text <- c(paragraphs, subtitles)
    
    # Clean up the content by removing unwanted lines
    clean_content <- gsub("\\\\W.*Privacy Policy", "", content_text)
    clean_content <- trimws(clean_content)
    
    # Write the matching paragraphs to the output file
    matching_content <- grep(keyword_pattern, clean_content, value = TRUE, ignore.case = TRUE)
    matching_content <- setdiff(matching_content, visited_paragraphs)
    
    if (length(matching_content) > 0) {
      # Print the matching paragraphs with serial numbers
      for (i in 1:length(matching_content)) {
        output_line <- paste0("URL: ", current_url, " | Counter: ", counter, " > ", matching_content[i])
        cat(output_line, "\n")
        cat(output_line, file = output_file, append = TRUE, sep = "\n")
        counter <- counter + 1
      }
      
      # Check if all matching paragraphs have been found
      if (length(visited_paragraphs) + length(matching_content) == length(clean_content)) {
        all_paragraphs_found <<- TRUE
      }
    }
    
    # Extract all the hyperlinks on the page
    links <- webpage_parsed %>%
      html_nodes("a") %>%
      html_attr("href")
    
    # Recursively scrape each hyperlink found
    for (link in links) {
      # Check if the link is absolute or relative
      if (!grepl("^https?://", link)) {
        link <- URLencode(paste0(url, link))
      }
      
      # Call the scrape_website function recursively
      scrape_website(link, keywords_file, visited_urls, counter, visited_paragraphs, output_file, all_paragraphs_found, max_urls)
    }
  }
}

# Start crawling from URLs provided in a text file with keywords from another text file
url_file <- "E:/website scrape/url list.txt"  # Path to the text file containing URLs (one per line)
keywords_file <- "E:/website scrape/to find.txt"  # Path to the text file containing keywords (one per line)
output_dir <- "E:/output/"  # Specify the output directory path here
output_file <- file.path(output_dir, paste0("output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
all_paragraphs_found <- FALSE  # Initialize the flag for all paragraphs found
max_urls <- 100  # Maximum number of URLs to be visited
scrape_website(url_file, keywords_file, output_file = output_file, all_paragraphs_found = all_paragraphs_found, max_urls = max_urls)
