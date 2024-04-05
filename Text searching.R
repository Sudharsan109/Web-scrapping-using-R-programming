# Step 1: Get the list of all text files in the drive
drive_path <- "D:\\SAMPLE\\TEXT\\"
text_files <- list.files(drive_path, pattern = "\\.txt$", full.names = TRUE)

# Step 2: Search for the keywords (case-insensitive search)
keywords <- c("railway systems", "culture")  # Replace with the keywords you want to search for

# Step 3: Preprocess the keywords
processed_keywords <- tolower(trimws(keywords))

# Step 4: Initialize functions for separate and combined search
check_all_keywords_present <- function(paragraph, keywords) {
  all_present <- all(sapply(keywords, function(keyword) grepl(paste0("\\b", keyword, "\\b"), paragraph, ignore.case = TRUE)))
  return(all_present)
}

find_combined_matches <- function(paragraphs, keywords) {
  matching_paragraphs <- character()
  for (i in seq_along(paragraphs)) {
    current_paragraph <- paragraphs[i]
    if (check_all_keywords_present(current_paragraph, keywords)) {
      matching_paragraphs <- c(matching_paragraphs, current_paragraph)
    }
  }
  return(matching_paragraphs)
}

# Step 5: Function to find the matching paragraphs for separate and combined searches
find_matching_paragraphs <- function(paragraphs, keywords) {
  separate_matches <- list()
  combined_matches <- character()
  
  for (keyword in keywords) {
    matching_paragraphs <- grep(paste0("\\b", keyword, "\\b"), paragraphs, value = TRUE, ignore.case = TRUE)
    if (length(matching_paragraphs) > 0) {
      separate_matches[[keyword]] <- matching_paragraphs
    } else {
      separate_matches[[keyword]] <- "Keyword not found in any paragraph."
    }
  }
  
  combined_matches <- find_combined_matches(paragraphs, keywords)
  
  return(list(separate_matches = separate_matches, combined_matches = combined_matches))
}

# ... (previous code)

# Initialize variables for counting files
total_text_files <- 0
files_with_keywords <- 0
files_with_keywords_flag <- FALSE

# Step 6: Loop through each text file and perform the search
for (file_path in text_files) {
  cat("\n\nSearching in file: ", file_path, "\n")
  
  # Read the text file
  text_content <- readLines(file_path)
  
  # Extract paragraphs from the text content
  paragraphs <- NULL
  current_paragraph <- ""
  
  for (line in text_content) {
    if (line != "") {
      current_paragraph <- paste(current_paragraph, line, sep = "\n")
    } else {
      paragraphs <- c(paragraphs, current_paragraph)
      current_paragraph <- ""
    }
  }
  
  # Add the last paragraph (if it exists and not followed by an empty line)
  if (current_paragraph != "") {
    paragraphs <- c(paragraphs, current_paragraph)
  }
  
  # Search for keywords
  matching_paragraphs <- find_matching_paragraphs(paragraphs, processed_keywords)
  
  # Output the paragraphs containing each keyword separately
  for (keyword in processed_keywords) {
    cat("\n\nKeyword: ", keyword, "\n")
    cat(matching_paragraphs$separate_matches[[keyword]], sep = "\n")
    
    # Check if keyword found in any paragraph
    if (!files_with_keywords_flag && length(grep(paste0("\\b", keyword, "\\b"), paragraphs, ignore.case = TRUE)) > 0) {
      files_with_keywords <- files_with_keywords + 1
      files_with_keywords_flag <- TRUE
    }
  }
  
  # Output the paragraphs containing all keywords combinedly
  cat("\n\nParagraphs containing all keywords:\n")
  if (length(matching_paragraphs$combined_matches) > 0) {
    cat(matching_paragraphs$combined_matches, sep = "\n")
    
    # Check if all keywords found in a paragraph
    if (!files_with_keywords_flag && length(find_combined_matches(paragraphs, processed_keywords)) > 0) {
      files_with_keywords <- files_with_keywords + 1
      files_with_keywords_flag <- TRUE
    }
  } else {
    cat("Keywords not found in any paragraph.")
  }
  
  # Increase the total text files count
  total_text_files <- total_text_files + 1
  
  # Reset the keyword found flag for the next file
  files_with_keywords_flag <- FALSE
}

# Output the total count of text files and files containing keywords
cat("\n\nTotal Text Files in the Drive: ", total_text_files, "\n")
cat("Files Containing Keywords: ", files_with_keywords, "\n")
