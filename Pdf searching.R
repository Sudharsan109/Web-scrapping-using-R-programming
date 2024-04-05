# Load required libraries
library(pdftools)

# Function to read text from a PDF file
read_pdf_file <- function(file_path) {
  text <- pdf_text(file_path)
  return(text)
}

# Function to find paragraphs containing any of the keywords
find_keyword_matches <- function(paragraphs, keyword) {
  keyword_matches <- character(0)
  for (paragraph in paragraphs) {
    processed_paragraph <- tolower(trimws(paragraph))
    if (grepl(paste0("\\b", keyword, "\\b"), processed_paragraph)) {
      keyword_matches <- c(keyword_matches, paragraph)
    }
  }
  return(keyword_matches)
}

# Step 1: List all PDF files in the drive
drive_path <- " "  # Replace with your drive path
pdf_files <- list.files(drive_path, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)

# Step 2: Search for the keywords in PDF files
keywords <- c(" ", " ")  # Replace with the keywords you want to search for

# Output the results to a text file
output_dir <- " "  # Specify the output directory path here
output_file <- file.path(output_dir, paste0("output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
output_conn <- file(output_file, open = "wt")
sink(output_conn, type = "output")

cat("\n\nTotal PDF Files in the Drive: ", length(pdf_files), "\n")

# Step 3: Loop through each PDF file and perform the search
for (file_path in pdf_files) {
  # Read the PDF file
  text_content <- read_pdf_file(file_path)
  
  # Skip files with empty content
  if (length(text_content) == 0) {
    next
  }
  
  # Extract paragraphs from the text content
  paragraphs <- unlist(strsplit(text_content, "\n\n"))
  
  # Search for keywords
  for (keyword in keywords) {
    keyword_matches <- find_keyword_matches(paragraphs, keyword)
    
    # Output the paragraphs containing the keyword
    if (length(keyword_matches) > 0) {
      cat("\n\nPDF File: ", file_path, "\n")
      cat("Keyword '", keyword, "' found in this PDF: ", length(keyword_matches), " times\n\n")
      
      # Output matching paragraphs
      cat("Matching paragraphs:\n")
      for (i in seq_along(keyword_matches)) {
        cat("\nParagraph ", i, ":\n")
        cat(keyword_matches[i], "\n")
      }
    }
  }
}

# Reset the standard output
sink(type = "message")
close(output_conn)
