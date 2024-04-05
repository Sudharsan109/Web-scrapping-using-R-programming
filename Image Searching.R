library(rvest)
library(stringr)
library(httr)

# Function to scan a website for images with alt text related to a specific word and download the matched images
scan_website_and_download_images <- function(url, target_word, save_path) {
  # Number of times to retry the connection
  num_retries <- 3
  
  for (retry_count in 1:num_retries) {
    tryCatch({
      # Read the HTML content of the website
      webpage <- read_html(url, timeout(20))
      
      # Extract all images from the website
      images <- html_nodes(webpage, "img")
      
      # Number of matched images downloaded
      downloaded_images <- 0
      
      # Create a folder based on the current date and time
      current_datetime <- Sys.time()
      folder_name <- format(current_datetime, "%Y-%m-%d_%H-%M-%S")
      folder_path <- file.path(save_path, folder_name)
      dir.create(folder_path)
      
      # Create a file to store the matching image details
      txt_file_path <- file.path(folder_path, "matching_images.txt")
      txt_file <- file(txt_file_path, "w")
      
      for (image in images) {
        # Extract the alt attribute of the image
        alt_text <- html_attr(image, "alt")
        
        # Check if the alt text contains the target word
        if (!is.na(alt_text) && str_detect(tolower(alt_text), fixed(tolower(target_word)))) {
          # Extract the image source URL
          image_url <- html_attr(image, "src")
          
          # Download the image
          response <- httr::GET(image_url)
          image_filename <- paste0("image_", downloaded_images + 1, ".jpg")
          image_filepath <- file.path(folder_path, image_filename)
          writeBin(httr::content(response, "raw"), image_filepath)
          
          downloaded_images <- downloaded_images + 1
          
          # Format the image details
          image_details <- paste("IMG|", image_url, "|", downloaded_images, ">", alt_text, "|")
          
          # Write the formatted image details to the text file
          cat(image_details, file = txt_file, append = TRUE)
          cat("\n\n", file = txt_file, append = TRUE)
        }
      }
      
      # Close the text file
      close(txt_file)
      
      # Return the number of downloaded images
      return(downloaded_images)
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      if (retry_count < num_retries) {
        cat("Retrying connection...\n")
      } else {
        cat("Connection could not be established after multiple retries.\n")
      }
      Sys.sleep(2)  # Wait for 2 seconds before retrying
    })
  }
  
  # Return 0 downloaded images if the connection fails
  return(0)
}

# Specify the URL of the website
url <- "https://thefactfile.org/facts-about-india/"

# Specify the target word to search for
target_word <- "5491"

# Specify the directory where the images folder will be created
save_path <- "D:/image"

# Call the function to scan the website for images with alt text related to the target word and download the matched images
downloaded_images <- scan_website_and_download_images(url, target_word, save_path)

if (downloaded_images > 0) {
  cat("Number of downloaded images:", downloaded_images, "\n")
  cat("Execution completed successfully.\n")
} else {
  cat("No matching images found.\n")
}
