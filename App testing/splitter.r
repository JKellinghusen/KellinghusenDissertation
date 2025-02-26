# library(rvest)
# library(stringr)

# # Define the file path
# file_path <- "C:/Users/jtorr/OneDrive/DrPH/Dissertation/App testing/Splitter/appendix-f-recommendation-text.html"

# # Read the HTML file
# html_content <- read_html(file_path)

# # Extract all h1 and h2 nodes
# headers <- html_content %>% html_nodes(xpath = "//h1 | //h2")

# # Function to create a valid file name from header text
# create_file_name <- function(header_text) {
#   # Remove invalid characters and trim whitespace
#   file_name <- str_replace_all(header_text, "[^a-zA-Z0-9\\s]", "")
#   file_name <- str_trim(file_name)
#   file_name <- str_replace_all(file_name, "\\s+", "_")
#   return(file_name)
# }

# # Loop through each header and create a new file
# for (i in seq_along(headers)) {
#   header <- headers[i]
#   header_text <- html_text(header, trim = TRUE)
#   file_name <- create_file_name(header_text)
  
#   # Get the content until the next h1 or h2 header
#   if (i < length(headers)) {
#     next_header <- headers[i + 1]
#     content <- html_content %>% html_nodes(xpath = paste0("//*[preceding-sibling::", header %>% html_name(), " and following-sibling::", next_header %>% html_name(), "]"))
#   } else {
#     content <- html_content %>% html_nodes(xpath = paste0("//*[preceding-sibling::", header %>% html_name(), "]"))
#   }
  
#   # Combine the header and its content
#   section_content <- paste0(as.character(header), paste0(as.character(content), collapse = ""))
  
#   # Write the section content to a new file in the same directory as the source file
#   output_file_path <- file.path(dirname(file_path), paste0(file_name, ".html"))
#   writeLines(section_content, output_file_path)
# }