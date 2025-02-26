library(xml2)
library(rvest)

# Path to the HTML file
file_path <- "C:/Users/jtorr/OneDrive/DrPH/Dissertation/App testing/filtered test.htm"

# Read the HTML file
html_content <- read_html(file_path)

# Print the contents of the HTML file
print(html_content)