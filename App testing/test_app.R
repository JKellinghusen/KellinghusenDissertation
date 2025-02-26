library(shiny)
library(stringr)

ui <- fluidPage(
    titlePanel("Display Text from File"),
    tabsetPanel(
        tabPanel("Build a culture of gratitude and recognition",
            mainPanel(
                htmlOutput("gratitudeContent")
            )
        ),
        tabPanel("Connect individual work with organizational mission",
            mainPanel(
                htmlOutput("missionContent")
            )
        ),
        tabPanel("Connection & Community",
            mainPanel(
                htmlOutput("connectionContent")
            )
        )
    )
)

server <- function(input, output) {
    # Function to create a valid file name from tab name
    create_file_name <- function(tab_name) {
        file_name <- str_replace_all(tab_name, "[^a-zA-Z0-9\\s]", "")
        file_name <- str_trim(file_name)
        file_name <- str_replace_all(file_name, "\\s+", "_")
        return(file_name)
    }
    
    # Function to read the content of a file
    read_file_content <- function(file_path) {
        if (file.exists(file_path)) {
            content <- paste(readLines(file_path, warn = FALSE, encoding = "windows-1252"), collapse = "\n")
            return(HTML(content))
        } else {
            return(HTML("<p>File not found.</p>"))
        }
    }
    
    # Directory path
    dir_path <- "C:/Users/jtorr/OneDrive/DrPH/Dissertation/App testing/Splitter"
    
    # File paths for each tab
    file_paths <- list(
        gratitude = file.path(dir_path, paste0(create_file_name("Build a culture of gratitude and recognition"), ".html")),
        mission = file.path(dir_path, paste0(create_file_name("Connect individual work with organizational mission"), ".html")),
        connection = file.path(dir_path, paste0(create_file_name("Connection & Community"), ".html"))
    )
    
    output$gratitudeContent <- renderUI({
        read_file_content(file_paths$gratitude)
    })
    
    output$missionContent <- renderUI({
        read_file_content(file_paths$mission)
    })
    
    output$connectionContent <- renderUI({
        read_file_content(file_paths$connection)
    })
}

shinyApp(ui = ui, server = server)