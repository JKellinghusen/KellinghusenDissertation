library(shiny)
library(shinydashboard)
library(ggplot2)
library(stringr)  # Load the stringr package for str_wrap function
library(plotly)  # Load the plotly package
library(htmltools)  # Load the htmltools package
library(rvest)
library(xml2)

# Declare global variables to avoid binding issues
globalVariables(c("combined_df", "Variable", "Value", "protection_from_harm", "connection_and_community", "work_life_harmony", "mattering_at_work", "opportunity_for_growth", "Component", "Average"))

# Source the R Data Processor script to pull in variables
source("C:/Users/jtorr/OneDrive/DrPH/Dissertation/R Data Analysis/R Data Processor.r")

# Print the essentials_averages list for debugging
print(essentials_averages)

# Define UI for the application
ui <- dashboardPage(
  dashboardHeader(disable = TRUE),  # Disable the default header
  dashboardSidebar(
    sidebarMenu(id = "sidebarMenu",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Protection from Harm", tabName = "protection", icon = icon("shield-alt"),
        menuSubItem(tags$b("Protection from Harm"), tabName = "protection"),
        menuSubItem(str_wrap("Physical safety", width = 20), tabName = "physical_safety"),
        menuSubItem(str_wrap("Psychological safety", width = 20), tabName = "psychological_safety"),
        menuSubItem(str_wrap("Adequate rest", width = 20), tabName = "adequate_rest"),
        menuSubItem(str_wrap("Normalize mental health", width = 20), tabName = "mental_health"),
        menuSubItem(str_wrap("Operationalize DEIA", width = 20), tabName = "deia")
      ),
      menuItem("Connection & Community", tabName = "connection", icon = icon("users"),
        menuSubItem(tags$b("Connection & Community"), tabName = "connection"),
        menuSubItem(str_wrap("Cultures of inclusion", width = 20), tabName = "inclusion"),
        menuSubItem(str_wrap("Trusted relationships", width = 20), tabName = "trusted_relationships"),
        menuSubItem(str_wrap("Collaboration and teamwork", width = 20), tabName = "collaboration")
      ),
      menuItem("Work-Life Harmony", tabName = "worklife", icon = icon("balance-scale"),
        menuSubItem(tags$b("Work-Life Harmony"), tabName = "worklife"),
        menuSubItem(str_wrap("Autonomy", width = 20), tabName = "autonomy"),
        menuSubItem(str_wrap("Flexibility", width = 20), tabName = "flexible_schedules"),
        menuSubItem(str_wrap("Paid leave", width = 20), tabName = "paid_leave"),
        menuSubItem(str_wrap("Respect boundaries", width = 20), tabName = "boundaries")
      ),
      menuItem("Mattering at Work", tabName = "mattering", icon = icon("briefcase"),
        menuSubItem(tags$b("Mattering at Work"), tabName = "mattering"),
        menuSubItem(str_wrap("Living wage", width = 20), tabName = "living_wage"),
        menuSubItem(str_wrap("Workplace decisions", width = 20), tabName = "engage_workers"),
        menuSubItem(str_wrap("Culture of gratitude", width = 20), tabName = "gratitude"),
        menuSubItem(str_wrap("Connect work mission", width = 20), tabName = "mission")
      ),
      menuItem("Opportunity for Growth", tabName = "growth", icon = icon("chart-line"),
        menuSubItem(tags$b("Opportunity for Growth"), tabName = "growth"),
        menuSubItem(str_wrap("Training and mentoring", width = 20), tabName = "training"),
        menuSubItem(str_wrap("Career advancement", width = 20), tabName = "career_advancement"),
        menuSubItem(str_wrap("Reciprocal feedback", width = 20), tabName = "feedback")
      )
    )
  ),  
  dashboardBody(
    fluidRow(
      box(title = "Workplace Wellness Assessment Dashboard", width = 12, status = "primary", solidHeader = TRUE)
    ),
    tabItems(
  tabItem(tabName = "home",
    fluidRow(
      box(title = "Protection from Harm", width = 3, height = 250, status = ifelse(!is.null(essentials_averages[["Protection from Harm"]]) && essentials_averages[["Protection from Harm"]] >= 0.6, "success", ifelse(!is.null(essentials_averages[["Protection from Harm"]]) && essentials_averages[["Protection from Harm"]] >= 0.4, "warning", "danger")), solidHeader = TRUE, 
      icon("shield-alt", class = "box-icon"), 
      class = "clickable-box", onclick = "Shiny.onInputChange('protection_btn', Math.random())",
      style = ifelse(!is.null(essentials_averages[["Protection from Harm"]]) && essentials_averages[["Protection from Harm"]] >= 0.6, "background-color: #dff0d8; border: 2px solid #3c763d; border-radius: 10px;", ifelse(!is.null(essentials_averages[["Protection from Harm"]]) && essentials_averages[["Protection from Harm"]] >= 0.4, "background-color: #fcf8e3; border: 2px solid #8a6d3b; border-radius: 10px;", "background-color: #f2dede; border: 2px solid #a94442; border-radius: 10px;")),
      p(tags$br(), tags$span(style = "font-size: 16px;", "Score:"), ifelse(!is.null(essentials_averages[["Protection from Harm"]]), ifelse(essentials_averages[["Protection from Harm"]] >= 0.6, "High", ifelse(essentials_averages[["Protection from Harm"]] >= 0.4, "Medium", "Low")), "N/A"))
      ),
      box(title = "Connection & Community", width = 3, height = 250, status = ifelse(!is.null(essentials_averages[["Connection & Community"]]) && essentials_averages[["Connection & Community"]] >= 0.6, "success", ifelse(!is.null(essentials_averages[["Connection & Community"]]) && essentials_averages[["Connection & Community"]] >= 0.4, "warning", "danger")), solidHeader = TRUE, 
      icon("users", class = "box-icon"), 
      class = "clickable-box", onclick = "Shiny.onInputChange('connection_btn', Math.random())",
      style = ifelse(!is.null(essentials_averages[["Connection & Community"]]) && essentials_averages[["Connection & Community"]] >= 0.6, "background-color: #dff0d8; border: 2px solid #3c763d; border-radius: 10px;", ifelse(!is.null(essentials_averages[["Connection & Community"]]) && essentials_averages[["Connection & Community"]] >= 0.4, "background-color: #fcf8e3; border: 2px solid #8a6d3b; border-radius: 10px;", "background-color: #f2dede; border: 2px solid #a94442; border-radius: 10px;")),
      p(tags$br(), tags$span(style = "font-size: 16px;", "Score:"), ifelse(!is.null(essentials_averages[["Connection & Community"]]), ifelse(essentials_averages[["Connection & Community"]] >= 0.6, "High", ifelse(essentials_averages[["Connection & Community"]] >= 0.4, "Medium", "Low")), "N/A"))
      ),
      box(title = "Work-Life Harmony", width = 3, height = 250, status = ifelse(!is.null(essentials_averages[["Work-Life Harmony"]]) && essentials_averages[["Work-Life Harmony"]] >= 0.6, "success", ifelse(!is.null(essentials_averages[["Work-Life Harmony"]]) && essentials_averages[["Work-Life Harmony"]] >= 0.4, "warning", "danger")), solidHeader = TRUE, 
      icon("balance-scale", class = "box-icon"), 
      class = "clickable-box", onclick = "Shiny.onInputChange('worklife_btn', Math.random())",
      style = ifelse(!is.null(essentials_averages[["Work-Life Harmony"]]) && essentials_averages[["Work-Life Harmony"]] >= 0.6, "background-color: #dff0d8; border: 2px solid #3c763d; border-radius: 10px;", ifelse(!is.null(essentials_averages[["Work-Life Harmony"]]) && essentials_averages[["Work-Life Harmony"]] >= 0.4, "background-color: #fcf8e3; border: 2px solid #8a6d3b; border-radius: 10px;", "background-color: #f2dede; border: 2px solid #a94442; border-radius: 10px;")),
      p(tags$br(), tags$span(style = "font-size: 16px;", "Score:"), ifelse(!is.null(essentials_averages[["Work-Life Harmony"]]), ifelse(essentials_averages[["Work-Life Harmony"]] >= 0.6, "High", ifelse(essentials_averages[["Work-Life Harmony"]] >= 0.4, "Medium", "Low")), "N/A"))
      ),
      box(title = "Mattering at Work", width = 3, height = 250, status = ifelse(!is.null(essentials_averages[["Mattering at Work"]]) && essentials_averages[["Mattering at Work"]] >= 0.6, "success", ifelse(!is.null(essentials_averages[["Mattering at Work"]]) && essentials_averages[["Mattering at Work"]] >= 0.4, "warning", "danger")), solidHeader = TRUE, 
      icon("briefcase", class = "box-icon"), 
      class = "clickable-box", onclick = "Shiny.onInputChange('mattering_btn', Math.random())",
      style = ifelse(!is.null(essentials_averages[["Mattering at Work"]]) && essentials_averages[["Mattering at Work"]] >= 0.6, "background-color: #dff0d8; border: 2px solid #3c763d; border-radius: 10px;", ifelse(!is.null(essentials_averages[["Mattering at Work"]]) && essentials_averages[["Mattering at Work"]] >= 0.4, "background-color: #fcf8e3; border: 2px solid #8a6d3b; border-radius: 10px;", "background-color: #f2dede; border: 2px solid #a94442; border-radius: 10px;")),
      p(tags$br(), tags$span(style = "font-size: 16px;", "Score:"), ifelse(!is.null(essentials_averages[["Mattering at Work"]]), ifelse(essentials_averages[["Mattering at Work"]] >= 0.6, "High", ifelse(essentials_averages[["Mattering at Work"]] >= 0.4, "Medium", "Low")), "N/A"))
      ),
      box(title = "Opportunity for Growth", width = 3, height = 250, status = ifelse(!is.null(essentials_averages[["Opportunity for Growth"]]) && essentials_averages[["Opportunity for Growth"]] >= 0.6, "success", ifelse(!is.null(essentials_averages[["Opportunity for Growth"]]) && essentials_averages[["Opportunity for Growth"]] >= 0.4, "warning", "danger")), solidHeader = TRUE, 
      icon("chart-line", class = "box-icon"), 
      class = "clickable-box", onclick = "Shiny.onInputChange('growth_btn', Math.random())",
      style = ifelse(!is.null(essentials_averages[["Opportunity for Growth"]]) && essentials_averages[["Opportunity for Growth"]] >= 0.6, "background-color: #dff0d8; border: 2px solid #3c763d; border-radius: 10px;", ifelse(!is.null(essentials_averages[["Opportunity for Growth"]]) && essentials_averages[["Opportunity for Growth"]] >= 0.4, "background-color: #fcf8e3; border: 2px solid #8a6d3b; border-radius: 10px;", "background-color: #f2dede; border: 2px solid #a94442; border-radius: 10px;")),
      p(tags$br(), tags$span(style = "font-size: 16px;", "Score:"), ifelse(!is.null(essentials_averages[["Opportunity for Growth"]]), ifelse(essentials_averages[["Opportunity for Growth"]] >= 0.6, "High", ifelse(essentials_averages[["Opportunity for Growth"]] >= 0.4, "Medium", "Low")), "N/A"))
      )
    )
  ),
  tabItem(tabName = "protection",
    fluidRow(
      box(title = "Protection from Harm", width = 12, status = "primary", solidHeader = TRUE,
        plotlyOutput("protectionChart"),
      )
    ),
    htmlOutput("protectionContent")
  ),
  tabItem(tabName = "physical_safety",
    fluidRow(
      box(title = "Physical Safety", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("physicalSafetyTable")
      )
    ),
    htmlOutput("physicalSafetyContent")
  ),
  tabItem(tabName = "psychological_safety",
    fluidRow(
      box(title = "Psychological Safety", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("psychologicalSafetyTable")
      )
    ),
    htmlOutput("psychologicalSafetyContent")
  ),
  tabItem(tabName = "adequate_rest",
    fluidRow(
      box(title = "Adequate Rest", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("adequateRestTable")
      )
    ),
    htmlOutput("adequateRestContent")
  ),
  tabItem(tabName = "mental_health",
    fluidRow(
      box(title = "Mental Health", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("mentalHealthTable")
      )
    ),
    htmlOutput("mentalHealthContent")
  ),
  tabItem(tabName = "deia",
    fluidRow(
      box(title = "DEIA", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("deiaTable")
      )
    ),
    htmlOutput("deiaContent")
  ),
  tabItem(tabName = "connection",
    fluidRow(
      box(title = "Connection & Community", width = 12, status = "primary", solidHeader = TRUE,
        plotlyOutput("connectionChart"),
      )
    ),
    htmlOutput("connectionContent")
  ),
  tabItem(tabName = "inclusion",
    fluidRow(
      box(title = "Inclusion", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("inclusionTable")
      )
    ),
    htmlOutput("inclusionContent")
  ),
  tabItem(tabName = "trusted_relationships",
    fluidRow(
      box(title = "Trusted Relationships", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("trustTable")
      )
    ),
    htmlOutput("trustedRelationshipsContent")
  ),
  tabItem(tabName = "collaboration",
    fluidRow(
      box(title = "Collaboration", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("collaborationTable")
      )
    ),
    htmlOutput("collaborationContent")
  ),
  tabItem(tabName = "worklife",
    fluidRow(
      box(title = "Work-Life Harmony", width = 12, status = "primary", solidHeader = TRUE,
        plotlyOutput("worklifeChart"),
      )
    ),
    htmlOutput("worklifeContent")
  ),
  tabItem(tabName = "autonomy",
    fluidRow(
      box(title = "Autonomy", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("autonomyTable")
      )
    ),
    htmlOutput("autonomyContent")
  ),
  tabItem(tabName = "flexible_schedules",
    fluidRow(
      box(title = "Flexible Schedules", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("flexibilityTable")
      )
    ),
    htmlOutput("flexibleSchedulesContent")
  ),
  tabItem(tabName = "paid_leave",
    fluidRow(
      box(title = "Paid Leave", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("paidLeaveTable")
      )
    ),
    htmlOutput("paidLeaveContent")
  ),
  tabItem(tabName = "boundaries",
    fluidRow(
      box(title = "Boundaries", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("boundariesTable")
      )
    ),
    htmlOutput("boundariesContent")
  ),
  tabItem(tabName = "mattering",
    fluidRow(
      box(title = "Mattering at Work", width = 12, status = "primary", solidHeader = TRUE,
        plotlyOutput("matteringChart"),
      )
    ),
    htmlOutput("matteringContent")
  ),
  tabItem(tabName = "living_wage",
    fluidRow(
      box(title = "Living Wage", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("livingWageTable")
      )
    ),
    htmlOutput("livingWageContent")
  ),
  tabItem(tabName = "engage_workers",
    fluidRow(
      box(title = "Workplace Decisions", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("workplaceDecisionsTable")
      )
    ),
    htmlOutput("engageWorkersContent")
  ),
  tabItem(tabName = "gratitude",
    fluidRow(
      box(title = "Gratitude", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("gratitudeTable")
      )
    ),
    htmlOutput("gratitudeContent")
  ),
  tabItem(tabName = "mission",
    fluidRow(
      box(title = "Mission", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("missionTable")
      )
    ),
    htmlOutput("missionContent")
  ),
  tabItem(tabName = "growth",
    fluidRow(
      box(title = "Opportunity for Growth", width = 12, status = "primary", solidHeader = TRUE,
        plotlyOutput("growthChart"),
      )
    ),
    htmlOutput("growthContent")
  ),
  tabItem(tabName = "training",
    fluidRow(
      box(title = "Training", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("trainingTable")
      )
    ),
    htmlOutput("trainingContent")
  ),
  tabItem(tabName = "career_advancement",
    fluidRow(
      box(title = "Career Advancement", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("advancementTable")
      )
    ),
    htmlOutput("careerAdvancementContent")
  ),
  tabItem(tabName = "feedback",
    fluidRow(
      box(title = "Feedback", width = 12, status = "primary", solidHeader = TRUE,
        tableOutput("feedbackTable")
      )
    ),
    htmlOutput("feedbackContent")
  )
    ),
    tags$head(
      tags$style(HTML("
        .clickable-box { cursor: pointer; text-align: center; }
        .box-title { text-align: center; }
        .box-icon { font-size: 50px; margin-top: 20px; }
      "))
    ),
    
  )
)

# Define server logic for the application
server <- function(input, output, session) {
  # Observe button clicks and update the tab items accordingly
  observeEvent(input$protection_btn, {
    updateTabItems(session, "sidebarMenu", "protection")
  })
  observeEvent(input$connection_btn, {
    updateTabItems(session, "sidebarMenu", "connection")
  })
  observeEvent(input$worklife_btn, {
    updateTabItems(session, "sidebarMenu", "worklife")
  })
  observeEvent(input$mattering_btn, {
    updateTabItems(session, "sidebarMenu", "mattering")
  })
  observeEvent(input$growth_btn, {
    updateTabItems(session, "sidebarMenu", "growth")
  })
  
  # Observe sidebar menu clicks and update the tab items accordingly
  observeEvent(input$sidebarMenu, {
    tabName <- input$sidebarMenu
    if (tabName %in% c("protection", "connection", "worklife", "mattering", "growth")) {
      updateTabItems(session, "sidebarMenu", tabName)
    }
  })

  # Convert Essentials variables to data frames
  protection_df <- data.frame(Component = names(protection_from_harm), Average = unlist(protection_from_harm))
  connection_df <- data.frame(Component = names(connection_and_community), Average = unlist(connection_and_community))
  worklife_df <- data.frame(Component = names(work_life_harmony), Average = unlist(work_life_harmony))
  mattering_df <- data.frame(Component = names(mattering_at_work), Average = unlist(mattering_at_work))
  growth_df <- data.frame(Component = names(opportunity_for_growth), Average = unlist(opportunity_for_growth))


  # Render bar plots for each Essential's components and their averages
  output$protectionChart <- renderPlotly({
    p <- ggplot(protection_df, aes(x = Component, y = Average, fill = Component)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Average, 2)), vjust = 1.5, size = 5, fontface = "bold") +
      theme_minimal() +
      labs(x = "Component", y = "Average") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none", plot.title = element_blank()) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))
    ggplotly(p, tooltip = "none")
  })

  output$connectionChart <- renderPlotly({
    p <- ggplot(connection_df, aes(x = Component, y = Average, fill = Component)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Average, 2)), vjust = 1.5, size = 5, fontface = "bold") +
      theme_minimal() +
      labs(x = "Component", y = "Average") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none", plot.title = element_blank()) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    ggplotly(p, tooltip = "none")
  })

  output$worklifeChart <- renderPlotly({
    p <- ggplot(worklife_df, aes(x = Component, y = Average, fill = Component)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Average, 2)), vjust = 1.5, size = 5, fontface = "bold") +
      theme_minimal() +
      labs(x = "Component", y = "Average") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none", plot.title = element_blank()) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    ggplotly(p, tooltip = "none")
  })

  output$matteringChart <- renderPlotly({
    p <- ggplot(mattering_df, aes(x = Component, y = Average, fill = Component)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Average, 2)), vjust = 1.5, size = 5, fontface = "bold") +
      theme_minimal() +
      labs(x = "Component", y = "Average") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none", plot.title = element_blank()) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    ggplotly(p, tooltip = "none")
  })

  output$growthChart <- renderPlotly({
    p <- ggplot(growth_df, aes(x = Component, y = Average, fill = Component)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Average, 2)), vjust = 1.5, size = 5, fontface = "bold") +
      theme_minimal() +
      labs(x = "Component", y = "Average") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none", plot.title = element_blank()) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    ggplotly(p, tooltip = "none")
  })


  # Render the table for Physical Safety
  output$physicalSafetyTable <- renderTable({
    physical_safety_questions <- components[["Prioritize workplace physical safety"]]
    combined_df[combined_df$Question %in% physical_safety_questions, ]
  })

  # Render the table for Psychological Safety
  output$psychologicalSafetyTable <- renderTable({
    psychological_safety_questions <- components[["Prioritize workplace psychological safety"]]
    combined_df[combined_df$Question %in% psychological_safety_questions, ]
  })

  # Render the table for Adequate Rest
  output$adequateRestTable <- renderTable({
    adequate_rest_questions <- components[["Enable adequate rest"]]
    combined_df[combined_df$Question %in% adequate_rest_questions, ]
  })

  # Render the table for Mental Health
  output$mentalHealthTable <- renderTable({
    mental_health_questions <- components[["Normalize and support mental health"]]
    combined_df[combined_df$Question %in% mental_health_questions, ]
  })

  # Render the table for DEIA
  output$deiaTable <- renderTable({
    deia_questions <- components[["Operationalize DEIA* norms, policies, and programs"]]
    combined_df[combined_df$Question %in% deia_questions, ]
  })

  # Render the table for Inclusion
  output$inclusionTable <- renderTable({
    inclusion_questions <- components[["Create cultures of inclusion and belonging"]]
    combined_df[combined_df$Question %in% inclusion_questions, ]
  })

  # Render the table for Trust
  output$trustTable <- renderTable({
    trust_questions <- components[["Cultivate trusted relationships"]]
    combined_df[combined_df$Question %in% trust_questions, ]
  })

  # Render the table for Collaboration
  output$collaborationTable <- renderTable({
    collaboration_questions <- components[["Foster collaboration and teamwork"]]
    combined_df[combined_df$Question %in% collaboration_questions, ]
  })

  # Render the table for Autonomy
  output$autonomyTable <- renderTable({
    autonomy_questions <- components[["Provide more autonomy over how work is done"]]
    combined_df[combined_df$Question %in% autonomy_questions, ]
  })

  # Render the table for Flexibility
  output$flexibilityTable <- renderTable({
    flexibility_questions <- components[["Make schedules as flexible and predictable as possible"]]
    combined_df[combined_df$Question %in% flexibility_questions, ]
  })

  # Render the table for Paid Leave
  output$paidLeaveTable <- renderTable({
    paid_leave_questions <- components[["Increase access to paid leave"]]
    combined_df[combined_df$Question %in% paid_leave_questions, ]
  })

  # Render the table for Boundaries
  output$boundariesTable <- renderTable({
    boundaries_questions <- components[["Respect boundaries between work and non-work time"]]
    combined_df[combined_df$Question %in% boundaries_questions, ]
  })

  # Render the table for Living Wage
  output$livingWageTable <- renderTable({
    living_wage_questions <- components[["Provide a living wage"]]
    combined_df[combined_df$Question %in% living_wage_questions, ]
  })

  # Render the table for Workplace Decisions
  output$workplaceDecisionsTable <- renderTable({
    workplace_decisions_questions <- components[["Engage workers in workplace decisions"]]
    combined_df[combined_df$Question %in% workplace_decisions_questions, ]
  })

  # Render the table for Gratitude
  output$gratitudeTable <- renderTable({
    gratitude_questions <- components[["Build a culture of gratitude and recognition"]]
    combined_df[combined_df$Question %in% gratitude_questions, ]
  })

  # Render the table for Mission
  output$missionTable <- renderTable({
    mission_questions <- components[["Connect individual work with organizational mission"]]
    combined_df[combined_df$Question %in% mission_questions, ]
  })

  # Render the table for Training
  output$trainingTable <- renderTable({
    training_questions <- components[["Offer quality training, education, and mentoring"]]
    combined_df[combined_df$Question %in% training_questions, ]
  })

  # Render the table for Advancement
  output$advancementTable <- renderTable({
    advancement_questions <- components[["Foster clear, equitable pathways for career advancement"]]
    combined_df[combined_df$Question %in% advancement_questions, ]
  })

  # Render the table for Feedback
  output$feedbackTable <- renderTable({
    feedback_questions <- components[["Ensure relevant, reciprocal feedback"]]
    combined_df[combined_df$Question %in% feedback_questions, ]
  })
  
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
    protection = file.path(dir_path, paste0(create_file_name("Protection from Harm"), ".html")),
    physical_safety = file.path(dir_path, paste0(create_file_name("Prioritize workplace physical safety"), ".html")),
    psychological_safety = file.path(dir_path, paste0(create_file_name("Prioritize workplace psychological safety"), ".html")),
    adequate_rest = file.path(dir_path, paste0(create_file_name("Enable adequate rest"), ".html")),
    mental_health = file.path(dir_path, paste0(create_file_name("Normalize and support mental health"), ".html")),
    deia = file.path(dir_path, paste0(create_file_name("Operationalize DEIA norms, policies, and programs"), ".html")),
    connection = file.path(dir_path, paste0(create_file_name("Connection & Community"), ".html")),
    inclusion = file.path(dir_path, paste0(create_file_name("Create cultures of inclusion and belonging"), ".html")),
    trusted_relationships = file.path(dir_path, paste0(create_file_name("Cultivate trusted relationships"), ".html")),
    collaboration = file.path(dir_path, paste0(create_file_name("Foster collaboration and teamwork"), ".html")),
    worklife = file.path(dir_path, paste0(create_file_name("Work-Life Harmony"), ".html")),
    autonomy = file.path(dir_path, paste0(create_file_name("Provide more autonomy over how work is done"), ".html")),
    flexible_schedules = file.path(dir_path, paste0(create_file_name("Make schedules as flexible and predictable as possible"), ".html")),
    paid_leave = file.path(dir_path, paste0(create_file_name("Increase access to paid leave"), ".html")),
    boundaries = file.path(dir_path, paste0(create_file_name("Respect boundaries between work and non-work time"), ".html")),
    mattering = file.path(dir_path, paste0(create_file_name("Mattering at Work"), ".html")),
    living_wage = file.path(dir_path, paste0(create_file_name("Provide a living wage"), ".html")),
    engage_workers = file.path(dir_path, paste0(create_file_name("Engage workers in workplace decisions"), ".html")),
    gratitude = file.path(dir_path, paste0(create_file_name("Build a culture of gratitude and recognition"), ".html")),
    mission = file.path(dir_path, paste0(create_file_name("Connect individual work with organizational mission"), ".html")),
    growth = file.path(dir_path, paste0(create_file_name("Opportunity for Growth"), ".html")),
    training = file.path(dir_path, paste0(create_file_name("Offer quality training, education, and mentoring"), ".html")),
    career_advancement = file.path(dir_path, paste0(create_file_name("Foster clear, equitable pathways for career advancement"), ".html")),
    feedback = file.path(dir_path, paste0(create_file_name("Ensure relevant, reciprocal feedback"), ".html"))
  )
  
  output$protectionContent <- renderUI({
    read_file_content(file_paths$protection)
  })
  
  output$physicalSafetyContent <- renderUI({
    read_file_content(file_paths$physical_safety)
  })
  
  output$psychologicalSafetyContent <- renderUI({
    read_file_content(file_paths$psychological_safety)
  })
  
  output$adequateRestContent <- renderUI({
    read_file_content(file_paths$adequate_rest)
  })
  
  output$mentalHealthContent <- renderUI({
    read_file_content(file_paths$mental_health)
  })
  
  output$deiaContent <- renderUI({
    read_file_content(file_paths$deia)
  })
  
  output$connectionContent <- renderUI({
    read_file_content(file_paths$connection)
  })
  
  output$inclusionContent <- renderUI({
    read_file_content(file_paths$inclusion)
  })
  
  output$trustedRelationshipsContent <- renderUI({
    read_file_content(file_paths$trusted_relationships)
  })
  
  output$collaborationContent <- renderUI({
    read_file_content(file_paths$collaboration)
  })
  
  output$worklifeContent <- renderUI({
    read_file_content(file_paths$worklife)
  })
  
  output$autonomyContent <- renderUI({
    read_file_content(file_paths$autonomy)
  })
  
  output$flexibleSchedulesContent <- renderUI({
    read_file_content(file_paths$flexible_schedules)
  })
  
  output$paidLeaveContent <- renderUI({
    read_file_content(file_paths$paid_leave)
  })
  
  output$boundariesContent <- renderUI({
    read_file_content(file_paths$boundaries)
  })
  
  output$matteringContent <- renderUI({
    read_file_content(file_paths$mattering)
  })
  
  output$livingWageContent <- renderUI({
    read_file_content(file_paths$living_wage)
  })
  
  output$engageWorkersContent <- renderUI({
    read_file_content(file_paths$engage_workers)
  })
  
  output$gratitudeContent <- renderUI({
    read_file_content(file_paths$gratitude)
  })
  
  output$missionContent <- renderUI({
    read_file_content(file_paths$mission)
  })
  
  output$growthContent <- renderUI({
    read_file_content(file_paths$growth)
  })
  
  output$trainingContent <- renderUI({
    read_file_content(file_paths$training)
  })
  
  output$careerAdvancementContent <- renderUI({
    read_file_content(file_paths$career_advancement)
  })
  
  output$feedbackContent <- renderUI({
    read_file_content(file_paths$feedback)
  })
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
    protection = file.path(dir_path, paste0(create_file_name("Protection from Harm"), ".html")),
    physical_safety = file.path(dir_path, paste0(create_file_name("Prioritize workplace physical safety"), ".html")),
    psychological_safety = file.path(dir_path, paste0(create_file_name("Prioritize workplace psychological safety"), ".html")),
    adequate_rest = file.path(dir_path, paste0(create_file_name("Enable adequate rest"), ".html")),
    mental_health = file.path(dir_path, paste0(create_file_name("Normalize and support mental health"), ".html")),
    deia = file.path(dir_path, paste0(create_file_name("Operationalize DEIA norms, policies, and programs"), ".html")),
    connection = file.path(dir_path, paste0(create_file_name("Connection & Community"), ".html")),
    inclusion = file.path(dir_path, paste0(create_file_name("Create cultures of inclusion and belonging"), ".html")),
    trusted_relationships = file.path(dir_path, paste0(create_file_name("Cultivate trusted relationships"), ".html")),
    collaboration = file.path(dir_path, paste0(create_file_name("Foster collaboration and teamwork"), ".html")),
    worklife = file.path(dir_path, paste0(create_file_name("Work-Life Harmony"), ".html")),
    autonomy = file.path(dir_path, paste0(create_file_name("Provide more autonomy over how work is done"), ".html")),
    flexible_schedules = file.path(dir_path, paste0(create_file_name("Make schedules as flexible and predictable as possible"), ".html")),
    paid_leave = file.path(dir_path, paste0(create_file_name("Increase access to paid leave"), ".html")),
    boundaries = file.path(dir_path, paste0(create_file_name("Respect boundaries between work and non-work time"), ".html")),
    mattering = file.path(dir_path, paste0(create_file_name("Mattering at Work"), ".html")),
    living_wage = file.path(dir_path, paste0(create_file_name("Provide a living wage"), ".html")),
    engage_workers = file.path(dir_path, paste0(create_file_name("Engage workers in workplace decisions"), ".html")),
    gratitude = file.path(dir_path, paste0(create_file_name("Build a culture of gratitude and recognition"), ".html")),
    mission = file.path(dir_path, paste0(create_file_name("Connect individual work with organizational mission"), ".html")),
    growth = file.path(dir_path, paste0(create_file_name("Opportunity for Growth"), ".html")),
    training = file.path(dir_path, paste0(create_file_name("Offer quality training, education, and mentoring"), ".html")),
    career_advancement = file.path(dir_path, paste0(create_file_name("Foster clear, equitable pathways for career advancement"), ".html")),
    feedback = file.path(dir_path, paste0(create_file_name("Ensure relevant, reciprocal feedback"), ".html"))
  )
  
  # Debugging: Print file paths
  print(file_paths)
  
  output$protectionContent <- renderUI({
    read_file_content(file_paths$protection)
  })
  
  output$physicalSafetyContent <- renderUI({
    read_file_content(file_paths$physical_safety)
  })
  
  output$psychologicalSafetyContent <- renderUI({
    read_file_content(file_paths$psychological_safety)
  })
  
  output$adequateRestContent <- renderUI({
    read_file_content(file_paths$adequate_rest)
  })
  
  output$mentalHealthContent <- renderUI({
    read_file_content(file_paths$mental_health)
  })
  
  output$deiaContent <- renderUI({
    read_file_content(file_paths$deia)
  })
  
  output$connectionContent <- renderUI({
    read_file_content(file_paths$connection)
  })
  
  output$inclusionContent <- renderUI({
    read_file_content(file_paths$inclusion)
  })
  
  output$trustedRelationshipsContent <- renderUI({
    read_file_content(file_paths$trusted_relationships)
  })
  
  output$collaborationContent <- renderUI({
    read_file_content(file_paths$collaboration)
  })
  
  output$worklifeContent <- renderUI({
    read_file_content(file_paths$worklife)
  })
  
  output$autonomyContent <- renderUI({
    read_file_content(file_paths$autonomy)
  })
  
  output$flexibleSchedulesContent <- renderUI({
    read_file_content(file_paths$flexible_schedules)
  })
  
  output$paidLeaveContent <- renderUI({
    read_file_content(file_paths$paid_leave)
  })
  
  output$boundariesContent <- renderUI({
    read_file_content(file_paths$boundaries)
  })
  
  output$matteringContent <- renderUI({
    read_file_content(file_paths$mattering)
  })
  
  output$livingWageContent <- renderUI({
    read_file_content(file_paths$living_wage)
  })
  
  output$engageWorkersContent <- renderUI({
    read_file_content(file_paths$engage_workers)
  })
  
  output$gratitudeContent <- renderUI({
    read_file_content(file_paths$gratitude)
  })
  
  output$missionContent <- renderUI({
    read_file_content(file_paths$mission)
  })
  
  output$growthContent <- renderUI({
    read_file_content(file_paths$growth)
  })
  
  output$trainingContent <- renderUI({
    read_file_content(file_paths$training)
  })
  
  output$careerAdvancementContent <- renderUI({
    read_file_content(file_paths$career_advancement)
  })
  
  output$feedbackContent <- renderUI({
    read_file_content(file_paths$feedback)
  })
}

shinyApp(ui = ui, server = server)