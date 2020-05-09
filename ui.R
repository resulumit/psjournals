# ui.R

library(shiny)
library(shinyWidgets)
library(psjournals)
library(dplyr)
library(DT)


psjournals <- psjournals %>%

  # add new variables, to be unselected later
  mutate(
    # calculate limits in terms of words, characters, and pages
    max_words = case_when(limit_unit == "words" ~ as.numeric(upper_limit),
                          limit_unit %in% c("page", "pages") ~ upper_limit * 400,
                          limit_unit == "characters" ~ round(upper_limit / 6),
                          TRUE ~ 0),
    max_characters = max_words * 6,
    max_pages = round(max_words / 400)
  )

# Start: page ----
fluidPage(

# App title ----
  titlePanel("Filter Political Science Journals"),

# Sidebar layout with input and output definitions ----
  sidebarLayout(

# Sidebar panel for inputs ----
  sidebarPanel(

# Input: Publisher ----
pickerInput(inputId = "publisher", label = "Publisher",
            choices = c("Taylor & Francis", "Sage", "Wiley", "Cambridge University Press",
                        "Oxford University Press", "Springer", "Palgrave", "Other"),
            selected = c("Taylor & Francis", "Sage", "Wiley", "Cambridge University Press",
                         "Oxford University Press", "Springer", "Palgrave", "Other"),
            options = list(`actions-box` = TRUE, size = 8,
                               `selected-text-format` = "count"),
            multiple = TRUE
    ),

# Input: Since ----
sliderInput(inputId = "publishedSince", label = "Since", step = 1, ticks = FALSE, sep = "",
            value = c(min(psjournals$since, na.rm = TRUE), max(psjournals$since, na.rm = TRUE)),
            min = min(psjournals$since, na.rm = TRUE), max = max(psjournals$since, na.rm = TRUE)),

# Input: Scope ----
searchInput(inputId = "widget", label = "Scope", placeholder = "Filter journals by scope",
  btnSearch = icon("search"), btnReset = icon("remove"), width = "100%"),

# Input: H5 Index ----
sliderInput(inputId = "h5Index", label = "H5 Index", step = 1, ticks = FALSE,
            value = c(0, max(psjournals$h5_index, na.rm = TRUE)),
            min = 0, max = max(psjournals$h5_index, na.rm = TRUE)),

# Input: H5 Median ----
sliderInput(inputId = "h5Median", label = "H5 Median", step = 1, ticks = FALSE,
            value = c(0, max(psjournals$h5_median, na.rm = TRUE)),
            min = 0, max = max(psjournals$h5_median, na.rm = TRUE)),

# Input: SSCI Rank ----
sliderInput(inputId = "ssciRank", label = "SSCI Rank", step = 1, ticks = FALSE,
            value = c(0, max(psjournals$ssci_rank, na.rm = TRUE)),
            min = 0, max = max(psjournals$ssci_rank, na.rm = TRUE)),

# Input: Article Type ----
pickerInput(inputId = "typeCategory", label = "Article Type",
            choices = c("Article" = "article", "Research Note" = "research_note",
                        "Book Review" = "book_review", "Literature Review" = "literature_review",
                        "Comment" = "comment", "Other" = "other"),
            selected = c("article", "research_note", "book_review", "literature_review",
                         "comment", "other"),
            options = list(`actions-box` = TRUE, size = 6,
                           `selected-text-format` = "count"),
            multiple = TRUE
),

# Input: Word Limit ----

# Conditional Input: Limit Type ----
selectInput(inputId ="limitType", label ="Lenght in ...",
            selected = "Words", c("Words", "Characters", "Pages")),

# Conditionals
conditionalPanel(
  condition = "input.limitType == 'Words'",
  sliderInput(inputId = "wordLimits", label = "Word Limits", step = 1, ticks = FALSE,
              value = c(0, max(psjournals$max_words, na.rm = TRUE)),
              min = 0, max = max(psjournals$max_words, na.rm = TRUE))),

conditionalPanel(
  condition = "input.limitType == 'Characters'",
  sliderInput(inputId = "characterLimits", label = "Character Limits", step = 1, ticks = FALSE,
              value = c(0, max(psjournals$max_characters, na.rm = TRUE)),
              min = 0, max = max(psjournals$max_characters, na.rm = TRUE))),

conditionalPanel(
  condition = "input.limitType == 'Pages'",
  sliderInput(inputId = "pageLimits", label = "Page Limits", step = 1, ticks = FALSE,
              value = c(0, max(psjournals$max_pages, na.rm = TRUE)),
              min = 0, max = max(psjournals$max_pages, na.rm = TRUE))),

# Input: Variables ----
pickerInput(inputId = "selectedVariables", label = "Variables to Display",
            choices = c("Journal", "Publisher", "Since",
                        "Scope", "H5 Index", "H5 Median",
                        "SSCI Rank", "Article Type",
                        "Length Limits" ),
            selected = c("Journal", "H5 Index", "Article Type", "Length Limits"),
            options = list(`actions-box` = TRUE, size = 9,
                           `selected-text-format` = "count"),
            multiple = TRUE
),

# Buttons: Download ----
downloadButton(outputId = "download_original", "Original Dataset")

# End: sidebarPanel ----
),

# Start: MainPanel ----
mainPanel(
  tabsetPanel(
    tabPanel("Table", DT::dataTableOutput(outputId = "table")),
    tabPanel("Notes",
             br(),
             strong("A Shiny app by", a("Resul Umit", href = "https://resulumit.com"), "(2020)"),
             br(),
             br(),
             p("The source code, including an R data package, and descriptions are available at", a("https://github.com/resulumit/psjournals.", href = "https://github.com/resulumit/psjournals")),
             p("This app is based on data from a comprehensive, but not exhaustive, list of political science journals. At the same time, the dataset might include journals that do not unambiguously belong to the discipline of political science. Some data points may be inaccurate as well, and others may become inaccurate over time. Consult journal websites for the most accurate information."),
             p("Please report any issues with the dataset, package, and/or the app at", a("https://github.com/resulumit/psjournals/issues", href = "https://github.com/resulumit/psjournals/issues"), "or to", a("resul.umit@unilu.ch.", href = "mailto:resul.umit@unilu.ch?subject=psjournals"))


             )))

# End: sidebarLayout ----
)

# End: fluidpage ----
)
