# ui.R

# load libraries
library(shiny)
library(shinyWidgets)
library(shinyBS)
library(psjournals)
library(dplyr)
library(DT)

# calculate limits in terms of words, characters, and pages
psjournals <- psjournals %>%
  mutate(max_words = case_when(limit_unit == "words" ~ as.numeric(upper_limit),
                               limit_unit %in% c("page", "pages") ~ upper_limit * 400,
                               limit_unit == "characters" ~ round(upper_limit / 6),
                               TRUE ~ 0),
         max_characters = max_words * 6,
         max_pages = round(max_words / 400))

# Start: page ----
fluidPage(

# App title ----
  titlePanel("Political Science Journals"),

# Help text ----
  helpText("An app that filters data on political science journals", HTML("&mdash;"),
            "by", a("Resul Umit", href = "https://resulumit.com"), "(2020)."),


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
            multiple = TRUE),

# Input: Since ----
sliderInput(inputId = "publishedSince", label = "Publishing since", step = 1, ticks = FALSE, sep = "",
            value = c(min(psjournals$since, na.rm = TRUE), max(psjournals$since, na.rm = TRUE)),
            min = min(psjournals$since, na.rm = TRUE), max = max(psjournals$since, na.rm = TRUE)),

# Input: Scope ----
textInput(inputId = "scope", label = "Journal scope", placeholder = "Filter by text pattern"),

# Input: Tooltip for Scope ----
bsTooltip(id = "scope", title = "E.g., <i>elec</i> would return journals mentioning <i>selection</i>, <i>Election</i>, <i>electorate</i> ... in their scope.",
          placement = "top", options = list(container = "body")),

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
pickerInput(inputId = "typeCategory", label = "Article type",
            choices = c("Article" = "article", "Research Note" = "research_note",
                        "Book Review" = "book_review", "Literature Review" = "literature_review",
                        "Comment" = "comment", "Other / Uncategorised" = "other/uncategorised"),
            selected = c("article", "research_note", "book_review", "literature_review",
                         "comment", "other/uncategorised"),
            options = list(`actions-box` = TRUE, size = 6,
                           `selected-text-format` = "count"),
            multiple = TRUE),

# Input: Lenght Limits ----
switchInput(inputId = "lengthLimits", label = "Length Limits", labelWidth = "100px"),

# Conditional Input: Limit Type ----
conditionalPanel(
  condition = "input.lengthLimits",
  selectInput(inputId ="limitType", label ="Length limits, translated to ...",
            selected = "Words", c("Words", "Characters", "Pages"))),

# Conditionals
conditionalPanel(
  condition = "input.lengthLimits & input.limitType == 'Words'",
  sliderInput(inputId = "wordLimits", label = "Number of words in your manuscript", step = 100, ticks = FALSE,
              value = c(4000, 6000),
              min = 0, max = max(psjournals$max_words, na.rm = TRUE))),

conditionalPanel(
  condition = "input.lengthLimits & input.limitType == 'Characters'",
  sliderInput(inputId = "characterLimits", label = "Number of characters in your manuscript", step = 1000, ticks = FALSE,
              value = c(24000, 36000),
              min = 0, max = max(psjournals$max_characters, na.rm = TRUE))),

conditionalPanel(
  condition = "input.lengthLimits & input.limitType == 'Pages'",
  sliderInput(inputId = "pageLimits", label = "Number of pages in your manuscript", step = 1, ticks = FALSE,
              value = c(10, 15),
              min = 0, max = max(psjournals$max_pages, na.rm = TRUE))),

# Input: Variables ----
pickerInput(inputId = "selectedVariables", label = "Variables to display",
            choices = c("Journal", "Publisher", "Since",
                        "Scope", "H5 Index", "H5 Median",
                        "SSCI Rank", "Article Type",
                        "Length Limits", "Last Updated"),
            selected = c("Journal", "H5 Index", "Article Type", "Length Limits"),
            options = list(`actions-box` = TRUE, size = 9,
                           `selected-text-format` = "count"),
            multiple = TRUE),

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
             p("The source code, including an R data package, and descriptions are available at", a("https://github.com/resulumit/psjournals.", href = "https://github.com/resulumit/psjournals")),
             p("This app is based on data from a comprehensive, but not exhaustive, list of political science journals. At the same time, the dataset might include journals that do not unambiguously belong to the discipline of political science. Some data points may be inaccurate as well, and others may become inaccurate over time. Consult journal websites for the most accurate information."),
             p("Please report any issues with the dataset, package, and/or the app at", a("https://github.com/resulumit/psjournals/issues", href = "https://github.com/resulumit/psjournals/issues"), "or to", a("resul.umit@gmail.com.", href = "mailto:resul.umit@gmail.com?subject=psjournals"))
             )))

# End: sidebarLayout ----
),

# Add: Google Analytics
tags$head(includeHTML(("google_analytics.html")))

# End: fluidpage ----
)
