# ui.R

# load libraries
library(shiny)
library(shinyWidgets)
library(shinyBS)
library(psjournals)
library(dplyr)
library(DT)

# calculate limits in terms of words, characters, and pages
psjournals <- psjournals |>
  mutate(max_words = case_when(limit_unit == "words" ~ as.numeric(upper_limit),
                               limit_unit %in% c("page", "pages") ~ upper_limit * 400,
                               limit_unit == "characters" ~ round(upper_limit / 6),
                               TRUE ~ 0),
         max_characters = max_words * 6,
         max_pages = round(max_words / 400))

# Start: page ----
fluidPage(

  tags$head(

    # CSS Styles for Input Background Colors
    tags$style(HTML("
      /* Define colors for specific inputs */
      .input-background1 {
          background-color: #d4f0f0;
          padding: 10px;
          border-radius: 10px;
          margin-bottom: 10px;
      }
      .input-background2 {
          background-color: #fff5d6;
          padding: 10px;
          border-radius: 5px;
          margin-bottom: 10px;
      }
      .input-background3 {
          background-color: #EAD4EB;
          padding: 10px;
          border-radius: 5px;
          margin-bottom: 10px;
      }
    ")),

    # Meta tag for UTF-8 encoding
    tags$meta(charset = "UTF-8")

    # Google Analytics
    #, includeHTML("google_analytics.html")
  ),

# App title ----
  titlePanel("Political Science Journals"),

# Help text ----
  helpText("An app that filters data on political science journals", HTML("&mdash;"),
            "by", a("Resul Umit", href = "https://resulumit.com")),


# Sidebar layout with input and output definitions ----
  sidebarLayout(

# Sidebar panel for inputs ----
  sidebarPanel(

div(class = "input-background1",

# Input: Scope ----
textInput(inputId = "scope", label = "Journal scope", placeholder = "Enter a word or pattern"),

# Input: Tooltip for Scope ----
bsTooltip(id = "scope", title = "E.g., <i>elec</i> would return journals mentioning <i>selection</i>, <i>Election</i>, <i>electorate</i> ... in their scope.",
          placement = "top", options = list(container = "body")),

# Input: Since ----
sliderInput(inputId = "publishedSince", label = "Publishing since", step = 1, ticks = FALSE, sep = "",
            value = c(min(psjournals$since, na.rm = TRUE), max(psjournals$since, na.rm = TRUE)),
            min = min(psjournals$since, na.rm = TRUE), max = max(psjournals$since, na.rm = TRUE)),

# Input: Publisher ----
pickerInput(inputId = "publisher", label = "Publisher",
            choices = c("Taylor & Francis", "SAGE", "Wiley", "Cambridge University Press",
                        "Oxford University Press", "Springer", "Palgrave", "Elsevier", "Other"),
            selected = c("Taylor & Francis", "SAGE", "Wiley", "Cambridge University Press",
                         "Oxford University Press", "Springer", "Palgrave", "Elsevier", "Other"),
            options = list(`actions-box` = TRUE, size = 9,
                               `selected-text-format` = "count"),
            multiple = TRUE)
),


div(class = "input-background2",

# Input: Impact Factor ----
sliderInput(inputId = "ifactor", label = "Impact Factor", step = 1, ticks = FALSE,
            value = c(0, ceiling(max(psjournals$ifactor, na.rm = TRUE))),
            min = 0, max = ceiling(max(psjournals$ifactor, na.rm = TRUE))),

# Input: H5 Index ----
sliderInput(inputId = "h5Index", label = "H5 Index", step = 1, ticks = FALSE,
            value = c(0, max(psjournals$h5_index, na.rm = TRUE)),
            min = 0, max = max(psjournals$h5_index, na.rm = TRUE)),

# Input: H5 Median ----
sliderInput(inputId = "h5Median", label = "H5 Median", step = 1, ticks = FALSE,
            value = c(0, max(psjournals$h5_median, na.rm = TRUE)),
            min = 0, max = max(psjournals$h5_median, na.rm = TRUE)),

# Checkbox to include NA values
checkboxInput(inputId = "includeNA_ifactor",
              label = "Include journals with unknown metrics",
              value = TRUE)

),


div(class = "input-background3",

# Input: Article Type ----
pickerInput(inputId = "typeCategory", label = "Article type",
            choices = c("Article" = "article",
                        "Research Note" = "research_note",
                        "Review" = "review",
                        "Data" = "data",
                        "Replication" = "replication",
                        "Comment" = "comment",
                        "Pedagogy" = "teaching",
                        "Collection" = "collection",
                        "Other / Uncategorised" = "other/uncategorised"),
            selected = c("article", "research_note", "review", "data", "replication",
                         "comment", "teaching", "collection", "other/uncategorised"),
            options = list(`actions-box` = TRUE, size = 6,
                           `selected-text-format` = "count"),
            multiple = TRUE),


# Length limit

fluidRow(

  column(6, textInput(inputId = "limitInput", label = "Manuscript Length", placeholder = "Enter a number or a range")),
  column(6, selectInput(inputId = "limitType", label = HTML("&nbsp;"),
                        selected = "Words", choices = c("Words", "Characters", "Pages")))
),

# Input: Tooltip for length_limit ----
bsTooltip(id = "length_limit", title = "Enter a number or a range (e.g., 7000 or 6000 - 8000)",
          placement = "top", options = list(container = "body")),


# Checkbox to include NA values
checkboxInput(inputId = "includeNA_limits",
              label = "Include article types with unknown or no upper limit",
              value = TRUE)
),

# Input: Variables ----
pickerInput(inputId = "selectedVariables", label = "Variables to display",
            choices = c("Publisher", "Since", "Scope", "Impact Factor",
                        "H5 Index", "H5 Median", "Article Type",
                        "Length Limits", "Last Updated"),
            selected = c("Impact Factor", "Article Type", "Length Limits"),
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
             p("This app was originally created in 2020, with the data most recently updated in full in November 2024."),
             p("The source code, including an R data package, and descriptions are available at", a("https://github.com/resulumit/psjournals.", href = "https://github.com/resulumit/psjournals")),
             p("This app is based on data from a comprehensive, but not exhaustive, list of political science journals. At the same time, the dataset might include journals that do not unambiguously belong to the discipline of political science. Some data points may be inaccurate, and others may become inaccurate over time. Consult journal websites for the most accurate information."),
             p("Please report any issues with the dataset, package, and/or the app at", a("https://github.com/resulumit/psjournals/issues", href = "https://github.com/resulumit/psjournals/issues"), "or to", a("resulumit@gmail.com.", href = "mailto:resulumit@gmail.com?subject=psjournals")),
             p("It can be cited as follows:"),
             p("Umit, Resul, 2024, psjournals: An R data package on political science journals,", a("https://doi.org/10.7910/DVN/UENCQA", href = "https://doi.org/10.7910/DVN/UENCQA"), "Harvard Dataverse, V3.")
             )))

# End: sidebarLayout ----
)

# End: fluidpage ----
)
