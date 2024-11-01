# server.R

library(shiny)
library(psjournals)
library(dplyr)
library(stringr)
library(DT)

function(input, output) {

  # Reactive expression for filtering the data based on inputs
  filtered_data <- reactive({

    # Parse length_limit input
    length_limits <- if (isTruthy(input$length_limit)) {
      as.numeric(unlist(strsplit(input$length_limit, "[-, ]+")))
    } else {
      NULL
    }

    # Filter data
    psjournals |>
      # Escape single and double quotes in `scope` to avoid issues
      mutate(
        scope = str_replace_all(scope, "'", "\\'"),
        scope = str_replace_all(scope, '"', '\\"')
      ) |>

      # add variables for html
      rowwise() |>
      mutate(
        # Remove line breaks and escape quotes in `scope`
        scope_clean = str_replace_all(scope, "\\r\\n|\\n", " "),
        scope_clean = str_replace_all(scope_clean, "'", "&apos;"),
        scope_clean = str_replace_all(scope_clean, '"', "&quot;"),

        # Create `scope_h` with `scope_summary` as the tooltip
        scope_h = paste0(
          "<span title='",
          scope_clean,
          "'>",
          word(scope_clean, 1, 5),
          "...",
          "</span>"
        ),

        # Hyperlink the journal names
        journal_h = paste0("<a href='", url, "' target='_blank'>", journal, "</a>")
      ) |>
      ungroup() |>

      # add other variables
      mutate(
        # calculate limits in terms of words, characters, and pages
        min_words = case_when(
          is.na(limit_unit) | is.na(lower_limit) ~ 0,
          limit_unit == "words" ~ as.numeric(lower_limit),
          limit_unit %in% c("page", "pages") ~ lower_limit * 400,
          limit_unit == "characters" ~ round(lower_limit / 6)
        ),

        max_words = case_when(
          is.na(limit_unit) |
            is.na(upper_limit) ~ max(psjournals[which(psjournals$limit_unit == "words"), ]$upper_limit),
          limit_unit == "words" ~ as.integer(upper_limit),
          limit_unit %in% c("page", "pages") ~ as.integer(upper_limit * 400),
          limit_unit == "characters" ~ as.integer(round(upper_limit / 6))
        ),

        min_characters = min_words * 6,
        max_characters = max_words * 6,

        min_pages = case_when(min_words == 0 ~ 0, TRUE ~ round(min_words / 400)),
        max_pages = round(max_words / 400),

        # re-group publisher
        re_publisher = case_when(
          publisher %in% c(
            "Taylor & Francis",
            "SAGE",
            "Cambridge University Press",
            "Oxford University Press",
            "Wiley",
            "Springer",
            "Palgrave",
            "Elsevier"
          ) ~ publisher,
          TRUE ~ "Other"
        ),

        # merge the original limit-related variables into one
        lenght_limits = case_when(
          !is.na(lower_limit) &
            !is.na(upper_limit) ~ paste(lower_limit, "to", upper_limit, limit_unit, sep = " "),
          is.na(lower_limit) &
            !is.na(upper_limit) ~ paste("Up to", upper_limit, limit_unit, sep = " "),
          !is.na(lower_limit) &
            is.na(upper_limit) ~ paste("More than", lower_limit, limit_unit, sep = " ")
        )
      ) |>

      # filter according to ui
      filter(
        if (isTruthy(input$scope)) {
          str_detect(str_to_lower(scope), str_to_lower(input$scope))
        } else TRUE,

        re_publisher %in% input$publisher,

        since >= input$publishedSince[1] &
          since <= input$publishedSince[2],

        # H5 Index filter, including NAs if checkbox is checked
        if (input$includeNA_ifactor) {
          is.na(h5_index) |
            (h5_index >= input$h5Index[1] & h5_index <= input$h5Index[2])
        } else {
          !is.na(h5_index) &
            h5_index >= input$h5Index[1] & h5_index <= input$h5Index[2]
        },

        # H5 Median filter, including NAs if checkbox is checked
        if (input$includeNA_ifactor) {
          is.na(h5_median) |
            (h5_median >= input$h5Median[1] & h5_median <= input$h5Median[2])
        } else {
          !is.na(h5_median) &
            h5_median >= input$h5Median[1] & h5_median <= input$h5Median[2]
        },

        # Impact Factor filter, including NAs if checkbox is checked
        if (input$includeNA_ifactor) {
          is.na(ifactor) |
            (ifactor >= input$ifactor[1] & ifactor <= input$ifactor[2])
        } else {
          !is.na(ifactor) &
            ifactor >= input$ifactor[1] & ifactor <= input$ifactor[2]
        },

        type_category %in% input$typeCategory,

        # 1. Condition to include NAs in `upper_limit` if specified by the user
        if (input$includeNA_limits) {
          is.na(upper_limit) | !is.na(upper_limit)
        } else {
          !is.na(upper_limit)
        },

        # 2. Length limit filter based on input$limitType
        if (isTruthy(length_limits)) {
          # Include NAs while applying the length filter
          is.na(upper_limit) |
            if (length(length_limits) == 1) {
              # Single number entered
              if (input$limitType == "Words") {
                min_words <= length_limits[1] & max_words >= length_limits[1]
              } else if (input$limitType == "Characters") {
                min_characters <= length_limits[1] & max_characters >= length_limits[1]
              } else if (input$limitType == "Pages") {
                min_pages <= length_limits[1] & max_pages >= length_limits[1]
              }
            } else if (length(length_limits) == 2) {
              # Two numbers entered (range)
              if (input$limitType == "Words") {
                min_words <= length_limits[2] & max_words >= length_limits[1]
              } else if (input$limitType == "Characters") {
                min_characters <= length_limits[2] & max_characters >= length_limits[1]
              } else if (input$limitType == "Pages") {
                min_pages <= length_limits[2] & max_pages >= length_limits[1]
              }
            } else {
              TRUE  # If input cannot be parsed, no filtering is applied
            }
        } else {
          TRUE  # If input$length_limit is empty, apply no filter
        }

      ) |>

      # rename columns
      rename(
        "Journal" = "journal_h",
        "Publisher" = "publisher",
        "Since" = "since",
        "Scope" = "scope_h",
        "Impact Factor" =  "ifactor",
        "H5 Index" = "h5_index",
        "H5 Median" = "h5_median",
        "Article Type" = "type",
        "Length Limits" = "lenght_limits",
        "Last Updated" = "last_updated"
      ) |>

      # select variables to display according to ui
      select(input$selectedVariables)
  })

  # Render the datatable based on the filtered data
  output$table <- DT::renderDataTable({
    DT::datatable(filtered_data(), options = list(pageLength = 20), rownames = FALSE, escape = FALSE)
  })

  # Downloadable data ----
  output$download_original <- downloadHandler(
    filename = function() {
      paste("psjournals", "-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE, na = "")
    }
  )
}

