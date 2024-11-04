# server.R

library(shiny)
library(psjournals)
library(dplyr)
library(stringr)
library(DT)

function(input, output) {

  # Reactive expression for filtering the data based on inputs
  filtered_data <- reactive({

    # Parse limitInput input by extracting only numeric values
    limit_inputs <- if (isTruthy(input$limitInput)) {
      # Use regex to extract all numeric sequences from the input string
      parsed <- as.numeric(unlist(str_extract_all(input$limitInput, "\\d+")))
      if (all(!is.na(parsed)) && length(parsed) > 0) {
        parsed  # Return parsed numeric vector only if all values are valid
      } else {
        NULL  # Return NULL if parsing fails
      }
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

        # Hyperlink journal names
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
        length_limits = case_when(
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

        # Length limit filter based on input$limitType and input$includeNA_limits
        if (isTruthy(limit_inputs)) {
          # Handle cases where one or two numeric values are provided in limit_inputs
          if (length(limit_inputs) == 1) {
            if (input$limitType == "Words") {
              (min_words <= limit_inputs[1] & max_words >= limit_inputs[1]) |
                (input$includeNA_limits & is.na(upper_limit))
            } else if (input$limitType == "Characters") {
              (min_characters <= limit_inputs[1] & max_characters >= limit_inputs[1]) |
                (input$includeNA_limits & is.na(upper_limit))
            } else if (input$limitType == "Pages") {
              (min_pages <= limit_inputs[1] & max_pages >= limit_inputs[1]) |
                (input$includeNA_limits & is.na(upper_limit))
            }
          } else if (length(limit_inputs) == 2) {
            # Handle cases where two numbers are entered (range)
            if (input$limitType == "Words") {
              (min_words <= limit_inputs[2] & max_words >= limit_inputs[1]) |
                (input$includeNA_limits & is.na(upper_limit))
            } else if (input$limitType == "Characters") {
              (min_characters <= limit_inputs[2] & max_characters >= limit_inputs[1]) |
                (input$includeNA_limits & is.na(upper_limit))
            } else if (input$limitType == "Pages") {
              (min_pages <= limit_inputs[2] & max_pages >= limit_inputs[1]) |
                (input$includeNA_limits & is.na(upper_limit))
            }
          } else {
            TRUE  # If input cannot be parsed, no filtering is applied
          }
        } else {
          input$includeNA_limits | !is.na(upper_limit)  # Apply includeNA_limits if no limit_input is provided
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
        "Length Limits" = "length_limits",
        "Last Updated" = "last_updated"
      ) |>

      # select variables to display according to ui + journal name
      select(any_of(c("Journal", input$selectedVariables, "journal", "max_words")))
  })

  # Render the datatable based on the filtered data
  output$table <- renderDataTable({

    # Dynamically find the column indices for `journal` and `journal_h`
    journal_index <- which(names(filtered_data()) == "journal") -1
    journal_h_index <- which(names(filtered_data()) == "Journal") -1

    # Dynamically find the column indices for `max_words` and `length_limits`
    max_words_index <- which(names(filtered_data()) == "max_words") -1
    length_limits_index <- which(names(filtered_data()) == "Length Limits") -1

    datatable(

      filtered_data(),
      options = list(
        pageLength = 20,
        rownames = FALSE,
        escape = FALSE,  # Ensures HTML tags are rendered
        columnDefs = list(

          # Hide the `journal` and `max_words` columns
          list(visible = FALSE, targets = journal_index),
          list(visible = FALSE, targets = max_words_index),

          # Set the sorting for journal_h (HTML link) based on the journal column's data
          list(orderData = journal_index, targets = journal_h_index),

          # Set the sorting for length_limit based on max_words (common limit)
          list(orderData = max_words_index, targets = length_limits_index)

        )
      ),
      escape = FALSE,  # Ensures HTML tags in `journal_h` are rendered correctly
      rownames = FALSE
    )
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

