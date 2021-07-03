# server.R

library(shiny)
library(psjournals)
library(dplyr)
library(stringr)
library(DT)


function(input, output) {

output$table <- DT::renderDataTable(DT::datatable(

psjournals %>%

    # add new variables, to be unselected later
    mutate(

          # hyperlink the journal names
           journal_h = paste0("<a href='", url,"' target='_blank'>",journal,"</a>"),

          # create short versions of scope
           scope_h = paste0("<span title='", scope , "'>", word(scope, 1, 5) , "...", "</span>"),

          # calculate limits in terms of words, characters, and pages
           min_words = case_when(is.na(limit_unit) | is.na(lower_limit) ~ 0,
                            limit_unit == "words" ~ as.numeric(lower_limit),
                            limit_unit %in% c("page", "pages") ~ lower_limit * 400,
                            limit_unit == "characters" ~ round(lower_limit / 6)),

           max_words = case_when(is.na(limit_unit) | is.na(upper_limit) ~ max(psjournals[which(psjournals$limit_unit == "words"), ]$upper_limit),
                            limit_unit == "words" ~ as.integer(upper_limit),
                            limit_unit %in% c("page", "pages") ~ as.integer(upper_limit * 400),
                            limit_unit == "characters" ~ as.integer(round(upper_limit / 6))),

           min_characters = min_words * 6,
           max_characters = max_words * 6,

           min_pages = case_when(min_words == 0 ~ 0, TRUE ~ round(min_words / 400)),
           max_pages = round(max_words / 400),

          # add zeros to slider variables
           h5_index_z = ifelse(is.na(h5_index), 0, h5_index),
           h5_median_z = ifelse(is.na(h5_median), 0, h5_median),
           ssci_rank_z = ifelse(is.na(ssci_rank), 0, ssci_rank),

          # re-group publisher
           re_publisher = case_when(publisher %in% c("Taylor & Francis", "SAGE",
                                                     "Cambridge University Press",
                                                     "Oxford University Press",
                                                     "Wiley", "Springer", "Palgrave",
                                                     "Elsevier") ~ publisher,
                                    TRUE ~ "Other"),

          # merge the original limit-related variables into one
           lenght_limits = case_when(!is.na(lower_limit) & !is.na(upper_limit) ~ paste(lower_limit, "to", upper_limit, limit_unit, sep = " "),
                                    is.na(lower_limit) & !is.na(upper_limit) ~ paste("Up to", upper_limit, limit_unit, sep = " "),
                                    !is.na(lower_limit) & is.na(upper_limit) ~ paste("More than", lower_limit, limit_unit, sep = " "))


          ) %>%

          # filter according to ui
           filter(re_publisher %in% input$publisher,
                  since >= input$publishedSince[1] & since <= input$publishedSince[2],
                  if (!is.null(input$scope)) str_detect(str_to_lower(scope), str_to_lower(input$scope)) else TRUE,
                  h5_index_z >= input$h5Index[1] & h5_index_z <= input$h5Index[2],
                  h5_median_z >= input$h5Median[1] & h5_median_z <= input$h5Median[2],
                  ssci_rank_z >= input$ssciRank[1] & ssci_rank_z <= input$ssciRank[2],
                  type_category %in% input$typeCategory,
                  case_when(input$lengthLimits == FALSE ~ TRUE,
                            input$limitType == "Words" ~ min_words <= input$wordLimits[1] & min_words <= input$wordLimits[2] & input$wordLimits[1] <= max_words & input$wordLimits[2] <= max_words,
                            input$limitType == "Characters" ~ min_characters <= input$characterLimits[1] & min_characters <= input$characterLimits[2] & input$characterLimits[1] <= max_characters & input$characterLimits[2] <= max_characters,
                            input$limitType == "Pages" ~ min_pages <= input$pageLimits[1] & min_pages <= input$pageLimits[2] & input$pageLimits[1] <= max_pages & input$pageLimits[2] <= max_pages)

           ) %>%

          # rename columns
           rename("Journal" = "journal_h", "Publisher" = "publisher", "Since" = "since",
                  "Scope" = "scope_h", "H5 Index" = "h5_index", "H5 Median" = "h5_median",
                  "SSCI Rank" =  "ssci_rank", "Article Type" = "type",
                  "Length Limits" = "lenght_limits", "Last Updated" = "last_updated") %>%

          # select variables to display according to ui
           select(input$selectedVariables),

          # Options: DT
           options = list(pageLength = 20),

          # Options: datatable
           rownames = FALSE,
           escape = FALSE

# end: output$table
))

# Downloadable data ----
output$download_original <- downloadHandler(
  filename = function() {
    paste("psjournals", "-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(psjournals, file, row.names = FALSE, na = "")
  }
)



}
