#' @title A Dataset of Article Types in Political Science Journals
#' @description This is a dataset of article types published in selected political science journals.
#' @format A data frame with 653 rows and 15 variables:
#' \describe{
#'   \item{\code{journal_id}}{integer, id number of the journal}
#'   \item{\code{journal}}{character, name of the journal}
#'   \item{\code{ssci_rank}}{integer, Web of Science's SSCI index of the journal, based on the 2-year Journal Impact Factor 2018}
#'   \item{\code{h5_index}}{integer, Google Scholar's H5 Index of the journal, based on the articles published in 2014-2018}
#'   \item{\code{h5_median}}{integer, Google Scholar's H5 Index of the journal, based on the articles published in 2014-2018}
#'   \item{\code{since}}{integer, year of establishment of the journal}
#'   \item{\code{publisher}}{character, publisher of the journal}
#'   \item{\code{url}}{character, URL link to the journal website}
#'   \item{\code{scope}}{character, scope of the journal}
#'   \item{\code{type_id}}{integer, id number of the article type, by each journal}
#'   \item{\code{type_category}}{character, category of the article type}
#'   \item{\code{type}}{character, name of the article type, as defined by the journal}
#'   \item{\code{lower_limit}}{character, lower (character/word/page) limit of the format}
#'   \item{\code{upper_limit}}{character, upper (character/word/page) limit of the format}
#'   \item{\code{limit_unit}}{character, unit of the type limits (character/word/page)}
#'}
#' @source
#'  \code{ssci_rank} comes from \url{https://ooir.org},
#'  \code{h5_index} and \code{h5_median} come from \url{https://scholar.google.ch/},
#'  all remaining variables are collected from journal websites.
"psjournals"
