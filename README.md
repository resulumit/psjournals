# ps_journals

[![CRAN](https://www.r-pkg.org/badges/version/psjournals)](https://cran.r-project.org/package=psjournals)
![Downloads](https://cranlogs.r-pkg.org/badges/psjournals)
[![Travis Build Status](https://travis-ci.org/resulumit/psjournals.png?branch=master)](https://travis-ci.org/resulumit/psjournals)
[![codecov.io](https://codecov.io/github/resulumit/psjournals/coverage.svg?branch=master)](https://codecov.io/github/resulumit/psjournals?branch=master)

## Description

This is an R data package on political science journals, with article types being the unit of observation. It comes with [an R Shiny app](https://resulumit.shinyapps.io/psjournals/) designed to filter the data, to facilitate exploring journals and their article types for manuscript submission.

The package is based on data from a comprehensive, but not exhaustive, list of political science journals.<sup>[1](#footnote1)</sup> At the same time, the dataset might include journals that do not unambiguously belong to the discipline of political science. Some data points may be inaccurate as well, and others may become inaccurate over time. Consult journal websites for the most accurate information.



it might include journals that do not clearly belong to the discipline of political science. Existing observations may be inaccurate. Consult the journal websites for the most accurate information.

## Installation

```R
# install
devtools::install_github("resulumit/psjournals")

# load
library(psjournals)

# view
View(psjournals)
```

## Application

An application of the package is live at [https://resulumit.shinyapps.io/psjournals](https://resulumit.shinyapps.io/psjournals).

## Variables

`journal_id`: *integer* --- id number of the journal

`journal`: *character* --- name of the journal<sup>[2](#footnote2)</sup>

`ssci_rank`: *integer* --- Web of Science's SSCI index of the journal, based on the 2-year Journal Impact Factor 2018<sup>[3](#footnote3)</sup>

`h5_index`: *integer* --- Google Scholar's H5 Index of the journal, based on the articles published in 2014--2018

`h5_median`: *integer* --- Google Scholar's H5 Index of the journal, based on the articles published in 2014--2018

`since`: *integer* --- year of establishment of the journal

`publisher`: *character* --- publisher of the journal

`url`: *character* --- URL link to the journal website

`scope`: *character* --- scope of the journal

`type_id`: *integer* --- id number of the article type, by each journal

`type_category`: *character* --- category of the article type

`type`: *character* --- name of the article type, as defined by the journal

`lower_limit`: *character* --- lower (character/word/page) limit of the format

`upper_limit`: *character* --- upper (character/word/page) limit of the format

`limit_unit`: *character* --- unit of the type limits (character/word/page)

## Issues

Any issues with the data, package, and/or app can be reported at [https://github.com/resulumit/psjournals/issues](https://github.com/resulumit/psjournals/issues ).


## Notes

<a name="footnote1">1</a>: The dataset includes 652 observations from 306 journals.

<a name="footnote2">2</a>: The initial list of journals has been put together from lists at apsa.org (see [here](https://www.apsanet.org/journals) and [here](https://www.apsanet.org/otherjournals
)) and wikipedia.org (see [here](https://en.wikipedia.org/w/index.php?title=Category:Political_science_journals&pageuntil=World+Politics#mw-pages) and [here](https://en.wikipedia.org/wiki/List_of_political_science_journals)).

<a name="footnote3">3</a>: In practice, this variable comes from ooir.org. See [here](https://ooir.org/journals.php?category=polisci).
