# psjournals

[![CRAN](https://www.r-pkg.org/badges/version/psjournals)](https://cran.r-project.org/package=psjournals)

## Description

This is an R data package on political science journals, with article types as the units of observation. It includes [an R Shiny app](https://resulumit.shinyapps.io/psjournals/) designed to filter the data, making it easier to explore journals and their article types for manuscript submission.

The dataset was last fully updated in November 2024, at which time it included 922 entries from 346 journals.

The package is based on a comprehensive, though not exhaustive, list of political science journals. Additionally, the dataset may include some journals that are not exclusively within the field of political science. While every effort has been made to ensure accuracy, some data points may be inaccurate and may become outdated over time. For the most up-to-date information, please refer to individual journal websites.

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

`id`: *integer* &mdash; id number of the journal

`journal`: *character* &mdash; name of the journal<sup>[1](#footnote1)</sup>

`ifactor`: *integer* &mdash; The Impact Factor of the journal<sup>[2](#footnote3)</sup>

`h5_index`: *integer* &mdash; Google Scholar's H5 Index of the journal

`h5_median`: *integer* &mdash; Google Scholar's H5 Index of the journal

`since`: *integer* &mdash; year of establishment of the journal

`publisher`: *character* &mdash; publisher of the journal

`url`: *character* &mdash; URL link to the journal website

`scope`: *character* &mdash; scope of the journal

`type_id`: *integer* &mdash; id number of the article type, by each journal

`type_category`: *character* &mdash; category of the article type

`type`: *character* &mdash; name of the article type, as defined by the journal

`lower_limit`: *character* &mdash; lower (character/word/page) limit of the format

`upper_limit`: *character* &mdash; upper (character/word/page) limit of the format

`limit_unit`: *character* &mdash; unit of the type limits (character/word/page)

`last_updated`: *date* &mdash; the date when each observation was last updated, in the "YYYY-MM" format

## Issues

Any issues with the data, package, and/or app can be reported at [https://github.com/resulumit/psjournals/issues](https://github.com/resulumit/psjournals/issues ).

## Citation

Umit, Resul, 2024, "psjournals: An R data package on political science journals", https://doi.org/10.7910/DVN/UENCQA, Harvard Dataverse, V3.

## Notes

<a name="footnote1">1</a>: The initial list of journals has been put together from lists at apsa.org (see [here](https://www.apsanet.org/journals) and [here](https://www.apsanet.org/otherjournals
)) and wikipedia.org (see [here](https://en.wikipedia.org/w/index.php?title=Category:Political_science_journals&pageuntil=World+Politics#mw-pages) and [here](https://en.wikipedia.org/wiki/List_of_political_science_journals)).

<a name="footnote2">2</a>: In practice, this variable comes from ooir.org. See [here](https://ooir.org/journals.php?metric=jif).
