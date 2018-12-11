
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chchpd

The goal of the `chchpd` package is to allow NZBRI researchers to access
data from the Christchurch Longitudinal Parkinson’s study. Rather than
deal directly with data in spreadsheets or databases, functions like
`import_participants()` and `import_motor_scores()` are provided, to
abstract away dealing with the raw data source. Currently, these access
data from shared online Google Sheets. Eventually, however, the
functions will be re-written to pull the data from the nascent NZBRI
REDCap Parkinson’s database. This change under-the-hood should be
invisible to the user, as the source of the data is abstracted behind
the functions provided. Data is returned as a separate dataframe for
each data set. Users are responsible for linking them together as
required.

The package was originally developed by Michael MacAskill, with
contributions from Daniel Myall, Toni Pitcher, and Reza Shoorangiz.
Please direct queries to Michael in the first instance.

## Installation

This package is only of interest and utility internally at NZBRI and
hence can’t be made available via CRAN. Therefore, the usual
installation route using `install.packages('chchpd')` is not possible,
and will yield a not-useful error message that the package is not
available for your version of R. Instead, install `chchpd` from its
development repository on Github as follows:

``` r
# install.packages('devtools')
devtools::install_github('nzbri/chchpd')
```

If `install_github()` is invoked subsequently, the package will be
downloaded and installed only if the version on Github is newer than the
one installed locally.

You can view the repository at <https://github.com/nzbri/chchpd>. Please
use the issue tracker at <https://github.com/nzbri/chchpd/issues> to
report any problems. Bear in mind that this repository is public to the
world, so if reporting data-related issues, be careful not to post
anything that contains identifiers.

## Example

Records across the various tables must be joined using either
`subject_id` as an index (e.g. for linking to the participant table, as
it includes information, such as sex, that is constant for a subject),
or `session_id` (to join various measures gathered at approximately the
same assessment session for a given participant, and which might change
at different time points.

The ‘glue’ between participants and the data gathered about them is the
‘sessions’ table. For each participant, this lists the various
assessment sessions they have had. In the data, these sessions were
often described by idiosyncratic labels, such as `999BIO_F2` (indicating
the follow-up session two years after baseline recruitment session in
the Progression study. But the same session might also have served as
the baseline in the more selective PET study, and been labelled, say,
`999BIO_PET0`. This often idiosyncratic labelling is cured by the
subject session maping table, which would have a record for both the
`999BIO_F2` and `999BIO_PET0` sessions, linking them to the same
standardised session code, which has a form like `999BIO_2016-0-28`.
When importing various data sources (like HADS or UPDRS), their
idosyncratic session labels are replaced by this standardised form. Thus
it is easy to join multiple tables togther systematically, as below.
Often a key step is to specify just a restricted set of sessions, by
specifying a particular study to filter them by. In the example code
below, we select just those sessions in the ‘PET’ study.

``` r
# load necessary packages:
library(chchpd)
library(magrittr) # for %>% pipe operator
library(dplyr)    # for joining functions

# import datasets of interest:
participants = import_participants()
sessions     = import_sessions(from_study = 'PET')
np           = import_neuropsyc()
updrs        = import_motor_scores()

# bind the records together, linked by subject or session IDs:
dat = right_join(participants, sessions, by = 'subject_id') %>% 
  left_join(np, by = 'session_id') %>% 
  left_join(updrs, by = 'session_id')
```