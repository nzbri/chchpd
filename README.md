
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chchpd

The goal of the `chchpd` package is to allow NZBRI researchers to access
data from the New Zealand Parkinson’s Progression (NZP<sup>3</sup>)
study. Rather than deal directly with data in spreadsheets or databases,
functions like `import_participants()` and `import_motor_scores()` are
provided, to abstract away dealing with the raw data source. Currently,
these access data from shared online Google Sheets. Some of those are
periodically exported from NZBRI’s Alice and REDCap Parkinson’s
databases, while (for the time being) some contain manually-entered
data. While the data source might change, the function used to obtain
will appear unchanged to the user, which is the benefit of this layer of
abstraction.

Data is returned as a separate dataframe for each data set. Users are
responsible for linking them together as required.

The package was originally developed by Michael MacAskill, with
contributions from Daniel Myall, Toni Pitcher, and Reza Shoorangiz.
Please direct queries to Michael in the first instance.

## Installation

This package is only of interest and utility internally at NZBRI and
hence can’t be made available via CRAN. Instead, it is hosted in an
online Github at <https://github.com/nzbri/chchpd>.

Therefore, the usual installation route using
`install.packages('chchpd')` is not possible, and will yield a
not-useful error message that the package is not available for your
version of R. Instead, install `chchpd` from its development repository
on Github as follows:

``` r
# install.packages('remotes')
remotes::install_github('nzbri/chchpd')
```

If `install_github('nzbri/chchpd')` is invoked subsequently, the package
will be downloaded and installed only if the version on Github is newer
than the one installed locally.

If problems arise in a new release, you can downgrade to a previous
version by specifying the name of a particular release to revert to,
e.g.

``` r
devtools::install_github('nzbri/chchpd@v0.1.5')
```

The list of releases is here: <https://github.com/nzbri/chchpd/releases>

Please use the issue tracker at <https://github.com/nzbri/chchpd/issues>
to report any problems. Bear in mind that this repository is public to
the world, so if reporting data-related issues, be careful not to post
anything that contains identifiers.

## Example usage

Records across the various tables must be joined using either
`subject_id` as an index (e.g. for linking to the participant table, as
it includes information, such as sex, that is constant for a subject),
or `session_id` (to join various measures gathered at approximately the
same assessment session for a given participant), as these might change
at different time points.

The ‘glue’ between participants and the data gathered about them is
generally the ‘sessions’ table. For each participant, this lists the
various assessment sessions they have had. In the raw data, these
sessions were often described by idiosyncratic labels, such as
`999BIO_F2` (indicating the follow-up session two years after the
baseline recruitment session in the Progression study). But the same
session might also have served as the baseline in the more selective PET
study, and been labelled, say, `999BIO_PET0`. This often idiosyncratic
labelling is cured by the subject session mapping table, which would
have a record for both the `999BIO_F2` and `999BIO_PET0` sessions,
linking them to the same standardised session code, which has a form
like `999BIO_2016-01-28`. When importing various data sources (like HADS
or UPDRS), their idiosyncratic session labels are replaced by this
standardised form. Thus it is easy to join multiple tables together
systematically, as below. Often a key step is to specify just a
restricted set of sessions, by specifying a particular study to filter
them by. In the example code below, we select just those sessions in the
‘PET’ study.

``` r
# load necessary packages:
library(chchpd)
library(dplyr)    # to join dataframes

# first establish your rights to view the data:
google_authenticate()

# then import datasets of interest:
participants = import_participants()
sessions     = import_sessions(from_study = 'PET')
np           = import_neuropsyc()
updrs        = import_motor_scores()

# bind the records together, linked by subject or session IDs:
dat = right_join(participants, sessions, by = 'subject_id') %>% 
  left_join(np, by = 'session_id') %>% 
  left_join(updrs, by = 'session_id')
```

## Data caching

To speed up development of your analysis scripts, data caching is now
enabled by default. That is, it can be quite slow to import data from a
Google spreadsheet, so a copy of the data will be cached for you. If you
subsequently attempt to re-import that data, if it is within a certain
interval (60 minutes by default), then the data won’t be downloaded
afresh: you will just instantly be returned the cached version.

If you want to force a fresh download of the data, or change other
options, you can do the following:

``` r
options('chchpd_use_cached' = FALSE)
options('chchpd_cache_duration' = 30) # duration in minutes
options('chchpd_suppress_warnings' = FALSE) # reduce warnings from googlesheets
```
