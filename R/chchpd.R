# List the required explicit imports from this package below. Keep these to a
# minimum and instead use explicit package::function() syntax wherever possible
# to avoid importing libraries, to make dependencies clearer in the code. Note
# that the list of imports in the DESCRIPTION file is just information at the
# time of installation, to ensure that those packages are installed and hence
# can be called by this package.
#
# Roxygen will take the @import statements below and use them to populate the
# NAMESPACE file:

#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
NULL # needed just so that roxygen will process the statements above.


#' @details The package allows NZBRI researchers to access and combine the
#'   various measures collected in the Christchurch Parkinson's Progression
#'   study. Currently these are spread across multiple sources (the Alice Django
#'   database, multiple Google spreadsheets and .csv files, and an Access
#'   database). Shortly these should all be migrated into one REDCap database
#'   and hence issues of data integrity should be greatly reduced. The functions
#'   exposed in this package should serve to ease the transition, and ease the
#'   implementation of analyses as required, without the current reliance upon
#'   fixed file paths and access to local source files.
#'
#' @keywords internal
"_PACKAGE"
#> [1] "_PACKAGE"

# functions that run when package is attached/loaded:
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Data import functions for the Christchurch ",
                               "Longitudinal Parkinson's Study. Should be run ",
                               "only by accredited researchers at NZBRI, who ",
                               "have authorised access to the data sources."))
}

.onLoad <- function(libname, pkgname){
  # make an environment to hold variables all functions need to know about:
  assign('chchpd_env', value = new.env(), envir = globalenv())

  # Need titles of the relevant Google sheets. Just simple strings at this
  # stage. Handles to the files come later, via googlesheets::gs_title()
  chchpd_env$clinical_filename = 'PD Progression clinical data'
  chchpd_env$neuropsyc_filename = 'All Z-scores Progression'
  chchpd_env$scan_filename = 'PD Scan numbers'
  chchpd_env$participant_filename = 'ParticipantExport'
  chchpd_env$session_filename = 'SessionExport'
  chchpd_env$subj_session_map_filename = 'SubjectSessionMapping'
  chchpd_env$bloods_filename = 'PD Bloods Tracking'
}
