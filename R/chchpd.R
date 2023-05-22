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
#'   various measures collected in the New Zealand Parkinson's Progression
#'   (NZP3) study. Currently these are spread across multiple sources (the Alice
#'   Django database, multiple Google spreadsheets and .csv files, and an Access
#'   database). Shortly these should all be migrated into one REDCap database
#'   and hence issues of data integrity should be greatly reduced. The functions
#'   exposed in this package should serve to ease the transition, and ease the
#'   implementation of analyses as required, without the a reliance upon fixed
#'   file paths and access to local source files.
#'
#' @keywords internal
"_PACKAGE"
#> [1] "_PACKAGE"

# Create a local environment to keep filenames. Better than using the global
# environment which would be affected by rm() function.
chchpd_env <- new.env(emptyenv())

# store references to the Google sheets which contain the data:
chchpd_env$participant_file_id <- # ParticipantExport spreadsheet:
  '1WmeDr5WbUzl2uA1wMlZTJdcn_F-q-nWMEmlCKjSFInk'

chchpd_env$session_file_id <- # SessionExport spreadsheet:
  '1HS_wlXRbWmGV3Db8ELuB53N6O1xVSmWJqlJ0AhyaMdY'

chchpd_env$clinical_file_id <- # PD Progression clinical data spreadsheet:
  '14Jb3qC1Ioazmpacpwtlqw-myFtIjBGR9D2y6znLVIbs' # new copy

chchpd_env$redcap_neuropsyc_file_id <- # RedcapExport spreadsheet:
  '1liI06efJe1mRI3Iz2lWwq_PeZDpZLnIhhjqyN1SQlX0'

chchpd_env$scan_file_id <- # PD Scan numbers spreadsheet:
  '1NVlN6GrzXuyP4u37iuO2NAfORJxC8bmaBWIZpPoyrR4'

chchpd_env$subj_session_map_file_id <- # SubjectSessionMapping spreadsheet:
  '1JAmxvXPU0cQlQAlYiZUwCxk1oDTu_84w4ZZUYZo2VP4'

chchpd_env$bloods_file_id <- # PD Bloods Tracking spreadsheet:
  '191uIITl3vqJKY97M87a72BVPt5CnnEZlZJ6sch8XFEU'

# as data imports are slow, allow them to be automatically cached until the
# source has changed:
chchpd_env$cached <- list() # store imported dataframes here by name

# functions that run when package is attached:
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Data import functions for the Christchurch ",
                               "Longitudinal Parkinson's Study. Should be run ",
                               "only by accredited researchers at NZBRI, who ",
                               "have authorised access to the data sources."))
  invisible()
}


# Functions available on load (i.e., works with :: as well.)
.onLoad <- function(libname, pkgname) {

  if (is.null(getOption('chchpd_use_cached', default = NULL)))
    options(chchpd_use_cached = TRUE)

  if (is.null(getOption('chchpd_suppress_warnings', default = NULL)))
    options(chchpd_suppress_warnings = TRUE) # Reduce warnings from googlesheets

  # Configure googledrive and googlesheets to use CHCHPD application. This
  # improves issues related to exhausting Google's resources.
  # Currently, this only allows @nzbri.org email addresses to login.
  invisible(googlesheets4::gs4_auth_configure(client = chchpd_oauth_app(use_server=chchpd_check_rstudio_server())))
  invisible(googledrive::drive_auth_configure(client = chchpd_oauth_app(use_server=chchpd_check_rstudio_server())))

  invisible()
}
