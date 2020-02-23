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

# Create a local environment to keep filenames. Better than using the global
# environment which would be affected by rm() function.
chchpd_env = new.env(emptyenv())

# store references to the Google sheets which contain the data:
chchpd_env$participant_file_id = # ParticipantExport spreadsheet:
  '1WmeDr5WbUzl2uA1wMlZTJdcn_F-q-nWMEmlCKjSFInk'

chchpd_env$session_file_id = # SessionExport spreadsheet:
  '1HS_wlXRbWmGV3Db8ELuB53N6O1xVSmWJqlJ0AhyaMdY'

###### This is one that googlesheets4 just refuses to read:
chchpd_env$clinical_file_id = # PD Progression clinical data spreadsheet:
  '14Jb3qC1Ioazmpacpwtlqw-myFtIjBGR9D2y6znLVIbs' # temp copy for testing
#'1kcPVaCGjpHVXKfCoLkIaGPa624RiJL47q1KxmA3QWdo' # original corrupted one
#'
chchpd_env$redcap_neuropsyc_file_id = # RedcapExport spreadsheet:
  '1liI06efJe1mRI3Iz2lWwq_PeZDpZLnIhhjqyN1SQlX0'

chchpd_env$scan_file_id = # PD Scan numbers spreadsheet:
  '1NVlN6GrzXuyP4u37iuO2NAfORJxC8bmaBWIZpPoyrR4'

chchpd_env$subj_session_map_file_id = # SubjectSessionMapping spreadsheet:
  '1JAmxvXPU0cQlQAlYiZUwCxk1oDTu_84w4ZZUYZo2VP4'

chchpd_env$bloods_file_id = # PD Bloods Tracking spreadsheet:
  '191uIITl3vqJKY97M87a72BVPt5CnnEZlZJ6sch8XFEU'

# as data imports are slow, allow them to be automatically cached for a period:
chchpd_env$default_recache_time = 60 # minutes
chchpd_env$cached_data = list() # store imported dataframes here by name
chchpd_env$cache_start = list() # store time when each data set is downloaded

# functions that run when package is attached/loaded:
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Data import functions for the Christchurch ",
                               "Longitudinal Parkinson's Study. Should be run ",
                               "only by accredited researchers at NZBRI, who ",
                               "have authorised access to the data sources."))


  if (is.null(getOption('chchpd_use_cached', default = NULL)))
    options(chchpd_use_cached = TRUE)

  if (is.null(getOption('chchpd_cache_update_time', default = NULL)))
    options(chchpd_cache_update_time = chchpd_env$default_recache_time) # 60 min

  if (is.null(getOption('chchpd_suppress_warnings', default = NULL)))
    options(chchpd_supress_warnings = TRUE) # Reduce warnings from googlesheets

}
