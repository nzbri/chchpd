# List the required explicit imports from this package below. Keep these to a
# minimum and instead use explicit package::function() syntax wherever possible
# to avoid importing libraries, to make dependencies clearer in the code. Note
# that the list of imports in the DESCRIPTION file is just information at the
# time of installation, to ensure that those packages are installed and hence
# can be called by this package.
#
# Roxygen will take the @import staments below and use them to populate the
# NAMESPACE file:

#' @importFrom magrittr %>%
#' @import dplyr
#' @import tidyr
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
#'   Functions exposed so far:
#'
#'   \code{\link{import_PET_scan_numbers}}
#' @keywords internal
"_PACKAGE"
#> [1] "_PACKAGE"
