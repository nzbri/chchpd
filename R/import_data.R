#' Import metadata on PET scans
#'
#' \code{import_PET_scan_numbers} accesses the scan number and dose information
#' associated with each PET scan session.
#'
#' This PET information is held in a fixed Google Sheet and so no parameters are
#' necessary for this function.
#'
#' @return A dataframe containing the PET metadata.
#'
#' @examples
#' \dontrun{
#' PET = import_PET_scan_numbers()
#' }
#' @export
import_PET_scan_numbers <- function() {
  # Import data on amyloid PET scans

  # Need a handle to the scan numbers Google sheet.
  # Access the spreadsheet by its title.
  # This requires an initial authentication in the browser:
  scan_ss = googlesheets::gs_title('PD Scan numbers')

  PET_scans = googlesheets::gs_read(ss = scan_ss,
                                    ws = 'PET_scans') %>%
    # tidy manual data entry errors of session IDs:
    mutate(subject_id = sanitise_session_ids(subject_id))

  return(PET_scans)
}
