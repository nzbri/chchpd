#' Change idiosyncratic session ID codes to a consistent canonical form
#'
#' \code{map_to_universal_session_id} Add a column of a canonical session ID to
#'a given dataframe, created by matching the session label from the dataframe
#'(which tends to be ad hoc and error prone) via a lookup table. That table maps
#' the many different session IDs used across various data sources to a single
#'canonical one that can be used to link records  more reliably.
#' For example, given a session code like 999BIO_F19,
#' return a code like 999BIO_2018-12-25. This allows matching across data
#' sources that could be using different labels for the same actual session.
#' The date is a single date that describes all the associated visits that
#' are linked to that session, even if they occurred some days or weeks apart.
#'
#' @param dataset A dataframe containing a session ID column that needs to be
#' made consistent.
#' @param make_session_label For most dataframes we need to concatenate the
#' subject ID and a session suffix to form a session label. This can be
#' suppressed for sources like the Alice exported session data that already
#' contain a unitary session label.
#' @param remove_double_measures Some measures might occur twice within an
#' overarching session (e.g. meds or UPDRS have occasionally been assessed
#' several times, separated by just days or weeks). Generally, we remove all
#' but the latest value from a session.
#' @param drop_original_columns There is usually no need to retain the original
#' idiosyncratic session columns once the new unified one has been provided.
#' @return The dataframe originally passed in to the function but with the
#' session IDs replaced with  canonical ones.
#'
#' @examples
#' \dontrun{
#' some_data = map_to_universal_session_id(some_data)
#' }
#' @export
map_to_universal_session_id <- function(dataset,
                                        make_session_label = TRUE,
                                        remove_double_measures = TRUE,
                                        drop_original_columns = TRUE) {

  # import the lookup table from a file that is regularly exported via
  # a cron job from the Alice database:
  session_code_map = googlesheets::gs_title(chchpd_env$subj_session_map_filename) %>%
    googlesheets::gs_read() %>%
    dplyr::rename(session_suffix = session_id) %>%
    tidyr::unite(col = input_id, subject_id, session_suffix,
                 sep = '_', remove = TRUE) %>%
    tidyr::unite(col = session_id, standardised_subject_id,
                 standardised_session_id,sep = '_', remove = FALSE) %>%
    dplyr::select(-standardised_session_id, -standardised_subject_id)

  # when needed, create a session label in the source dataframe by concatenating
  # its subject id with its session suffix column:
  if (make_session_label) {
    dataset %<>%
      tidyr::unite(col = session_label,
                   subject_id, session_suffix, sep = '_',
                   remove = drop_original_columns)
  }

  # now look up the session label from the given dataset, map it to the
  # universal session id:
  dataset %<>%
    # use the idiosyncratic label to map to a standardised session id:
    dplyr::left_join(session_code_map, by = c('session_label' = 'input_id')) %>%
    # set column order:
    dplyr::select(session_id, dplyr::everything())

  if (make_session_label & drop_original_columns) {
    dataset %<>% dplyr::select(-session_label)
  }

  # show records that failed to match to a universal session ID:
  unmatched = dataset %>%
    dplyr::filter(is.na(session_id) | session_id == '0') %>% # not sure why but
    # with meds at least, we get 0 rather than NA for non-matches
    dplyr::select(-session_id) # is NA

  # only produce output if there is at least 1 non-match:
  if (nrow(unmatched) > 0 ) {
    print('MISSES!!!! Failed to match to a universal session ID:')
    if (ncol(unmatched) < 6) {
      print(knitr::kable(unmatched))
    } else {
      print(knitr::kable(unmatched[1:5]))
    }
  }

  # some people got multiple assessments of some measures per overall session
  # (e.g. an abbreviated session triggered a full assessment a few
  # days or weeks later and the UPDRS was done on each occasion). In
  # this case, we delete all but one measure per universal session id
  # to prevent issues with multiple matches with other data:

  if (remove_double_measures) {
    dataset %<>%
      dplyr::group_by(session_id) %>%
      # sort by date, but the name of this variable changes across data frames
      # # (e.g. HADS_date, UPDRS_date, etc):
      dplyr::arrange_at(vars(ends_with('_date'))) %>%
      # count duplicates within a session:
      dplyr::mutate(n = row_number()) %>% # n==1 will be the latest one
      dplyr::filter(n == 1) %>% # remove all but last record
      dplyr::select(-n) %>% # drop the temporary counter
      dplyr::ungroup()
  }

  return(dataset)
}

sanitise_session_ids <- function(ids) {
  # session IDs are sometimes manually entered incorrectly in the
  # source data. We clean them up to improve matching across datasets.
  ids %<>%
    stringr::str_to_upper() %>% # remove any lower case letters
    stringr::str_trim() %>% # remove leading and trailing whitespace
    stringr::str_replace_all(' ', '_') %>% # fill internal spaces with _
    stringr::str_replace(pattern = '-PET', replacement = '_PET') %>%
    # replace other hyphens with underscores but only if they
    # occur before an F or D with digits (and sometimes '.') session suffix
    # (i.e. don't replace any that occur legitimately as missing
    # initials in some early subject IDs):
    stringr::str_replace(pattern = '-([FD]|FU)([0-9.]+)', replacement = '_\\1\\2') %>%
    # replace any _FU + digits with just _F
    stringr::str_replace(pattern = '_FU([0-9.]+)', replacement = '_F\\1') #%>%
  # delete any redundant /P1, /P2 suffixes:
  #str_replace(pattern = '\\/P[0-2])', replacement = '')
  # HINT: you can check regexs interactively online at https://regex101.com
  # but note that we need to escape our \ characters here as \\
  #
  # TODO As a check that this works, but also as way to improve data quality,
  # should compare the sanitised to the original session codes and print out
  # a list where they differ, so errors can be corrected in the source data.

  return(ids)
}

tabulate_duplicates <- function(df, varname, print_results = FALSE) {
  # produce a table of which values are duplicated within a
  # specified variable in a dataframe, and their number
  # of repetitions. varname must be given as a string to
  # avoid dplyr's non-standard evaluation.
  duplicates = df %>%
    dplyr::group_by_(varname) %>% # group_by_() works programatically
    dplyr::summarise(n = n()) %>%
    dplyr::filter(n > 1) %>%
    dplyr::arrange_('n', varname) %>% # arrange_() works programatically
    dplyr::arrange(desc(n)) %>%
    dplyr::select_(varname, 'n')

  if (print_results & nrow(duplicates) > 0) {
    print('Duplicates:')
    print(knitr::kable(duplicates))
  }

  return(duplicates)

}
