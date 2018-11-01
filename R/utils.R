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
