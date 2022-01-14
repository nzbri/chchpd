#' View current `chchpd` package options
#'
#' \code{get_chchpd_options} View a list of package options, such as whether
#' data should be cached and for how long, and whether to suppress Google sheets
#' warnings.
#'
#' @return A list of options and their current values.
#'
#' @examples
#' \dontrun{
#' get_chchpd_options()
#' }
#' @export
get_chchpd_options <- function() {
  use_cached <- getOption('chchpd_use_cached',
                         default = TRUE)
  suppress_warnings <- getOption('chchpd_suppress_warnings',
                                default = NULL)
  return(list(use_cached = use_cached,
              suppress_warnings = suppress_warnings))
}

# A helper function to hack googlesheets4's default behaviour to identify column types.
# gs_read_helper reads spreadsheets as string and uses readr package to convert types.
# In case of google's resource limits, this function will attempt multiple times to load a spreadsheet.
gs_read_helper <- function(ss, sheet = NULL, range = NULL, col_types = NULL, na = c('', 'NA', 'None'), ... ){
  # If the user has not authenticated, do it now.
  chchpd_has_token(ensure_token = TRUE)
  
  
  max_retries <- 5
  for(i in 0:max_retries){
    # Read everything as string.
    data <- try(googlesheets4::range_read(ss, sheet = sheet, range = range, col_types = 'c', na = na, ...), silent = TRUE)
    if(!is(data, 'try-error')){
      break
    }
    if(stringr::str_detect(str = attr(data, 'condition')$message, 'RESOURCE_EXHAUSTED') && i < max_retries){
      wait_time <- min(120, (i+1)*30)
      print(glue::glue('Google responded with a \'RESOURCE_EXHAUSTED\' error and loading data may take a while. {max_retries - i} more attempts will be performed.\nRetry will be performed in {wait_time} seconds ...\n'))
    } else if(stringr::str_detect(str = attr(data, 'condition')$message, 'RESOURCE_EXHAUSTED') && i == max_retries) {
        stop('If you see this message, I have failed to load the data due to google\'s resource-quota limits. Please wait for a few minutes and re-run your script.')
    } else {
      stop(data)
    }
    Sys.sleep(wait_time)
  }
  
  # Fix column types.
  data <- suppressMessages(readr::type_convert(data, col_types = col_types, na = na))
  return(data)
}

# datasets are all now imported from google sheets centrally in this function,
# which is only called if the import_helper function has determined that caching
# is not in effect. Previously, the initial import was done in each of the
# individual dataset import functions. They now serve only to do the processing
# and tidying of the raw datasets.
import_helper_googlesheet <- function(dataset) {
  dataset <- tolower(dataset) # label of spreadsheet to import (e.g. HADS)

  if (dataset == 'session_code_map') {
    data <- gs_read_helper(ss = chchpd_env$subj_session_map_file_id,
                          col_types = readr::cols(standardised_session_id = readr::col_character()))
    
  } else if (dataset == 'participants') {
    data <- gs_read_helper(
      ss = chchpd_env$participant_file_id,
      na = c('NA', 'None'))
    
  } else if (dataset == 'sessions') {
    data <- gs_read_helper(ss = chchpd_env$session_file_id,
                          col_types = readr::cols(MRIScanNo = readr::col_character()))
    
  } else if (dataset == 'mds_updrs') {
    data <- gs_read_helper(
      ss = chchpd_env$clinical_file_id,
      sheet = 'MDS_UPDRS',
      na = c('na', 'NA', 'NA1', 'NA2', 'NA3', 'NA4', 'NA5', 'NA6', 'UR'),
      col_types = readr::cols(Q1_8 = readr::col_number(),
                              Q3_15a = readr::col_number())
      )

  } else if (dataset == 'old_updrs') {
    data <- gs_read_helper(
      ss = chchpd_env$clinical_file_id,
      sheet = 'Old_UPDRS',
      na = c('na', 'NA', 'NA1', 'NA2', 'NA3', 'NA4', 'NA5', 'NA6', 'UR'))
    
  } else if (dataset == 'hads') {
    data <- gs_read_helper(
      ss = chchpd_env$clinical_file_id,
      sheet = 'HADS',
      na = c('NA', 'NA1', 'NA2', 'NA3', 'NA4', 'NA5', 'NA6'),
    ) %>%
      dplyr::select(-Anxiety_total,-Depression_total) # don't include the
    # totals columns, as they don't deal well with missing values and we
    # calculate them afresh anyway

  } else if (dataset == 'meds') {
    data <- gs_read_helper(
      ss = chchpd_env$clinical_file_id,
      sheet = 'Medication',
      na = c('NA', 'NA1', 'NA2', 'NA3', 'NA4', 'NA5', 'NA6'),
      col_types = readr::cols(duodopa = readr::col_number(),
                              cr_tolcapone = readr::col_number())
    )

  } else if (dataset == 'np') {
    data <- gs_read_helper(
      ss = chchpd_env$redcap_neuropsyc_file_id,
      na = 'NA',
      col_types = readr::cols(pdq = readr::col_number())
    )
    
  } else if (dataset == 'mri') {
    data <- gs_read_helper(ss = chchpd_env$scan_file_id,
                          sheet = 'MRI_scans',
                          col_types = readr::cols(`Scan #` = readr::col_character(), 
                                                  `Remapped Scan #` = readr::col_character(), 
                                                  ASL = readr::col_character(), 
                                                  FLAIR = readr::col_character(), 
                                                  T2 = readr::col_character(), 
                                                  fcMRI = readr::col_character(), 
                                                  FieldMap = readr::col_character(),
                                                  `SPGR QSM` = readr::col_character(), 
                                                  `DTI, B=2500,2mm` = readr::col_character(), 
                                                  PEPOLAR = readr::col_character()))
    
  } else if (dataset == 'pet') {
    data <- gs_read_helper(ss = chchpd_env$scan_file_id,
                          sheet = 'PET_scans', 
                          col_types = readr::cols(perfusion_PET_scan = readr::col_character(),
                                                  amyloid_PET_scan = readr::col_character(),
                                                  Injected = readr::col_character()                                                   
                          )
    )
                           
  } else if (dataset == 'bloods') {
    data <- gs_read_helper(
      ss = chchpd_env$bloods_file_id,
      sheet = 'data'
    )
    
  } else if (dataset == 'hallucinations') {
    data <- gs_read_helper(
      ss = chchpd_env$clinical_file_id,
      sheet = 'Hallucinations Questionnaire',
      na = c('N/A', 'NA', 'NA1', 'NA2', 'NA3',
             'NA4', 'NA5', 'NA6', 'UR'),
      col_types = readr::cols(hallucinations_date = readr::col_date(), 
                              `Q2-D` = readr::col_number(),
                              `Q3-D` = readr::col_number(),
                              `Q6-D` = readr::col_number(),
                              `Q8-D` = readr::col_number(),
                              `Q10-D` = readr::col_number(),
                              `Q20-D` = readr::col_number()
      ),
      # this sheet has a row above the col names:
      skip = 1
    )
  }

  # tidy spaces, caps etc from variable names, except to retain backwards
  # compatibility of capitalised variable names in some datasets:
  if ( !(dataset %in% c('mds_updrs', 'old_updrs', 'hads'))) {
    data %<>% janitor::clean_names()
  }

  # report errors where cell contents don't match expected type for the column:
  if (nrow(readr::problems(data)) > 0) {
    print(readr::problems(data))
  }

  return(data)
}

# this function is called by each of the individual dataset import functions so
# that they receive either a cached version of the data, or a freshly imported
# version:
import_helper <- function(dataset) {
  cache_opts <- get_chchpd_options()

  sheet_modified_time <- get_googlesheet_modifiedtime(dataset)

  use_cached <- cache_opts$use_cached &&
    !(is.null(names(chchpd_env$cached))) &&
    (dataset %in% names(chchpd_env$cached)) &&
    (as.numeric(
      difftime(sheet_modified_time, chchpd_env$cached[[dataset]]$cached_time,
               units = 'sec')
    ) < 1)

  if (use_cached) {
    return(chchpd_env$cached[[dataset]]$data)
  } else { # import afresh from the google sheet:
    run_silent <- getOption('chchpd_suppress_warnings', default = TRUE)

    print_message <- TRUE
    while( as.numeric(difftime(Sys.time(), sheet_modified_time, units = 'sec')) < 10 ){
      if (print_message){
        print('The specified googlesheet is updating, please wait ...')
        print_message <- FALSE
      }

      sheet_modified_time <- get_googlesheet_modifiedtime(dataset)
    }

    sanity_check <- FALSE

    while (!sanity_check){

      if (run_silent) {
        data <-
          suppressWarnings(suppressMessages(import_helper_googlesheet(dataset)))
      } else {
        data <- import_helper_googlesheet(dataset)
      }

      sheet_modified_time_after_download <- get_googlesheet_modifiedtime(dataset)

      if (as.numeric(difftime(sheet_modified_time_after_download, sheet_modified_time, units = 'sec')) < 1)
      {
        sanity_check <- TRUE
      } else {
        sheet_modified_time <- sheet_modified_time_after_download
        print('Googlesheet was modified during the download. It will be downloaded again after 5 sec ... ')
        Sys.sleep(5)
      }

    }

    chchpd_env$cached[[dataset]]$data <- data
    chchpd_env$cached[[dataset]]$cached_time <- sheet_modified_time

    return(data)
  }
}

# A routine to identify modification time of a googlesheet.
get_googlesheet_modifiedtime <- function(dataset) {
  dataset <- tolower(dataset) # label of spreadsheet to import (e.g. HADS)

  if (dataset == 'session_code_map') {
    file_id <- chchpd_env$subj_session_map_file_id
  } else if (dataset == 'participants') {
    file_id <- chchpd_env$participant_file_id
  } else if (dataset == 'sessions') {
    file_id <- chchpd_env$session_file_id
  } else if (dataset == 'mds_updrs') {
    file_id <- chchpd_env$clinical_file_id
  } else if (dataset == 'old_updrs') {
    file_id <- chchpd_env$clinical_file_id
  } else if (dataset == 'hads') {
    file_id <- chchpd_env$clinical_file_id
  } else if (dataset == 'meds') {
    file_id <- chchpd_env$clinical_file_id
  } else if (dataset == 'np') {
    file_id <- chchpd_env$redcap_neuropsyc_file_id
  } else if (dataset == 'mri') {
    file_id <- chchpd_env$scan_file_id
  } else if (dataset == 'pet') {
    file_id <- chchpd_env$scan_file_id
  } else if (dataset == 'bloods') {
    file_id <- chchpd_env$bloods_file_id
  } else if (dataset == 'hallucinations') {
    file_id <- chchpd_env$clinical_file_id
  }

  # Check there's an active token with chchpd app.
  chchpd_has_token(ensure_token = TRUE)
  modified_time <- googledrive::drive_get(id = file_id) %>%
    dplyr::mutate(modified = lubridate::as_datetime(purrr::map_chr(drive_resource, "modifiedTime"))) %>%
    dplyr::select(modified)

  return(modified_time$modified)
}


#' Allow Google Drive authorisation via a server
#'
#' \code{google_authenticate} To access data on a Google drive requires that the
#' user is authenticated, to ensure that you are entitled to view it.
#'
#' If using RStudio on a server, specifying `use_server = TRUE` will institute
#' a different authentication process to when you are running locally.
#'
#' @param email Allows you to nominate a particular nzbri.org e-mail address to
#' use for authentication. This can save you having to specify it manually each
#' time the script is run, but shouldn't be used in reproducible workflows
#' (where other users have to be able to run the code). The default value of
#' `TRUE` means that if authentication has occurred before, and there is just
#' one e-mail address available to select, it will automatically do that.
#'
#' @param use_server If \code{TRUE}, use the copy/paste authentication process.
#' If \code{FALSE}, use fully browser-contained authentication.
#'
#' @return No return value: the function initiates a process which results in
#' the writing of a \code{.httr-oauth} token file to disk.
#' @export
google_authenticate <- function(email = chchpd_user(),
                                use_server = chchpd_check_rstudio_server()) {
  googlesheets4::gs4_auth(email = email, 
                          scopes = c("https://www.googleapis.com/auth/spreadsheets.readonly",
                                        "https://www.googleapis.com/auth/drive.readonly",
                                        "https://www.googleapis.com/auth/drive.metadata.readonly"), 
                          use_oob = use_server)
  
  token <- googlesheets4::gs4_token()
  googledrive::drive_auth(token = token)
}

#' Import participant demographics
#'
#' \code{import_participants} Access the participant information that should be
#' constant across all sessions (for example, age at diagnosis, sex, etc).
#'
#' This information is exported periodically from the Alice database.
#'
#' @param anon_id If \code{TRUE}, return an anonymised \code{subject_id}.
#' Otherwise, return the in-house, non-secure ID.
#' Currently, this option is not useful if other datasets are being imported,
#' as they still contain the standard ID. We need to implement a global setting
#' that dictates whether all functions return anonymised IDs.
#'
#' @param identifiers If \code{TRUE}, include identifying information (names,
#' dates of birth & death, etc). This should not be used routinely for research.
#' i.e. this information should only ever be needed for study management
#' purposes.
#' \code{anon_id} must be set to \code{FALSE} for this parameter to have any
#' effect.
#'
#' @return A dataframe containing the participant data.
#'
#' @examples
#' \dontrun{
#' participants <- import_participants()
#' }
#' @export
import_participants <- function(anon_id = FALSE, identifiers = FALSE) {

  # import a file that is regularly exported via a cron job
  # from the Alice database:
  participants <- import_helper('participants')

  participants %<>%
    dplyr::rename(participant_status = status,
                  excluded_from_followup = excluded,
                  participant_group = disease_group,
                  side_of_onset = side_affected) %>%
    # replace missing string values with NA:
    dplyr::mutate(handedness = dplyr::na_if(handedness, ''),
                  side_of_onset = dplyr::na_if(side_of_onset, 'Unknown')) %>%
    # set format of some variables:
    dplyr::mutate(handedness = factor(handedness),
                  side_of_onset = factor(side_of_onset),
                  birth_date = lubridate::ymd(birth_date),
                  sex = factor(sex, levels = c('Male', 'Female')),
                  participant_status = factor(participant_status),
                  dead = !is.na(date_of_death)) %>%
    # calculate age as at today (even if dead). Generally for analyses, would
    # use the age from the session table (i.e. age at which the data was
    # gathered):
    dplyr::mutate(age_today =
                    round(lubridate::interval(birth_date, lubridate::today())/
                            lubridate::years(1),
                          digits = 1)) %>%
    dplyr::mutate(age_at_death =
                    round(lubridate::interval(birth_date, date_of_death)/
                            lubridate::years(1),
                          digits = 1))

  if (anon_id) { # return only anonymous id and no identifiers:
    participants %<>%
      dplyr::select(anon_id, dead, participant_status,
                    excluded_from_followup, participant_group, sex,
                    side_of_onset, handedness, symptom_onset_age, diagnosis_age,
                    age_at_death, age_today, education, ethnicity) %>%
      dplyr::rename(subject_id = anon_id)
  }
  else if (identifiers) { # for management purposes only, return names, DOB, etc
    participants %<>%
      dplyr::select(subject_id, survey_id, first_name, last_name, date_of_death,
                    dead, participant_status, birth_date, age_today,
                    excluded_from_followup, participant_group, sex,
                    side_of_onset, handedness, symptom_onset_age, diagnosis_age,
                    education, ethnicity)
  }
  else { # return internal id code but no other identifiers:
    participants %<>%
      dplyr::select(subject_id, survey_id, dead, participant_status,
                    excluded_from_followup,
                    participant_group, sex, side_of_onset, handedness,
                    symptom_onset_age, diagnosis_age, age_at_death, age_today,
                    education, ethnicity)
  }

  tabulate_duplicates(participants, 'subject_id')
  return(participants)
}

#' Import links between sessions and studies
#'
#' \code{import_sessions} Access a table that links a given session to the
#' (possibly multiple) studies of which it is a part, and the group within that
#' study to which the associated participant belongs.
#'
#' This information is exported periodically from the Alice database.
#'
#' @param from_study Optionally specify the name of a specific study to limit
#'   the records returned to just those from that study. Inspect the sessions
#'   spreadsheet to see the valid options. e.g. \code{'PET'} or
#'   \code{'Follow-up'} for the Progression study. Can also specify a list to
#'   include sessions from multiple studies, e.g.
#'   \code{c('Follow-up', 'PD DNA')}.
#'
#' @param exclude If \code{TRUE}, don't return sessions that have been excluded
#'   from a given study. For safety, we default to \code{TRUE} so that you don't
#'   get junk data unless you explicitly ask for it. You might set this to
#'   \code{FALSE} in order to get all records if you need to document
#'   exclusions, for example to create a participant recruitment and exclusion
#'   flowchart.
#'
#' @param print_exclude_summary If \code{TRUE}, print a table that lists
#'   the number of records that are excluded from each study. If this looks
#'   problematic, you could set \code{exclude = FALSE} to import all records to
#'   identify which ones were excluded.
#'
#' @return A dataframe containing the session-group-study data.
#'
#' @examples
#' \dontrun{
#' sessions <- import_sessions()
#' }
#' @export
import_sessions <- function(from_study = NULL, exclude = TRUE, print_exclude_summary = TRUE) {

  # import a file that is regularly exported via a cron job
  # from the Alice database:
  sessions <- import_helper('sessions') %>%
    # extract session suffix (e.g. F2, D0), useful for some purposes:
    dplyr::mutate(session_suffix =
                    stringr::str_match(session_id, '_(.*)')[,2]) %>%
    dplyr::select(session_id, subject_id, session_suffix, study, date,
                  study_group, study_excluded, mri_scan_no) %>%
    dplyr::rename(session_label = session_id,
                  session_date = date) %>%
    dplyr::mutate(study = factor(study),
                  session_date = lubridate::ymd(session_date)) %>%
    # tidy up errors in subject ids and session suffixes in source data:
    map_to_universal_session_id(make_session_label = FALSE,
                                remove_double_measures = FALSE)

  if (exclude) {
    excluded_session <- sessions %>%
      dplyr::filter(!is.na(study_excluded) & study_excluded == TRUE)

    if (print_exclude_summary & nrow(excluded_session) > 0){
      cat('Excluded cases:')
      exclusion_summary = excluded_session %>%
        dplyr::ungroup() %>%
        dplyr::group_by(study) %>%
        dplyr::summarise(`Excluded sessions` = dplyr::n())


      print(knitr::kable(exclusion_summary, caption = 'Automatically excluded sessions.', align = 'l'))
    }

    sessions %<>% dplyr::filter(is.na(study_excluded) | study_excluded != TRUE)
  }

  # Updated to filter multiple studies at once.
  if (assertthat::not_empty(from_study) &&
      all(sapply(from_study, assertthat::is.string))) {
    sessions %<>% dplyr::filter(study %in% from_study)
  }

  # need to calculate the age at each session. To do this, we require the birth
  # date of each subject. Eventually we will stop returning this to users via
  # the import_participant() function, so we can't expect the user to feed the
  # birth dates to this function.
  # Unfortunately that means we have to call that function ourselves in order
  # to get the birth date behind the scenes. This likely means that
  # import_participant() will get called twice in most analysis pipelines, once
  # explicitly by the user, and once implicitly in this function.
  DOBs <- import_participants(anon_id = FALSE, identifiers = TRUE) %>%
    dplyr::select(subject_id, birth_date)

  # join the sessions to the DOBs to calculate age at each session:
  sessions %<>%
    dplyr::left_join(DOBs, by = 'subject_id') %>%
    dplyr::mutate(age = as.numeric(round(difftime(session_date, birth_date,
                                                  units = 'days')/365.25, digits = 1))) %>%
    dplyr::select(session_id:session_date, age, study_group:mri_scan_no) %>%
    dplyr::group_by(subject_id) %>%
    dplyr::arrange(subject_id, session_date) %>%
    dplyr::ungroup()

  return(sessions)
}

#'Import UPDRS/MDS-UPDRS motor scores.
#'
#'\code{import_motor_scores} Import the total Part III motor score and the Hoehn
#'& Yahr stage from the UPDRS/MDS-UPDRS datasets
#'
#'At the beginnining of the study, the original 1987 UPDRS was still in use.
#'This function in turn calls the \code{import_old_UPDRS} function that will
#'import data from that assessment and scale it using an established method so
#'that the Part III total becomes comparable to the Part III MDS-UPDRS total
#'from later sessions. For scaling details, see Goetz et al. 2012. Mov
#'Disorders, 27(10):1239-1242
#'
#'To bind the two datasets, we need to drop all but the summary scores. If you
#'want access to individual items scores, use the individual functions
#'\code{import_old_UPDRS} or \code{import_MDS_UPDRS}
#'
#'@return A dataframe containing the motor score information.
#'
#' @examples
#' \dontrun{
#' updrs <- import_motor_scores()
#' }
#'@export
import_motor_scores <- function() {
  # The study used both the original and revised MDS version of the UPDRS.
  # To get a dataframe that contains just the Part III motor score and H & Y
  # score, we need to import both datasets separately then combine the MDS
  # Part III and the equivalent scaled Part III from the old UPDRS file, as well
  # as the H & Y.
  MDS <- import_MDS_UPDRS(concise = FALSE)

  old <- import_old_UPDRS()

  # select just the lowest-common-denominator columns from each dataset,
  # so they can be united:
  MDS %<>%
    dplyr::select(session_id, UPDRS_date, H_Y, Part_III) %>%
    dplyr::mutate(UPDRS_source = 'MDS-UPDRS')

  old %<>%
    dplyr::select(session_id, visit_date, H_Y, MDS_Part_III) %>%
    # make names consistent:
    dplyr::rename(UPDRS_date = visit_date, Part_III = MDS_Part_III) %>%
    dplyr::mutate(UPDRS_source = 'UPDRS 1987')

  motor_scores <- dplyr::bind_rows(old, MDS)

  # some people got multiple UPDRS assessments per overall session
  # (e.g. an abbreviated session triggered a full assessment a few
  # days or weeks later and the UPDRS was done on each occasion). In
  # this case, we delete all but one UPDRS per universal session id,
  # to prevent issues with multiple matches with other data:
  motor_scores %<>%
    dplyr::group_by(session_id) %>%
    dplyr::arrange(desc(UPDRS_date)) %>%
    # count duplicates within a session:
    dplyr::mutate(n = dplyr::row_number()) %>% # n==1 will be the latest one
    dplyr::filter(n == 1) %>% # remove all but last record
    dplyr::select(-n, -UPDRS_date) # drop the temporary counter

  return(motor_scores)

}

#' Import MDS-UPDRS scores.
#'
#' \code{import_MDS_UPDRS} Import the total scores calculated from the
#' MDS-UPDRS individual items for each part and the Hoehn & Yahr stage.
#'
#' @param concise If \code{TRUE}, return only H & Y stage and totals for Parts
#' I, II, and III. If \code{FALSE}, retain all individual item scores.
#'
#' @return A dataframe containing the scores.
#'
#' @examples
#' \dontrun{
#' full_mds_updrs <- import_MDS_UPDRS(concise = FALSE)
#' }
#' @export
import_MDS_UPDRS <- function(concise = TRUE) {
  MDS_UPDRS <- import_helper('mds_updrs') %>%
    # we have some columns to explicitly record whether values are known to be
    # missing (vs perhaps just not having been entered yet). Make boolean:
    dplyr::mutate(H_Y_missing      = (H_Y_missing == 'Y'),
                  part_III_missing = (part_III_missing == 'Y')) %>%
    # tidy up errors in subject ids and session suffixes in source data:
    map_to_universal_session_id()

  # re-calculate part scores for the MDS-UPDRS
  # (i.e. don't rely on the spreadsheet-calculated totals)
  # Here, we make dynamic index references using column names rather
  # than literal numbered ranges
  # e.g. Q1_1 to Q1_13 instead of [6:18] for Part I, to make things less
  # brittle if columns are added to the source spreadsheet.
  Q1_1_i  <- which(colnames(MDS_UPDRS) == 'Q1_1')
  Q1_13_i <- which(colnames(MDS_UPDRS) == 'Q1_13')
  Q2_1_i  <- which(colnames(MDS_UPDRS) == 'Q2_1')
  Q2_13_i <- which(colnames(MDS_UPDRS) == 'Q2_13')
  Q3_1_i  <- which(colnames(MDS_UPDRS) == 'Q3_1')
  Q3_18_i <- which(colnames(MDS_UPDRS) == 'Q3_18')

  MDS_UPDRS %<>%
    dplyr::mutate(Part_I   = rowSums(.[Q1_1_i:Q1_13_i], na.rm = TRUE),
                  Part_II  = rowSums(.[Q2_1_i:Q2_13_i], na.rm = TRUE),
                  Part_III = rowSums(.[Q3_1_i:Q3_18_i], na.rm = TRUE))

  # some people are missing whole parts of the scale (eg in some assessments,
  # only Part III is assessed).
  MDS_UPDRS$Part_1_missing <- round(rowSums(is.na(MDS_UPDRS[Q1_1_i:Q1_13_i]))/13)
  MDS_UPDRS$Part_2_missing <- round(rowSums(is.na(MDS_UPDRS[Q2_1_i:Q2_13_i]))/13)
  MDS_UPDRS$Part_3_missing <- round(rowSums(is.na(MDS_UPDRS[Q3_1_i:Q3_18_i]))/33)

  # set the corresponding totals to NA, as some still get calculated
  # erroneously as zero:
  MDS_UPDRS$Part_I[MDS_UPDRS$Part_1_missing   == 1] = NA
  MDS_UPDRS$Part_II[MDS_UPDRS$Part_2_missing  == 1] = NA
  MDS_UPDRS$Part_III[MDS_UPDRS$Part_3_missing == 1] = NA

  # drop records where both H&Y and Part III are missing:
  MDS_UPDRS %<>%
    dplyr::filter(!(is.na(H_Y) & is.na(part_III_missing))) %>%
    dplyr::select(-H_Y_missing, -part_III_missing)

  if (concise == TRUE) { # return only summary measures
    MDS_UPDRS %<>% dplyr::select(session_id, H_Y, Part_I:Part_III)
  }

  return(MDS_UPDRS)
}

#' Import original UPDRS (1987 version) scores.
#'
#' \code{import_old_UPDRS} Import original UPDRS scale data, which was used in
#' the earliest years of  data collection. The raw items here are different, and
#' so we also calculate Part II and Part III scores which have been scaled to
#' match the MDS-UPDRS equivalents using the method described by from Goetz et
#' al. 2012. Mov Disorders, 27(10):1239-1242.
#'
#' @param concise If \code{TRUE}, return only H & Y stage and totals for Parts
#'   I, II, and III. If \code{FALSE}, retain all individual item scores.
#'
#' @return A dataframe containing the scores.
#'
#' @examples
#' \dontrun{
#' part_III <- import_motor_scores()
#' }
#' @export
import_old_UPDRS <- function() {
  old_UPDRS <- import_helper('old_updrs') %>%
    # remove 2nd sessions from Charlotte Graham's amantadine follow-up study,
    # where there wasn't a full NP session done, as the session was only a few
    # months after the baseline session:
    dplyr::filter(Full_neuropsych == 'y') %>%
    # tidy up errors in subject ids and session suffixes in source data:
    map_to_universal_session_id()

  # calculate part scores for the UPDRS
  # (non-motor, ADL, motor).
  # Here, we make references using column names rather than numbered ranges
  # e.g. Q1_1 to Q1_13 instead of [6:18] for Part I, to make things less
  # brittle if columns are added to the source spreadsheet, but this requires
  # some jiggery pokery to get the indices to the numbered columns

  Q1_i  <- which(colnames(old_UPDRS) == 'Q1') # Part 1, non-motor
  Q4_i  <- which(colnames(old_UPDRS) == 'Q4') #
  Q5_i  <- which(colnames(old_UPDRS) == 'Q5') # Part 2, ADLs
  Q17_i <- which(colnames(old_UPDRS) == 'Q17') #
  Q18_i <- which(colnames(old_UPDRS) == 'Q18') # Part 3, motor
  Q31_i <- which(colnames(old_UPDRS) == 'Q31') #

  old_UPDRS %<>%
    dplyr::mutate(Part_I_raw   = rowSums(.[Q1_i:Q4_i], na.rm = TRUE),
                  Part_II_raw  = rowSums(.[Q5_i:Q17_i], na.rm = TRUE),
                  Part_III_raw = rowSums(.[Q18_i:Q31_i], na.rm = TRUE))


  # scale old UPDRS part scores into values compatible with MDS-UPDRS. We use
  # conversion factors from Goetz et al. 2012. Mov Disorders, 27(10):1239-1242
  old_UPDRS %<>%
    dplyr::mutate(
      MDS_Part_II = dplyr::case_when(
        is.na(.$H_Y) ~ NA_real_, # need to explicitly handle missing H&Y
        .$H_Y < 3  ~ (.$Part_II_raw * 1.1 + 0.2),
        .$H_Y < 4  ~ (.$Part_II_raw * 1.0 + 1.5),
        .$H_Y <= 5 ~ (.$Part_II_raw * 1.0 + 4.7),
        TRUE ~ NA_real_), # any other possible, out of range, cases
      MDS_Part_III = dplyr::case_when(
        is.na(.$H_Y) ~ NA_real_, # need to explicitly handle missing H&Y
        .$H_Y < 3  ~ (.$Part_III_raw * 1.2 + 2.3),
        .$H_Y < 4  ~ (.$Part_III_raw * 1.2 + 1.0),
        .$H_Y <= 5 ~ (.$Part_III_raw * 1.1 + 7.5),
        TRUE ~ NA_real_)) # any other possible, out of range, values

  return(old_UPDRS)
}

#' Import Hospital Anxiety and depression Scale (HADS) data.
#'
#' \code{import_HADS} Return the subtotals for HADS anxiety and HADS depression.
#'
#' This data is held in a fixed Google Sheet and so no parameters are
#' necessary for this function.
#'
#' @param concise If \code{TRUE}, return only the Anxiety and Depression sub-
#' totals. If \code{FALSE}, also return all individual item scores.
#'
#' @return A dataframe containing the HADS data.
#'
#' @examples
#' \dontrun{
#' hads <- import_HADS()
#' }
#' @export
import_HADS <- function(concise = TRUE) {
  HADS <- import_helper('hads')

  HADS %<>% map_to_universal_session_id()

  # calculate component scores for the HADS:
  HADS %<>%
    # can't just sum columns directly, as NAs propagate, so use rowSums:
    dplyr::mutate(HADS_anxiety = # odd items
                    rowSums(cbind(Q1, Q3, Q5, Q7, Q9, Q11, Q13), na.rm = TRUE),
                  HADS_depression = # even items
                    rowSums(cbind(Q2, Q4, Q6, Q8, Q10, Q12, Q14), na.rm = TRUE))

  if (concise == TRUE) { # return only summary measures
    HADS %<>% dplyr::select(session_id, HADS_anxiety, HADS_depression)
  }

  return(HADS)
}

#' Import medications data.
#'
#' \code{import_medications} Return doses of anti-parkinsonian medications in
#' use at the time of the assessment session.
#'
#' @param concise If \code{TRUE}, return only the calculated LED (levodopa
#'   equivalent daily dose). If \code{FALSE}, also return all individual
#'   medication doses.
#'
#' @return A dataframe containing medication doses.
#'
#' @examples
#' \dontrun{
#' meds <- import_medications()
#' }
#' @export
import_medications <- function(concise = TRUE) {
  # Medications list:
  meds <- import_helper('meds') %>%
    map_to_universal_session_id()

  ## calculate levodopa equivalent dose (LED)
  # The source data contains total daily doses expressed as a single number for
  # each type of medication (mg/day).

  # replace all NAs with 0 in the medication dose columns (from 6 onwards):
  meds[6:(ncol(meds)-2)][is.na(meds[6:(ncol(meds)-2)])] <- 0

  ## calculate the LED subtotal for each type of medication.
  # total immediate release l-dopa - combination of sinemet, madopar, sindopa,
  # kinson (ref = systematic review):
  meds %<>% dplyr::mutate(ldopa = sinemet + madopar + sindopa + kinson)

  # controlled release l-dopa - combination of sinemet CR and madopar HBS
  # (ref = systematic review Mov. Disord. 2010. 25(15):2649-2653):
  meds %<>% dplyr::mutate(cr_ldopa = (sinemet_cr + madopar_hbs) * 0.75)

  # conversion if COMT inhibitors are taken. Need to convert CR ldopa first
  # then multiply by COMT factor:
  meds %<>%
    dplyr::mutate(comt_ir =
                    dplyr::if_else(ir_entacapone > 0,
                                   (ir_entacapone * 0.33),
                                   dplyr::if_else(ir_tolcapone > 0,
                                                  (ir_tolcapone * 0.5), 0)),
                  comt_cr =
                    dplyr::if_else(cr_entacapone > 0,
                                   (cr_entacapone * 0.75 * 0.33),
                                   dplyr::if_else(cr_tolcapone > 0,
                                                  (cr_tolcapone * 0.75 *0.5), 0)))

  # conversion of dopamine agonists and other PD meds
  meds %<>% dplyr::mutate(
    amantadine.led = amantadine * 1, # ref = syst review
    apomorphine.led = apomorphine * 10, # ref = syst review
    bromocriptine.led = bromocriptine * 10, #ref = syst review (suppl. material)
    pergolide.led = pergolide * 100, # ref - Evans (2004) Mov Disord 19:397-405
    lisuride.led = lisuride * 100, # ref - Parkin (2002) Mov Disord 17:682-692
    ropinirole.led = ropinirole * 20, # ref = syst review
    pramipexole.led = pramipexole * 100, # ref = syst review
    selegiline.led = selegiline * 10, # ref = syst review. This value (* 10) is
    # for oral formulations. If on sublingual formulations need to use
    # conversion factor (* 80). Tim does not have any patients taking sublingual
    # formulations.
    rotigotine.led = rotigotine * 30, # ref = syst review
    duodopa.led = duodopa * 1.11 # ref = syst review. Only available to those
    # in clinical trial.
  )
  # Rasagiline (conversion factor * 100) #ref = syst review. Has not been
  # included since it is currently not available in NZ.

  # Anticholinergics are not included in LED calculations but make an indicator
  # column anyway:
  meds <- dplyr::mutate(meds, anticholinergics =
                         as.factor(ifelse ((orphenadrine > 0 | benztropine > 0 |
                                              procyclidine > 0), 'Yes', 'No')))

  # calculate levodopa equivalent dose:
  meds %<>% dplyr::mutate(
    LED = (ldopa + cr_ldopa + comt_ir + comt_cr + amantadine.led +
             apomorphine.led + bromocriptine.led + pergolide.led +
             lisuride.led + ropinirole.led + pramipexole.led +
             selegiline.led + rotigotine.led),
    LED = round(LED, digits = 2))

  # some people got multiple meds assessments per overall session
  # (e.g. an abbreviated session triggered a full assessment a few
  # days or weeks later and the meds were taken on each occasion). In
  # this case, we delete all but one per universal session id
  # to prevent issues with multiple matches to other data:
  meds %<>%
    dplyr::group_by(session_id) %>%
    dplyr::arrange(desc(med_date)) %>%
    # count duplicates within a session:
    dplyr::mutate(n = dplyr::row_number()) %>% # n==1 will be the latest one
    dplyr::filter(n == 1) %>% # remove all but last record
    dplyr::select(-n) # drop the temporary counter

  if (concise == TRUE) { # return only LED
    meds %<>% dplyr::select(session_id, LED)
  }

  return(meds)
}

#' Import neuropsyc data.
#'
#' \code{import_neuropsyc} Return various test results from the neuropsyc
#' data, as periodically exported from the REDCap database.
#'
#' @param concise If \code{TRUE}, return only selected variables (e.g. MoCA,
#'   global z, domain z). If \code{FALSE}, also return all individual test
#'   scores, allowing more detailed analyses.
#'
#' @return A dataframe containing neuropsyc scores. Notes for particular variables:
#' 
#' \describe{
#'   \item{global_z}{A mean of all z-scores. Is returned as NA if there
#'      are fewer than four z-scores or if the session is marked as 
#'      incomplete in REDCap (data not fully collected or entered).}
#'   \item{global_z_no_language}{A historical version of global_z that is the mean
#'      of domain mean scores, excluding the language domain.}
#'   \item{n_z_scores}{The number of z_scores that have been used in calculating
#'      the global_z mean and cognitive status.}
#'   \item{mci_criteria_requirements_met}{Whether the number of test requirements
#'      by domain for Level-II criteria have been met when determining the 
#'      cognitive status.}
#'   \item{cognitive_status}{Normal-range cognition (N),  Mild cognitive 
#'      impairment (MCI) or dementia (D). Will return NA when there are
#'      fewer than four z-scores or the session is marked as incomplete
#'      and doesn't have an overriding dementia diagnosis. Several of these
#'      diagnoses don't strictly meet Level-II criteria and this can be 
#'      checked by looking at the mci_criteria_requirements_met variable.
#'      Depending on your analysis some of these may need to be excluded.
#'      In some cases this is due to missing language tests from when the 
#'      study started, in other cases it is due to a greater number of tests
#'      not being administered (and can be examined by looking at n_z_scores).}
#' }
#'   
#' @examples
#' \dontrun{
#' np <- import_neuropsyc()
#' }
#' @export
import_neuropsyc <- function(concise = TRUE) {
  np <- import_helper('np')

  np %<>% # name selected variables neatly
    dplyr::rename(session_date = np1_date,
                  np_group = group,
                  full_assessment = session_type,
                  cognitive_status = nzbri_criteria,
                  global_z_no_language = global_z_historical,
                  MoCA = moca,
                  WTAR = wtar_wais_3_fsiq,
                  attention_domain = attention_mean,
                  executive_domain = executive_mean,
                  visuo_domain = visuo_mean,
                  learning_memory_domain = memory_mean,
                  language_domain = language_mean)

  # do some tidying:
  np %<>%
    # label some factors neatly:
    dplyr::mutate(
      cognitive_status = factor(cognitive_status,
                                levels = c('U', 'MCI', 'D'),
                                labels = c('N', 'MCI', 'D'),
                                ordered = TRUE),
      # make some columns boolean:
      full_assessment =
        dplyr::if_else(full_assessment == 'Short', FALSE, TRUE, TRUE),
      np_excluded =
        dplyr::case_when(neuropsych_excluded == 'Y' ~ TRUE,
                         neuropsych_excluded == 'N' ~ FALSE,
                         TRUE ~ NA))

  np %<>%
    dplyr::arrange(subject_id, session_date) %>%
    dplyr::group_by(subject_id) %>% # within each subject
    # get the baseline values of some variables:
    dplyr::mutate(date_baseline = dplyr::first(session_date),
                  global_z_baseline = dplyr::first(global_z),
                  diagnosis_baseline = dplyr::first(diagnosis),
                  session_number = dplyr::row_number(),
                  years_from_baseline =
                    as.numeric(round(difftime(session_date, date_baseline,
                                              units = 'days')/365.25,
                                     digits = 2)),
                  # Find longest follow-up time:
                  FU_latest = max(years_from_baseline)) %>%
    dplyr::ungroup() %>% # leaving it grouped can cause issues later
    dplyr::select(-subject_id, -session_date)

  # drop variables to make the data easier to manage:
  if (concise == TRUE) { # return only summary measures
    np %<>%
      dplyr::select(session_id, np_excluded,
                    full_assessment, n_z_scores, mci_criteria_requirements_met,
                    diagnosis, np_group, cognitive_status,
                    MoCA, WTAR, global_z, global_z_no_language, npi,
                    attention_domain,executive_domain, visuo_domain,
                    learning_memory_domain, language_domain, date_baseline,
                    global_z_baseline, diagnosis_baseline, session_number,
                    years_from_baseline, FU_latest)
  } else { # return most columns, for more detailed analysis
    np %<>%
      dplyr::select(session_id, np_excluded,
                    full_assessment, np_group, cognitive_status, MoCA,
                    WTAR,
                    dplyr::everything()) %>%
      dplyr::select(-session_labels, -sex, -age, -education)
  }

  return(np)
}

#' Import metadata on MRI scans
#'
#' \code{import_MRI} accesses the scan number and associated information
#'  from each MRI scanning session.
#'
#' @param exclude If \code{TRUE}, don't return sessions that have been marked as
#'   excluded. For safety, we default to \code{TRUE} so that you don't get junk
#'   data unless you explicitly ask for it. You might set this to \code{FALSE}
#'   in order to get all records if you need to document exclusions, for example
#'  to create a participant recruitment and exclusion flowchart.
#'
#' @param print_exclude_summary If \code{TRUE}, print a table that lists
#'   the number of records that have been excluded. If this looks problematic,
#'   you could set \code{exclude = FALSE} to import all records to identify
#'   which ones were excluded.
#'
#' @param exclude If \code{TRUE}, don't return records marked as excluded.
#'
#' @return A dataframe containing the MRI metadata.
#'
#' @examples
#' \dontrun{
#' mri <- import_MRI()
#' }
#' @export
import_MRI <- function(exclude = TRUE, print_exclude_summary = TRUE) {
  # Import data on MRI scans

  # Need a handle to the scan numbers Google sheet.
  # Access the spreadsheet by its title.
  # This requires an initial authentication in the browser:
  MRI <- import_helper('mri') %>%
    dplyr::rename(mri_excluded = excluded) %>%
    # tidy manual data entry errors of session IDs:
    dplyr::mutate(subject_id = sanitise_session_ids(subject_id))

  if (exclude) {
    excluded_MRI <- MRI %>%
      dplyr::filter(!is.na(mri_excluded) & mri_excluded != 'Included')

    if (print_exclude_summary & nrow(excluded_MRI)>0){
      cat('Excluded cases:')
      excluded_MRI %>%
        dplyr::select(subject_id, scan_number, scan_date) %>%
        knitr::kable(caption = 'Automatically excluded MRI scans.') %>%
        print()
    }

    MRI %<>%
      dplyr::filter(is.na(mri_excluded) | mri_excluded == 'Included') %>%
      dplyr::select(-mri_excluded, -study)
  }

  return(MRI %>% map_to_universal_session_id())
}

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
#' pet <- import_PET_scan_numbers()
#' }
#' @export
import_PET <- function(exclude = TRUE) {
  # Need a handle to the scan numbers Google sheet.
  # Access the spreadsheet by its title.
  # This requires an initial authentication in the browser:
  PET <- import_helper('pet') %>%
    # tidy manual data entry errors of session IDs:
    dplyr::mutate(subject_id = sanitise_session_ids(subject_id))

  # choose whether to filter out excluded records before they get to the user:
  if (exclude) {
    PET %<>%
      dplyr::filter(is.na(pet_excluded) | pet_excluded != TRUE) %>%
      dplyr::select(-pet_excluded)
  }

  # drop unneeded variables and give some unique names:
  PET %<>% dplyr::select(-do_not_use_group) %>%
    dplyr::rename(pet_np1_date = np1_date,
                  pet_mri_date = mri_date,
                  pet_mri_scan = mri_scan,
                  pet_dose = dose,
                  pet_notes = notes)

  return(PET)
}

#' Import data from biosamples collection sessions.
#'
#' \code{import_bloods}
#'
#' This PET information is held in a fixed Google Sheet and so no parameters are
#' necessary for this function.
#'
#' @return A dataframe containing the blood samples and other data.
#'
#' @examples
#' \dontrun{
#' bloods <- import_bloods()
#' }
#' @export
import_bloods <- function() {
  # Need a handle to the bloods Google sheet.
  # Access the spreadsheet by its title.
  # This requires an initial authentication in the browser:
  bloods <- import_helper('bloods') %>%
    # drop records which are scheduled but have not yet occurred:
    dplyr::filter(!is.na(date_collected)) %>%
    # tidy any manual data entry errors of session IDs:
    dplyr::mutate(blood_tube_id = sanitise_session_ids(blood_tube_id)) %>%
    tidyr::unite(subject_id, session_suffix, col = 'true_session_id',
                 sep = '_', remove = FALSE) %>%
    map_to_universal_session_id(remove_double_measures = FALSE,
                                drop_original_columns = FALSE)

  return(bloods)
}

#' Import data from the Sydney hallucinations questionnaire.
#'
#' \code{import_hallucinations} Get patient and significant-other ratings of
#' hallucinations and delusions, usinf the sacle developed by Prof. Simon Lewis'
#' team in Sydney.
#'
#' @return A dataframe containing the hallucination data.
#'
#' @examples
#' \dontrun{
#' hallucinations <- import_hallucinations()
#' }
#' @export
import_hallucinations <- function() {
  hallucinations <- import_helper('hallucinations') %>%
    dplyr::mutate(hallucinations_date = lubridate::ymd(hallucinations_date)) %>%
    # for some reason, we get lots of blanks rows:
    dplyr::filter(!subject_id == '') %>%
    # tidy up errors in subject ids and session suffixes in source data.
    # Don't remove double measures, as we get two rows per session: one from the
    # patient and one from the significant other.
    map_to_universal_session_id(remove_double_measures = FALSE,
                                drop_original_columns = FALSE)

  return(hallucinations)
}
