#' Allow Google Drive authorisation via a server
#'
#' \code{google_authenticate} To access data on a Google drive requires that the
#' user is authenticated. This will happen automatically if required when using
#' any of the \code{import_} functions in this package. The standard
#' browser-based authorisation process will not work, however, when running
#' RStudio via a server (because the IP addresses of the user's computer and the
#' server do not match). This function allows authorisation to proceed for
#' server-based users.
#'
#' If using RStudio on a server, then you should run this function before
#' attempting to import any datasets from the Google drive. This will institute
#' a different authentication process. You will still be taken to a webpage,
#' but instead of authenticating within that page, you will be given a string
#' of characters to copy. You must then paste that into the RStudio console
#' (there should be a prompt there expecting this input). This will generate
#' an authentication token that will be stored on your folder on the server.
#' Be aware that this token (in the \code{.httr-oauth} file) could allow any
#' user to access your files and privileges on the Google drive. Do not
#' transmit or distribute it, or allow it to be included in version control
#' (this also applies if this file is created in a local folder when
#' running RStudio on your own computer).
#'
#' @param use_server If \code{TRUE}, use the copy/paste authentication process.
#' If \code{FALSE}, use fully browser-contained authentication.
#'
#' The ability to specify \code{use_server = FALSE} is provided for completeness
#' but shouldn't be necessary in most cases (maybe your script is set up to run
#' either locally or on the server, and this would allows the authentication
#' method to be specified conditionally).
#'
#' @return No return value: the function initiates a process which results in
#' the writing of a \code{.httr-oauth} token file to disk.
#' @export
google_authenticate <- function(use_server = TRUE) {
  options(httr_oob_default = use_server)
  googlesheets::gs_auth()
}

#' Import participant demographics
#'
#' \code{import_participants} Access the participant information that should be
#' constant across all sessions (for example, date of birth, sex, etc).
#'
#' This information is exported priodically from the Alice database.
#'
#' @param anon If \code{TRUE}, return the anonymous ID. Otherwise, return the
#' in-house, non-secure ID.
#'
#' @return A dataframe containing the participant data.
#'
#' @examples
#' \dontrun{
#' participants = import_participants()
#' }
#' @export
import_participants <- function(anon = FALSE) {

  # import a file that is regularly exported via a cron job
  # from the Alice database:
  participants = googlesheets::gs_title(chchpd_env$participant_filename) %>%
    googlesheets::gs_read(na = c('NA', 'None'))

  # report errors where cell contents don't match expected type for the column:
  if (nrow(readr::problems(participants)) > 0) {
    print(readr::problems(participants))
  }

  participants %<>%
    dplyr::rename(subject_id = SubjectId,
                  anon_id = AnonId,
                  survey_id = SurveyId,
                  participant_status = Status,
                  date_of_death = DateOfDeath,
                  excluded_from_followup = Excluded,
                  birth_date = BirthDate,
                  participant_group = DiseaseGroup,
                  sex = Sex,
                  side_of_onset = SideAffected,
                  symptom_onset_age = SymptomOnsetAge,
                  diagnosis_age = DiagnosisAge,
                  education = Education,
                  ethnicity = Ethnicity) %>%
    dplyr::mutate(birth_date = lubridate::ymd(birth_date),
                  sex = factor(sex, levels = c('Male', 'Female')),
                  participant_status = factor(participant_status),
                  dead = !is.na(date_of_death),
                  side_of_onset = factor(side_of_onset)) %>%
    dplyr::select(subject_id, anon_id, survey_id, date_of_death, dead,
                  participant_status, excluded_from_followup, birth_date,
                  participant_group, sex, side_of_onset, symptom_onset_age,
                  diagnosis_age, education, ethnicity)

  tabulate_duplicates(participants, 'subject_id')

  return(participants)
}

#' Import links between sessions and studies
#'
#' \code{import_sessions} Access a table that links a given session to the
#' (possibly multiple) studies of which it is a part, and the group within that
#' study to which the associated participant belongs.
#'
#' This information is exported priodically from the Alice database.
#'
#' @param from_study Optionally specify the name of a specific study to limit
#'   the records returned to just those from that study. Inspect the sessions
#'   spreadsheet to see the valid opions. e.g. \code{'PET'} or
#'   \code{'Follow-up'} for the Progression study.
#'
#' @param exclude If \code{TRUE}, don't return sessions that have been excluded
#'   from a given study.
#'
#' @return A dataframe containing the session-group-study data.
#'
#' @examples
#' \dontrun{
#' sessions = import_sessions()
#' }
#' @export
import_sessions <- function(from_study = NULL, exclude = TRUE) {

  # import a file that is regularly exported via a cron job
  # from the Alice database:
  sessions = googlesheets::gs_title(chchpd_env$session_filename) %>%
    googlesheets::gs_read() %>%
    # extract session suffix (e.g. F2, D0), useful for some purposes:
    dplyr::mutate(session_suffix = stringr::str_match(SessionId, '_(.*)')[,2]) %>%
    dplyr::select(SessionId, SubjectId, session_suffix, Study, Date, StudyGroup,
                  StudyExcluded, MRIScanNo) %>%
    dplyr::rename(session_label = SessionId,
                  subject_id = SubjectId,
                  session_date = Date,
                  study = Study,
                  study_group = StudyGroup,
                  study_excluded = StudyExcluded) %>%
    dplyr::mutate(study = factor(study),
                  session_date = lubridate::ymd(session_date)) %>%
    # filter out scheduled future visits:
    #filter(session_date < lubridate::today()) %>%
    # tidy up errors in subject ids and session suffixes in source data:
    map_to_universal_session_id(make_session_label = FALSE,
                                remove_double_measures = FALSE)

  if (exclude) {
    sessions %<>% dplyr::filter(is.na(study_excluded) | study_excluded != TRUE)
  }

  if (assertthat::is.string(from_study)) {
    sessions %<>% dplyr::filter(study == from_study)
  }

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
#' updrs = import_motor_scores()
#' }
#'@export
import_motor_scores <- function() {
  # The study used both the original and revised MDS version of the UPDRS.
  # To get a dataframe that contains just the Part III motor score and H & Y
  # score, we need to import both datasets separately then combine the MDS
  # Part III and the equivalent scaled Part III from the old UPDRS file, as well
  # as the H & Y.
  MDS = import_MDS_UPDRS(concise = FALSE)

  old = import_old_UPDRS()

  # select just the lowest-common-denominator columns from each dataset,
  # so they can be united:
  MDS %<>%
    dplyr::select(session_id, UPDRS_date, H_Y, Part_III) %>%
    dplyr::mutate(UPDRS_source = 'MDS-UPDRS')

  old %<>%
    dplyr::select(session_id, visit_date, H_Y, MDS_Part_III) %>%
    # make names consistent:
    dplyr::rename(UPDRS_date = visit_date, Part_III= MDS_Part_III) %>%
    dplyr::mutate(UPDRS_source = 'UPDRS 1987')

  motor_scores = dplyr::bind_rows(old, MDS)

  # some people got multiple UPDRS assessments per overall session
  # (e.g. an abbreviated session triggered a full assessment a few
  # days or weeks later and the UPDRS was done on each occasion). In
  # this case, we delete all but one UPDRS per universal session id,
  # to prevent issues with multiple matches with other data:
  motor_scores %<>%
    dplyr::group_by(session_id) %>%
    dplyr::arrange(desc(UPDRS_date)) %>%
    # count duplicates within a session:
    dplyr::mutate(n = row_number()) %>% # n==1 will be the lastest one
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
#' full_mds_updrs = import_MDS_UPDRS(concise = FALSE)
#' }
#' @export
import_MDS_UPDRS <- function(concise = TRUE) {
  # get a handle to the clinical spreadsheet:
  clinical_ss = googlesheets::gs_title(chchpd_env$clinical_filename)

  MDS_UPDRS = googlesheets::gs_read(ss = clinical_ss,
                                    ws = 'MDS_UPDRS',
                                    na = c('na', 'NA', 'NA1', 'NA2', 'NA3',
                                           'NA4', 'UR')) %>%
    # safely convert some columns to numeric:
    dplyr::mutate(H_Y = as.numeric(H_Y)) %>%
    dplyr::mutate_each(dplyr::funs(as.numeric), dplyr::starts_with('Q')) %>%
    dplyr::mutate_each(dplyr::funs(as.numeric),
                       dplyr::starts_with('Part_', ignore.case = FALSE)) %>%
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

  Q1_1_i  = which(colnames(MDS_UPDRS) == 'Q1_1')
  Q1_13_i = which(colnames(MDS_UPDRS) == 'Q1_13')
  Q2_1_i  = which(colnames(MDS_UPDRS) == 'Q2_1')
  Q2_13_i = which(colnames(MDS_UPDRS) == 'Q2_13')
  Q3_1_i  = which(colnames(MDS_UPDRS) == 'Q3_1')
  Q3_18_i = which(colnames(MDS_UPDRS) == 'Q3_18')

  MDS_UPDRS %<>%
    dplyr::mutate(Part_I   = rowSums(.[Q1_1_i:Q1_13_i], na.rm = TRUE),
                  Part_II  = rowSums(.[Q2_1_i:Q2_13_i], na.rm = TRUE),
                  Part_III = rowSums(.[Q3_1_i:Q3_18_i], na.rm = TRUE))

  # some people are missing whole parts of the scale (eg in some assessments,
  # only Part III is assessed).
  MDS_UPDRS$Part_1_missing = round(rowSums(is.na(MDS_UPDRS[Q1_1_i:Q1_13_i]))/13)
  MDS_UPDRS$Part_2_missing = round(rowSums(is.na(MDS_UPDRS[Q2_1_i:Q2_13_i]))/13)
  MDS_UPDRS$Part_3_missing = round(rowSums(is.na(MDS_UPDRS[Q3_1_i:Q3_18_i]))/33)

  # set the corresponding totals to NA, as some still get calculated
  # erroneously as zero:
  MDS_UPDRS$Part_I[MDS_UPDRS$Part_1_missing   == 1] = NA
  MDS_UPDRS$Part_II[MDS_UPDRS$Part_2_missing  == 1] = NA
  MDS_UPDRS$Part_III[MDS_UPDRS$Part_3_missing == 1] = NA

  # drop records where both H&Y and Part III are missing:
  MDS_UPDRS %<>%
    dplyr::filter(!is.na(H_Y) & !is.na(part_III_missing)) %>%
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
#' part_III = import_motor_scores()
#' }
#' @export
import_old_UPDRS <- function() {
  # get a handle to the clinical spreadsheet:
  clinical_ss = googlesheets::gs_title(chchpd_env$clinical_filename)

  old_UPDRS = googlesheets::gs_read(ss = clinical_ss,
                                    ws = 'Old_UPDRS') %>%
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

  Q1_i  = which(colnames(old_UPDRS) == 'Q1') # Part 1, non-motor
  Q4_i  = which(colnames(old_UPDRS) == 'Q4') #
  Q5_i  = which(colnames(old_UPDRS) == 'Q5') # Part 2, ADLs
  Q17_i = which(colnames(old_UPDRS) == 'Q17') #
  Q18_i = which(colnames(old_UPDRS) == 'Q18') # Part 3, motor
  Q31_i = which(colnames(old_UPDRS) == 'Q31') #

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
#' hads = import_HADS()
#' }
#' @export
import_HADS <- function(concise = TRUE) {
  # get a handle to the clinical spreadsheet:
  clinical_ss = googlesheets::gs_title(chchpd_env$clinical_filename)

  HADS = googlesheets::gs_read(ss = clinical_ss,
                               ws = 'HADS',
                               col_types = readr::cols(subject_id =
                                                         readr::col_character(),
                                                       session_suffix =
                                                         readr::col_character(),
                                                HADS_date = readr::col_date(),
                                                .default = readr::col_integer()),# the item scores
                               na = c('NA', 'NA1', 'NA2', 'NA3'),
                               range = googlesheets::cell_cols('A:Q')) # don't
  # include the totals columns, as they don't deal well with missing values and
  # we calculate them afresh below anyway.

  # report errors where cell contents don't match expected type for the column:
  if (nrow(readr::problems(HADS)) > 0) {
    print(kable(readr::problems(HADS)))
  }

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
#' meds = import_medications()
#' }
#' @export
import_medications <- function(concise = TRUE) {
  # get a handle to the clinical spreadsheet:
  clinical_ss = googlesheets::gs_title(chchpd_env$clinical_filename)

  # Medications list:
  meds = googlesheets::gs_read(ss = clinical_ss,
                               ws = 'Medication',
                               na = c('NA', 'NA1', 'NA2', 'NA3'))

  # report errors where cell contents don't match expected type for the column:
  if (nrow(readr::problems(meds)) > 0) {
    print(readr::problems(meds))
  }

  meds %<>%
    # alter format of some columns:
    dplyr::mutate(apomorphine = as.numeric(apomorphine),
                  duodopa = as.numeric(duodopa),
                  cr_tolcapone = as.numeric(cr_tolcapone)) %>%
    map_to_universal_session_id()

  ## calculate levodopa equivalent dose (LED)
  # The source data contains total daily doses expressed as a single number for
  # each type of medication (mg/day).

  # replace all NAs with 0 in the medication dose columns (from 6 onwards):
  meds[6:ncol(meds)][is.na(meds[6:ncol(meds)])] <- 0

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
  meds = dplyr::mutate(meds, anticholinergics =
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
    dplyr::mutate(n = row_number()) %>% # n==1 will be the latest one
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
#' spreadsheet.
#'
#' @param concise If \code{TRUE}, return only the selected variables (e.g. MoCA,
#'   global z, domain z). If \code{FALSE}, also return all individual test
#'   scores, allowing more detailed analyses.
#'
#' @return A dataframe containing medication doses.
#'
#' @examples
#' \dontrun{
#' np = import_neuropsyc()
#' }
#' @export
import_neuropsyc <- function(concise = TRUE) {
  # get a handle to the neuropsyc spreadsheet:
  neuropsyc_ss = googlesheets::gs_title(chchpd_env$neuropsyc_filename)

    np = googlesheets::gs_read(ss = neuropsyc_ss,
                               ws = 'All Data',
                               na = c('na', 'NA', 'NA1', 'NA2', 'NA3', 'NA4',
                                      'NA5', 'NA6', '#DIV/0!'))

  # report errors where cell contents don't match expected type for the column:
  if (nrow(readr::problems(np)) > 0) {
    print(readr::problems(np))
  }

  np %<>%
    # remove empty rows:
    dplyr::filter(subject_id != '') %>%
    janitor::clean_names() %>% # remove spaces from variable names, etc
    # tidy up errors in subject ids and session suffixes in source data:
    map_to_universal_session_id(remove_double_measures = TRUE)

  # extract the subject id and session date from the standardised
  # session id:
  np %<>%
    tidyr::separate(col = session_id, into = c('subject_id', 'session_date'),
                    sep = '_', remove = FALSE)

  np %<>%
    dplyr::filter(excluded_y_n != 'Y')

  # drop variables to make the data easier to manage:
  if (concise == TRUE) { # return only summary measures
    np %<>%
    dplyr::select(subject_id, session_id, excluded_y_n, session_date,
                  full_or_short_assessment, checked, pd_control,
                  nzbri_criteria, age, sex, mo_ca,
                  total_all_domains, npi_sleep, attention_total,
                  executive_function_total, visuo_total, learning_memory_total,
                  language_total)
    } else { # return most columns, for more detailed analysis
      np %<>%
        dplyr::select(subject_id, session_id, excluded_y_n, session_date,
                      full_or_short_assessment, checked, pd_control,
                      nzbri_criteria, age, sex, mo_ca,
                      wtar_predicted_wais_iii_fsiq,
                      adas_cog_70:version) # most indic tests are here
    }

  np %<>% # name selected variables neatly
    dplyr::rename(excluded_np = excluded_y_n,
                  full_assessment = full_or_short_assessment,
                  np_group = pd_control,
                  cognitive_status = nzbri_criteria,
                  MoCA = mo_ca,
                  global_z = total_all_domains,
                  attention_domain = attention_total,
                  executive_domain = executive_function_total,
                  visuo_domain = visuo_total,
                  learning_memory_domain = learning_memory_total,
                  language_domain = language_total)

  # do some tidying:
  np %<>%
    # label some factors neatly:
    dplyr::mutate(
      sex = factor(sex, levels = c('M', 'F'),
                   labels = c('Male', 'Female')),
      cognitive_status = factor(cognitive_status,
                                levels = c('U', 'MCI', 'PDD'),
                                labels = c('N', 'MCI', 'D'),
                                ordered = TRUE),
      session_date = lubridate::ymd(session_date), # convert from char to date
      # make the 'short assessment' column boolean:
      full_assessment =
        dplyr::if_else(full_assessment == 'Short', FALSE, TRUE, TRUE)) %>%
    # associate a cognitive status with short assessment sessions. We assign a
    # status here, simply by going back to the next full assessment.
    # carry forward the status values, to fill in blank cells:
    dplyr::group_by(subject_id) %>%
    dplyr::arrange(session_date) %>%
    tidyr::fill(cognitive_status) %>% # fill = 'last observation carried forward'
    dplyr::ungroup()

  # make a single diagnosis column
  np %<>%
    tidyr::unite(col = diagnosis,
                 np_group, cognitive_status, remove = FALSE) %>%
    dplyr::mutate(diagnosis =
                    factor(diagnosis, levels =
                             c('Control_N',  'Control_MCI', 'Control_NA',
                               'PD_N', 'PD_MCI', 'PD_D', 'PD_NA'),
                           labels =
                             c('Control',  'Control-MCI', 'Control unknown',
                               'PD-N', 'PD-MCI', 'PDD', 'PD unknown')))

  np %<>%
    # filter out scheduled future visits (i.e booked but no data yet):
    dplyr::filter(session_date < lubridate::today()) %>%
    dplyr::arrange(subject_id, session_date) %>%
    dplyr::group_by(subject_id) %>% # within each subject
    # get the baseline values of some variables:
    dplyr::mutate(age_baseline = dplyr::first(age),
                  date_baseline = dplyr::first(session_date),
                  global_z_baseline = dplyr::first(global_z),
                  diagnosis_baseline = dplyr::first(diagnosis),
                  session_number = dplyr::row_number(),
                  years_from_baseline =
                    round(difftime(session_date, date_baseline,
                                   units = 'days')/365.25, digits = 2),
                  # Find longest followup time:
                  FU_latest = max(years_from_baseline)) %>%
    dplyr::ungroup() # leaving it grouped can cause issues later

  return(np)
}

#' Import metadata on MRI scans
#'
#' \code{import_MRI} accesses the scan number and associated information
#'  from each MRI scanning session.
#'
#' @param exclude If \code{TRUE}, don't return records marked as excluded.
#'
#' @return A dataframe containing the MRI metadata.
#'
#' @examples
#' \dontrun{
#' mri = import_MRI()
#' }
#' @export
import_MRI <- function(exclude = TRUE) {
  # Import data on MRI scans

  # Need a handle to the scan numbers Google sheet.
  # Access the spreadsheet by its title.
  # This requires an initial authentication in the browser:
  scan_ss = googlesheets::gs_title(chchpd_env$scan_filename)

  MRI = googlesheets::gs_read(ss = scan_ss,
                              ws = 'MRI_scans') %>%
    janitor::clean_names() %>%
    dplyr::rename(mri_excluded = excluded) %>%
    # tidy manual data entry errors of session IDs:
    dplyr::mutate(subject_id = sanitise_session_ids(subject_id))

  if (exclude) {
    MRI %<>%
      dplyr::filter(is.na(mri_excluded) | mri_excluded == 'Included') %>%
      dplyr::select(-mri_excluded)
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
#' pet = import_PET_scan_numbers()
#' }
#' @export
import_PET <- function(exclude = TRUE) {
  # Need a handle to the scan numbers Google sheet.
  # Access the spreadsheet by its title.
  # This requires an initial authentication in the browser:
  scan_ss = googlesheets::gs_title(chchpd_env$scan_filename)

  PET = googlesheets::gs_read(ss = scan_ss,
                              ws = 'PET_scans') %>%
    # tidy manual data entry errors of session IDs:
    dplyr::mutate(subject_id = sanitise_session_ids(subject_id)) %>%
    janitor::clean_names()

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
#' bloods = import_bloods()
#' }
#' @export
import_bloods <- function() {
  # Need a handle to the bloods Google sheet.
  # Access the spreadsheet by its title.
  # This requires an initial authentication in the browser:
  bloods_ss = googlesheets::gs_title(chchpd_env$bloods_filename)

  bloods =
    googlesheets::gs_read(ss = bloods_ss,
                          ws = 'data',
                          col_types =
                            readr::cols(height_m = readr::col_number(),
                                        weight_kg = readr::col_number(),
                                        creatinine_umol_l = readr::col_number(),
                                        urate_mmol_l = readr::col_number())) %>%
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
#' hallucinations = import_hallucinations()
#' }
#' @export
import_hallucinations <- function() {
  # get a handle to the clinical spreadsheet:
  clinical_ss = googlesheets::gs_title(chchpd_env$clinical_filename)

  hallucinations =
    googlesheets::gs_read(ss = clinical_ss,
                          ws = 'Hallucinations Questionnaire',
                          na = c('na', 'NA', 'NA1', 'NA2', 'NA3', 'NA4', 'UR'),
                          # this sheet has a row above the column names:
                          skip = 1) %>%
    janitor::clean_names() %>% # remove spaces from variable names, etc
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
