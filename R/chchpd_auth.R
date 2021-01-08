chchpd_has_token <- function(ensure_token = FALSE){
  has_token <- googlesheets4::gs4_has_token() && googledrive::drive_has_token()
  if (ensure_token){
    google_authenticate(email = chchpd_user())
  }
  has_token <- googlesheets4::gs4_has_token() && googledrive::drive_has_token()
  invisible(has_token)
}


from_permitted_package <- function(env = parent.frame()) {
  env <- topenv(env, globalenv())
  if (!isNamespace(env)) {
    return(FALSE)
  }
  
  nm <- getNamespaceName(env)
  nm %in% c("chchpd")
}

check_permitted_package <- function(env = parent.frame()) {
  if (!from_permitted_package(env)) {
    msg <- paste(
      "Attempt to directly access a credential that can only be used within tidyverse packages.",
      "This error may mean that you need to:",
      "  * Create a new project on Google Cloud Platform",
      "  * Enable relevant APIs for your project",
      "  * Create an API key and/or an OAuth client ID",
      "  * Configure your requests to use your API key and OAuth client ID",
      sep = "\n"
    )
    rlang::abort(msg)
  }
  invisible(env)
}

#' Default Google app to use with CHCHPD.
chchpd_oauth_app <- function() {
  check_permitted_package(parent.frame())
  coa()
}

#' Check whether the rstudio is running in server mode.
chchpd_check_rstudio_server <- function(){
  check_server <- try(rstudioapi::versionInfo()$mode == 'server', silent = TRUE)
  if(length(check_server) == 0 || is(T, 'try-error')){
    check_server <- FALSE
  }
  invisible(check_server)
}


chchpd_user <- function(){
  if(googlesheets4::gs4_has_token()){
    user <- googlesheets4::gs4_user()
  } else if (googledrive::drive_has_token()) {
    user <- googledrive::drive_user()
  } else {
    user <- TRUE
  }
  return(user)
}