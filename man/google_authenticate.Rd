% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_data.R
\name{google_authenticate}
\alias{google_authenticate}
\title{Allow Google Drive authorisation via a server}
\usage{
google_authenticate(
  email = chchpd_user(),
  use_server = chchpd_check_rstudio_server()
)
}
\arguments{
\item{email}{Allows you to nominate a particular nzbri.org e-mail address to
use for authentication. This can save you having to specify it manually each
time the script is run, but shouldn't be used in reproducible workflows
(where other users have to be able to run the code). The default value of
`TRUE` means that if authentication has occurred before, and there is just
one e-mail address available to select, it will automatically do that.}

\item{use_server}{If \code{TRUE}, use the copy/paste authentication process.
If \code{FALSE}, use fully browser-contained authentication.}
}
\value{
No return value: the function initiates a process which results in
the writing of a \code{.httr-oauth} token file to disk.
}
\description{
\code{google_authenticate} To access data on a Google drive requires that the
user is authenticated, to ensure that you are entitled to view it.
}
\details{
If using RStudio on a server, specifying `use_server = TRUE` will institute
a different authentication process to when you are running locally.
}
