#' Observations of Human Rights Violations
#'
#' The Syrian Archive contains a record of of videos uploaded to the internet
#' by witnesses of the Syrian Civil War. Each row
#'
#' @source \url{https://syrianarchive.org/}, downloaded 2017-05-12
#' @format A data frame with columns:
#' \describe{
#'  \item{Description}{A summary of what is shown the the corresponding video.}
#'  \item{City}{Name of the City where the video was recorded.}
#'  \item{Neighborhood}{A more specific name of the area where the video was recorded.}
#'  \item{Latitude}{Latitude of recording location.}
#'  \item{Longitude}{Longitude of recording location.}
#'  \item{Reference_Code}{A code assigned by the The Syrain Archive.}
#'  \item{Violation_Type}{The type of human rights violation including but not
#'    limited to civilian casualties, use of illegal weapons, and unlawful attacks.}
#'  \item{Recording_Date}{The date when the video was recorded in ISO 8601.}
#'  \item{Recording_Hour}{A value between 0 and 23.}
#'  \item{Recording_Minute}{A value between 0 and 59.}
#'  \item{Source}{The source of the video (usually a news organization,
#'    governmental organization, or an individual) if the source is known.}
#'  \item{Youtube_ID}{The YouTube URL of the video, if the video was uploaded to YouTube.}
#'  \item{Weapons_Used}{Weapons depicted in the video including but not limitted to
#'    chemical weapons and cluster munitions.}
#'  \item{URLs_And_News}{News stories or other articles related to the corresponding video.}
#'  \item{Page_Title}{A code (seemingly a hash) that corresponds to a web page on
#'    The Syrian Archve.}
#'  \item{Video_URL}{The URL to the video on The Syrain Archive.}
#'  \item{Database_ID}{The ID for the video as assigned by The Syrian Archive.}
#' }
#'
#' @examples
#'  violations
"violations"
