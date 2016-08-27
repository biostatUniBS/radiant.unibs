#' Launch Radiant in the default browser
#'
#'
#' @export
radiant.unibs <- function() {
  if (!"package:radiant.unibs" %in% search())
    if (!require(radiant.unibs)) stop("Calling radiant.unibs start function but radiant.unibs is not installed.")
  runApp(system.file("app", package = "radiant.unibs"), launch.browser = TRUE)
}
