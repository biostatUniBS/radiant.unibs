#' Launch Radiant in the default browser
#'
#'
#' @export
radiant.biostat <- function() {
  if (!"package:radiant.biostat" %in% search())
    if (!require(radiant.biostat)) stop("Calling radiant.biostat start function but radiant.biostat is not installed.")
  runApp(system.file("app", package = "radiant.biostat"), launch.browser = TRUE)
}
