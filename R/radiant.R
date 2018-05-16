#' Launch radiant in the default browser
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant()
#' }
#' @export
radiant.unibs <- function() radiant.data::launch(package = "radiant", run = "browser")
