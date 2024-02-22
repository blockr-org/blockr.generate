#' @export
define <- function(..., ignore = FALSE){
  structure(
    list(...),
    ignore = ignore,
    class = "definition"
  )
}

get_definition <- function(x, name) UseMethod("get_definition", x)

#' @export
get_definition.default <- function(x, name){
  list()
}

#' @export
get_definition.definition <- function(x, name){
  x[[name]]
}

should_ignore <- function(x) UseMethod("should_ignore")

#' @export
should_ignore.default <- function(x) FALSE

#' @export
should_ignore.definition <- function(x){
  attr(x, "ignore")
}
