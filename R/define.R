#' Define
#' 
#' Define a function or argument for processing.
#' 
#' @param ... Named arguments to define.
#' @param ignore Whether to ignore the arguments from fields and function call.
#' 
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

handle_fields <- function(fields, function_definition, all_args){
  nms <- names(fields)

  fields <- lapply(nms, \(nm) {
    arg_def <- get_definition(function_definition, nm)

    if(should_ignore(arg_def))
      return(NULL)

    if(length(arg_def))
      return(arg_def)

    arg_def <- get_definition(all_args, nm)

    if(should_ignore(arg_def))
      return(NULL)

    if(length(arg_def))
      return(arg_def)

    fields[[nm]]
  })

  names(fields) <- nms

  if(!length(fields))
    return(c())

  fields[sapply(fields, length) > 0L]
}
