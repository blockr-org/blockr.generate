#' Define
#' 
#' Define a function or argument for processing.
#' 
#' @param ... Named arguments to define.
#' @param ignore Whether to ignore the arguments from fields and function call.
#' 
#' @export
define <- function(
  ..., 
  ignore = FALSE, 
  input = NULL, 
  output = NULL,
  type = NULL,
  classes = NULL,
  output_function = NULL,
  render_function = NULL
){
  structure(
    list(...),
    ignore = ignore,
    input = input,
    output = output,
    type = type,
    classes = classes,
    output_function = output_function,
    render_function = render_function,
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

get_input <- function(x) UseMethod("get_input")

#' @export
get_input.definition <- function(x){
  get_attr(x, "input")
}

get_output <- function(x) UseMethod("get_output")

#' @export
get_output.definition <- function(x){
  get_attr(x, "output")
}

get_type <- function(x) UseMethod("get_type")

#' @export
get_type.definition <- function(x){
  get_attr(x, "type")
}

get_class <- function(x) UseMethod("get_class")

#' @export
get_class.definition <- function(x){
  get_attr(x, "classes")
}

get_render_function <- function(x) UseMethod("get_render_function")

#' @export
get_render_function.definition <- function(x){
  get_attr(x, "render_function")
}

get_output_function <- function(x) UseMethod("get_output_function")

#' @export
get_output_function.definition <- function(x){
  get_attr(x, "output_function")
}

get_attr <- function(x, attr) UseMethod("get_attr")

#' @export
get_attr.definition <- function(x, attr){
  attr(x, attr)
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
