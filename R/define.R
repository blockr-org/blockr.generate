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
  render_function = NULL,
  evaluate_function = NULL,
  block_combiner = NULL,
  generate_server_function = NULL
){
  structure(
    rlang::enquos(...),
    ignore = ignore,
    input = input,
    output = output,
    type = type,
    classes = classes,
    output_function = output_function,
    render_function = render_function,
    evaluate_function = evaluate_function,
    generate_server_function = generate_server_function,
    block_combiner = block_combiner,
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
  def <- x[[name]]

  if(rlang::is_quosure(def) && grepl("define\\(", rlang::quo_text(def)))
    def <- rlang::eval_tidy(def)

  def
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

#' @export
get_input.quosure <- function(x){
  x |> rlang::quo_text()
}

get_output <- function(x) UseMethod("get_output")

#' @export
get_output.definition <- function(x){
  get_attr(x, "output")
}

#' @export
get_output.quosure <- function(x){
  x |> rlang::quo_text()
}

get_type <- function(x) UseMethod("get_type")

#' @export
get_type.definition <- function(x){
  get_attr(x, "type")
}

#' @export
get_type.default <- function(x){
  NULL
}

get_class <- function(x) UseMethod("get_class")

#' @export
get_class.definition <- function(x){
  get_attr(x, "classes")
}

#' @export
get_class.default <- function(x){
  NULL
}

get_render_function <- function(x) UseMethod("get_render_function")

#' @export
get_render_function.definition <- function(x){
  get_attr(x, "render_function")
}

#' @export
get_render_function.quosure <- function(x){
  x
}

get_output_function <- function(x) UseMethod("get_output_function")

#' @export
get_output_function.definition <- function(x){
  get_attr(x, "output_function")
}

#' @export
get_output_function.quosure <- function(x){
  x
}

get_evaluate_function <- function(x) UseMethod("get_evaluate_function")

#' @export
get_evaluate_function.definition <- function(x){
  get_attr(x, "evaluate_function")
}

#' @export
get_evaluate_function.quosure <- function(x){
  x
}

get_generate_server_function <- function(x) UseMethod("get_generate_server_function")

#' @export
get_generate_server_function.definition <- function(x){
  get_attr(x, "generate_server_function")
}

#' @export
get_generate_server_function.quosure <- function(x){
  x
}

get_block_combiner <- function(x) UseMethod("get_block_combiner")

#' @export
get_block_combiner.definition <- function(x){
  get_attr(x, "block_combiner")
}

#' @export
get_block_combiner.quosure <- function(x){
  x
}

get_attr <- function(x, attr) UseMethod("get_attr")

#' @export
get_attr.definition <- function(x, attr){
  attr(x, attr)
}
