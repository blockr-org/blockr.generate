#' Define
#' 
#' Define a function or argument for processing.
#' 
#' @param ... Named arguments to define.
#' @param ignore Whether to ignore the arguments from fields and function call.
#' @param input,output Object accepted as I/O.
#' @param type Block type, used for inheritance.
#' @param registry_name,registry_description Name and description in of the block in the registry.
#' @param classes Additional classes.
#' @param output_function Function to generate output method.
#' @param render_function Function to generate render method.
#' @param evaluate_function Function to generate evaluate method.
#' @param block_combiner Function to combine blocks.
#' @param generate_server_function Function to generate server function.
#' @param title,description Title and description of field.
#' @param download_ui,download_server Download UI and Server methods.
#' 
#' @export
define <- function(
  ..., 
  ignore = FALSE, 
  input = NULL, 
  output = NULL,
  type = NULL,
  title = NULL,
  registry_name = NULL,
  registry_description = NULL,
  description = NULL,
  classes = NULL,
  output_function = NULL,
  render_function = NULL,
  evaluate_function = NULL,
  block_combiner = NULL,
  generate_server_function = NULL,
  download_ui = NULL,
  download_server = NULL
){
  structure(
    rlang::enquos(...),
    ignore = ignore,
    input = input,
    output = output,
    type = type,
    registry_name = registry_name,
    registry_description = registry_description,
    classes = classes,
    output_function = output_function,
    render_function = render_function,
    evaluate_function = evaluate_function,
    generate_server_function = generate_server_function,
    block_combiner = block_combiner,
    download_ui = download_ui,
    download_server = download_server,
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

get_registry_name <- function(x) UseMethod("get_registry_name")

#' @export
get_registry_name.definition <- function(x){
  get_attr(x, "registry_name")
}

#' @export
get_registry_name.quosure <- function(x){
  x |> rlang::quo_text()
}

get_registry_description <- function(x) UseMethod("get_registry_description")

#' @export
get_registry_description.definition <- function(x){
  get_attr(x, "registry_description")
}

#' @export
get_registry_description.quosure <- function(x){
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

get_title <- function(x) UseMethod("get_title")

#' @export
get_title.definition <- function(x){
  get_attr(x, "title")
}

#' @export
get_title.default <- function(x){
  ""
}

get_description <- function(x) UseMethod("get_description")

#' @export
get_description.definition <- function(x){
  get_attr(x, "description")
}

#' @export
get_description.default <- function(x){
  ""
}

get_download_ui <- function(x) UseMethod("get_download_ui")

#' @export
get_download_ui.definition <- function(x){
  get_attr(x, "download_ui")
}

#' @export
get_download_ui.quosure <- function(x){
  x
}

get_download_server <- function(x) UseMethod("get_download_server")

#' @export
get_download_server.definition <- function(x){
  get_attr(x, "download_server")
}

#' @export
get_download_server.quosure <- function(x){
  x
}
