make_code <- function(
  fn, 
  package = NULL, 
  config = define(),
  all_args = define()
){
  obj <- get(fn)

  default <- list(
    fn = "",
    block = "",
    register = ""
  )

  if(!inherits(obj, "function"))
    return(default)

  def <- get_definition(config, fn) %||% define()

  if(should_ignore(def))
    return(default)

  args <- formals(obj)

  # maybe just assign dynamic fields instead of discarding them?
  args <- args[names(args) != "..."]

  fields <- args |>
    argument_to_field(def, all_args) |>
    handle_fields(def, all_args)

  if(length(fields))
    fields <- paste0(names(fields), " = ", fields, collapse = ",\n      ")
  else
    fields <- ""

  # function calls, we cannot set a field to a function
  if(length(args))
    args <- args[sapply(args, \(x) !inherits(x, "call"))]

  classes <- make_class(fn, all_args, def)

  expr <- make_expression(fn, args, def, all_args)
  if(length(package) > 0L)
    expr <- paste0(package, "::", expr)

  code <- sprintf(
    "#' @import blockr
new_%s_block <- function(data, ...){
  blockr::new_block(
    name = \"%s_block\",
    expr = quote(
      %s
    ),
    fields = list(
      %s
    ),
    class = %s
  )
}",
    fn,
    fn,
    expr,
    fields,
    classes
  )

  init <- sprintf(
    "#' @export
%s_block <- function(data, ...){
  blockr::initialize_block(new_%s_block(data, ...), data)
}", 
    fn, 
    fn
  )

  if("data" %in% c(get_type(def), get_type(config))){
    init <- sprintf(
      "%s_block <- function(...){
  blockr::initialize_block(new_%s_block(...))
}", 
      fn, 
      fn
    )
  }

  block <- paste0(code, "\n\n", init)

  render_function <- get_render_function(def) %||% get_render_function(all_args)
  if(length(render_function)){
    renderer <- sprintf(
      "#' @method server_output %s_block
#' @export
server_output.%s_block <- %s",
      fn,
      fn,
      deparse_(render_function)
    )

    block <- paste0(
      block,
      "\n\n",
      renderer
    )
  }

  output_function <- get_output_function(def) %||% get_output_function(all_args)
  if(length(output_function)){
    out <- sprintf(
      "#' @method uiOutputBlock %s_block
#' @export
uiOutputBlock.%s_block <- %s",
      fn,
      fn,
      deparse_(output_function)
    )

    block <- paste0(
      block,
      "\n\n",
      out 
    )
  }
  
  evaluate_function <- get_evaluate_function(def) %||% get_evaluate_function(all_args)
  if(length(evaluate_function)){
    out <- sprintf(
      "#' @method evaluate_block %s_block
#' @export
evaluate_block.%s_block <- %s",
      fn,
      fn,
      deparse_(evaluate_function)
    )

    block <- paste0(
      block,
      "\n\n",
      out 
    )
  }

  generate_server_function <- get_generate_server_function(def) %||% get_generate_server_function(all_args)
  if(length(generate_server_function)){
    out <- sprintf(
      "#' @method generate_server %s_block
#' @export
generate_server.%s_block <- %s",
      fn,
      fn,
      deparse_(generate_server_function)
    )

    block <- paste0(
      block,
      "\n\n",
      out 
    )
  }

  block_combiner <- get_block_combiner(def) %||% get_block_combiner(all_args)
  if(length(block_combiner)){
    out <- sprintf(
      "#' @method block_combiner %s_block
#' @export
block_combiner.%s_block <- %s",
      fn,
      fn,
      deparse_(block_combiner)
    )

    block <- paste0(
      block,
      "\n\n",
      out 
    )
  }

  register <- sprintf(
    "
  blockr::register_block(
    %s_block,
    \"%s\",
    \"A block\",
    input = \"%s\",
    output = \"%s\",
    package = pkgname,
    classes = %s
  )",
    fn,
    fn,
    get_input(def) %||% get_input(all_args),
    get_output(def) %||% get_output(all_args),
    classes
  )

  list(
    fn = fn,
    block = block,
    register = register
  )
}
