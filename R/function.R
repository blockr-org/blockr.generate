make_function <- function(
  fn, 
  package, 
  config,
  default_type = "transform", 
  default_input = "data.frame", 
  default_output = "data.frame"
){
  obj <- get(fn)

  default <- list(
    fn = "",
    block = "",
    register = ""
  )

  if(!inherits(obj, "function"))
    return(default)

  def <- get_definition(config, fn)

  if(should_ignore(def))
    return(default)

  args <- formals(obj)
  # maybe just assign dynamic fields instead of discarding them?
  args <- args[names(args) != "..."]

  # we remove arguments that default to NULL
  # because we don't have the ability to infer their type
  # and because we cannot set a field to NULL
  if(length(args)){
    args <- args[sapply(args, Negate(is.null))]
  }

  # function calls, we cannot set a field to a function
  if(length(args))
    args <- args[sapply(args, \(x) !inherits(x, "call"))]

  fields <- args |>
    lapply(argument_to_field) |>
    handle_fields(def)

  if(length(fields))
    fields <- paste0(names(fields), ' = ', fields, collapse = ',\n      ')
  else
    fields <- ""

  code <- sprintf(
"new_%s_block <- function(data, ...){
  blockr::new_block(
    name = \"%s_block\",
    expr = quote({
      %s::%s
    }),
    fields = list(
      %s
    ),
    class = c(\"%s_block\", \"%s_block\")
  )
}",
    fn,
    fn,
    package,
    make_expression(fn, args, def),
    fields,
    fn,
    default_type
  )

  init <- sprintf(
"%s_block <- function(data, ...){
  blockr::initialize_block(new_%s_block(data, ...), data)
}", fn, fn
  )

  if(default_type == "data"){
    init <- sprintf(
"%s_block <- function(...){
  blockr::initialize_block(new_%s_block(...))
}", 
      fn, 
      fn
    )
  }

  block <- paste0(code, "\n\n", init)
  
  register <- sprintf(
"blockr::register_block(
  %s_block,
  \"%s\",
  \"A block\",
  input = \"%s\",
  output = \"%s\",
  classes = c(\"%s_block\", \"data_block\"),
  package = pkgname
)",
    fn,
    fn,
    default_input,
    default_output,
    fn
  )

  list(
    fn = fn,
    block = block,
    register = register
  )
}
