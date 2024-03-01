argument_to_field <- function(x){ # nolint
  l <- length(x) 

  if(inherits(x, "character") || inherits(x, "factor") && l > 1){
    return(
      sprintf("blockr::new_string_field(\"%s\")", x)
    )
  }

  if(inherits(x, "numeric") && l > 1){
    return(
      sprintf("blockr::new_numeric_field(%s, min = %s, max = %s)", x, min(x), max(x))
    )
  }

  if(inherits(x, "logical")){
    return(
      sprintf("blockr::new_switch_field(%s)", x)
    )
  }

  if(is.numeric(x)){
    return(
      sprintf("blockr::new_numeric_field(%s, -1000, 1000)", x)
    )
  }

  if(is.null(x))
    return(NULL)

  "blockr::new_string_field()"
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
      return(deparse_(arg_def))

    fields[[nm]]
  })

  names(fields) <- nms

  if(!length(fields))
    return(c())

  fields[sapply(fields, length) > 0L]
}
