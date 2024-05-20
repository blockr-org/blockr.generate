argument_to_field <- function(fields, function_definition, all_args){ # nolint
  nms <- names(fields)

  i <- 1L
  fields |>
    lapply(\(x){
      l <- length(x) 

      fn_def <- get_definition(function_definition, nms[i])
      arg_def <- get_definition(all_args, nms[i])
      def <- fn_def %||% arg_def

      title <- get_title(def) %||% nms[i]
      desc <- get_description(def)

      i <<- i + 1L

      if(inherits(x, "call")){
        called <- tryCatch(
          eval(x),
          error = \(e) e
        )

        if(!inherits(called, "error"))
           x <- called
      }

      if((inherits(x, "character") || inherits(x, "factor")) && l > 1){
        opts <- paste0(x, collapse = "', '")
        opts <- paste0("c('", opts, "')")

        return(
          sprintf(
            "blockr::new_select_field(\"%s\", %s, title = \"%s\", description = \"%s\")", 
            x[1],
            opts,
            title = title, 
            description = desc
          )
        )
      }

      if(inherits(x, "numeric") && l > 1){
        return(
          sprintf(
            "blockr::new_numeric_field(%s, min = %s, max = %s, title = \"%s\", description = \"%s\")", 
            x[1], min(x), max(x), title = title, description = desc
          )
        )
      }

      if(inherits(x, "logical")){
        return(
          sprintf(
            "blockr::new_switch_field(%s, title = \"%s\", description = \"%s\")", 
            x[1], title = title, description = desc
          )
        )
      }

      if(is.numeric(x)){
        return(
          sprintf(
            "blockr::new_numeric_field(%s, -1000, 1000, title = \"%s\", description = \"%s\")", 
            x[1], title = title, description = desc
          )
        )
      }

      if(is.character(x) || is.factor(x)){
        return(
          sprintf(
            "blockr::new_string_field(\"%s\", title = \"%s\", description = \"%s\")", 
            x[1], title = title, description = desc
          )
        )
      }

      if(is.null(x))
        return(NULL)

      sprintf(
        "blockr::new_string_field(title = \"%s\", description = \"%s\")", 
        title = title, description = desc
      )
    })
}

handle_fields <- function(fields, function_definition, all_args){
  nms <- names(fields)

  fields <- lapply(nms, \(nm) {
    arg_def <- get_definition(function_definition, nm)

    if(should_ignore(arg_def))
      return(NULL)

    if(length(arg_def))
      return(deparse_(arg_def))

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
