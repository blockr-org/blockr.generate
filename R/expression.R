make_expression <- function(fn, args, function_definition, all_args){
  args <- args |>
    names() |>
    lapply(\(arg) {
      if(arg == "data") return(NULL)

      arg_def <- get_definition(function_definition, arg)

      if(should_ignore(arg_def)) return(NULL)

      arg_def <- get_definition(all_args, arg)

      if(should_ignore(arg_def)) return(NULL)

      if(is.null(args[[arg]]) && !length(arg_def))
        return(NULL)

      arg
    })

  args <- args[sapply(args, length) > 0L]

  if(!length(args)) return(sprintf("%s()", fn))

  args <- paste0(args, " = .(", args, ")")

  sprintf("%s(\n        %s\n      )", fn, paste(args, collapse = ",\n        "))
}
