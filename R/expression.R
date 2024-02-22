make_expression <- function(fn, args, function_definition){
  args <- args |>
    names() |>
    lapply(\(arg) {
      if(arg == "data") return(NULL)

      arg_def <- get_definition(function_definition, arg)

      if(should_ignore(arg_def)) return(NULL)

      arg
    })

  args <- args[sapply(args, length) > 0L]
  args <- paste0(args, " = .(", args, ")")

  sprintf("%s(\n        %s\n      )", fn, paste(args, collapse = ",\n        "))
}