make_class <- function(fn, config, function_definition = define()){
  classes <- c(
    fn,
    get_class(function_definition) %||% get_class(config),
    get_type(function_definition) %||% get_type(config)
  )

  classes <- classes[!is.na(classes)]

  classes <- paste0("\"", classes, "_block", "\"", collapse = ", ")
  paste0("c(", classes, ")")
}
