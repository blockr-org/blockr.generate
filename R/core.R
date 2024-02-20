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

  "blockr::new_string_field()"
}

make_expression <- function(fn, args){
  if(length(args)){
    args <- names(args)
    args <- paste0(args, " = .(", args, ")")
    args <- gsub("\\.\\(data\\)", "data", args)
  }

  sprintf("%s(\n        %s\n      )", fn, paste(args, collapse = ",\n        "))
}

#' Generate Blocks
#' 
#' Generate block code from package in `R/` directory.
#' 
#' @param package Name of package.
#' @param default_type Default block type.
#' @param default_input Default block input type.
#' @param default_output Default block output type.
#' @param ignore_args Arguments to omit from fields.
#' @param class Additional class to pass to block.
#' @param ... Named list of special fields for each function.
#' 
#' @export
generate_blocks <- function(
  package,
  default_type = c("transform", "data", "plot"),
  default_input = "data.frame",
  default_output = "data.frame",
  ignore_args = c(),
  class = NULL,
  ...
){
  default_type <- match.arg(default_type)

  library(package, character.only = TRUE)
  fns <- ls(sprintf("package:%s", package), all.names = TRUE)
  fns <- fns[!grepl("^%", fns)]

  special_fields <- list(...)

  zzz <- fns |> 
    lapply(\(fn){
      obj <- get(fn)
      if(!inherits(obj, "function")) return("")

      args <- formals(obj)
      # maybe just assign dynamic fields instead of discarding them?
      args <- args[names(args) != "..."]

      # we remove arguments that default to NULL
      # because we don't have the ability to infer their type
      # and because we cannot set a field to NULL
      if(length(args)){
        args <- args[sapply(args, \(x) !is.null(x))]
      }

      # function calls, we cannot set a field to a function
      if(length(args))
        args <- args[sapply(args, \(x) !inherits(x, "call"))]

      fields <- args |>
        lapply(argument_to_field)

      if(length(special_fields)){
        nms <- names(fields)
        fields <- lapply(nms, \(nm) {
          special <- special_fields[[nm]]

          if(!length(special))
            return(fields[[nm]])

          special
        })
        names(fields) <- nms
      }

      fields <- fields[!names(fields) %in% ignore_args]

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
        make_expression(fn, args),
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
}", fn, fn
      )
      }

      code <- paste0(code, "\n\n", init)
      
      fl <- sprintf("R/%s.R", fn)
      file.create(fl)
      writeLines(code, fl)

      sprintf(
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
    })

  zzz <- zzz[zzz != ""]
  zzz <- paste0(zzz, collapse = "\n\n  ")
  zzz <- sprintf(
".onLoad <- function(libname, pkgname) {\n
  %s
}",
    zzz
  )
  writeLines(zzz, "R/zzz.R")
  invisible()
}
