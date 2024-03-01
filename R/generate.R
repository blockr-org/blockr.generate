#' Generate Blocks
#' 
#' Generate block code from package in `R/` directory.
#' 
#' @param package Name of package.
#' @param class Additional class to pass to block.
#' @param functions,all_functions Functions to define.
#' @param filter_functions callback that filters functions,
#'  it must accept a character vector and return a character vector.
#' 
#' @export
generate_blocks <- function(
  package,
  filter_functions = identity,
  class = NULL,
  functions = define(),
  all_functions = define()
){

  library(package, character.only = TRUE)
  fns <- ls(sprintf("package:%s", package), all.names = TRUE)
  fns <- fns[!grepl("^%", fns)] # remove infixes
  fns <- filter_functions(fns)

  code <- fns |> 
    lapply(\(fn){
      make_code(
        fn, 
        package, 
        functions, 
        all_functions
      )
    })

  code |>
    lapply(\(code) {
      if(code$fn == "") return()

      writeLines(code$block, sprintf("R/%s_block.R", code$fn))
    })

  zzz <- code |>
    sapply(\(code) code$register)

  zzz <- paste0(zzz, collapse = "\n")
  zzz <- paste0(".onLoad <- function(libname, pkgname){\n", zzz, "\n}")
  writeLines(zzz, "R/zzz.R")
  invisible()
}
