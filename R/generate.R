#' Generate Blocks
#' 
#' Generate block code from package in `R/` directory.
#' 
#' @param package Name of package.
#' @param fn Function to create a block for.
#' @param functions,all_functions Functions to define.
#' @param filter_functions callback that filters functions,
#'  it must accept a character vector and return a character vector.
#' 
#' @name generate_blocks
#' 
#' @export
generate_blocks <- function(
  package,
  filter_functions = identity,
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
  invisible(code)
}

#' @rdname generate_blocks
#' @export
generate_block <- function(
  fn,
  filter_functions = identity,
  functions = define(),
  all_functions = define()
){
  UseMethod("generate_block", fn)
}

#' @rdname generate_blocks
#' @export
generate_block.function <- function(
  fn,
  filter_functions = identity,
  functions = define(),
  all_functions = define()
){
  fn <- deparse(substitute(fn))

  code <- make_code(
    fn, 
    config = functions, 
    all_args = all_functions
  )

  writeLines(code$block, sprintf("R/%s_block.R", code$fn))

  zzz <- paste0(code$register, collapse = "\n")
  zzz <- paste0(".onLoad <- function(libname, pkgname){\n", zzz, "\n}")
  writeLines(zzz, "R/zzz.R")
  invisible(code)
}

#' @rdname generate_blocks
#' @export
generate_block.character <- function(
  fn,
  filter_functions = identity,
  functions = define(),
  all_functions = define()
){
  code <- make_code(
    fn, 
    config = functions, 
    all_args = all_functions
  )
  
  writeLines(code$block, sprintf("R/%s_block.R", code$fn))

  zzz <- paste0(code$register, collapse = "\n")
  zzz <- paste0(".onLoad <- function(libname, pkgname){\n", zzz, "\n}")
  writeLines(zzz, "R/zzz.R")
  invisible(code)
}
