#' Generate Blocks
#' 
#' Generate block code from package in `R/` directory.
#' 
#' @param package Name of package.
#' @param default_type Default block type.
#' @param default_input Default block input type.
#' @param default_output Default block output type.
#' @param class Additional class to pass to block.
#' @param ... Named list of special fields for each function.
#' 
#' @export
generate_blocks <- function(
  package,
  default_type = c("transform", "data", "plot"),
  default_input = "data.frame",
  default_output = "data.frame",
  class = NULL,
  config = define()
){
  default_type <- match.arg(default_type)

  library(package, character.only = TRUE)
  fns <- ls(sprintf("package:%s", package), all.names = TRUE)
  fns <- fns[!grepl("^%", fns)]

  code <- fns |> 
    lapply(\(fn){
      make_function(
        fn, 
        package, 
        config, 
        default_type, 
        default_input, 
        default_output
      )
    })

  code |>
    lapply(\(code) {
      if(code$fn == "") return()

      writeLines(code$block, sprintf("R/%s_block.R", code$fn))
    })

  zzz <- code |>
    sapply(\(code) code$register)

  writeLines(zzz, "R/zzz.R")
  invisible()
}
