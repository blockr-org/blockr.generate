deparse_ <- function(x){
  if(rlang::is_quosure(x) && grepl("define\\(", rlang::quo_text(x)))
    def <- rlang::eval_tidy(x)

  x |>
    rlang::quo_text() |> 
    paste0(collapse = "\n")
}
