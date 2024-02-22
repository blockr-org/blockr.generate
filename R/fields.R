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
