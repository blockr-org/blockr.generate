<!-- badges: start -->
<!-- badges: end -->

# blockr.generate

:warning: This is a very basic draft for proof of concept.

Generate blockr code

## Installation

You can install the development version of blockr.generate from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("blockr-org/blockr.generate")
```

## Example

``` r
library(blockr.generate)

generate_blocks(
    package = "ggstatsplot",
    default_input = "data.frame",
    default_output = "plot",
    default_type = "plot",
    all_args = define(
        data = define(ignore = TRUE)
    )
)
```

