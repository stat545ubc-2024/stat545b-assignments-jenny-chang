Assignment_b1
================
STAT545
2024-10-24

### Making and documenting the function:

``` r
#' Count Unique Values by Group
#'
#' This function calculates the number of unique values in a specified column (`summ_var`) within each group defined by one or more grouping variables. It’s useful for understanding the distribution of unique values within groups.
#' 
#' @param data A data frame or tibble containing the data to be summarized. Chose `data` as it clearly indicates the primary data input.
#' @param summ_var The variable for which unique values are counted within each group. Named `summ_var` to clearly denote that this is the summarized variable in the count.
#' @param ... One or more grouping variables within the dataset. These variables define the grouping structure for summarizing `summ_var`. The `...` notation allows for flexible input of single or multiple variables.
#' @param .groups A character string indicating how to handle grouping in the result. Options are "drop" (default), "keep", "drop_last", or "rowwise". The `.groups` name follows `dplyr` syntax for familiarity.
#'
#' @return A tibble with one row per group and a column `unique_sum`, showing the count of unique values in `summ_var` for each group defined by `...`.
#' @export
#' 
#' 
#' 

count_unique <- function(data, summ_var, ..., .groups = "drop") {
  data %>%
    group_by(...) %>%
    summarize(unique_sum = n_distinct({{ summ_var }}[!is.na({{ summ_var }})]), .groups = .groups)
}
```

### ————————————————-

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(gapminder)
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

``` r
library(palmerpenguins)
```

### Examples of use of this function:

**Example 1. Count unique countries by continent**

By counting the unique countries by continent, it gives us a sense of
how many distinct countries are represented per continent.

``` r
count_unique(gapminder, country, continent)
```

    ## # A tibble: 5 × 2
    ##   continent unique_sum
    ##   <fct>          <int>
    ## 1 Africa            52
    ## 2 Americas          25
    ## 3 Asia              33
    ## 4 Europe            30
    ## 5 Oceania            2

**Example 2. Count Unique Countries by Continent and Year**

By using both continent and year as grouping variables, we see the
number of unique countries for each year within each continent.

``` r
count_unique(gapminder, country, continent, year, .groups = "keep")
```

    ## # A tibble: 60 × 3
    ## # Groups:   continent, year [60]
    ##    continent  year unique_sum
    ##    <fct>     <int>      <int>
    ##  1 Africa     1952         52
    ##  2 Africa     1957         52
    ##  3 Africa     1962         52
    ##  4 Africa     1967         52
    ##  5 Africa     1972         52
    ##  6 Africa     1977         52
    ##  7 Africa     1982         52
    ##  8 Africa     1987         52
    ##  9 Africa     1992         52
    ## 10 Africa     1997         52
    ## # ℹ 50 more rows

**Example 3: Deliberately causing an error by calling a non-existent
variable**

In this example, \`non_existent_var\` does not exist in the
\`gapminder\` dataset, so an error will be triggered when trying to
summarize by non_existent_var. By setting \`error = TRUE\`, the document
will render normally, and the error message will display in the output.

``` r
count_unique(gapminder, non_existent_var, continent)
```

    ## Error in `summarize()`:
    ## ℹ In argument: `unique_sum =
    ##   n_distinct(non_existent_var[!is.na(non_existent_var)])`.
    ## ℹ In group 1: `continent = Africa`.
    ## Caused by error:
    ## ! object 'non_existent_var' not found

### Testing this function:

**Testing vector with no NA’s.** There are no NA’s in the species
variable of the *palmerpenguins* dataset.

This test checks if the summary showing each island and the number of
unique species observed on it are the same with my function and with
using the *group_by %\>% summarize* function.

``` r
sum(is.na(penguins$species))
```

    ## [1] 0

``` r
test_that("count_unique function correctly counts unique species by island", {
  
  unique_species_count <- penguins %>%
    group_by(island) %>%
    summarize(unique_sum = n_distinct(species), .groups = "drop")

  expect_equal(count_unique(penguins, species, island), unique_species_count)

})
```

    ## Test passed 🎊

**Testing vector that has NA’s, also tests with a numerical variable
instead of categorical.** There are 2 NA’s in the numerical
*flipper_length_mm* variable of the *palmerpenguins* dataset.

This test checks if the summary showing the number of unique
flipper_length_mm observed on for each species are the same with my
function and with using the *filter is.NA %\>% group_by %\>% summarize*
function.

``` r
sum(is.na(penguins$flipper_length_mm))
```

    ## [1] 2

``` r
test_that("count_unique function handles missing values in `flipper_length_mm`", {
  
  expected_result <- penguins %>%
    filter(!is.na(flipper_length_mm)) %>% 
    group_by(species) %>%
    summarize(unique_sum = n_distinct(flipper_length_mm), .groups = "drop")
  
  expect_equal(count_unique(penguins, flipper_length_mm, species), expected_result)
})
```

    ## Test passed 🥳

**Test 3: Handling an empty dataset.** Filtering penguins to 0 row
renders the dataset empty with no rows.

This test checks if an empty input dataset (resulting from a filter that
yields no matching rows) will correctly return an output with zero rows.

``` r
test_that("count_unique function handles vector of length 0 correctly", {

  empty_penguins <- penguins %>% filter(island == "Nonexistent")
  
  expect_equal(nrow(count_unique(empty_penguins, species, island)), 0) 
})
```

    ## Test passed 🥳
