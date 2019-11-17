
<!-- README.md is generated from README.Rmd. Please edit that file -->

# megametadata

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/JDOsborne1/meta-monster.svg?branch=master)](https://travis-ci.com/JDOsborne1/meta-monster)
<!-- badges: end -->

### Metadata and data dictionary linking tools for R

Designed to be a set of tools for working with the metadata of certain,
“core” clean files. Once the ETL steps are complete, it is fairly
common to end up with a single data structure for each level of
resolution with all the variables needed at that resolution.

One useful thing would be to make the system easy to interact with
outside of R, and make it easy for a layperson to understand if needed
too.

Enter YAML: A structured language which is both human and machine
readable. Since it allows for nested hierarchical variables, without any
enforcement on rectangularity, it should be the perfect mix of
interactive and automated.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JDOsborne1/meta-monster")
```
