
usethis::use_testthat()
# usethis::use_test()


usethis::use_description(fields = list(
  `Authors@R` = 'person("Kamil", "Wais",
                 email = "kamil.wais@gmail.com",
                 role = c("aut", "cre"),
                 comment = c(ORCID = "0000-0002-4062-055X"))',
  License = "MIT + file LICENSE",
  Title = "Create Reproducible Data Objects",
  Description = "Reproducible Data Objects (RDO)
  encapsulate both data and R code needed to reproduce the data.
  The RDOs can be composed into complex tree hierarchies.
  Interacting with such complex RDO is similar to interacting with a single RDO.
  You can rerun the code and refresh the cache, check RDO's status,
  validate if the code still gives the cached data,
  clear data cache, or access code and data cache of any of the dependencies.
  The RDOs can be cloned and code of dependencies can be modified.
  The RDOs can be send to a user via Plumber API."
))
usethis::use_mit_license(name = "Kamil Wais")

usethis::use_package("purrr")
usethis::use_package("data.table")
usethis::use_package("visNetwork", type = "Suggests")
usethis::use_package("testthat", type = "Suggests")
usethis::use_package_doc()


