
# usethis::use_testthat()
# usethis::use_test()


usethis::use_description(fields = list(
  `Authors@R` = 'person("Kamil", "Wais",
                 email = "kamil.wais@gmail.com",
                 role = c("aut", "cre"),
                 comment = c(ORCID = "0000-0002-4062-055X"))',
  License = "MIT + file LICENSE",
  Title = "Reproducible Data Objects",
  Description = "A Reproducible Data Object (RDO) encapsulates both data and R code needed to reproduce those data. Each RDO can have other RDOs as dependencies. RDOs can be composed into complex tree hierarchies. Interacting with an RDO tree is similar to interacting with a single RDO. You can (re)run the code and refresh the cache, check status, validate if the code still gives the same cached data, clear data cache, access code and data cache of any of the dependencies. RDOs can be cloned and code of cloned dependencies can be modified."
))
usethis::use_package("purrr")
usethis::use_package("data.table")
usethis::use_package("visNetwork", type = "Suggests")
usethis::use_package("testthat", type = "Suggests")
# usethis::use_mit_license(name = "Kamil Wais")
# usethis::use_package_doc()

# usethis::use_readme_rmd()
usethis::use_lifecycle_badge(stage = "Experimental")

# usethis::use_pkgdown()

pkgdown::build_site()
 pkgdown:::build_site_external()


usethis::use_version()
usethis::use_dev_version()

usethis::use_vignette("single-rdo-without-dependencies",
                      "Single RDO without dependencies")

usethis::use_vignette("rdo-with-dependencies",
                      "RDO with dependencies")

