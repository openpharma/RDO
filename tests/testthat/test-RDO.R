# library(testthat)
# RDO::RDO$debug("get_dependencies")
# RDO::RDO$undebug("get_dependencies")
options(digits.secs = 6)

# Single RDO without dependencies #############################################
context("Single RDO without dependencies")

# _RDO initialization ---------------------------------------------------------

data_1_loading <- RDO::RDO$new(name = "data_1_loading")

test_that("RDO initialization", {

  expect_equal(
    data_1_loading$get_name(),
    "data_1_loading")

  expect_equal(
    data_1_loading$data,
    NULL)

  expect_equal(
    data_1_loading$r_code,
    NULL)

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    FALSE)

  expect_equal(
    class(data_1_loading$get_status()),
    "list")

  expect_equal(
    data_1_loading$get_status()$is_validated,
    FALSE)

  expect_equal(
    class(data_1_loading$get_status()$created),
    c("POSIXct", "POSIXt"))

})

# _setting r_code -------------------------------------------------------------

data_1_loading$r_code <- expression({
  data_1_loading <- mtcars
})

test_that("setting r_code", {capture_output({

  expect_equal(
    data_1_loading$r_code,
    expression(data_1_loading = {
      data_1_loading <- mtcars
      }))

  expect_equal(
    data_1_loading$get_r_code(),
    expression(data_1_loading = {
      data_1_loading <- mtcars
    }))

  # expect_equal(
  #   data_1_loading$is_validated(verbose = FALSE),
  #   FALSE)

})})

# _evaluation of r_code ======================================================
test_that("evaluation of r_code", {capture_output({

  expect_equal(
    eval(data_1_loading$r_code, envir = new.env()),
    mtcars)

})})

# _caching the data -----------------------------------------------------------
test_that("running r_code and caching the data", {

  data_1_loading$data <- NULL

  expect_equal(
    data_1_loading$data,
    NULL)

  # expect_equal(
  #   data_1_loading$is_validated(),
  #   FALSE)

  expect_equal(
    data_1_loading$run_r_code(cache = FALSE,
                                 verbose = FALSE),
    mtcars)

  expect_equal(
    data_1_loading$data,
    NULL)

  expect_equal(
    data_1_loading$run_r_code(verbose = FALSE),
    mtcars)

  expect_equal(
    data_1_loading$data,
    mtcars)
#
#   expect_equal(
#     data_1_loading$is_validated(),
#     TRUE)

})



# _implicit (in)validation ----------------------------------------------------
test_that("implicit (in)validation", {

  data_1_loading$data <- mtcars

  data_1_loading$r_code <- expression({
    data_1_loading <- iris
  })

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    FALSE)
  expect_equal(
    data_1_loading$run_r_code(verbose = FALSE),
    iris)

  expect_equal(
    data_1_loading$data,
    iris)

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    TRUE)

  data_1_loading$r_code <- expression({
    data_1_loading <- mtcars
  })

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    FALSE)

  expect_equal(
    data_1_loading$data,
    iris)

  expect_equal(
    data_1_loading$run_r_code(verbose = FALSE),
    mtcars)

  expect_equal(
    data_1_loading$data,
    mtcars)

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    TRUE)

})


# _explicit (in)validation ----------------------------------------------------
test_that("explicit (in)validation", {

  data_1_loading$r_code <- expression({
    data_1_loading <- mtcars
  })

  data_1_loading$run_r_code(verbose = FALSE)

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    TRUE)

  expect_equal(
    data_1_loading$invalidate(verbose = FALSE),
    c("data_1_loading" = FALSE))

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    FALSE)

  expect_equal(
    eval(data_1_loading$get_r_code(), envir = new.env()),
    data_1_loading$data)

  expect_equal(
    (data_1_loading$validate(verbose = FALSE)),
    c("data_1_loading" = TRUE))

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    TRUE)

  data_1_loading$data <- iris

  expect_equal(
    data_1_loading$validate(verbose = FALSE),
    c("data_1_loading" = FALSE))

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    FALSE)

  data_1_loading$data <- mtcars

  expect_equal(
    (data_1_loading$validate(verbose = FALSE)),
    c("data_1_loading" = TRUE))

  expect_equal(
    data_1_loading$is_validated(verbose = FALSE),
    TRUE)

})





# Multiple RDOs with dependencies #############################################
context("Multiple RDOs with dependencies")

data_1_top_half <-
  RDO::RDO$new(name = "data_1_top_half",
               dependencies = list(data_1_loading))

data_1_bottom_half <-
  RDO::RDO$new(name = "data_1_bottom_half",
               dependencies = list(data_1_loading))

data_1_binding_rows <-
  RDO::RDO$new(name = "data_1_binding_rows",
               dependencies = list(data_1_top_half,
                                   data_1_bottom_half))


data_2_loading <-
  RDO::RDO$new(name = "data_2_loading")

data_2_select_rows <-
  RDO::RDO$new(name = "data_2_select_rows",
               dependencies = list(data_2_loading))

data_2_select_cols <-
  RDO::RDO$new(name = "data_2_select_cols",
               dependencies = list(data_2_select_rows))



data_3_binding_datasets <-
  RDO::RDO$new(name = "data_3_binding_datasets",
               dependencies = list(data_1_binding_rows,
                                   data_2_select_cols))



# _checking dependencies ------------------------------------------------------
test_that("checking dependencies", {

  expect_equal(
    names(data_1_top_half$get_dependencies()),
    c("data_1_loading"))

  expect_equal(
    names(data_1_bottom_half$get_dependencies()),
    c("data_1_loading"))

  expect_equal(
    names(data_1_binding_rows$get_dependencies()),
    c("data_1_top_half", "data_1_bottom_half"))

  expect_equal(
    data_1_loading$has_dependencies(),
    FALSE)

  expect_equal(
    data_1_top_half$has_dependencies(),
    TRUE)

  expect_equal(
    data_1_bottom_half$has_dependencies(),
    TRUE)

  expect_equal(
    data_1_binding_rows$has_dependencies(),
    TRUE)

  expect_equal(
    names(data_1_binding_rows$get_dependencies(deep = TRUE)),
    c("data_1_loading", "data_1_top_half", "data_1_bottom_half"))

  expect_equal(
    names(data_3_binding_datasets$get_dependencies(deep = TRUE)),
    c("data_1_loading",
      "data_1_top_half", "data_1_bottom_half",
      "data_2_loading", "data_2_select_rows", "data_1_binding_rows",
      "data_2_select_cols"))

})

# _setting r_code -------------------------------------------------------------

data_1_top_half$r_code <- expression({
  data_1_top_half <- head(data_1_loading, NROW(mtcars)/2)
})

data_1_bottom_half$r_code <- expression({
  data_1_bottom_half <- tail(data_1_loading, NROW(mtcars)/2)
})

data_1_binding_rows$r_code <- expression({
  data_1_binding_rows <- rbind(data_1_top_half, data_1_bottom_half)
})

data_2_loading$r_code <- expression({
  data_2_loading <- iris
})

data_2_select_rows$r_code <- expression({
  set.seed(1234)
  sampled_rows <- c(rep(TRUE, 32), rep(FALSE, NROW(iris) - 32))
  data_2_select_rows <- data_2_loading[sample(sampled_rows, NROW(iris)), ]
})

data_2_select_cols$r_code <- expression({
  data_2_select_cols <- data_2_select_rows[, c("Species", "Petal.Width")]
})

data_3_binding_datasets$r_code <- expression({
  data_3_binding_datasets <- cbind(data_2_select_cols, data_1_binding_rows)
})

test_that("getting r_code", {

  expect_equal(
    names(data_3_binding_datasets$get_r_code()),
    c("data_3_binding_datasets"))

  expect_equal(
    names(data_3_binding_datasets$get_r_code(deep = TRUE)),
    c("data_1_loading",
      "data_1_top_half", "data_1_bottom_half",
      "data_2_loading",
      "data_2_select_rows",
      "data_1_binding_rows",
      "data_2_select_cols",
      "data_3_binding_datasets"))

})


# _printing r_code -----------------------------------------------------------
test_that("printing r_code", {

  expect_output(
    data_1_binding_rows$print_r_code(),
    "^data_1_binding_rows.*$")

  expect_output(
    data_1_binding_rows$print_r_code(deep = TRUE),
    "^data_1_loading.*data_1_top_half.*data_1_bottom_half.*$")

  expect_output(
    data_1_binding_rows$r_code,
    "^data_1_loading.*data_1_binding_rows .*$")

})



# _running r_code deep-------------------------------------------------------
test_that("running r_code deep", {

  expect_equal(
    data_1_binding_rows$is_validated(verbose = FALSE),
    FALSE)

  expect_error(
    (data_3_binding_datasets$run_r_code(verbose = FALSE)))

  expect_equal(
    data_3_binding_datasets$run_r_code(deep = TRUE, verbose = FALSE),
    {
      set.seed(1234)
      sampled_rows <- c(rep(TRUE, 32), rep(FALSE, NROW(iris) - 32))
      cbind(
        iris[sample(sampled_rows, NROW(iris)), c("Species", "Petal.Width")],
        mtcars)
    })

  expect_output(
    data_3_binding_datasets$run_r_code(deep = TRUE, verbose = TRUE),
    "^.*data_1_binding_rows.*data_2_select_cols' is validated.*$")

})



# _deep validation -----------------------------------------------------------
test_that("deep validation", {

})


















#   data_3_binding_datasets$is_validated()
#   data_3_binding_datasets$is_validated(deep = TRUE)
#   data_3_binding_datasets$validate()
#   # data_3_binding_datasets$invalidate()
#
# data_2_select_rows$invalidate()
#   data_2_select_rows$data <- NULL
#   data_3_binding_datasets$is_validated(deep = TRUE)
#
#   data_3_binding_datasets$validate(deep = F)
#   data_3_binding_datasets$validate(deep = TRUE)
#
#     data_3_binding_datasets$run_r_code(deep = TRUE)
#
#
#   data_2_loading$data <- NULL
#
#
#
#
#     data_3_binding_datasets$run_r_code(deep = TRUE)
#     data_3_binding_datasets$run_r_code(deep = F)
#
#
#
#
#
#       data_3_binding_datasets$data %>% head()
#
#
#       data_3_binding_datasets$get_r_code(TRUE)
#       data_3_binding_datasets$print_r_code(TRUE)
#       invisible(data_3_binding_datasets$r_code)
#
#
#
#     test_data_bind$validate()
#   test_data_bind$invalidate()
#   test_data_bind$data <- NULL
#   test_data$r_code <- expression({
#   test_data <- 111
# })
#   test_data_half_bottom$data <- NULL
#   test_data_half_bottom$is_validated()
#
#   test_data_bind$is_validated(deep  = T)
#   test_data_bind$validate(deep = FALSE)
#   test_data_bind$validate()
# test_data_bind$data
# test_data_bind$r_code
# (eval(expr = test_data_bind$r_code, envir = new.env(parent = .GlobalEnv)))
# (eval(expr = test_data_bind$r_code))
# test_data_bind$get_r_code(deep = T)
#     test_data_bind$run_r_code(deep = F)
#     (test_data_bind$run_r_code(deep = T))
#
#   test_data_half_bottom$data <- NULL
#   test_data_bind$is_validated(deep  = T)
#
#     (test_data_bind$run_r_code(deep = TRUE, cache = F))
#   test_data_bind$is_validated(deep  = T)
#
#   test_data_bind$invalidate()
#   test_data_bind$is_validated(deep  = T)
#   test_data_bind$invalidate(deep = T)
#   test_data_bind$is_validated(deep  = T)
#     test_data_bind$run_r_code(deep = F)
#     test_data_bind$run_r_code(deep = TRUE)
#   test_data_bind$is_validated(deep  = T)
#
#   test_data <- 111
#   test_data_half_top <- head(test_data, NROW(mtcars)/2)
# test_data_half_bottom <- tail(test_data, NROW(mtcars)/2)
# test_data_bind <- rbind(test_data_half_top, test_data_half_bottom)
#

  test_that("validating deeply", {


})



test_that("invalidating deeply", {})
test_that("invalidating the whole tree", {})



# object.size(test_data_bind$data)
# object.size(test_data_half_top$data)
# object.size(test_data_half_bottom$data)


# TODO: is_validated(deep = FALSE)
# TODO: invalidate(deep = TRUE)


# TODO: FORCE == TRUE
# TODO: test_data_half_top$validate()
# TODO: stataus deep == TRUE



# TODO: prune_data_cache()
# TODO: validate(deep = FALSE)
# TODO: validate(deep = TRUE)
# TODO: is_validated(deep = TRUE)
# TODO: invalidate(deep = FALSE)



test_that("shared dependencies", {})
test_that("removing intermediary RDOs", {})
test_that("validating the top RDO", {})
test_that("pruning the tree", {})

