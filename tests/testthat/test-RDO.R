# library(testthat)

# Single RDO without dependencies #############################################
context("Single RDO without dependencies")

# _RDO initialization =========================================================
test_data <- RDO::RDO$new(name = "test_data")

test_that("RDO initialization", {

  expect_equal(
    test_data$get_name(),
    "test_data")

  expect_equal(
    test_data$data,
    NULL)

  expect_equal(
    test_data$r_code,
    NULL)

  expect_equal(
    test_data$is_validated(),
    FALSE)

  expect_equal(
    class(test_data$get_status()),
    "list")

  expect_equal(
    test_data$get_status()$is_validated,
    FALSE)

  expect_equal(
    class(test_data$get_status()$created),
    c("POSIXct", "POSIXt"))

})

# _setting r_code =============================================================
test_data$r_code <- expression({
  test_data <- mtcars
})

test_that("setting r_code", {capture_output({

  expect_equal(
    test_data$r_code,
    expression(test_data = {
      test_data <- mtcars
      }))

  expect_equal(
    test_data$get_r_code(),
    expression(test_data = {
      test_data <- mtcars
    }))

  expect_equal(
    test_data$is_validated(),
    FALSE)

})})

# _evaluation of r_code ======================================================
test_that("evaluation of r_code", {capture_output({

  expect_equal(
    eval(test_data$r_code, envir = new.env()),
    mtcars)

})})

# _caching the data -----------------------------------------------------------
test_that("caching the data", {

  expect_equal(
    test_data$data,
    NULL)

  expect_equal(
    test_data$is_validated(),
    FALSE)

  expect_equal(
    test_data$run_r_code(verbose = FALSE),
    mtcars)

  expect_equal(
    test_data$data,
    mtcars)

  expect_equal(
    test_data$is_validated(),
    TRUE)

})

# _implicit (in)validation ----------------------------------------------------
test_that("implicit (in)validation", {

  test_data$r_code <- expression({
    test_data <- iris
  })

  expect_equal(
    test_data$is_validated(),
    FALSE)

  expect_equal(
    test_data$run_r_code(verbose = FALSE),
    iris)

  expect_equal(
    test_data$data,
    iris)

  expect_equal(
    test_data$is_validated(),
    TRUE)



  test_data$data <- NULL

  expect_equal(
    test_data$is_validated(),
    FALSE)

  expect_equal(
    test_data$data,
    NULL)

  expect_equal(
    test_data$run_r_code(verbose = FALSE),
    iris)

  expect_equal(
    test_data$data,
    iris)

  expect_equal(
    test_data$is_validated(),
    TRUE)

})


# _explicit (in)validation ----------------------------------------------------
test_that("explicit (in)validation", {

  test_data$r_code <- expression({
    test_data <- mtcars
  })

  test_data$run_r_code(verbose = FALSE)

  expect_equal(
    test_data$is_validated(),
    TRUE)

  expect_equal(
    test_data$invalidate(verbose = FALSE),
    c("test_data" = FALSE))

  expect_equal(
    test_data$is_validated(),
    FALSE)

  expect_equal(
    (test_data$validate(verbose = FALSE)),
    c("test_data" = TRUE))

  test_data$data <- iris

  expect_equal(
    test_data$validate(verbose = FALSE),
    c("test_data" = FALSE))

  expect_equal(
    test_data$is_validated(),
    FALSE)

  test_data$run_r_code(verbose = FALSE)

})

# Multiple RDOs with dependencies #############################################
context("Multiple RDOs with dependencies")


# RDO::RDO$debug("get_dependencies")
# RDO::RDO$undebug("get_dependencies")

test_data_half_top <-
  RDO::RDO$new(name = "test_data_half_top",
               dependencies = list(test_data))

test_data_half_bottom <-
  RDO::RDO$new(name = "test_data_half_bottom",
               dependencies = list(test_data))

test_data_bind <-
  RDO::RDO$new(name = "test_data_bind",
               dependencies = list(test_data_half_top,
                                   test_data_half_bottom))


# _checking dependencies ------------------------------------------------------
test_that("checking dependencies", {

  expect_equal(
    names(test_data_half_top$get_dependencies()),
    c("test_data"))

  expect_equal(
    names(test_data_half_bottom$get_dependencies()),
    c("test_data"))

  expect_equal(
    names(test_data_bind$get_dependencies()),
    c("test_data_half_top", "test_data_half_bottom"))

  expect_equal(
    test_data$has_dependencies(),
    FALSE)

  expect_equal(
    test_data_half_top$has_dependencies(),
    TRUE)

  expect_equal(
    test_data_half_bottom$has_dependencies(),
    TRUE)

  expect_equal(
    test_data_bind$has_dependencies(),
    TRUE)

  expect_equal(
    names(test_data_bind$get_dependencies(deep = TRUE)),
    c("test_data", "test_data_half_top", "test_data_half_bottom"))

})

# _getting r_code -------------------------------------------------------------

test_data_half_top$r_code <- expression({
  test_data_half_top <- head(test_data, NROW(mtcars)/2)
})

test_data_half_bottom$r_code <- expression({
  test_data_half_bottom <- tail(test_data, NROW(mtcars)/2)
})

test_data_bind$r_code <- expression({
  test_data_bind <- rbind(test_data_half_top, test_data_half_bottom)
})


test_that("getting r_code", {

  expect_equal(
    names(test_data$get_r_code(deep = TRUE)),
    c("test_data"))

  expect_equal(
    names(test_data_half_top$get_r_code(deep = TRUE)),
    c("test_data", "test_data_half_top"))

  expect_equal(
    names(test_data_half_bottom$get_r_code(deep = TRUE)),
    c("test_data", "test_data_half_bottom"))

  expect_equal(
    names(test_data_bind$get_r_code(deep = TRUE)),
    c("test_data",
      "test_data_half_top", "test_data_half_bottom",
      "test_data_bind"))

})


# _printing r_code -----------------------------------------------------------
test_that("printing r_code", {

  expect_output(
    test_data_bind$print_r_code(),
    "^test_data_bind.*$")

  expect_output(
    test_data_bind$print_r_code(deep = TRUE),
    "^test_data.*test_data_half_bottom.*test_data_bind.*$")

  expect_output(
    test_data_bind$r_code,
    "^test_data.*test_data_half_bottom.*test_data_bind.*$")

})



# _running deep r_code -------------------------------------------------------
test_that("running deep r_code", {

  expect_equal(
    test_data$is_validated(),
    TRUE)

  test_data_half_top$run_r_code(verbose = FALSE)

  expect_equal(
    test_data_half_top$is_validated(),
    TRUE)

  expect_equal(
    test_data_half_bottom$is_validated(),
    FALSE)

  expect_equal(
    test_data_bind$is_validated(),
    FALSE)

  expect_error(
    test_data_bind$run_r_code(verbose = FALSE))

  expect_equal(
    test_data_bind$run_r_code(deep = TRUE, verbose = FALSE),
    mtcars)

  expect_equal(
    test_data_bind$is_validated(),
    TRUE)

})

  test_data_bind$validate(deep = TRUE)
  test_data_bind$invalidate()
  test_data_bind$data <- NULL
  test_data$data <- NULL
  test_data_bind$is_validated()

  test_data_bind$validate(deep = FALSE)
  test_data_bind$validate(deep = T)

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

