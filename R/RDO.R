# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
# usethis::use_test()
#' @export

RDO <-
  R6::R6Class(
    classname = "RDO",

    # PUBLIC ##################################################################
    public = list(

      # TODO: print default rdo

      initialize = function(name,
                            dependencies = list()
                            ) {

        # TODO: list of dendencies
        # private$code_cached <- code
        private$name <- name

        timestamp <- Sys.time()
        attr(timestamp, "tzone") <- "UTC"

        private$status$created <- timestamp


        dependency_names <-
          purrr::map_chr(dependencies, function(dependency) {

            dependency$get_name()

          })

        names(dependencies) <- dependency_names

        private$dependencies <- dependencies


      }, # end of initialize



      get_status = function() {

        private$status

      }, # end of get_status



      get_name = function() {

        private$name

      }, # end of get_name



      has_dependencies = function() {

        if (NROW(private$dependencies) > 0) {

          TRUE

        } else {

          FALSE

        } # end of if
      }, # end of has_dependencies

      get_dependencies = function(deep = FALSE) {

        has_dependencies <- self$has_dependencies()
        dependencies <- private$dependencies

        if (!deep) return(dependencies)

        if (!has_dependencies) return(NULL)

        nested_dependencies <-
          purrr::map(dependencies, function(rdo) {

            rdo$get_dependencies(deep = TRUE) %>%
              purrr::compact()

          }) # end of map

        dependencies <-
          c(nested_dependencies,
            dependencies) %>% unlist()

        dependency_names <-
         dependencies %>%
         purrr::map_chr(~.x$get_name())

        dependencies <-
          dependencies %>%
          setNames(dependency_names)

        duplicated_dependecies <-
          names(dependencies) %>%
          duplicated()

        dependencies[!duplicated_dependecies]

      }, # end of get_dependencies



      get_r_code = function(deep = FALSE
                            ) {

        has_dependencies <- self$has_dependencies()

        if (!has_dependencies | !deep) {

          r_code <- private$r_code_expression

          if (!is.null(r_code)) names(r_code) <- self$get_name()

          return(r_code)

        } # end if

        rdos <- self$get_dependencies(deep = TRUE)

        r_code <-
          purrr::map(rdos, function(rdo) {

          rdo$get_r_code(deep = FALSE)

          }) %>%
          unname() %>%
          do.call(what = c, args = .)

        r_code_self <- self$get_r_code(deep = FALSE)
        names(r_code_self) <- self$get_name()

        r_code <- c(r_code, r_code_self)

        return(r_code)

      }, # end of get_r_code


      print_r_code = function(deep = FALSE
                              ) {

        r_code_text <-
          self$get_r_code(deep = deep) %>%
          as.list() %>%
          purrr::map(~ .x %>% as.character()) %>%
          purrr::map_chr(function(code_line) {

            if (code_line[1] == "{") return(code_line[-1])

          })

        r_code_text %>% cat(fill = TRUE)

        return(invisible(NULL))

      }, # end of print_r_code



      run_r_code = function(deep = FALSE,
                            cache = TRUE,
                            verbose = TRUE
                            ) {

        rdo_name <- self$get_name()

        if (verbose) cat("Evaluating RDO:", rdo_name, "... ")

        has_dependencies <- self$has_dependencies()

        if (!has_dependencies) {

          temp_data <- eval(expr = self$get_r_code(deep = FALSE))

          if (verbose) cat("done!\n")

        } else {

          if (verbose) cat("has dependencies ...\n")

          dependecies <- self$get_dependencies()

          temp_data <-
            eval(expr  = self$get_r_code(deep = FALSE),
                 envir =
                   purrr::map(dependecies, function(rdo) {

                     rdo_name <- rdo$get_name()

                     if (verbose) cat("Evaluating RDO:", rdo_name, "... ")

                     is_validated <- rdo$is_validated()

                     if (!is_validated & deep == FALSE) {

                       warning("Dependecy object '", rdo_name,
                            "' has no validated data cache. "
                            # "You can try to run with argument deep=TRUE ",
                            # "to automatically update the data cache ",
                            # "of all dependencies."
                            )

                       return(NULL)

                     } # end of if

                     if (!is_validated & deep == TRUE) {

                       rdo_data <- rdo$run_r_code(deep = TRUE,
                                                  cache = cache,
                                                  verbose = verbose)

                     } # end of if

                     if (is_validated) {

                       rdo_data <- rdo$data

                     } # end of if

                     if (verbose) cat("done!\n")

                     rdo_data

                   }) %>% setNames(names(dependecies)))

        } # end of if

        if (cache) {

          self$data <- temp_data

          private$status$is_validated <- TRUE

        } # end of if

        if (verbose) cat("Done!\n")

        invisible(temp_data)

      }, # end of if



      is_validated = function() {

        private$status$is_validated
        # TODO: deep == TRUE

      }, # end of if



      invalidate = function(deep = FALSE,
                            verbose = TRUE
                            ) {

        self_name <- self$get_name()

        if (verbose) cat("Invalidating RDO: '", self_name, "' ... ", sep = "")

        private$status$is_validated <- FALSE

        if (verbose) cat("done.\n")

        invisible(
          setNames(c(private$status$is_validated), self_name))


        # TODO: deep == TRUE

      }, # end of if



      validate = function(deep = FALSE,
                          verbose = TRUE
                          ) {

        # TODO: deep == TRUE
        # TODO: step_by_step == FALSE
        # TODO: step_by_step == FALSE

        if (deep & self$has_dependencies()) {

          dependencies <- self$get_dependencies(deep = TRUE)

          purrr::walk(dependencies, function(rdo) {

            rdo$validate(deep = FALSE, verbose = verbose)

            })

        } # end of if

        self_name <- self$get_name()

        if (verbose) cat("Validating RDO: '", self_name, "' ... ", sep = "")

        is_validated <- identical(self$data,
                                  self$run_r_code(deep = FALSE,
                                                  cache = FALSE,
                                                  verbose = FALSE))

        private$status$is_validated <- is_validated

        if (verbose) {

          if (is_validated) cat("done.\n") else
            cat("unsuccessful! NOT validated!\n")

        } # end if

        # invisible(
          setNames(c(is_validated), self_name)
          # )

      } # end of validate

    ), # end of public

    # ACTIVE BINDINGS #########################################################
    active = list(

      r_code = function(value) {

        if (missing(value)) {

          self$print_r_code(deep = TRUE)

          self$get_r_code(deep = TRUE)

        } else {

          private$r_code_expression <- value

          private$status$is_validated <- FALSE


        } # end of if

      }, # end of r_code

      data = function(value) {

        if (missing(value)) {

          return(private$data_cache)

        } else {

          private$data_cache <- value

          private$status$is_validated <- FALSE

        } # end of if
      } # end of data

    ), # end of active

    # PRIVATE #################################################################
    private = list(

      name = NULL,

      dependencies  = list(),

      status = list(
        created = NULL,
        is_validated = FALSE
      ),

      # TODO: last_change
      # TODO:


      data_cache = NULL,

      r_code_expression = NULL

    ) # end of private

) # end of RDO R6Class

