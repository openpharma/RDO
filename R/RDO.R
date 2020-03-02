#' @title RDO (Reproducible Data Object)
#'
#' @name RDO
#'
#' @description Create and interact with Reproducible Data Objects.
#'
#' @import data.table
#'
#' @export
NULL

RDO <-
  R6::R6Class(
    classname = "RDO",

    # PUBLIC ##################################################################
    public = list(

      #' @description Default print method of current RDO status
      #' @param ... Other params for print function.
      print = function(...) {

        has_dependencies <- self$has_dependencies()
        dependencies_names <- names(self$get_dependencies())
        status <- self$get_status()

        if (is.null(status$touched))   status$touched   <- "(never)"
        if (is.null(status$validated)) status$validated <- "(never)"

        cat("<RDO>\n")
        cat("Name:", self$get_name(), "\n")
        cat("Dependencies:",
            ifelse(has_dependencies,
                   paste0("\n- ", paste(dependencies_names,
                                        sep = "",
                                        collapse = "\n- "), "\n"), "none.\n"))
        cat("Status:\n")
        cat("- created:       ",
            as.character(status$created, usetz = TRUE), "\n")
        cat("- last changed:  ",
            as.character(status$changed, usetz = TRUE), "\n")
        cat("- last touched:  ",
            as.character(status$touched, usetz = TRUE), "\n")
        cat("- last validated:",
            as.character(status$validated, usetz = TRUE), "\n")
        cat("- last run time: ",
            as.character(status$run_time), "\n")
        cat("- run time total:",
            as.character(private$get_run_time(deep = TRUE)), "\n")
        cat("- is validated?  ", as.character(status$is_validated), "\n")
        cat("- is locked?     ", as.character(status$is_locked), "\n")
        cat("- cache size:    ",
            format(x = status$cache_size, units = "Mb", digits = 4L),
            "\n")
        cat("- cached total:  ",
            format(x = status$cache_size_total, units = "Mb", digits = 4L),
            "\n")
      },


      #' @description Creating a new RDO object,
      #' @param name Unique name of the object.
      #' @param dependencies An RDO object or a list of RDO objects.
      #' @return A new 'RDO' object.
      initialize = function(name,
                            dependencies = list()) {

        self$add_dependencies(dependencies = dependencies)
        dependencies_names <- names(self$get_dependencies(deep = TRUE))

        if (name %in% dependencies_names)
          stop("RDO with this name is already in deep dependencies!")

        private$name <- name
        private$set_status(status = "created")
      },

      #' @description Getting current status.
      #' @return A list.
      get_status = function() {

        status_extended <-
          c(name = self$get_name(),
            private$status,
            run_time_total = private$get_run_time(deep = TRUE),
            cache_size = private$get_cache_size(),
            cache_size_total = private$get_cache_size(deep = TRUE))

        status_extended$cache_size <-
          `class<-`(status_extended$cache_size, value = "object_size")

        status_extended$cache_size_total <-
          `class<-`(status_extended$cache_size_total, value = "object_size")

        status_extended
      },

      #' @description Getting object name.
      #' @return A character with object name.
      get_name = function() {
        private$name
      },


      #' @description Checking if object has dependencies.
      #' @return TRUE if has or FALSE if not.
      has_dependencies = function() {
        if (NROW(private$dependencies) > 0) {TRUE} else {FALSE}
      },


      #' @description Adding new or update existing RDO dependencies.
      #' @param dependencies An RDO object or a list of RDO objects.
      #' @return The RDO object (self) returned invisibly.
      add_dependencies = function(dependencies = list()) {

        if (!is.list(dependencies))
          dependencies <- list(dependencies)

        purrr::walk(dependencies, function(rdo) {
          rdo_name <- rdo$get_name()
          private$dependencies[[rdo_name]] <- rdo
        })

        invisible(self)
      },


      #' @description Getting dependencies of the object.
      #' @param deep A logical.
      #' Should the function return only direct dependencies (FALSE) or
      #' also deep indirect dependencies (dependencies of dependencies).
      #' Default is FALSE.
      #' @return A named list of RDO dependencies wih unique names.
      get_dependencies = function(deep = FALSE) {

        has_dependencies <- self$has_dependencies()
        dependencies     <- private$dependencies

        if (!deep | !has_dependencies) return(dependencies)

        nested_dependencies <-
          purrr::map(dependencies, function(rdo) {
            purrr::compact(rdo$get_dependencies(deep = TRUE))
          })

        dependencies <- unlist(c(nested_dependencies, dependencies))
        dependencies_names <- purrr::map_chr(dependencies, ~ .x$get_name())
        dependencies <- purrr::set_names(dependencies, dependencies_names)

        duplicated_dependency <- duplicated(names(dependencies))

        dependencies[!duplicated_dependency]

      },


      #' @description Getting dependency register showing which RDO is
      #' a direct parent for other RDO dependencies.
      #' @return A data.frame.
      get_dependency_register = function() {

        has_dependencies <- self$has_dependencies()

        if (!has_dependencies) {return(NULL)}

        dependencies <- self$get_dependencies()

        purrr::map_df(dependencies, function(rdo) {
          dplyr::bind_rows(
            tibble::tibble(
              dependency = rdo$get_name(),
              parent = self$get_name()
            ),
            rdo$get_dependency_register()
          )
        })
      },

      #' @description Ploting the tree of RDO dependencies.
      #' Needs \code{visNetwork} package for plotting.
      plot_dependencies = function() {

        if (!requireNamespace("visNetwork", quietly = TRUE)) {
          stop("Please install the 'visNetwork' package.")
        }

        register     <- self$get_dependency_register()
        dependencies <- self$get_dependencies(deep = TRUE)

        rdos <- c(dependencies, self)
        rdos <- setNames(rdos, c(names(dependencies), self$get_name()))

        suppressWarnings({
          nodes <- purrr::map_df(rdos, function(rdo) {
            purrr::map(rdo$get_status(), ~ ifelse(is.null(.x), NA, .x))
          })
        })

        nodes$shape <- "box"
        nodes$shadow <- TRUE
        nodes$id    <- nodes$name
        nodes$label <- nodes$name
        nodes$color <- ifelse(nodes$is_validated, NA_character_, "orange")
        nodes$title <- paste0(
          "Is validated? ", nodes$is_validated, "<br>",
          "Is locked? ", nodes$is_locked, "<br>",
          "Created: ", as.character(nodes$created,
                                      usetz = TRUE), "<br>",
          "Changed: ", as.character(nodes$changed,
                                      usetz = TRUE), "<br>",
          "Validated: ", as.character(nodes$validated,
                                      usetz = TRUE), "<br>",
          "Touched: ", as.character(nodes$touched,
                                      usetz = TRUE), "<br>",
          "Run time: ", nodes$run_time, "<br>",
          "Run time total: ", nodes$run_time_total, "<br>",
          "Cache size: ",
          format(x = (`class<-`(nodes$cache_size,
                                value = "object_size")),
                 units = "Mb", digits = 4L),
          "<br>",
          "Cache total: ",
          format(x = (`class<-`(nodes$cache_size_total,
                                value = "object_size")),
                 units = "Mb", digits = 4L),
          "<br>")

        edges <- data.frame(
          stringsAsFactors = FALSE,
          from = register$dependency,
          to = register$parent
        )

        vis_network <- visNetwork::visNetwork(nodes = nodes, edges = edges)
        vis_network <- visNetwork::visEdges(vis_network, arrows = "to")
        vis_network <- visNetwork::visHierarchicalLayout(
          vis_network,
          direction = "DU",
          sortMethod = "directed",
          blockShifting = FALSE)

        vis_network

      },


      #' @description Getting reproducible R code.
      #' @param deep A logical.
      #' Should the function return only code for this particular RDO (FALSE)
      #' or also from all dependencies.
      #' Default is FALSE.
      #' @return An R named expression.
      get_code = function(deep = FALSE) {

        has_dependencies <- self$has_dependencies()

        if (!has_dependencies | !deep) {
          code <- private$code_expression

          if (!is.null(code))
            names(code) <- self$get_name()

          return(code)
        }

        self_dependencies <- self$get_dependencies(deep = TRUE)

        code <-
          purrr::map(self_dependencies, function(rdo) {
            rdo$get_code(deep = FALSE)
          })

        code <- do.call(what = c, args = unname(code))

        code_self <- self$get_code(deep = FALSE)
        names(code_self) <- self$get_name()

        code <- c(code, code_self)

        return(code)
      },


      #' @description Printing reproducible R code.
      #' @param deep A logical.
      #' Should the function return only code for this particular RDO (FALSE)
      #' or also from all dependencies.
      #' Default is FALSE.
      #' @param verbose A logical.
      #' Should the messages be sent to console.
      #' If the param is not set, it is read from
      #' an environmental variable \code{RDO_VERBOSE}.
      #' If the variable is not set, than the default is TRUE.
      #' @return A charater. Reproducible R code returned invisibly.
      print_code = function(deep = FALSE,
                            verbose = Sys.getenv("RDO_VERBOSE")) {

        verbose <- as.logical(verbose)
        if (is.na(verbose)) verbose <- TRUE

        code_text <-
          purrr::map(as.list(self$get_code(deep = deep)),
                     ~ as.character(.x))

        code_text <-
          purrr::map_chr(code_text, function(code_line) {
            if (code_line[1] == "{") {
              code_line <- code_line[-1]
            }
            paste(code_line, collapse = "\n")
          })

        code_text <- paste(code_text, collapse = '\n')
        code_text <- paste(code_text, "\n")

        if (verbose) cat(code_text)

        invisible(code_text)
      },


      #' @description Runing reproducible R code.
      #' @param deep A logical.
      #' Should the function run only code for this particular RDO (FALSE)
      #' or should it run also dependencies' code if they are not validated.
      #' The function is lazy and it checks
      #' if all deep dependencies are validated first.
      #' If so, there is no need to rerun their code again.
      #' Default is FALSE.
      #' @param cache A logical.
      #' Should the result of code evaluation be cached inside an RDO object.
      #' Default is TRUE.
      #' @param verbose A logical.
      #' Should the messages be sent to console.
      #' If the param is not set, it is read from
      #' an environmental variable \code{RDO_VERBOSE}.
      #' If the variable is not set, than the default is TRUE.
      #' @return The RDO object (self) returned invisibly.
      run = function(deep = FALSE,
                     cache = TRUE,
                     verbose = Sys.getenv("RDO_VERBOSE")) {

        verbose <- as.logical(verbose)
        if (is.na(verbose)) verbose <- TRUE

        rdo_name <- self$get_name()

        if (verbose) cat("Evaluating RDO:", rdo_name, "... ")

        has_dependencies <- self$has_dependencies()

        if (has_dependencies) {
          if (verbose) cat("has dependencies ...\n")
        } else {
          timing_start <- Sys.time()
          temp_data <-
            tryCatch(

              eval(expr = self$get_code(deep = FALSE),
                   envir = new.env(),
                   enclos = parent.env(env = globalenv())),

              error = function(e) {
                message("Error while running RDO code! \n",
                        "Always check for missing dependencies... ")
                stop(e)
              }
            )

          private$status$run_time <- Sys.time() - timing_start
          if (verbose) cat("done!\n")
        }

        if (has_dependencies & deep) {

          dependencies <- self$get_dependencies(deep = TRUE)

          purrr::walk(dependencies, function(rdo) {

            rdo_name     <- rdo$get_name()
            is_validated <- rdo$is_validated(verbose = verbose)
            dependecies  <- rdo$get_dependencies()

            dependencies_changed <-
              purrr::map(dependecies, function(rdo) {
                rdo$get_status()$changed
              })

            dependencies_changed <- unlist(dependencies_changed)
            self_validated <- self$get_status()$validated
            dependencies_changed <- any(dependencies_changed >= self_validated)

            if (verbose & dependencies_changed)
              cat("Dependencies of RDO:", rdo_name, "have changed!\n")

            if (!is_validated | dependencies_changed) {

              rdo$run(deep = FALSE,
                      cache = cache,
                      verbose = verbose)
            }
          })
        }

        if (has_dependencies) {

          dependecies <- self$get_dependencies()

          temp_envir <-
            purrr::map(dependecies, function(rdo) {

              rdo_name     <- rdo$get_name()
              is_validated <- rdo$is_validated(verbose = verbose)

              if (!is_validated)
                stop("Dependency object '", rdo_name, "' is not validated. ")

              rdo$cache
            })

          temp_envir <- setNames(temp_envir, names(dependecies))

          timing_start <- Sys.time()

          temp_data <-
            tryCatch(

              eval(expr = self$get_code(deep = FALSE),
                   envir = temp_envir,
                   enclos = parent.env(env = globalenv())),

              error = function(e) {
                message("Error while running RDO code! \n",
                        "Always check for missing dependencies... ")
                stop(e)
              }
            )

          private$status$run_time <- Sys.time() - timing_start

        }

        if (cache) {
          self$cache <- temp_data
          private$set_status(status = "validated")
        }

        if (verbose) cat("...evaluation of", rdo_name, "completed.\n")

        if (cache == FALSE) return(temp_data)
        invisible(self)
      },


      #' @description Checking if RDO is validated.
      #' A RDO is validated when the result of running reproducible R code
      #' saved inside the RDO is the same as data cached inside the RDO.
      #' @param deep A logical.
      #' Should the function validate only this particular RDO (FALSE)
      #' or should it validate also all deep dependencies (TRUE).
      #' Default is FALSE.
      #' @param verbose A logical.
      #' Should the messages be sent to console.
      #' If the param is not set, it is read from
      #' an environmental variable \code{RDO_VERBOSE}.
      #' If the variable is not set, than the default is TRUE.
      #' @return A logical. TRUE if RDO is validated, FALSE if not.
      is_validated = function(deep = FALSE,
                              verbose = Sys.getenv("RDO_VERBOSE")) {

        verbose <- as.logical(verbose)
        if (is.na(verbose)) verbose <- TRUE

        self_name        <- self$get_name()
        has_dependencies <- self$has_dependencies()
        are_validated    <- c()

        if (deep & has_dependencies) {
          dependecies <- self$get_dependencies(deep = deep)

          are_validated <-
            purrr::map_lgl(dependecies, function(rdo) {
              rdo$is_validated(deep = FALSE, verbose = verbose)
            })

          are_validated <- all(are_validated)
          are_validated
        }

        is_validated <- private$status$is_validated

        if (verbose) {
          cat("RDO: '", self_name, "' is ", sep = "")
          if (is_validated) cat("validated.\n") else cat("NOT VALIDATED!\n")
        }

        is_validated <- all(are_validated, is_validated)
        return(is_validated)
      },


      #' @description Invalidating the RDO explicitly
      #' by setting the 'is_validated' status to 'FALSE'.
      #' @param deep A logical.
      #' Should the function invalidate only this particular RDO (FALSE)
      #' or should it invalidate also all deep dependencies (TRUE).
      #' Default is FALSE.
      #' @param verbose A logical.
      #' Should the messages be sent to console.
      #' If the param is not set, it is read from
      #' an environmental variable \code{RDO_VERBOSE}.
      #' If the variable is not set, than the default is TRUE.
      #' @return The RDO object (self) returned invisibly.
      invalidate = function(deep = FALSE,
                            verbose = Sys.getenv("RDO_VERBOSE")) {

        verbose <- as.logical(verbose)
        if (is.na(verbose)) verbose <- TRUE

        has_dependencies <- self$has_dependencies()

        if (deep & has_dependencies) {
          dependecies <- self$get_dependencies(deep = deep)

          purrr::walk(dependecies, function(rdo) {
            rdo_name <- rdo$get_name()
            rdo$invalidate(deep = FALSE, verbose = verbose)
          })
        }

        self_name <- self$get_name()

        if (verbose) cat("Invalidating RDO: '", self_name, "' ... ", sep = "")
        private$set_status(status = "invalidated")
        if (verbose) cat("done.\n")

        invisible(self)
      },


      #' @description Validating the RDO explicitly
      #' by running deep reproducible R code
      #' and checking if the result is the same as cached data inside the RDO.
      #' @param deep A logical.
      #' Should the function validate only this particular RDO (FALSE)
      #' or should it validate also all deep dependencies (TRUE).
      #' Default is FALSE.
      #' @param verbose A logical.
      #' Should the messages be sent to console.
      #' If the param is not set, it is read from
      #' an environmental variable \code{RDO_VERBOSE}.
      #' If the variable is not set, than the default is TRUE.
      #' @return The RDO object (self) returned invisibly.
      validate = function(deep = FALSE,
                          verbose = Sys.getenv("RDO_VERBOSE")) {

        verbose <- as.logical(verbose)
        if (is.na(verbose)) verbose <- TRUE

        has_dependencies <- self$has_dependencies()

        eval_envir <- new.env()

        if (!deep & has_dependencies) {

          dependecies <- self$get_dependencies()

          eval_envir <-
            purrr::map(dependecies, function(rdo) {

              rdo_name     <- rdo$get_name()
              is_validated <- rdo$is_validated(verbose = verbose)

              if (!is_validated)
                stop("Dependency object '", rdo_name, "' is not validated. ")

              rdo$cache
            })

          eval_envir <- setNames(eval_envir, names(dependecies))
        }

        self_name <- self$get_name()

        if (verbose) cat("Validating RDO: '", self_name, "' ... ", sep = "")

        self_deep_code <- self$get_code(deep = deep)

        eval_data <- eval(expr = self_deep_code, envir = eval_envir)

        is_validated <- identical(self$cache, eval_data)

        if (is_validated)  private$set_status(status = "validated")
        if (!is_validated) private$set_status(status = "invalidated")

        if (verbose) {
          if (is_validated) cat("done.\n") else
            cat("NOT validated!\n")
        }

        invisible(self)
      },


      #' @description Locking the RDO object.
      #' When RDO object is locked you
      #' cannot change the R code and data cache saved inside the object.
      #' @param deep A logical.
      #' Should the function lock only this particular RDO (FALSE)
      #' or should it lock also all deep dependencies (TRUE).
      #' Default is FALSE.
      #' @param verbose A logical.
      #' Should the messages be sent to console.
      #' If the param is not set, it is read from
      #' an environmental variable \code{RDO_VERBOSE}.
      #' If the variable is not set, than the default is TRUE.
      #' @return The RDO object (self) returned invisibly.
      lock = function(deep = FALSE,
                      verbose = Sys.getenv("RDO_VERBOSE")) {

        verbose <- as.logical(verbose)
        if (is.na(verbose)) verbose <- TRUE

        if (deep & self$has_dependencies()) {

          dependencies <- self$get_dependencies(deep = deep)

          purrr::walk(dependencies, function(rdo) {
            rdo$lock(deep = FALSE, verbose = verbose)
          })
        }

        self_name <- self$get_name()

        if (verbose) cat("Locking RDO: '", self_name, "' ... ", sep = "")
        private$status$is_locked <- TRUE
        if (verbose) cat("done.\n")

        invisible(self)
      },


      #' @description Unlocking previously locked RDO object.
      #' @param deep A logical.
      #' Should the function unlock only this particular RDO (FALSE)
      #' or should it unlock also all deep dependencies (TRUE).
      #' Default is FALSE.
      #' @param verbose A logical.
      #' Should the messages be sent to console.
      #' If the param is not set, it is read from
      #' an environmental variable \code{RDO_VERBOSE}.
      #' If the variable is not set, than the default is TRUE.
      #' @return The RDO object (self) returned invisibly.
      unlock = function(deep = FALSE,
                        verbose = Sys.getenv("RDO_VERBOSE")) {

        verbose <- as.logical(verbose)
        if (is.na(verbose)) verbose <- TRUE

        if (deep & self$has_dependencies()) {

          dependencies <- self$get_dependencies(deep = deep)

          purrr::walk(dependencies, function(rdo) {
            rdo$unlock(deep = FALSE, verbose = verbose)
          })
        }

        self_name <- self$get_name()

        if (verbose) cat("Unlocking RDO: '", self_name, "' ... ", sep = "")
        private$status$is_locked <- FALSE
        if (verbose) cat("done.\n")

        invisible(self)
      },


      #' @description Checking if an RDO object is locked.
      #' @param deep A logical.
      #' Should the function check only this particular RDO (FALSE)
      #' or should it check also all deep dependencies (TRUE).
      #' Default is FALSE.
      #' @param verbose A logical.
      #' Should the messages be sent to console.
      #' If the param is not set, it is read from
      #' an environmental variable \code{RDO_VERBOSE}.
      #' If the variable is not set, than the default is TRUE.
      #' @return A logical. TRUE if RDO is locked, FALSE if not.
      is_locked = function(deep = FALSE,
                           verbose = Sys.getenv("RDO_VERBOSE")) {

        verbose <- as.logical(verbose)
        if (is.na(verbose)) verbose <- TRUE

        has_dependencies <- self$has_dependencies()
        are_locked <- c()

        if (has_dependencies & deep) {

          dependencies <- self$get_dependencies(deep = deep)

          are_locked <-
            purrr::map_lgl(dependencies, function(rdo) {
              rdo$is_locked(deep = FALSE, verbose = verbose)
            })

          are_locked <- all(are_locked)
        }

        self_name <- self$get_name()

        if (verbose) cat("RDO: '", self_name, "' is ", sep = "")
        is_locked <- private$status$is_locked

        if (verbose) {
          if (is_locked) cat("LOCKED!\n") else cat("unlocked.\n")
        }

        is_locked <- all(are_locked, is_locked)
        is_locked
      },


      #' @description Prunning (clearing) RDO data cache by setting
      #' cache to NULL.
      #' It can save memory when we no longer need
      #' to keep cache in depencencies.
      #' @param deep A logical.
      #' Should the function prune cache of only this particular RDO (FALSE)
      #' or should it prune cache also of deep dependencies (TRUE).
      #' If an RDO is locked the cache in this particular RDO is not pruned.
      #' Default is FALSE.
      #' @param verbose A logical.
      #' Should the messages be sent to console.
      #' If the param is not set, it is read from
      #' an environmental variable \code{RDO_VERBOSE}.
      #' If the variable is not set, than the default is TRUE.
      #' @return The RDO object (self) returned invisibly.
      prune_cache = function(deep = FALSE,
                             verbose = Sys.getenv("RDO_VERBOSE")) {

        verbose <- as.logical(verbose)
        if (is.na(verbose)) verbose <- TRUE

        if (!self$has_dependencies())
          stop("This RDO doesn't have any dependencies!")

        dependencies <- self$get_dependencies(deep = deep)

        purrr::walk(dependencies, function(rdo) {

          rdo_name  <- rdo$get_name()
          is_locked <- rdo$is_locked(verbose = FALSE)

          if (!is_locked) {
            rdo$cache <- NULL
            if (verbose) cat("Cache in RDO: '", rdo_name, "' was ",
                             "cleared.\n", sep = "")
          } else {
            if (verbose) cat("RDO: '", rdo_name, "' is locked!\n", sep = "")
          }
        })

        invisible(self)
      },


      #' @description Prunning RDO dependencies by ensuring that RDO objects
      #' in deep dependencies with the same name point to the same RDO objects.
      #' This type of prunning may be useful after deep clonning of
      #' complex RDO tree with duplicated dependencies.
      #' @param verbose A logical.
      #' Should the messages be sent to console.
      #' If the param is not set, it is read from
      #' an environmental variable \code{RDO_VERBOSE}.
      #' If the variable is not set, than the default is TRUE.
      #' @return The RDO object (self) returned invisibly.
      prune_dependencies = function(verbose = Sys.getenv("RDO_VERBOSE")) {

        verbose <- as.logical(verbose)
        if (is.na(verbose)) verbose <- TRUE

        dependency_register <- self$get_dependency_register()

        dependency_register <- na.omit(dependency_register)

        duplicated_clones <-
          unique(dependency_register$dependency[
            duplicated(dependency_register$dependency)])

        duplicated_clones <-
          dependency_register[
            dependency_register$dependency %in% duplicated_clones,
            c("dependency", "parent")]

        if (NROW(duplicated_clones) > 0) {

          dependencies <- self$get_dependencies(deep = TRUE)

          purrr::pwalk(duplicated_clones, function(dependency, parent) {

            if (verbose) {
              cat("Prunning dependency:", dependency,
                  "in parent:", parent, "... ")
            }

            if (parent == self$get_name()) {
              self$add_dependencies(dependencies = dependencies[[dependency]])
            } else {
              dependencies[[parent]]$add_dependencies(
                dependencies = dependencies[[dependency]])
            }

            if (verbose) cat("done.\n")
          })
        }

        invisible(self)
      }
    ),


    # ACTIVE BINDINGS #########################################################
    active = list(

      #' @description
      #' Setting or printing reproducible R code of an RDO object.
      code = function(value) {

        if (missing(value)) {
          cat("<RDO code>\n")
          self$print_code(deep = FALSE, verbose = TRUE)
          return(self)
        } else {

          if (!is.null(value) && !is.expression(value))
            stop("The code is neither NULL nor an R expression. ",
                 "Use 'RDO$code <- expression({<your code>})'.")

          if (private$status$is_locked)
            stop("This RDO is locked! Cannot overwrite the code.")

          private$code_expression <- value
          private$set_status(status = "invalidated")
        }
      },


      #' @description
      #' Setting or returning data cache of an RDO object.
      #' @return Cached object. If no object is cached returns NULL.
      cache = function(value) {

        if (missing(value)) {

          private$set_status(status = "touched")
          return(private$data_cache)

        } else {

          if (private$status$is_locked)
            stop("This RDO is locked! Cannot overwrite the RDO's cache.")

          private$data_cache <- value
          private$set_status(status = "touched")
          private$set_status(status = "invalidated")
        }
      }
    ),

    # PRIVATE #################################################################
    private = list(

      name = NULL,
      dependencies  = list(),

      status = list(
        created = NULL,
        changed = NULL,
        touched = NULL,
        validated = NULL,
        is_validated = FALSE,
        is_locked = FALSE,
        run_time = NULL
      ),

      data_cache = NULL,
      code_expression = NULL,

      set_status = function(status) {

        timestamp <- Sys.time()
        attr(timestamp, "tzone") <- "UTC"

        if (status == "touched") {
          private$status$touched <- timestamp
        }

        if (status == "changed") {
          private$status$changed <- timestamp
        }

        if (status == "created") {
          private$status$created <- timestamp
          private$status$changed <- timestamp
        }

        if (status == "validated") {
          private$status$changed <- timestamp
          private$status$is_validated <- TRUE
          private$status$validated <- timestamp
        }

        if (status == "invalidated") {
          private$status$changed <- timestamp
          private$status$is_validated <- FALSE
        }
      },


      get_cache_size = function(deep = FALSE) {

        cache_size <- list()

        if (deep & self$has_dependencies()) {

          dependencies <- self$get_dependencies(deep = deep)

          cache_size <-
            purrr::map(dependencies, function(rdo) {
              object.size(rdo$cache)
            })
        }

        self_cache_size <- setNames(object.size(self$cache), self$get_name())

        cache_size <- c(cache_size, self_cache_size)

        cache_size_total <- sum(unlist(cache_size))

        invisible(cache_size_total)

      },


      get_run_time = function(deep = FALSE) {

        run_time <- list()

        if (deep & self$has_dependencies()) {

          dependencies <- self$get_dependencies(deep = deep)

          run_time <-
            purrr::map(dependencies, function(rdo) {
              rdo$get_status()$run_time
            })
        }

        self_run_time <- private$status$run_time

        run_time <- c(run_time, self_run_time)

        run_time_total <- sum(unlist(run_time))

        invisible(run_time_total)

      },


      deep_clone = function(name, value) {

        if (name == "dependencies") {

          rdo_dependencies_cloned <-
            purrr::map(value, function(rdo) {
              value <- rdo$clone(deep = TRUE)
            })

          rdo_dependencies_cloned <-
            setNames(rdo_dependencies_cloned, names(value))

          rdo_dependencies_cloned

        } else {value}
      }
    )
 )

