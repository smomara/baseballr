#' @title Check Chadwick installation
#' @description
#' Utility functions to help ensure that Chadwick is set up correctly. 
#' @return If Chadwick is not installed `NULL`. If Chadwick is installed, the path to the `cwevent` binary.
#' @export
#' @examples
#' chadwick_path()
#' 

chadwick_path <- function() {
  if (.Platform$OS.type == "windows") {
    cmd <- "where"
  } else {
    cmd <- "which"
  }
  
  path <- tryCatch(
    system2(cmd, "cwevent", stdout = TRUE),
    warning = function(w) {
      message("cwevent is not installed. Please see https://github.com/chadwickbureau/chadwick/releases for installation instructions. ")
    }
  )
  if (!is.null(path) && file.exists(path)) {
    return(dirname(path))
  }
}

#' @rdname chadwick_path
#' @return `TRUE` or `FALSE`
#' @export
#' @examples
#' chadwick_is_installed()
#' 

chadwick_is_installed <- function() {
  path <- chadwick_path()
  !is.null(path) && file.exists(path)
}

#' @rdname chadwick_path
#' @export
#' @return Path to the Chadwick shared library. 
#' @examples
#' chadwick_find_lib()

chadwick_find_lib <- function() {
  if (!is.null(cw_path <- chadwick_path())) {
    find_command <- sprintf('find %s -name "libchadwick*"', dirname(cw_path))
    lib_paths <- system(find_command, intern = TRUE)
    unique_dir_paths <- unique(dirname(lib_paths))
    return(unique_dir_paths)
  }
}

#' @rdname chadwick_path
#' @export

chadwick_set_ld_library_path <- function() {
  new_ld_library_path <- paste(
    chadwick_find_lib(), 
    Sys.getenv("LD_LIBRARY_PATH"), 
    sep = ":"
  )
  Sys.setenv(LD_LIBRARY_PATH = new_ld_library_path)
}

#' @rdname chadwick_path
#' @description
#' The easiest way for the [Chadwick CLI](https://github.com/chadwickbureau/chadwick/releases) 
#' tools to work on *nix systems is to
#' set the `LD_LIBRARY_PATH` environment variable. Unfortunately this environment
#' variable is not set by default during the Chadwick installation. 
#' 
#' `chadwick_ld_library_path()` checks to find the Chadwick shared libraries, and then
#' set the `LD_LIBRARY_PATH` environment variable. 
#' If `chadwick_ld_library_path()` returns `TRUE`, the `cwevent` command line program 
#' that \code{\link{retrosheet_data}}
#' depends on should work. 
#' 
#' The other functions documented here are mostly for internal use. 
#' 
#' @export
#' @seealso [retrosheet_data()]
#' @examples
#' \dontrun{
#' if (chadwick_ld_library_path()) {
#'   retrosheet_data(tempdir())
#' }
#' }

chadwick_ld_library_path <- function() {
  safe_split <- function(path) {
    if (nzchar(path)) {
      unlist(stringr::str_split(path, pattern = ":"))
    } else {
      character(0)
    }
  }

  old_ld_library_paths <- safe_split(Sys.getenv("LD_LIBRARY_PATH"))
  lib_paths <- chadwick_find_lib()
  
  if (!all(lib_paths %in% old_ld_library_paths)) {
    chadwick_set_ld_library_path()
  }

  new_ld_library_paths <- safe_split(Sys.getenv("LD_LIBRARY_PATH"))
  all(lib_paths %in% new_ld_library_paths)
}


