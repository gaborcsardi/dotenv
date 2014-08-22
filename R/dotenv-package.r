
## --------------------------------------------------------------------
#' Load configuration parameters from .env into environment variables
#'
#' @docType package
#' @name dotenv-package
#' @import magrittr
NULL

. <- "Keep it shut"

#' @importFrom falsy %&&%

.onLoad <- function(libname, pkgname) {
  file.exists(".env") %&&% load_dot_env()
}

#' Load environment variables from the specified file
#'
#' @param file The name of the file to load from.
#' @export
#' @importFrom falsy %||%

load_dot_env <- function(file = ".env") {

  file.exists(file) %||% stop("dot-env file does not exist", call. = TRUE)

  file %>%
    readLines() %>%
    lapply(parse_dot_line) %>%
    set_env()
}

line_regex <- "^\\s*(?<export>export\\s+)?(?<key>[^=]+)=(?<value>.*)$"

parse_dot_line <- function(line) {
  match <- regexpr(line_regex, line, perl = TRUE)
  (match != -1) %||% stop("Cannot parse dot-env line: ", substr(line, 1, 40),
                          call. = FALSE)
  extract_match(line, match) %>%
    extract(c("key", "value")) %>%
    as.list()
}

extract_match <- function(line, match) {
  mapply(attr(match, "capture.start"),
         attr(match, "capture.length"),
         FUN = function(start, length)
           substr(line, start, start + length - 1)) %>%
    set_names(attr(match, "capture.names"))
}

set_env <- function(pairs) {
  structure(.Data  = lapply(pairs, "[[", "value"),
            .Names = sapply(pairs, "[[", "key")
            ) %>%
    do.call(Sys.setenv, .)
}
