#' Copy the example data to a folder you can edit
#'
#' Copies the demo's `transactions.csv` and `nav_1685.csv` into `path`,
#' creating the folder if needed. Use the copy to see the file formats, then
#' replace its contents with your own transactions and your fund's prices and
#' point [run_app()] at it. Don't edit the files inside the installed package:
#' they're replaced whenever the package is reinstalled.
#'
#' @param path Folder to copy the example files into.
#' @param overwrite Replace files with the same names already in `path`?
#' @return The folder's full path, invisibly, ready to pass to [run_app()].
#' @export
#' @examples
#' dir <- use_example_data(file.path(tempdir(), "my401k"))
#' list.files(dir)
#' x <- load_401k(dir)
use_example_data <- function(path, overwrite = FALSE) {
  files <- list.files(demo_data_dir(), full.names = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  taken <- file.exists(file.path(path, basename(files)))
  if (any(taken) && !overwrite) {
    stop(sprintf("%s already has %s. Pass overwrite = TRUE to replace.",
                 path, paste(basename(files)[taken], collapse = " and ")), call. = FALSE)
  }
  file.copy(files, path, overwrite = TRUE)
  path <- normalizePath(path, winslash = "/")
  message(sprintf("Copied %s to %s.", paste(basename(files), collapse = " and "), path))
  invisible(path)
}

#' Folder holding the demo data
#'
#' The demo files inside the installed package. Read them, or copy them with
#' [use_example_data()], but don't edit them in place.
#'
#' @return Path to the bundled demo `transactions.csv` and `nav_1685.csv`.
#' @export
#' @examples
#' list.files(demo_data_dir())
demo_data_dir <- function() {
  # packageName() rather than the literal name, so the web build's dependency
  # scan doesn't take this for a dependency on the installed package.
  system.file("extdata", package = utils::packageName(), mustWork = TRUE)
}

# Your folder if the r401k.data_dir option is set, otherwise the demo.
default_data_dir <- function() {
  getOption("r401k.data_dir", demo_data_dir())
}
