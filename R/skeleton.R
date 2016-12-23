bookdown_skeleton <- function(path) {

  # ensure directory exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # copy 'resources' folder to path
  skeleton_resources <- system.file(
    "rstudio/templates/project/resources",
    package = "bookdown"
  )

  skeleton_files <- list.files(skeleton_resources,
                               recursive = TRUE,
                               include.dirs = FALSE)

  source <- file.path(skeleton_resources, skeleton_files)
  target <- file.path(path, skeleton_files)
  file.copy(source, target)

  TRUE
}
