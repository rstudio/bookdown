#' @importFrom xfun in_dir sans_ext with_ext same_path read_utf8 write_utf8
NULL

.onLoad = function(lib, pkg) {
  register_eng_math()
}
