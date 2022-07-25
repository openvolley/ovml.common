#' Path to the cache directory used for model weight files and other data
#'
#' @return The path as a string
#'
#' @export
ovml_cache_dir <- function() {
    out <- rappdirs::user_cache_dir("ovml", "openvolley")
    if (!dir.exists(out)) dir.create(out, recursive = TRUE)
    out
}

#' Conditional download utility
#'
#' If the file already exists in the `ovml_cache_dir()`, it won't be downloaded.
#'
#' @param url string: URL of file to download
#' @param dest string: local basename of file, if missing will be taken from the URL
#' @param expected_sha1 string: the expected SHA1 hash of the file
#'
#' @return The path to the file in the [ovml_cache_dir()]
#'
#' @export
ovml_download_if <- function(url, dest, expected_sha1 = NULL) {
    if (length(url) < 1 || is.na(url)) stop("no download url provided")
    ## dest is basename of destination file
    if (missing(dest)) dest <- basename(url)
    weights_file <- file.path(ovml_cache_dir(), dest)
    if (!dir.exists(dirname(weights_file))) dir.create(dirname(weights_file), recursive = TRUE)
    if (!file.exists(weights_file)) {
        message("downloading weights from ", url, " to ", weights_file)
        curl::curl_download(url, destfile = weights_file, quiet = !interactive())
        if (!check_sha1(weights_file, expected_sha1)) {
            try(unlink(weights_file), silent = TRUE)
            stop("SHA1 of downloaded weights file does not match expected, deleting")
        }
    } else {
        if (!check_sha1(weights_file, expected_sha1)) warning("SHA1 of weights file does not match expected, perhaps you need to re-download it?")
    }
    weights_file
}

#' Check file SHA1 hash against expected
#'
#' @param filename string: file to check
#' @param expected string: expected SHA1 hash
#'
#' @return `TRUE` if the SHA1 hash matches the expected value, or if no expected value was provided.
#'
#' @export
check_sha1 <- function(filename, expected = NULL) {
    if (!file.exists(filename)) {
        warning("weights file does not exist")
        FALSE
    } else {
        if (!is.null(expected)) {
            digest::digest(filename, algo = "sha1", file = TRUE) %eq% expected
        } else {
            TRUE
        }
    }
}
