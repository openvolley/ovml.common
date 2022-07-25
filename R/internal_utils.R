#' Internal utilities
#'
#' @param x,y,divisor,im,sz,preserve_aspect : generic input parms
#' @export
str_trim <- function(x) {
    gsub("^[[:space:]]+", "", gsub("[[:space:]]+$", "", x))
}

#' @rdname str_trim
#' @export
is_num_scalar <- function(x) is.numeric(x) && length(x) == 1L

#' @rdname str_trim
#' @export
`%eq%` <- function (x, y) ifelse(is.null(x) || is.null(y), FALSE, x == y & !is.na(x) & !is.na(y))

#' @rdname str_trim
#' @export
make_divisible <- function(x, divisor) {
    ## returns x evenly divisible by divisor
    ceiling(x / divisor) * divisor
}

#' @rdname str_trim
#' @export
image_wh <- function(im) {
    if (is.character(im)) im <- image_read(im)
    as.numeric(image_info(im)[, c("width", "height")])
}

#' @rdname str_trim
#' @export
image_resz <- function(im, sz, preserve_aspect = TRUE) {
    if (preserve_aspect) {
        out <- image_scale(im, geometry_size_pixels(width = sz, height = sz, preserve_aspect = TRUE))
        image_extent(out, geometry_size_pixels(width = sz, height = sz), color = "#808080")
    } else {
        image_scale(im, geometry_size_pixels(width = sz, height = sz, preserve_aspect = FALSE))
    }
}

