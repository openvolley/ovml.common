
#' The IOU of two bounding boxes
#'
#' @param box1 numeric: 4-element box vector
#' @param box2 matrix: nx4 matrix of boxes
#'
#' @return A vector of length n with the intersection over union (IOU) value for each box pair
#'
#' @export
bbox_iou <- function(box1, box2) {
    ## intersection rectangle
    inter_rect_x1 <- pmax(box1[1], box2[, 1])
    inter_rect_y1 <- pmax(box1[2], box2[, 2])
    inter_rect_x2 <- pmin(box1[3], box2[, 3])
    inter_rect_y2 <- pmin(box1[4], box2[, 4])
    ## intersection area
    inter_area <- pmax(inter_rect_x2 - inter_rect_x1 + 1, 0) * pmax(inter_rect_y2 - inter_rect_y1 + 1, 0)
    ## box areas
    b1_area = (box1[3] - box1[1] + 1)*(box1[4] - box1[2] + 1)
    b2_area = (box2[, 3] - box2[, 1] + 1)*(box2[, 4] - box2[, 2] + 1)
    ## iou
    inter_area / (b1_area + b2_area - inter_area)
}

#' Convert xywh format to bounding box format
#'
#' @param prediction matrix: n x m x q matrix of predictions, where the first 4 columns in q are  x y w h
#'
#' @return A matrix of the same size, with coordinates changed to xmin, ymin, xmax, ymax
#'
#' @export
xywh2box <- function(prediction) {
    ## coords in predictions are xmid, ymid, width, height
    box_corner <- array(dim = c(dim(prediction)[1:2], 4))
    box_corner[, , 1] <- prediction[, , 1] - prediction[, , 3]/2 ## xmin
    box_corner[, , 2] <- prediction[, , 2] - prediction[, , 4]/2 ## ymin
    box_corner[, , 3] <- prediction[, , 1] + prediction[, , 3]/2 ## xmax
    box_corner[, , 4] <- prediction[, , 2] + prediction[, , 4]/2 ## ymax
    prediction[, , 1:4] <- box_corner
    prediction
}

#' Rescale boxes
#'
#' Detection boxes are generally on a scaled image, with size according to the network configuration. This function takes boxes on those images and rescales back to the original image dimensions, optionally accounting for letterboxing.
#'
#' @param bboxes numeric: boxes
#' @param original_w numeric: original image width in pixels
#' @param original_h numeric: original image height in pixels
#' @param input_image_size numeric: the network image size in pixels
#' @param letterboxing logical: were the input images letterboxed?
#'
#' @return Rescaled boxes
#'
#' @export
rescale_boxes <- function(bboxes, original_w, original_h, input_image_size, letterboxing = TRUE) {
    ## raw predicted bboxes are e.g. 416 x 416, so we need to scale them to the aspect-ratio image
    iwh <- cbind(original_w, original_h)
    if (letterboxing) {
        iwh <- t(apply(iwh, 1, function(z) z/max(z)))
        sc <- iwh * input_image_size ## the letterbox size, in pixels
        sco <- (1-iwh) / 2 * input_image_size ## the letterbox margins, in pixels
        bboxes[, c(1, 3)] <- (bboxes[, c(1, 3)] - sco[, 1]) / sc[, 1] * original_w
        bboxes[, c(2, 4)] <- (bboxes[, c(2, 4)] - sco[, 2]) / sc[, 2] * original_h
    } else {
        ## simple scaling from 416 x 416 to original w x h
        bboxes[, c(1, 3)] <- bboxes[, c(1, 3)] / input_image_size * original_w
        bboxes[, c(2, 4)] <- bboxes[, c(2, 4)] / input_image_size * original_h
    }
    bboxes
}
