#' Process raw detections from pose detection network
#'
#' @param pose matrix: pose detection network output
#' @param original_w integer: input image width
#' @param original_h integer: input image height
#' @param input_image_size integer: network image size
#' @param as string: return results as "segments" or "keypoints"
#' @param letterboxing logical: TRUE if the input images were letterboxed to retain their original aspect ratio
#'
#' @return A data.frame
#'
#' @export
process_pose_dets <- function(pose, original_w, original_h, input_image_size, as = "segments", letterboxing = FALSE) {
    if (nrow(pose) < 1) return(NULL)
    skeleton <- list(c(16, 14), c(14, 12), c(17, 15), c(15, 13), c(12, 13), c(6, 12),
                     c(7, 13), c(6, 7), c(6, 8), c(7, 9), c(8, 10), c(9, 11), c(2, 3),
                     c(1, 2), c(1, 3), c(2, 4), c(3, 5), c(4, 6), c(5, 7))
    kpts <- c("nose", "left_eye", "right_eye", "left_ear", "right_ear", "left_shoulder", "right_shoulder", "left_elbow", "right_elbow", "left_wrist", "right_wrist", "left_hip", "right_hip", "left_knee", "right_knee", "left_ankle", "right_ankle")
    names(skeleton) <- unlist(lapply(skeleton, function(pts) paste(kpts[pts], collapse = " - ")))
    unletter <- function(y) {
        if (letterboxing) {
            original_h - (y / input_image_size - (1 - original_h / original_w) / 2) / (original_h / original_w) * original_h
        } else {
            original_h - (y / input_image_size) / (original_h / original_w) * original_h
        }
    }
    if (as == "segments") {
        ppx <- do.call(rbind, lapply(seq_len(nrow(pose)), function(i) {
            this <- pose[i, 8:58]
            do.call(rbind, lapply(seq_along(skeleton), function(j) c(i, j, this[(skeleton[[j]] - 1) * 3 + rep(1:3, each = 2)])))
        }))
        ppx <- setNames(as.data.frame(ppx), c("object", "segment", "x1", "x2", "y1", "y2", "conf1", "conf2"))
        ppx$y1 <- unletter(ppx$y1)
        ppx$y2 <- unletter(ppx$y2)
        ppx$x1 <- ppx$x1 / input_image_size * original_w
        ppx$x2 <- ppx$x2 / input_image_size * original_w
        ppx$segment <- names(skeleton)[ppx$segment]
    } else {
        ppx <- do.call(rbind, lapply(seq_len(nrow(pose)), function(i) {
            this <- pose[i, 8:58]
            do.call(rbind, lapply(seq_along(kpts), function(j) c(i, j, this[(j - 1) * 3 + (1:3)])))
        }))
        ppx <- setNames(as.data.frame(ppx), c("object", "keypoint", "x", "y", "conf"))
        ppx$y <- unletter(ppx$y)
        ppx$x <- ppx$x / input_image_size * original_w
        ppx$keypoint <- kpts[ppx$keypoint]
    }
    ppx
}
