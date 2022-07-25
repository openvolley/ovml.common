#' Class labels
#'
#' @param dataset string: which dataset? One of
#' - "coco" (used with a variety of models)
#' - "mvb" (used with e.g. the yolov4-mvb model)
#' @return A character vector of class labels
#'
#' @export
ovml_class_labels <- function(dataset = "coco") {
    assert_that(is.string(dataset))
    dataset <- tolower(dataset)
    dataset <- match.arg(dataset, c("coco", "mvb"))
    switch(dataset,
           "coco" = {
               ## old, with missing elements. This might be needed with the original C darknet implementation?
               ##c("person", "bicycle", "car", "motorbike", "airplane", "bus", "train", "truck", "boat", "traffic light", "fire hydrant", NA_character_, "stop sign", "parking meter", "bench", "bird", "cat", "dog", "horse", "sheep", "cow", "elephant", "bear", "zebra", "giraffe", NA_character_, "backpack", "umbrella", NA_character_, NA_character_, "handbag", "tie", "suitcase", "frisbee", "skis", "snowboard", "sports ball", "kite", "baseball bat", "baseball glove", "skateboard", "surfboard", "tennis racket", "bottle", NA_character_, "wine glass", "cup", "fork", "knife", "spoon", "bowl", "banana", "apple", "sandwich", "orange", "broccoli", "carrot", "hot dog", "pizza", "donut", "cake", "chair", "couch", "pottedplant", "bed", NA_character_, "diningtable", NA_character_, NA_character_, "toilet", NA_character_, "tvmonitor", "laptop", "mouse", "remote", "keyboard", "cell phone", "microwave", "oven", "toaster", "sink", "refrigerator", NA_character_, "book", "clock", "vase", "scissors", "teddy bear", "hair drier", "toothbrush")
               c("person", "bicycle", "car", "motorcycle", "airplane", "bus", "train", "truck", "boat", "traffic light", "fire hydrant", "stop sign", "parking meter", "bench", "bird", "cat", "dog", "horse", "sheep", "cow", "elephant", "bear", "zebra", "giraffe", "backpack", "umbrella", "handbag", "tie", "suitcase", "frisbee", "skis", "snowboard", "sports ball", "kite", "baseball bat", "baseball glove", "skateboard", "surfboard", "tennis racket", "bottle", "wine glass", "cup", "fork", "knife", "spoon", "bowl", "banana", "apple", "sandwich", "orange", "broccoli", "carrot", "hot dog", "pizza", "donut", "cake", "chair", "couch", "potted plant", "bed", "dining table", "toilet", "tv", "laptop", "mouse", "remote", "keyboard", "cell phone", "microwave", "oven", "toaster", "sink", "refrigerator", "book", "clock", "vase", "scissors", "teddy bear", "hair drier", "toothbrush")
           },
           "mvb" = c("volleyball"),
           stop("unexpected dataset: ", dataset)
           )
}
