# Utility functions

#' @import dplyr
NULL

#' Get heatmap row/column indices through coordinates
#'
#' \code{get_ht_row_col} gets heatmap row/column indices in the original matrix through coordinates.
#'
#' @importFrom ComplexHeatmap row_order column_order
#' @importFrom InteractiveComplexHeatmap htPositionsOnDevice
#' @importFrom grid deviceDim
#'
#' @param object \code{\linkS4Class{HeatmapList}} object from \code{\link[ComplexHeatmap]{draw}}.
#' @param x,y Numeric values as coordinates in "npc" unit (see \code{\link[grid]{unit}} for details).
#'
#' @return A \code{\link{data.frame}} from \code{\link[InteractiveComplexHeatmap]{htPositionsOnDevice}}
#' with two additional columns \code{row_idx} and \code{column_idx} for row/column indices
#' in original matrix.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @keywords internal
#' @noRd
get_ht_row_col_idx <- function(object, x, y) {
  pos <- InteractiveComplexHeatmap::htPositionsOnDevice(object, valueOnly = TRUE, calibrate = FALSE)
  dev_dim <- grid::deviceDim(unit(1, "npc"), unit(1, "npc"), valueOnly = TRUE)
  pos[c("x_min", "x_max")] <- lapply(pos[c("x_min", "x_max")], function(x) x / dev_dim[["w"]])
  pos[c("y_min", "y_max")] <- lapply(pos[c("y_min", "y_max")], function(x) x / dev_dim[["h"]])
  obj_row_order <- ComplexHeatmap::row_order(object)
  if (is.integer(obj_row_order) == TRUE) {
    # Heatmap should not be sliced if row order is integer
    stopifnot(nrow(pos) == 1L)
    obj_row_order <- list(obj_row_order)
  }
  obj_col_order <- ComplexHeatmap::column_order(object)
  if (is.integer(obj_col_order) == TRUE) {
    # Heatmap should not be sliced if column order is integer
    stopifnot(nrow(pos) == 1L)
    obj_col_order <- list(obj_col_order)
  }
  # Determine which slice the coordinate is in
  x_min <- x_max <- y_min <- y_max <- NULL
  pos_coord <- pos %>%
    as.data.frame() %>%
    dplyr::filter(dplyr::between(x, x_min, x_max) == TRUE, dplyr::between(y, y_min, y_max) == TRUE)
  stopifnot(nrow(pos_coord) <= 1L)
  # Return NA for the case where coordinates are not on heatmap tiles
  res <- pos_coord
  # Derive row and column indices in original matrix
  if (nrow(pos_coord) > 0L) {
    obj_row_order_slice <- obj_row_order[[pos_coord[["row_slice"]]]]
    obj_col_order_slice <- obj_col_order[[pos_coord[["column_slice"]]]]
    row_idx <- as.integer(cut(y, seq(pos_coord[["y_min"]], pos_coord[["y_max"]],
                                     length.out = length(obj_row_order_slice) + 1L),
                              include.lowest = TRUE))
    row_idx <- length(obj_row_order_slice) + 1L - row_idx
    col_idx <- as.integer(cut(x, seq(pos_coord[["x_min"]], pos_coord[["x_max"]],
                                     length.out = length(obj_col_order_slice) + 1L),
                              include.lowest = TRUE))
    res[["row_idx"]] <- obj_row_order_slice[row_idx]
    res[["column_idx"]] <- obj_col_order_slice[col_idx]
  } else {
    res[["row_idx"]] <- NA_integer_
    res[["column_idx"]] <- NA_integer_
  }
  res
}
