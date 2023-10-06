# Main game functions

#' @include utils.R
NULL

# Main game function

#' Run mine sweeper game
#'
#' \code{run_game} runs mine sweeper game. It opens a window through \code{\link[grDevices]{x11}}.
#' Please do not run multiple instances (windows) of the game. For game instructions, please check the details.
#'
#' @importFrom grDevices dev.cur dev.set dev.off x11 setGraphicsEventHandlers getGraphicsEvent getGraphicsEventEnv
#' @importFrom rlang env_bind env_has env_parent
#' @importFrom hms as_hms
#' @importFrom mmand shapeKernel dilate
#' @importFrom utils head
#' @importFrom pals cols25
#'
#' @param n_mine,n_row,n_col Integers as numbers of mines/rows/columns in game panel,
#' \code{0 < n_mines < n_row * n_col}.
#' @param unmask_button,flag_button Integers as buttons to unmask/flag tiles.
#' Usually, 0 for left mouse button (to unmask) and 2 for right mouse button (to flag).
#' @param digit_char,mine_char,flag_char Characters to mark counts/mines/flags.
#' @param unmask_color,mask_color,hit_color,wrong_color Strings as colors for
#' unmasked tiles / masked tiles / mine-triggering tile / wrongly flagged tiles.
#' @param digit_color,mine_color,flag_color Strings as colors for \code{digit_char}/
#' \code{mine_char}/\code{flag_char} on game panel.
#' @param window_title String as window title.
#' @param x11_args Named list of arguments passed on to \code{\link[grDevices]{x11}},
#' excluding \code{title}.
#' @param time_prec Integer as time precision (e.g. 0 for seconds or 3 for miliseconds).
#' @param restart_key Character as the key to restart game, used when you have won or lost a game.
#' @param exit_key_press Integer as the times of boss key presses (boss key is any key
#' other than \code{restart_key}) required to close game window. This may come in handy in emergency (:D).
#' @param debug Logical, internal use only.
#'
#' @details Game instructions:
#' \describe{
#'   \item{Goal}{All tiles start masked. If you unmask all tiles without mines, you win the game;
#'   if you unmask any tiles containing mines, you lose the game.}
#'   \item{Unmask tiles}{You may click with \code{unmask_button} on any masked tile to unmask.
#'   The first tile unmasked cannot contain a mine. Timing starts when any tile is unmasked.}
#'   \item{Find out mines}{Each unmasked tile tells you the number of mines in adjacent tiles, unless itself contains a mine.
#'   You may use these numbers to find out the adjacent tiles with mines.}
#'   \item{Flag mines}{You may click with \code{flag_button} on an unmasked tile to flag it as a possible mine.
#'   Flagged tiles cannot be unmasked, unless you click on them with \code{flag_button} again to remove the flags.}
#'   \item{Unmask adjacent tiles}{You may click with \code{unmask_button} on any unmasked tile to unmask adjacent unflagged tiles,
#'   after you have set up the number of flags that equals to the count of adjacent mines shown on this tile.
#'   If any unflagged adjacent tiles contain mines, one of them is triggered and you lose the game.}
#'   \item{Game statistics}{After you win or lose a game, game statistics are recorded.}
#'   \item{Restart game}{You may restart game with \code{restart_key} at any time. If you have not won or lost the game,
#'   it is not recorded in game statistics.}
#'   \item{**BOSS KEY!**}{You may press boss key (any key other than \code{restart_key}) for \eqn{\ge} \code{exit_key_press} times
#'   to close game window.}
#' }
#'
#' @return A \code{\link{data.frame}} containing game statistics, invisibly, where each row is a game play.
#' To look at game statistics, save the output of \code{run_game()} to a variable
#' for later use or run \code{print(run_grame)} to print.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @export
#'
#' @examples
#' if (interactive() == TRUE) {
#'   run_game()
#' }
run_game <- function(
    n_mine = max(round(0.2 * n_row * n_col), 1L), n_row = 10L, n_col = 10L,
    unmask_button = 0L, flag_button = 2L,
    digit_char = format(seq_len(8L), scientific = FALSE),
    mine_char = "M", flag_char = "F",
    unmask_color = "#9F9F9F", mask_color = "#3F3F3F",
    hit_color = "#9F3F3F", wrong_color = "#7F1F7F",
    digit_color = pals::cols25(n = 8L),
    mine_color = "black", flag_color = "white",
    window_title = "Mine Sweeper in R",
    x11_args = list(), time_prec = 3L,
    restart_key = "r", exit_key_press = 2L, debug = FALSE) {
  # Check game settings
  stopifnot(n_row * n_col > 1L, n_mine > 0L, n_mine < n_row * n_col)

  # Initialize game statistics
  game_stat <- data.frame(
    n_row = integer(),
    n_col = integer(),
    n_mine = numeric(),
    success = logical(),
    time = character()
  )

  # Initialize common variables
  alive_state <- button <- ht_obj <- mask_mat <- init_state <- flag_mat <- row_idx <- col_idx <-
    start_time <- exit_confirm <- on_event <- seed <- NULL

  # Functions for graphic events
  set_dev <- function(dev) {
    if (grDevices::dev.cur() != dev) {
      grDevices::dev.set(dev)
    }
    invisible(NULL)
  }

  init_game <- function() {
    set_dev(event_env[["which"]])
    # Initialize game parameters
    rlang::env_bind(rlang::env_parent(),
                    alive_state = TRUE,
                    button = integer(),
                    init_state = TRUE,
                    exit_confirm = 0L,
                    on_event = FALSE)
    # Initialize mask matrix
    rlang::env_bind(rlang::env_parent(),
                    mask_mat = matrix(TRUE, nrow = n_row, ncol = n_col),
                    flag_mat = matrix(FALSE, nrow = n_row, ncol = n_col))
    # Plot game panel
    rlang::env_bind(
      rlang::env_parent(),
      ht_obj = plot_game_panel(value_mat = matrix("U", nrow = n_row, ncol = n_col),
                               label_mat = matrix("", nrow = n_row, ncol = n_col),
                               color_spec = c(U = mask_color),
                               label_color_spec = "black",
                               alive_state = alive_state,
                               remain_mine = n_mine,
                               restart_key = restart_key,
                               start_time = NULL,
                               time_prec = time_prec)
    )
    invisible(NULL)
  }

  mouse_down <- function(buttons, x, y) {
    if (on_event == FALSE) {
      # Set exclusive event handling
      rlang::env_bind(rlang::env_parent(), on_event = TRUE)
      set_dev(event_env[["which"]])
      rlang::env_bind(rlang::env_parent(), button = buttons)
      # Reset event blocking
      rlang::env_bind(rlang::env_parent(), on_event = FALSE)
    }
    invisible(NULL)
  }

  mouse_up <- function(buttons, x, y) {
    if (on_event == FALSE) {
      # Set exclusive event handling
      rlang::env_bind(rlang::env_parent(), on_event = TRUE)
      set_dev(event_env[["which"]])
      cur_button <- button
      rlang::env_bind(rlang::env_parent(), button = integer())
      if (flag_button %in% cur_button == TRUE) {
        action_type <- "flag"
      } else if (unmask_button %in% cur_button == FALSE) {
        return(invisible(NULL))
      }
      # Derive tile clicked
      tile_select <- get_ht_row_col_idx(object = ht_obj, x = x, y = y)
      tile_row <- tile_select[["row_idx"]]
      tile_col <- tile_select[["column_idx"]]
      if (unmask_button %in% cur_button == TRUE) {
        if (mask_mat[tile_row, tile_col, drop = TRUE] == TRUE) {
          action_type <- "unmask"
        } else {
          action_type <- "multi_unmask"
        }
      }
      # Return if click is not on any tile
      if ((is.na(tile_row) == TRUE) || (is.na(tile_col) == TRUE)) {
        return(invisible(NULL))
      }
      # Take an action
      switch (action_type,
              unmask = {
                # Initialize game when first unmask is made
                if (init_state == TRUE) {
                  # Set start time when the first unmask is made
                  rlang::env_bind(rlang::env_parent(),
                                  init_state = FALSE,
                                  start_time = Sys.time(),
                                  seed = .Random.seed)
                  mine_mat <- gen_mine_mat(n_row = n_row,
                                           n_col = n_col,
                                           n_mine = n_mine,
                                           init_row = tile_row,
                                           init_col = tile_col)
                  count_mat <- gen_count_mat(mine_mat = mine_mat)
                  count_mat_label_lgl <- (count_mat %in% seq_len(8L) == TRUE)
                  count_mat_label <- count_mat
                  count_mat_label[count_mat_label_lgl == TRUE] <- digit_char[count_mat[count_mat_label_lgl == TRUE]]
                  rlang::env_bind(rlang::env_parent(),
                                  mine_mat = mine_mat,
                                  count_mat = count_mat,
                                  count_mat_label = count_mat_label)
                }
                if ((is.na(alive_state) == FALSE) &&
                    (alive_state == TRUE) &&
                    # Require masked tile
                    (mask_mat[tile_row, tile_col, drop = TRUE] == TRUE) &&
                    (flag_mat[tile_row, tile_col, drop = TRUE] == FALSE)) {
                  # Unmask a tile with mine: dead
                  if (mine_mat[tile_row, tile_col, drop = TRUE] == TRUE) {
                    new_mask_mat <- mask_mat
                    new_mask_mat[tile_row, tile_col] <- FALSE
                    rlang::env_bind(rlang::env_parent(),
                                    alive_state = FALSE,
                                    mask_mat = new_mask_mat)
                    hit_row_idx <- tile_row
                    hit_col_idx <- tile_col
                  } else {
                    # Unmask a tile
                    new_mask_mat <- unmask_game_panel(row_idx = tile_row,
                                                      col_idx = tile_col,
                                                      mask_mat = mask_mat,
                                                      count_mat = count_mat,
                                                      no_mine_zone = find_no_mine_zone(count_mat, flag_mat))
                    rlang::env_bind(rlang::env_parent(), mask_mat = new_mask_mat)
                  }
                }
              },
              flag = {
                # Toggle flags
                if ((is.na(alive_state) == FALSE) &&
                    (alive_state == TRUE) &&
                    # Require masked tile
                    (mask_mat[tile_row, tile_col, drop = TRUE] == TRUE)) {
                  new_flag_mat <- flag_mat
                  new_flag_mat[tile_row, tile_col] <- !new_flag_mat[tile_row, tile_col]
                  rlang::env_bind(rlang::env_parent(), flag_mat = new_flag_mat)
                }
              },
              multi_unmask = {
                # Unmask multiple tiles around an unmasked one given proper flags (require unmasked tile)
                multi_unmask_valid <- (mask_mat[tile_row, tile_col, drop = TRUE] == FALSE)
                if (multi_unmask_valid == TRUE) {
                  # Count number of flags around the selected tile
                  tile_mask <- matrix(FALSE, nrow = nrow(flag_mat), ncol = ncol(flag_mat))
                  tile_mask[tile_row, tile_col] <- TRUE
                  tile_mask <- mmand::dilate(tile_mask, mmand::shapeKernel(width = 2L, dim = 2L, type = "box"))
                  tile_mask[tile_row, tile_col] <- FALSE
                  # When number of flags match the mine count, trigger multi-unmask
                  multi_unmask_valid <- (count_mat[tile_row, tile_col, drop = TRUE] ==
                                           sum(flag_mat & tile_mask, na.rm = TRUE))
                }
                if ((is.na(alive_state) == FALSE) && (alive_state == TRUE) && (multi_unmask_valid == TRUE)) {
                  tile_to_unmask <- tile_mask & mask_mat & (flag_mat == FALSE)
                  unmask_scheme <- data.frame(row_idx = as.integer(row(tile_to_unmask) * tile_to_unmask),
                                              col_idx = as.integer(col(tile_to_unmask) * tile_to_unmask),
                                              mine_state = as.vector(mine_mat)) %>%
                    dplyr::filter(row_idx > 0L, col_idx > 0L)
                  if (nrow(unmask_scheme) == 0L) {
                    return(invisible(NULL))
                  }
                  # When there are mines, unmask to the first tile with mine and trigger it
                  if (any(unmask_scheme[["mine_state"]] == TRUE)) {
                    rlang::env_bind(rlang::env_parent(), alive_state = FALSE)
                    mine_scheme_idx <- which(unmask_scheme[["mine_state"]] == TRUE)[1L]
                    hit_row_idx <- unmask_scheme[["row_idx"]][mine_scheme_idx]
                    hit_col_idx <- unmask_scheme[["col_idx"]][mine_scheme_idx]
                    unmask_scheme <- utils::head(unmask_scheme, n = mine_scheme_idx - 1L)
                  }
                  new_mask_mat <- Reduce(
                    function(cur_res, idx) {
                      unmask_game_panel(row_idx = unmask_scheme[["row_idx"]][idx],
                                        col_idx = unmask_scheme[["col_idx"]][idx],
                                        mask_mat = cur_res,
                                        count_mat = count_mat,
                                        no_mine_zone = find_no_mine_zone(count_mat, flag_mat))
                    },
                    x = seq_len(nrow(unmask_scheme)),
                    init = mask_mat
                  )
                  if (any(unmask_scheme[["mine_state"]] == TRUE)) {
                    new_mask_mat[hit_row_idx, hit_col_idx] <- FALSE
                  }
                  rlang::env_bind(rlang::env_parent(), mask_mat = new_mask_mat)
                }
              },
              stop("Unknown action type: ", action_type)
      )
      # Check whether player wins the game (set alive_state = NA)
      if ((is.na(alive_state) == FALSE) &&
          (init_state == FALSE) &&
          (identical(mask_mat, mine_mat) == TRUE)) {
        game_won <- TRUE
        rlang::env_bind(rlang::env_parent(), alive_state = NA)
      }
      # Visualize game panel (alive or won/dead in this action)
      if ((rlang::env_has(nms = c("game_won")) == TRUE) ||
          ((is.na(alive_state) == FALSE) && (alive_state == TRUE)) ||
          all(rlang::env_has(nms = c("hit_row_idx", "hit_col_idx")) == TRUE)) {
        rlang::env_bind(
          rlang::env_parent(),
          ht_obj = plot_game_panel_wrap(
            mine_mat = if (init_state == TRUE) {
              matrix(NA, nrow = n_row, ncol = n_col)
            } else {
              mine_mat
            },
            count_mat = if (init_state == TRUE) {
              matrix(NA_integer_, nrow = n_row, ncol = n_col)
            } else {
              count_mat
            },
            count_mat_label = if (init_state == TRUE) {
              matrix(NA_character_, nrow = n_row, ncol = n_col)
            } else {
              count_mat_label
            },
            mask_mat = mask_mat,
            flag_mat = flag_mat,
            mine_char = mine_char,
            flag_char = flag_char,
            unmask_color = unmask_color,
            mask_color = mask_color,
            hit_color = hit_color,
            wrong_color = wrong_color,
            digit_color = digit_color,
            mine_color = mine_color,
            flag_color = flag_color,
            row_idx = if (alive_state == TRUE) NA_integer_ else hit_row_idx,
            col_idx = if (alive_state == TRUE) NA_integer_ else hit_col_idx,
            alive_state = alive_state,
            restart_key = restart_key,
            remain_mine = n_mine - sum(flag_mat, na.rm = TRUE),
            start_time = if (init_state == TRUE) NULL else start_time,
            time_prec = time_prec
          )
        )
        # Check whether game is finished in this action
        if ((is.na(alive_state) == TRUE) || (alive_state == FALSE)) {
          new_game_stat <- rbind(
            game_stat,
            data.frame(
              n_row = n_row,
              n_col = n_col,
              n_mine = n_mine,
              success = (is.na(alive_state) == TRUE),
              time = as.character(hms::as_hms(round(Sys.time() - start_time, digits = time_prec)))
            )
          )
          rlang::env_bind(rlang::env_parent(), game_stat = new_game_stat)
        }
      }
      # Reset event blocking
      rlang::env_bind(rlang::env_parent(), on_event = FALSE)
    }
    invisible(NULL)
  }

  key_press <- function(key) {
    set_dev(event_env[["which"]])
    if (restart_key %in% key == TRUE) {
      init_game()
    } else {
      rlang::env_bind(rlang::env_parent(), exit_confirm = exit_confirm + 1L)
      # Exit game after exit_key_press consecutive key presses other than restart_key
      if (exit_confirm >= exit_key_press) {
        # Force normal termination
        debug <- FALSE
        return(invisible(game_stat))
      }
    }
    invisible(NULL)
  }

  do.call(grDevices::x11, c(list(title = window_title), x11_args))
  grDevices::setGraphicsEventHandlers(
    prompt = window_title,
    onMouseDown = mouse_down,
    onMouseUp = mouse_up,
    onKeybd = key_press
  )
  event_env <- grDevices::getGraphicsEventEnv()
  on.exit({
    if (debug == FALSE) {
      grDevices::dev.off(event_env[["which"]])
    } else {
      if (is.null(seed) == FALSE) {
        message("[debug] random seed = ", paste(head(seed), collapse = ", "), ", ...")
      }
    }
  }, add = TRUE)
  init_game()
  grDevices::getGraphicsEvent(consolePrompt = "")
  invisible(game_stat)
}

# Generate game matrices

#' Generate mine matrix
#'
#' \code{gen_mine_mat} generates mine matrix.
#'
#' @param n_row,n_col Integers as number of rows/columns of game panel.
#' @param n_mine Integer as number of mines.
#' @param init_row,init_col Integers as initial number of row/column.
#' The first tile selected cannot be a mine.
#'
#' @return A logical matrix indicating whether here is a mine or not.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @keywords internal
#' @noRd
gen_mine_mat <- function(n_row, n_col, n_mine, init_row, init_col) {
  stopifnot(n_mine < n_row * n_col)
  res <- matrix(FALSE, nrow = n_row, ncol = n_col)
  res[sample(setdiff(seq_len(n_row * n_col), (init_col - 1L) * n_row + init_row), size = n_mine)] <- TRUE
  res
}

#' Generate mine count matrix
#'
#' \code{gen_count_mat} generates count matrix indicating the number of mines in adjacent 8 tiles.
#' For tiles with mines, the counts are masked with NA.
#'
#' @param mine_mat Logical matrix indicating whether here is a mine or not.
#'
#' @return An integer matrix indicating the number of mines in adjacent tiles.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' # Generate mine matrix
#' mine_mat <- gen_mine_mat(n_row = 10L, n_col = 10L, n_mine = 10L, init_row = 1L, init_col = 1L)
#' # Generate count matrix
#' count_mat <- gen_count_mat(mine_mat)
gen_count_mat <- function(mine_mat) {
  conv_filter <- matrix(1L, nrow = 3L, ncol = 3L)
  conv_filter[2L, 2L] <- 0L
  res <- gsignal::conv2(mine_mat, conv_filter, shape = "same")
  res[mine_mat == TRUE] <- NA_integer_
  res
}

#' Find connected no mine zone
#'
#' \code{find_no_mine_zone} finds connected regions with no mines
#' and label them according to connectivity.
#'
#' @importFrom mgc ConnCompLabel
#'
#' @param count_mat Integer matrix indicating the number of mines in adjacent tiles.
#' @param no_mine_count Integer as the count or less as no mines.
#'
#' @return Integer matrix with labels as connected no mine zones.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @keywords internal
#' @noRd
find_no_mine_zone <- function(count_mat, flag_mat, no_mine_count = 0L) {
  count_mask_mat <- (is.na(count_mat) == FALSE) &
    (count_mat <= no_mine_count) &
    (flag_mat == FALSE)
  mgc::ConnCompLabel(count_mask_mat)
}

#' Recursively remove mask on game panel
#'
#' \code{unmask_game_panel} removes one tile mask from game panel.
#' If the tile mask covers "0" (no mines around), adjacent tiles are recursively
#' removed.
#'
#' @importFrom mmand dilate shapeKernel
#'
#' @param row_idx,col_idx Integers as row/column indices to remove mask.
#' @param mask_mat Logical matrix indicating whether a tile is masked.
#' @param count_mat Integer matrix indicating the number of mines in adjacent tiles.
#' @param no_mine_zone Integer matrix as \code{find_no_mine_zone(count_mat)}.
#'
#' @return Logical matrix with tile masks removed.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' # Generate mask matrix
#' mask_mat <- matrix(TRUE, nrow(mine_mat), ncol(mine_mat))
#' # Remove a tile mask
#' unmask_game_panel(1L, 1L, mask_mat, count_mat)
unmask_game_panel <- function(row_idx, col_idx, mask_mat, count_mat, no_mine_zone) {
  stopifnot(is.na(count_mat[row_idx, col_idx, drop = TRUE]) == FALSE)
  res <- mask_mat
  res[row_idx, col_idx] <- FALSE
  if (count_mat[row_idx, col_idx, drop = TRUE] > 0L) {
    res
  } else {
    unmask_region <- (no_mine_zone == no_mine_zone[row_idx, col_idx, drop = TRUE])
    unmask_region <- mmand::dilate(unmask_region, mmand::shapeKernel(width = 2L, dim = 2L, type = "box"))
    mask_mat & (unmask_region == FALSE)
  }
}

# Plot mine game panel as heatmap

#' Plot mine game panel
#'
#' \code{plot_game_panel} plots mine game panel.
#'
#' @importFrom ComplexHeatmap draw
#' @importFrom grid grid.text gpar unit convertHeight
#' @importFrom hms as_hms
#'
#' @param value_mat Matrix for background of the tiles.
#' @param label_mat Matrix for labels of the tiles.
#' @param color_spec Named character vector as colors for tiles.
#' @param label_color_spec Named character vector as colors for labels.
#'
#' @return A \code{\linkS4class{HeatmapList}} returned from \code{\link[ComplexHeatmap]{draw}}.
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' # Generate count matrix
#' count_mat <- gen_count_mat(mine_mat)
#' count_mat[count_mat == 0] <- ""
#' # Plot heatmap
#' plot_game_panel(
#'   mine_mat,
#'   count_mat,
#'   color_spec = circlize::colorRamp2(c(FALSE, TRUE), colors = c("green", "red")),
#'   label_color_spec = `names<-`(c(NA_character_, circlize::colorRamp2(breaks = c(1L, 4.5L, 8L), colors = c("darkblue", "darkgreen", "darkred"))(1L:8L), "black", "black"), c("", 1L:8L, "NA", "F"))
#' )
plot_game_panel <- function(value_mat, label_mat, color_spec, label_color_spec, alive_state, remain_mine, restart_key, start_time = NULL, time_prec) {
  restart_msg <- paste0("(press [", restart_key, "] to restart)")
  ht_obj <- ComplexHeatmap::Heatmap(
    value_mat,
    col = color_spec,
    cell_fun = function(j, i, x, y, width, height, fill) {
      cell_label <- label_mat[i, j, drop = TRUE]
      if (cell_label %in% names(label_color_spec) == TRUE) {
        label_color <- label_color_spec[cell_label]
        grid::grid.text(cell_label,
                        gp = grid::gpar(col = label_color,
                                        fontsize = grid::convertHeight(grid::unit(height, "npc"),
                                                                       "points",
                                                                       valueOnly = TRUE) * 0.7),
                        x = x, y = y)
      } else {
        if (nchar(cell_label) > 0L) {
          stop("cell label not found in the names of label_color_spec: ", cell_label)
        }
      }
    },
    rect_gp = grid::gpar(col = "black"),
    column_title = paste0(
      if (is.na(alive_state) == TRUE) {
        paste("Bingo", restart_msg, sep = " ")
      } else {
        if (alive_state == TRUE) "Alive" else paste("Dead", restart_msg, sep = " ")
      },
      ", unflagged mine(s): ", remain_mine, ", time: ",
      if (is.null(start_time) == TRUE) {
        hms::as_hms(0)
      } else {
        round(hms::as_hms(Sys.time() - start_time), digits = time_prec)
      }
    ),
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    show_heatmap_legend = FALSE,
    use_raster = FALSE
  )
  ComplexHeatmap::draw(ht_obj)
}

#' Wrapper function around plot_game_panel
#'
#' \code{plot_game_panel_wrap} is a wrapper around \code{plot_game_panel}.
#' It generates \code{value_mat} and \code{label_mat} from game matrices.
#'
#' @param mine_mata,count_mat,mask_mat,flag_mat Game matrices.
#' @inheritParams run_game
#' @inheritDotParams plot_game_panel -value_mat -label_mat -alive_state
#'
#' @inherit plot_game_panel return
#'
#' @note Change log:
#' \itemize{
#'   \item{0.1.0 Xiurui Zhu - Initiate the function.}
#' }
#' @author Xiurui Zhu
#'
#' @keywords internal
#' @noRd
plot_game_panel_wrap <- function(mine_mat, count_mat, count_mat_label, mask_mat, flag_mat,
                                 mine_char, flag_char, unmask_color, mask_color,
                                 hit_color, wrong_color, digit_color, mine_color,
                                 flag_color, row_idx, col_idx,
                                 no_mine_count = 0L, alive_state, ...) {
  # Set tile color matrix
  ## U = unmask
  ## M = mask
  ## H = tile unmasked with mine
  ## W = wrongly flagged tile
  value_mat <- mask_mat
  value_mat[] <- ifelse(mask_mat == TRUE, "M", "U")
  # Set tile label matrix
  ## "" = no mine or masked
  ## flag_char = flag
  ## mine_char = none-flagged mine
  label_mat <- count_mat_label
  label_mat[mask_mat == TRUE] <- ""
  label_mat[count_mat <= no_mine_count] <- ""
  label_mat[flag_mat == TRUE] <- flag_char
  if ((is.na(alive_state) == FALSE) && (alive_state == FALSE)) {
    # Mark tiles with non-flagged mines as unmasked
    value_mat[(mine_mat == TRUE) & (flag_mat == FALSE)] <- "U"
    # Mark the triggered mine
    value_mat[row_idx, col_idx] <- "H"
    # Mark wrong flags
    value_mat[(flag_mat == TRUE) & (mine_mat == FALSE)] <- "W"
    # Reveal mine_char
    label_mat[(mine_mat == TRUE) & (flag_mat == FALSE)] <- mine_char
  }
  color_spec <- c(U = unmask_color, M = mask_color, H = hit_color, W = wrong_color)
  label_color_spec <- c(digit_color, mine_color, flag_color, "black")
  names(label_color_spec) <- c(format(seq_along(digit_color), scientific = FALSE),
                               mine_char, flag_char, "")
  plot_game_panel(
    value_mat = value_mat,
    label_mat = label_mat,
    color_spec = color_spec,
    label_color_spec = label_color_spec,
    alive_state = alive_state,
    ...
  )
}
