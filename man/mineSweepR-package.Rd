% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mineSweepR-package.R
\docType{package}
\name{mineSweepR-package}
\alias{mineSweepR}
\alias{mineSweepR-package}
\title{mineSweepR: Mine Sweeper Game}
\description{
This is the very popular mine sweeper game! The game requires you to find out tiles that contain mines through clues from unmasking neighboring tiles. Each tile that does not contain a mine shows the number of mines in its adjacent tiles. If you unmask all tiles that do not contain mines, you win the game; if you unmask any tile that contains a mine, you lose the game. For further game instructions, please run `help(run_game)` and check details. This game runs in X11-compatible devices with `grDevices::x11()`.
}
\examples{
# Run mine sweeper game
if (interactive() == TRUE) {
  run_game()
}
}
\author{
\strong{Maintainer}: Xiurui Zhu \email{zxr6@163.com}

}
\keyword{internal}
