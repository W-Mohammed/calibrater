#' Rosenbrock function
#'
#' @param xx function input, a vector of two or more. The length of (number
#' of values in) \code{xx} determines the dimensionality of the function.
#' @param ... artefact to allow this function to work with the code that
#' was scripted for other goodness-of-fit functions
#'
#' @return
#' @export
#'
#' @examples
calibTest_rosen <- function(xx = c(1,1), ...) {
  ##########################################################################
  #
  # ROSENBROCK FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it
  # and/or modify it under the terms of the GNU General Public License as
  # published by the Free Software Foundation; version 2.0 of the License.
  # Accordingly, this program is distributed in the hope that it will be
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  with(as.list(xx), {
    d <- length(xx)
    xi <- xx[1:(d-1)]
    xnext <- xx[2:d]

    sum <- sum(100*(xnext-xi^2)^2 + (xi-1)^2)

    y <- sum
    return("Rosenbrock" = y)
  })
}

# #' Rescaled Rosenbrock function
# #'
# #' @param xx function input, a vector of four or more. The length of
# #' (number of values in) \code{xx} determines the dimensionality of the
# #' function.
# #'
# #' @return
# #' @export
# #'
# #' @examples
# calibTest_rosen_sc <- function(xx) {
#   ##########################################################################
#   #
#   # ROSENBROCK FUNCTION, RESCALED
#   #
#   # Authors: Sonja Surjanovic, Simon Fraser University
#   #          Derek Bingham, Simon Fraser University
#   # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
#   #
#   # Copyright 2013. Derek Bingham, Simon Fraser University.
#   #
#   # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
#   # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
#   # derivative works, such modified software should be clearly marked.
#   # Additionally, this program is free software; you can redistribute it
#   # and/or modify it under the terms of the GNU General Public License as
#   # published by the Free Software Foundation; version 2.0 of the License.
#   # Accordingly, this program is distributed in the hope that it will be
#   # useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#   # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#   # General Public License for more details.
#   #
#   # For function details and reference information, see:
#   # http://www.sfu.ca/~ssurjano/
#   #
#   ##########################################################################
#   #
#   # INPUT:
#   #
#   # xx = c(x1, x2, ..., xd)
#   #
#   ##########################################################################
#
#   xxbar <- 15*xx - 5
#   xibar <- xxbar[1:3]
#   xnextbar <- xxbar[2:4]
#
#   sum <- sum(100*(xnextbar-xibar^2)^2 + (1 - xibar)^2)
#
#   y <- (sum - 3.827*10^5) / (3.755*10^5)
#   return(y)
# }
