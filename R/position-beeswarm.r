#' Jitter points to avoid overplotting, using the beeswarm package to avoid
#' overlapping points when/where possible
#'
#' @family position adjustments
#' @param method beeswarm package method (one of "swarm", "center", "hex" or "square)
#' @param corral beeswarm package corral (one of "none", "gutter", "wrap", "random" or "omit")
#' @export
#' @examples
#' qplot(am, vs, data = mtcars)
#' 
#' # jittering will generally be too much for small datasets:
#' qplot(am, vs, data = mtcars, position = "jitter")
#' # even with limited jittering, things can look weird:
#' qplot(am, vs, data = mtcars, position = position_jitter(w = 0.1, h = 0.1))
#' # instead, use beeswarm to get a centered representation
#' qplot(am, vs, data = mtcars, position = "beeswarm")
#'
#' # jitter positioning works well for large datasets, where it
#' # take up as much space as a boxplot or a bar
#' qplot(class, hwy, data = mpg, geom = c("boxplot", "jitter"))
#' # here use beeswarm "corral" parameter to control
#' # overlapping points
#' qplot(class, hwy, data = mpg, position=position_beeswarm(corral="wrap")) + geom_boxplot()
position_beeswarm <- function (method = NULL, corral = NULL, extra = NULL) { 
  PositionBeeswarm$new(width = method, height = corral, extra = extra)
}

PositionBeeswarm <- proto(Position, {
  objname <- "beeswarm"
 
  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    check_required_aesthetics(c("x", "y"), names(data), "position_beeswarm")
    
    if (is.null(.$width)) .$width <- "swarm"
    if (is.null(.$height)) .$height <- "none"

    if (is.null(.$extra) || is.null(.$extra$cex)) cex <- 1
    else cex <- .$extra$cex
    
    x <- data$x
    y <- data$y

    try_require("beeswarm")

    bs <- beeswarm(y~x, method=.$width, corral=.$height, do.plot=F, cex=cex)

    data$x <- bs[,1]
    data$y <- bs[,2]

    data
  }
  
})
