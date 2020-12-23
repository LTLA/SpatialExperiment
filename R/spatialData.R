#' @rdname spatialData-methods
#' @title Methods for handling spatial-related data
#' @aliases spatialData spatialCoords spatialDataNames
#' 
#' @description 
#' The set of functions described below is designed to handle spatial-related 
#' data stored inside a \code{SpatialExperiment}'s \code{colData}, including:
#' \itemize{
#' \item{\code{spatialData/Coords()} 
#'   to retrieve a table of all spatially related data or 
#'   spatial coordinates, respectively, and (optionally) other
#'   observation metadata (\code{colData}) variables of interest.}
#' \item{\code{spatialDataNames()}
#'   to retrieve a character vector of spatial coordinate names}
#' } 
#' 
#' @param x a \code{\link{SpatialExperiment}}.
#' @param cd_keep character vector specifying 
#'   additional \code{colData} columns to retain.
#' @param sample_id specifies which sample(s) to keep; valid values are 
#'   TRUE (default) for all samples, NULL for the first entry available, 
#'   or a character vector with elements in \code{unique(x$sample_id)}.
#' @param as.df logical; should the retrieved data be converted 
#'   to a \code{data.frame}? By default, they are returned as a matrix.
#' 
#' @return 
#' \describe{
#' \item{spatialData/Coords}{
#'   a \code{DataFrame/matrix} of all spatial-related 
#'   data or spatial coordinates, respectively.
#'   When \code{as.df = TRUE}, data are returned as a \code{data.frame};
#'   when \code{cd_keep} is specified, these are included as well.}
#' \item{spatialDataNames}{
#'   a character vector of spatial-related data names. 
#'   E.g. for spot-based experiments, these include
#'   xyz-coordinate and array position names.}
#' }
#'
#' @examples
#' example(SpatialExperiment)
#' spatialDataNames(se)
#' head(spatialData(se))
#' head(spatialCoords(se))
#' 
#' @export
setMethod(f="spatialData", signature="SpatialExperiment",
    function(x, cd_keep=NULL, sample_id=TRUE, as.df=FALSE)
    {
        df <- .get_spatialData(x, cd_keep, sample_id, as.df, which = "data")
        if (!as.df) return(df)
        data.frame(df, check.names = FALSE)
    }
)

#' @rdname spatialData-methods
#' @export
setMethod(f="spatialCoords", signature="SpatialExperiment",
    function(x, cd_keep=NULL, sample_id=TRUE, as.df=FALSE)
    {
        df <- .get_spatialData(x, cd_keep, sample_id, as.df, which = "coords")
        if (!as.df) return(as.matrix(df))
        data.frame(df, check.names = FALSE)
    }
)

#' @rdname spatialData-methods
#' @export
setMethod("spatialDataNames", "SpatialExperiment", function(x) x@spaCoordsNms)

.get_row_idx <- function(x, sample_id) {
    if (isTRUE(sample_id)) {
        idx <- seq(ncol(x))
    } else if (is.null(sample_id)) {
        idx <- x$sample_id == unique(x$sample_id)[1]
    } else if (is.character(sample_id)) {
        stopifnot(sample_id %in% x$sample_id)
        idx <- x$sample_id %in% sample_id
    } else stop("'sample_id' invalid; should be TRUE, NULL or a", 
        " character vector with elements in 'colData' field 'sample_id'.")
    return(idx)
}

.get_spatialData <- function(x, 
    cd_keep, sample_id, as.df, 
    which=c("data", "coords")) 
{
    i <- get_row_idx(x, sample_id)
    j <- switch(
        match.arg(which), 
        data=x@spaCoordsNms,
        coords=grep("^(x|y|z)_coord$", names(colData(x)), value = TRUE)
    )
    
    if (!is.null(cd_keep)) {
        stopifnot(
            is.character(cd_keep), 
            cd_keep %in% names(colData(x)))
        j <- unique(c(j, cd_keep))
    }
    
    colData(x)[i, j, drop = FALSE]
}