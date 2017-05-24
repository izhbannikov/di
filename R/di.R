#' This function calculates Deficit Index (DI) and returns results 
#' in form of an original data frame with additional DI column.
#' 
#' @param dat A data frame.
#' @param cols A list of column names.
#' @param age A name of column which represents age of a patient.
#' Default: \code{NULL}.
#' @param visible A flag to show DI plot (mean DI in a population by age)
#' Default: \code{FALSE}
#' @return An original data frame with additional DI column.
#' @examples
#' library(di)
#' ddi <- di(dd, di_columns, age="Age_orig", visible = T)
#' @rdname di-di
#' @export
di <- function(dat, cols, age=NULL, visible=FALSE) {
    ###################### DI calculation #############################
    tmp <- dat[,cols]
    # Basic preprocessing
    ## Replace negatives with NAs
    tmp[tmp < 0] <- NA
    ## Rescale
    tmp.rescaled <- apply(tmp, 2, FUN = rescale)
    # Calculate DI
    di.val <- rowSums(tmp.rescaled, na.rm = T)/dim(tmp)[2]
    data.di <- cbind(dat, di=di.val)
    ###################################################################
  
    if(visible & !is.null(age)) {
        tryCatch({
            plot.di(data.di, age)
        }, error = function(e) {
            print(e)
        })
    }
  
    return(data.di)
}

plot.di <- function(ddi, age) {
    # Plot DI
    bins <- 10
    cutpoints <- quantile(ddi[[age]],(0:bins)/bins, na.rm=T)
    binned <- cut(ddi[[age]],cutpoints,include.lowest=TRUE)
    mean.di <- tapply(ddi[["di"]], binned, mean)
    plot(x = cutpoints[1:length(mean.di)], y=mean.di, xlab="Age", ylab = "DI", main="Average DI by age in a population")
}