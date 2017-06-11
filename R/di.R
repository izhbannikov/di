#' This function calculates the Deficit Index (DI) and returns results 
#' as list: \code{di} a column-vector containing 
#' deficit indexes for each individual and
#' \code{columns} that were used to calculate the \code{di}.
#' 
#' @param dat A data frame. Required parameter.
#' @param cols A list of column names.
#' Default: \code{NULL}.
#' @param invert A list of columns which have to be inverted.
#' Default: \code{NULL}.
#' @param rescale A flag that tell the program to rescale columns 
#' if the values a not 0/1. Default: \code{TRUE}.
#' @param age A name of column which represents age of a patient.
#' Default: \code{NULL}.
#' @param visible A flag to show DI plot (mean DI in a population by age)
#' Default: \code{FALSE}
#' @return A list of two: \code{di} a column-vector containing 
#' deficit indexes for each individual and
#' \code{columns} (rescaled if flag \code{rescale} was set to \code{TRUE})
#' that were used to calculate the \code{di}.
#' @examples
#' library(di)
#' dd <- data.frame(subj=seq(1:100), 
#'                  var1=rbinom(100,1,.5), 
#'                  var2=rbinom(100,1,.5), 
#'                  var3=rbinom(100,1,.5))
#' ddi <- di(dd, c("var1", "var2", "var3"))
#' @rdname di-di
#' @export
di <- function(dat, cols=NULL, invert=NULL, rescale=TRUE, age=NULL, visible=FALSE) {
    # Basic preprocessing
    if(class(dat) != "data.frame") {
        stop("Parameter dat must be a data frame. Aborting.")
    }
  
    if(!is.null(cols)) {
        tmp <- dat[,cols]
    } else {
        tmp <- dat
    }
  
    ## Replace negatives with NAs
    tmp[tmp < 0] <- NA
    
    ## Invert values if necessary
    if(!is.null(invert)) {
        for(col in invert) {
            tmp[[col]] <- max(tmp[[col]], na.rm = TRUE) - tmp[[col]]
        }
    }
    
    ## Rescale
    if(rescale) {
        tmp.rescaled <- apply(tmp, 2, FUN = rescale)
    } else {
        tmp.rescaled <- tmp
    }
    
    # Calculate DI
    di.val <- rowSums(tmp.rescaled, na.rm = T)/dim(tmp.rescaled)[2]
    data.di <- cbind(dat, di=di.val)
    ###################################################################
  
    if(visible & !is.null(age)) {
        tryCatch({
            plot.di(data.di, age)
        }, error = function(e) {
            print(e)
        })
    }
  
    return(list(di=di.val, columns=tmp.rescaled))
}

plot.di <- function(ddi, age, bins=10) {
    # Plot DI
    bins <- bins
    while(1) {
        tryCatch({
            cutpoints <- quantile(ddi[[age]],(0:bins)/bins, na.rm=T)
            binned <- cut(ddi[[age]],cutpoints,include.lowest=TRUE)
            mean.di <- tapply(ddi[["di"]], binned, mean)
            plot(x = cutpoints[1:length(mean.di)], y=mean.di, 
                 xlab="Age", ylab = "DI", main="Average DI by age in a population")
            break
        }, error=function(e) {
            bins <- bins - 1
            if(bins == 0)
                break
        })
      }
}