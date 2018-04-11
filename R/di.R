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
#' @param rescale.custom A custom rescaling. See example below.
#' Default: \code{NULL}.
#' @param rescale.avoid A set of column names for which rescaling should be avoided.
#' Default: \code{NULL}.
#' @param bins A number of bins for plotting the DI against age from a dataset. 
#' Default: \code{7}.
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
#' 
#' # Cusom rescaling
#' ddi <- di(dd, c("var1", "var2", "var3"), rescale.custom=c("var1:0.1:0.5"))
#' ddi
#' @rdname di-di
#' @export
di <- function(dat, 
               cols=NULL, 
               invert=NULL, 
               rescale=TRUE, 
               age=NULL, 
               rescale.custom=NULL,
               rescale.avoid=NULL,
               bins=7,
               visible=FALSE) {
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
    
    ## Rescaling
    custom.resc <- list()
    if(!is.null(rescale.custom)) {
        for(item in rescale.custom) {
            splits <- unlist(strsplit(item, ":"))
            if(length(splits) == 1 | !(splits[1] %in% names(tmp))) {
                msg <- sprintf("Custom rescaling '%s' incorrectly defined and will be omitted!", item)
                warning(msg)
            } else {
                # Total number of levels
                num.lev <- length(as.numeric(levels(as.factor(tmp[, splits[1]]))))
                
                if((length(splits) - 1) != num.lev) {
                    msg <- sprintf("Custom rescaling '%s' incorrectly defined and will be omitted!", item)
                    warning(msg)
                } else {
                    custom.resc[[splits[1]]] <- c()
                    if(!is.null(rescale.avoid)) {
                        rescale.avoid <- c(rescale.avoid, splits[1])
                    } else {
                        rescale.avoid <- splits[1]
                    }
                    
                    for(i in 2:length(splits)) {
                        custom.resc[[splits[1]]] <- c(custom.resc[[splits[1]]], splits[i])
                    }
                    
                    custom.resc[[splits[1]]] <- as.numeric(custom.resc[[splits[1]]])
                }
            }
        }
    }
    
    if(length(custom.resc) != 0) {
        for(item in names(custom.resc)) {
            for(j in 1:length(custom.resc[[item]])) {
                tmp[which(tmp[[item]] == as.numeric(levels(as.factor(tmp[, item])))[j]), item] <- custom.resc[[item]][j]
            }
        }
    }
    
    if(rescale) {
        tmp.rescaled <- tmp
        for(item in names(tmp)[which(!(names(tmp) %in% rescale.avoid))]) {
            tmp.rescaled[, item] <- rescale(tmp.rescaled[, item])
        }
        #tmp.rescaled <- apply(tmp[, which(!(names(tmp) %in% rescale.avoid))], 2, FUN = rescale)
    } else {
        tmp.rescaled <- tmp
    }
    
    # Calculate DI
    #di.val <- rowSums(tmp.rescaled, na.rm = T)/dim(tmp.rescaled)[2]
    di.val <- rowSums(tmp.rescaled, na.rm = T)/rowSums(!is.na(tmp.rescaled))
    data.di <- cbind(dat, di=di.val)
    ###################################################################
  
    if(visible & !is.null(age)) {
        tryCatch({
            plot.di(data.di, age, bins)
        }, error = function(e) {
            print(paste("Please use lower # of bins. Current # is", bins))
        })
    }
  
    return(list(di=di.val, columns=tmp.rescaled))
}

plot.di <- function(ddi, age, bins=7) {
    # Plot DI
    bins <- bins
    cutpoints <- quantile(ddi[[age]],(0:bins)/bins, na.rm=T)
    binned <- cut(ddi[[age]],cutpoints,include.lowest=TRUE)
    mean.di <- tapply(ddi[["di"]], binned, mean)
    plot(x = cutpoints[1:length(mean.di)], y=mean.di, 
         xlab="Age", ylab = "DI", main="Average DI by age in a population")
}