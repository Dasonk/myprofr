
##' Plots the call stack using back graphics
##'
##' Could be prettier but it doesn't have the bridge problem
##' @title Plots the call stack
##' @param x An object of class 'MyRprof'
##' @param minlab The minimum 'length' for a call to get a label
##' @param add logical value indicating if you want to have the option to click on rectangles after the initial plot is made to add labels
##' @param xlim The limits for plotting for time
##' @param col The color of the rectangles
##' @param quiet logical value indicating if you want instructions printed if add = T
##' @param ... other arguments passed onto rect
##' @method plot MyRprof
##' @S3method plot MyRprof
##' @author Dason Kurkiewicz
plot.MyRprof <- function(x, minlab = 0.1, add = FALSE, xlim = NULL, col = "grey", quiet = FALSE, ...){
    ## If xlim is NULL then use the range
    if(is.null(xlim)){
        xlim <- range(x$start, x$end)
    }
    
    ## Make a blank plot that has limits that we want
    plot(1, 1,
         xlim = xlim,
         ylim = range(x$level) + c(-.5, .5),
         type = "n",
         main = "Call Stack Over Time",
         xlab = "Time (in seconds)",
         ylab = "Call Stack Level")
    
    ## Plot rectangles for everything in the call stack
    rect(x$start, x$level - 0.5, x$end, x$level + 0.5, col = col, ...)
    
    ## Only plot labels for calls longer than minlab
    labeldat <- x[x$length >= minlab,]
    if(dim(labeldat)[1] > 0){
        ## Plot labels.
        ## with(labeldat, text((start + end)/2, level, func)) # label in center
        with(labeldat, text(start, level, func, pos = 4)) # label at start
    }
    
    if(add){
        if(!quiet){
            cat("Left click on the plot to add labels to rectangles\n")
            cat("Depending on your system and the way you are interacting\n")
            cat("with R you may need to:\n")
            cat(" - Just 'right click'\n")
            cat(" - Right click and select 'stop'\n")
            cat(" - If you are using RStudio - Click 'Finished' in the upper right or hit 'Esc'\n")
        }
        flush.console()
        
        pnt <- locator(1)
        while(!is.null(pnt)){
            px <- pnt$x
            py <- pnt$y
            ## Find which elements in the call stack the user clicked on
            j <- which(x$start <= px & px <= x$end & (x$level - 0.5) <= py & py <= (x$level + 0.5))
            if(length(j) > 0){
                j <- x[j,]
                with(j, text(start, level, func, pos = 4))
            }
            pnt <- locator(1)
        }
    } ## End of add
}
