##############################
## Author: Dason Kurkiewicz  #
## Date  : March 28, 2012    #
##############################

####################################################
## Helper Functions ################################
####################################################

##' Returns a list with the stack at each interval as the elements
##'
##' .. content for \details{} ..
##' @title Parse Rprof file
##' @param filename A character string that is either a direct path or a relative path to a file that is output from Rprof
##' @return A list with the stack at each interval as the elements.
##' @author Dason Kurkiewicz
.parseRprof <- function(filename){
    ## Import the file
    j <- scan(filename, sep = "\n", what = character())

    ## Try to extract the interval length
    ## If there are multiple lines it only grabs
    ## the interval from the first one
    interval <- as.numeric(strsplit(grep("sample.interval", j, fixed = T, val = T), "=")[[1]][2])/1000000

    ## Remove all lines with sample.interval
    dat <- j[-grep("sample.interval", j, fixed = TRUE)]
    ## Split by spaces
    dat <- strsplit(dat, " ")
    ## Reverse the vectors so the bottom of the stack
    ## Is at the front of the vector
    dat <- sapply(dat, rev)
    return(list(dat = dat, interval = interval))
}


##' Figures out which elements of y are continued from x.
##'
##' Stuff
##' @title Which continued
##' @param x vec1
##' @param y vec2
##' @return A vector indicating the indices of the values of y that are continued from x
##' @author Dason Kurkiewicz
.whichContinued <- function(x, y){
    id <- x[1:length(y)] == y
    id <- which(cumsum(!id) == 0)
    return(id)
}


##' Attempts to determine the namespace a function comes from
##'
##' Notes: - This is not vectorized and is expected to
##'          be the function for sapply or similar.
##'        - The namespace needs to be loaded to be detected
##'          So if you're analyzing a Rprof file that wasn't
##'          created during the current session if you don't
##'          reload any necessary libraries the namespaces
##'          might not be detected.
##' @title Figure out a function's namespace
##' @param x A character string corresponding to a function of interest
##' @return The best guess as to what namespace the function comes from
##' @author Dason Kurkiewicz
.getNamespace <- function(x){
    j <- do.call(getAnywhere, list(x))$where
    id <- grep("namespace:", j)
    if(length(id) == 0){
        return(NA)
    }
    ans <- gsub("namespace:", "", j[id[1]])
    return(ans)
}

##' blah
##'
##' wchoo
##' @title makedf
##' @param x An element of dat (so something like dat[[3]])
##' @param cont A vector of which elements in x are continued from the previous element of dat.  This should be obtained by a call to .whichContinued
##' @param time Which element of dat we're looking at (if dat[[3]] then time = 3)
##' @return A data frame with the full call stack at that time.
##' @author Dason Kurkiewicz
.makeDF <- function(x, cont, time){
    j <- data.frame(func = x,
                    level = seq_along(x),
                    time = time,
                    cont = FALSE,
                    length = NA,
                    keep = FALSE,
                    leaf = FALSE)
    j$cont[cont] <- TRUE
    ## Set the last element in the stack as a leaf
    j[nrow(j), "leaf"] <- TRUE
    return(j)
}

##' Takes dat and an index and returns the corresponding data frame for the call stack with the 'cont' column appropropriately filled out.
##'
##' What I said up above
##' @title Awesomesauce
##' @param i Which index of dat we're using
##' @param dat The list corresponding to the call stacks
##' @return A data frame for the index passed in
##' @author Dason Kurkiewicz
.modDF <- function(i, dat){
    if(i == 1){
        return(.makeDF(dat[[i]], FALSE, i))
    }
    return(.makeDF(dat[[i]], .whichContinued(dat[[i-1]], dat[[i]]), i))
}

## dat: A list output from .parseRprof
## Gets all stack calls into one dataframe
##' Blah
##'
##' Blah 2
##' @title create full df
##' @param dat The list corresponding to the call stack
##' @return A data frame with the full call stack for each interval
##' @author Dason Kurkiewicz
.createFullDF <- function(dat){
    do.call(rbind, lapply(seq(dat), .modDF, dat = dat))
}

##' Cats
##'
##' Not cats
##' @title Clean list into data frame
##' @param dat List of the call stack (from .parseRprof)
##' @param interval The interval time used in Rprof
##' @param namespace A logical value indicating if namespaces for functions should be detected.  This is a lengthy process so it is set to FALSE by default.
##' @return A cleaned up data frame with only the calls necessary
##' @author Dason Kurkiewicz
.cleanDF <- function(dat, interval = .02, namespace = namespace){
    ## Put all stack calls into a single data frame
    ans <- .createFullDF(dat)

    ## We want to loop over the function calls
    ## at each level and figure out which
    ## were separate calls.  Since we could have something like
    ## ddply, rnorm, ddply : we need to be able to split it
    ## up and tell that those ddply calls are separate
    ## -
    ## We loop to figure out unique ids and find the length
    ## of each call
    lvs <- factor(interaction(ans[,1], ans[,2]))
    for(lev in levels(lvs)){
        id <- which(lvs == lev)
        k <- !ans[id, "cont"]
        group <- cumsum(k)
        tmp <- ans[id,]
        tmp$keep <- k
        tmp[which(k), "length"] <- rle(group)[[1]]
        ans[id,] <- tmp
    }

    ## Only keep the first time a function is seen in the stack
    ans <- ans[ans$keep,]

    ## Compute start time, end time, and length of call
    ans$start <- (ans$time-1)*interval
    ans$end <- ans$start + interval*ans$length
    ans$length <- ans$length*interval
    ## We want to keep leaf but we want it at the end of the dataframe
    leaf <- ans$leaf
    ## We don't need these columns
    ans <- ans[ , !(colnames(ans) %in% c("time", "cont", "keep", "leaf"))]
    ## But we do want leaf...
    ans$leaf <- leaf

    if(namespace){
        ans$namespace <- sapply(ans$func, .getNamespace)
    }

    return(ans)
}

##################################################
## Useful Functions ##############################
##################################################



##' Takes a file corresponding to output from Rprof and cleans it up and produces a nice data frame.
##'
##' Does what I said
##' @title Clean Rprof
##' @param filename A character string that contains either a relative or direct path to the file containing the Rprof output.
##' @param namespace A logical value indicating if you want the namespace detected.
##' @return A nice data frame
##' @export
##' @author Dason Kurkiewicz
cleanRprof <- function(filename, namespace = FALSE){
    ## Get a list that contains
    ## - dat - contains the stack as elements of a list for each time int
    ## - interval - The interval used in Rprof.  Only detected once
    ##              if the interval changes this isn't detected
    tmp <- .parseRprof(filename)
    dat <- tmp$dat
    interval <- tmp$interval
    ## Clean up the list and return a data frame
    ## with the relevant information
    ans <- .cleanDF(dat, interval = interval, namespace = namespace)
    ## Give it a class of "MyRprof" so we can use S3
    ## methods for the plot command
    class(ans) <- c("MyRprof", "data.frame")
    return(ans)
}

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


################################################
## Examples ####################################
################################################

## ## Use examples from productplots for profiling...
## if(!("Prodplot.txt" %in% dir())){
##     Rprof("Prodplot.txt")
##     library(productplots)
##     if (require("ggplot2")) {
##          prodplot(happy, ~ happy, "hbar")
##          prodplot(happy, ~ happy, "hspine")

##          prodplot(happy, ~ sex + happy, c("vspine", "hbar"))
##          prodplot(happy, ~ sex + happy, stacked())

##          ## The levels argument can be used to extract a given level of the plot
##          prodplot(happy, ~ sex + happy, stacked(), level = 1)
##          prodplot(happy, ~ sex + happy, stacked(), level = 2)
##     }
##     Rprof(NULL)
## }

## ## You can use any output file from Rprof you want
## dat <- cleanRprof("Prodplot.txt")
## head(dat, 20)
## ## Unlike profr my version outputs the function calls in
## ## order whereas profr outputs based on the level depth.
## ## I like to think of it as I'm doing a depth first search of the
## ## stack through time and profr is doing a breadth first search

## ## I also wrote a little option to try to automatically
## ## detect the namespace a function belongs to.
## ## It isn't too fast though so I turned it off by default
## dat <- cleanRprof("Prodplot.txt", namespace = TRUE)
## ## cleanRprof returns an object of class MyRprof and I wrote
## ## and S3 plotting method for that to take care of the 'bridge'
## ## problem.  Really the bridge problem is caused by the way
## ## profr is summarizing the call stack.
## plot(dat)

## ## There are quite a few options you can use in the plotting method.
## plot(dat, minlab = .1, add = TRUE, xlim = c(1,1.5), col = "blue", quiet = F)
