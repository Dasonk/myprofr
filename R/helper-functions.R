# Helper functions


## Returns a list with the stack at each interval as the elements
##
## .. content for \details{} ..
## @title Parse Rprof file
## @param filename A character string that is either a direct path or a relative path to a file that is output from Rprof
## @return A list with the stack at each interval as the elements.
## @author Dason Kurkiewicz
parseRprof <- function(filename){
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


## Figures out which elements of y are continued from x.
##
## Stuff
## @title Which continued
## @param x vec1
## @param y vec2
## @return A vector indicating the indices of the values of y that are continued from x
## @author Dason Kurkiewicz
whichContinued <- function(x, y){
    id <- x[1:length(y)] == y
    id <- which(cumsum(!id) == 0)
    return(id)
}


## Attempts to determine the namespace a function comes from
##
## Notes: - This is not vectorized and is expected to
##          be the function for sapply or similar.
##        - The namespace needs to be loaded to be detected
##          So if you're analyzing a Rprof file that wasn't
##          created during the current session if you don't
##          reload any necessary libraries the namespaces
##          might not be detected.
## @title Figure out a function's namespace
## @param x A character string corresponding to a function of interest
## @return The best guess as to what namespace the function comes from
## @author Dason Kurkiewicz
getNamespace <- function(x){
    j <- do.call(getAnywhere, list(x))$where
    id <- grep("namespace:", j)
    if(length(id) == 0){
        return(NA)
    }
    ans <- gsub("namespace:", "", j[id[1]])
    return(ans)
}

## blah
##
## wchoo
## @title makedf
## @param x An element of dat (so something like dat[[3]])
## @param cont A vector of which elements in x are continued from the previous element of dat.  This should be obtained by a call to .whichContinued
## @param time Which element of dat we're looking at (if dat[[3]] then time = 3)
## @return A data frame with the full call stack at that time.
## @author Dason Kurkiewicz
makeDF <- function(x, cont, time){
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

## Takes dat and an index and returns the corresponding data frame for the call stack with the 'cont' column appropropriately filled out.
##
## What I said up above
## @title Awesomesauce
## @param i Which index of dat we're using
## @param dat The list corresponding to the call stacks
## @return A data frame for the index passed in
## @author Dason Kurkiewicz
modDF <- function(i, dat){
    if(i == 1){
        return(makeDF(dat[[i]], FALSE, i))
    }
    return(makeDF(dat[[i]], whichContinued(dat[[i-1]], dat[[i]]), i))
}

## dat: A list output from .parseRprof
## Gets all stack calls into one dataframe
## Blah
##
## Blah 2
## @title create full df
## @param dat The list corresponding to the call stack
## @return A data frame with the full call stack for each interval
## @author Dason Kurkiewicz
createFullDF <- function(dat){
    do.call(rbind, lapply(seq(dat), modDF, dat = dat))
}

## Cats
##
## Not cats
## @title Clean list into data frame
## @param dat List of the call stack (from .parseRprof)
## @param interval The interval time used in Rprof
## @param namespace A logical value indicating if namespaces for functions should be detected.  This is a lengthy process so it is set to FALSE by default.
## @return A cleaned up data frame with only the calls necessary
## @author Dason Kurkiewicz
cleanDF <- function(dat, interval = .02, namespace = namespace){
    ## Put all stack calls into a single data frame
    ans <- createFullDF(dat)
    
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
        ans$namespace <- sapply(ans$func, getNamespace)
    }
    
    return(ans)
}