
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
    tmp <- parseRprof(filename)
    dat <- tmp$dat
    interval <- tmp$interval
    ## Clean up the list and return a data frame
    ## with the relevant information
    ans <- cleanDF(dat, interval = interval, namespace = namespace)
    ## Give it a class of "MyRprof" so we can use S3
    ## methods for the plot command
    class(ans) <- c("MyRprof", "data.frame")
    return(ans)
}