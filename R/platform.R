
## system2 wrapper
system2 <- 
function(command, args = character(), stdout = "", ...) {
    if (.Platform$OS  == "windows" &&
        .Platform$GUI == "Rgui") {
	## Work around the problem that system2 will never
	## redirect to a file (see gnuwin/sys-win32.c) and
	## try to provide a more consistent behavior than 
	## shell.
	##
	## see base/R/windows/system.R
	stdout <-
	if (is.character(stdout) && nzchar(stdout)) 
	    paste("", stdout, sep = ">")
	else
	    NULL
	args <- c("/c", shQuote(command), args, stdout)
	command <- Sys.getenv("COMSPEC")
	## bail out
	if (!nzchar(command))
	    stop("environment variable 'COMSPEC' not set")
	## discard output to stdout
	stdout <- NULL
    }
    base::system2(
	command = command, 
	args	= args, 
	stdout	= stdout, 
	...
    )
}

