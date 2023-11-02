# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  R dependencies for this project are managed with `renv`
#' @author Hauke Licht
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# install renv if not available
if (!"renv" %in% row.names(installed.packages()))
  install.packages("renv")

# locate lock file in working directory
lock_file <- "renv.lock"

# if renv lock file available, ... 
if (file.exists(lock_file)) {
  # ...just restore its state
  renv::restore(lockfile = lock_file)
} else {
  # otherwise
  # initialize renv in working directory
  renv::init(bare = TRUE)
  
  # read packages in requirements file
  lines <- readLines(file.path("code", "r_requirements.txt"), warn = FALSE)
  pkgs <- lines[-grep("^#", lines)]
  
  # iterate over packages and install them if needed
  for (pkg in pkgs) {
    pname <- sub("^(.+/)?([a-zA-z.-]+)@.+", "\\2", pkg, perl = TRUE) 
    if (pname %in% row.names(installed.packages()))
      next
    message("attempting install of package ", sQuote(pname))
    renv::install(pkg, prompt = FALSE)
  }
  
  # make snapshot (create 'renv.lock')
  renv::snapshot()
}

