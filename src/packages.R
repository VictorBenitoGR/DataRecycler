# * DataRecycler | GNU General Public License v3.0
# * https://github.com/VictorBenitoGR/DataRecycler

# *** PACKAGES *** ------------------------------------------------------------

# List of packages
packages <- c(
  "dplyr", #        Data manipulation/transformation
  "stringr", #      Strings (text) manipulation
  "lubridate", #    Parse, manipulate, and format dates and times
  "openxlsx" #      Reading/writing/editing excel files
  # ! add "," at the end of the previous package to add a new one
)

# *** FUNCTION | INSTALL AND LOAD PACKAGES *** --------------------------------

# Function to install and load packages
install_and_load <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    install.packages(new_packages, dependencies = TRUE)
  }

  loaded <- sapply(packages, require, character.only = TRUE)
  if (all(loaded)) {
    message("All packages were successfully installed and loaded.")
  } else {
    not_loaded <- packages[!loaded]
    warning(paste(
      "Error: Some packages failed to load -",
      paste(not_loaded, collapse = ", ")
    ))
  }
}


# *** INSTALL AND LOAD PACKAGES *** -------------------------------------------

# Install and load packages
install_and_load(packages)
