# Project:   mi-gspcr-evs
# Objective: Install packages in the required version
# Author:    Edoardo Costantini
# Created:   2023-07-12
# Modified:  2024-02-02
# Notes: 

# Install local version of packages --------------------------------------------

# Install gspcr
install.packages(
    "../input/gspcr_0.9.2.93.tar.gz",
    repos = NULL,
    type = "source"
)

# Install experimental mice
install.packages(
    "../input/mice_3.16.0.0009.tar.gz",
    repos = NULL,
    type = "source"
)

# Check installed packages -----------------------------------------------------

# Define a function to check package versions
extract_packages <- function(x) {
    # x = utils::sessionInfo() # any session info used by a project
    # List of other packages
    other_packages <- sapply(
        x$otherPkgs,
        function(i) {
            i$Version
        }
    )

    # Create a data.frame containing the information we need
    packages <- data.frame(
        package = names(other_packages),
        version = other_packages,
        row.names = 1:length(other_packages)
    )

    # Output
    return(packages)
}

# Create an empty data.frame of packages to be updated/downgraded
update_list <- data.frame(
    package = character(),
    local = character(),
    project = character()
)

# Load session info from project
R_session_project <- readRDS("../input/R-session-20230712-144051.rds")

# Store/load information about the project package versions
project_packages <- extract_packages(R_session_project)

# Store information about the current R session packages versions
session_packages <- data.frame(
    package = rownames(installed.packages()),
    version = installed.packages()[, "Version"]
)

# Check versions are right
for (i in 1:nrow(project_packages)) {
    index <- project_packages$package[i] == session_packages$package

    if(any(index)){
        update_list[i, ] <- c(
            project_packages$package[i],
            session_packages$version[index],
            project_packages$version[i]
        )
    } else {
        update_list[i, ] <- c(
            project_packages$package[i],
            "-",
            project_packages$version[i]
        )
    }

}

# Define whether the package should be reinstalled
update_list$install <- update_list$local != update_list$project

# Install all packages in right version
for(i in 1:nrow(update_list)){
    if(update_list[i, 'install'] == TRUE){
        remotes::install_version(
            package = update_list[i, 'package'],
            version = update_list[i, 'project']
        )
    }
}
