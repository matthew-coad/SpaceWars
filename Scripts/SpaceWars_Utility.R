
# Development only utility functions that start and stop a local cluster to
# Speed up training time
if (!exists("spacewars_cluster")) {
    spacewars_cluster <- NULL
}

start_cluster <- function() {
    if (is.null(spacewars_cluster)) {
        spacewars_cluster <<- parallel::makeCluster(6)
        doParallel::registerDoParallel(spacewars_cluster)
    } else {
        message("Cluster is already running")
    }
}

stop_cluster <- function() {
    if (!is.null(spacewars_cluster)) {
        parallel::stopCluster(spacewars_cluster)
        spacewars_cluster <<- NULL
    }
}
