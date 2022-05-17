#' Point clustering methods for spatial data with plotting functions
#'
#' Calculates cluster membership using DBScan
#'
#' @param x an `sf` point pattern. Should be the point pattern of interest.
#' @param region an `sf` polygon feature that defines the study region.
#' @param dist a distance measure that defines the maximum neighborhood radius within points
#' will be considered for cluster assignment.
#' @param minPts The minimum number of points required within the dist measure for core points.
#' @param verbose Should `dbscan` results be printed? Defaults to TRUE.
#' @param plot Should a plot be generated? Defaults to TRUE.
#' @param return_pts Should the original x value be returned? Defaults to TRUE. The returned
#' x value will contain a new column `K` which contains integer identifiers for DBSCAN clusters,
#' where `K = 0` is a noise point, and `K > 0 ` is a cluster ID.
#'
#'
#' @details The `cluster_points` function applies the Density-based Spatial Clustering
#' of Applications with Noise (DBSCAN) algorithm. DBSCAN is a fast clustering
#' method that distinguishes clusters of points from "noise" points based on a
#' user-defined neighborhood distance and a minimum points threshold. The
#' `cluster_points` function simplifies the fitting and visualization of spatial
#' point features. Users can return the original data with cluster IDs attached for
#' further analysis.
#'
#'#' @examples
#' data("newhaven")
#' data("nh_hom")
#'
#' cluster_out <- cluster_points(x = nh_hom, region = newhaven, dist = 2000, minPts = 5)
#'
#' @references
#'
#' Hahsler M, Piekenbrock M, Doran D (2019). dbscan: Fast Density-Based Clustering with R. Journal of Statistical Software, 91(1), 1-30. doi: 10.18637/jss.v091.i01
#'
#' @export

## CLUSTER POINTS
# main clustering function, returns point shapefile

cluster_points <- function(x,
                           region,
                           dist,
                           minPts,
                           verbose = T,
                           plot = T,
                           return_pts = T){

  # get coordinates as a matrix
  dm <- as.matrix(sf::st_coordinates(x))

  # run DBScan and append cluster ids to data x
  K <- dbscan::dbscan(dm, eps = dist, minPts = minPts)
  x$K <- K$cluster

  # check for no clusters, offer advice
  if (length(unique(K$cluster)) == 1) {
    warning("No clusters found using given parameters\n \tTry adjusting `minPts = ` or `dist = `")
  }

  # plotting functions
  if(plot == T){

    # get cluster info
    nK <- length(unique(K$cluster[K$cluster >0]))
    nN <- length((K$cluster[K$cluster == 0]))

    # use default sf plotting functions
    plot(sf::st_union(sf::st_geometry(region)))
    plot(x["K"],
         pch = c(16,1)[ifelse(x$K > 0, 1,2)],
         cex = c(1,.8)[ifelse(x$K > 0, 1,2)],
         main = "", add = T)
    title(paste0("DBScan Clustering\n",
                 nK, " Clusters; ",
                 nN, " Noise Points") , adj = 0.5, line = 0)
  }

  # cluster information
  if(verbose == T)
    print(K)
  # return data with cluster ids
  if(return_pts == T)
    return(x)
}
