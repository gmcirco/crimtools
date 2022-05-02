#' Kernel density estimation with plot functions
#'
#' Computes a kernel density estimate for an `sf` point pattern.
#'
#' @param x `sf` point pattern.
#' @param region `sf` polygon that defines the study region.
#' @param kernel Type of kernel to be used for the KDE estimation. Default is
#' 'gaussian' but can also use 'epanechnikov', 'quartic', or 'disc'.
#' @param bdw Bandwidth for kernel smoothing. Should be in projected values (meters
#' or feet). Can also provide any function that returns a bandwidth estimate. Default
#' behavior is to use `spatstat.core::bw.ppl`, but any of the base functions from the
#' spatstat family should work.
#' @param npixel Dimensions of grid points to be used.
#' @param plot Should a quick plot be generated?
#' @param return_grid Should the raster grid be returned? If TRUE, returns a
#' SpatialPixelsDataFrame, otherwise returns a dataframe with
#' columns for the density value, X, and Y. Defaults to FALSE.
#'
#' @details The kernel_density function uses functionality in the `spatstat` package
#' to compute kernel density estimates for `sf` objects. The function can be used
#' to return a dataframe with density estimates for grid cells. This can further
#' be used for visualization purposes in `ggplot2`
#'
#' @examples
#'
#' data("newhaven")
#' data("nh_hom")
#'
#' # kernel density estimation with default bandwidth
#' kde_out <- kernel_density(x = nh_hom, region = newhaven)
#'
#' head(kde_out)
#'
#' # Plotting kde in ggplot
#' library(ggplot2)
#'
#' ggplot(kde_out) +
#'  geom_tile(aes(x = X, y = Y, fill = density)) +
#'  coord_equal() +
#'  scale_fill_viridis_c() +
#'  theme_void()
#'
#' @export

# KERNEL DENSITY
# main estimation function
# returns a dataframe compatible with ggplot
kernel_density <- function(x,
                           region,
                           bdw = spatstat.core::bw.ppl,
                           kernel = 'gaussian',
                           npixel = 100,
                           plot = T,
                           return_grid = F){

  # (1) do some checking
  ## check for sf data
  stopifnot("Object x is not of type 'sf'" = is(x, "sf"))
  stopifnot("Object region is not of type 'sf'" = is(region, "sf"))

  # (2) convert values to ppp
  ## first set global options
  ## grid cell size for kernel density est.
  spatstat.geom::spatstat.options(npixel = npixel)

  ## get boundaries of region
  xowin <- spatstat.geom::as.owin(region)

  ## get spatial coordinates of points
  xpp <- sf::st_coordinates(x)[,1:2]

  ## convert to ppp
  xpp <- suppressWarnings( spatstat.geom::ppp(xpp[,1],xpp[,2],window = xowin) )

  # (3) create a density estimate
  ## then convert to a dataframe that can be used w/ ggplot
  ## if 'auto' = TRUE, use bw.ppl as default
  # can also pass in any function that returns a bandwidth estimate
  if(is.function(bdw))
    cat("Calculating bandwith...\n")
    bdw <- .bdwfun(xpp, bdw)
    cat(paste0("Bandwidth: ", round(bdw, 1)))

  ## fit density function using specified options
  xdens <- spatstat.core::density.ppp(xpp, kernel = kernel, sigma = bdw)

  ## convert kernel density estimate to a raster file
  ## and add back in the correct crs
  r <- raster::raster(xdens)
  raster::crs(r) <- suppressWarnings(sp::proj4string(as(x, "Spatial")))

  ## finally, convert to a SpatialPixelsDataFrame
  ## and create a separate sf file
  ## user can specify what they want as output
  r <- as(r, "SpatialPixelsDataFrame")
  r.sf <- sf::st_as_sf(r)
  kde_out <- cbind.data.frame(density = r.sf$layer, sf::st_coordinates(r.sf))

  # (4) output stuff
  ## check if user wants a quick plot
  ## add title with kernel used and bandwidth
  if(plot == T)
    plot(xdens, main = paste0(kernel," Kernel\nBandwidth: ", round(bdw,1) ))

  ## check for user-specified return options
  if(return_grid == T){
    return(r)
  } else
  return(kde_out)
}

# .BDWFUN
# take any function and return a bandwidth estimate
# x should be a .ppp object with window provided
# f is any function name
.bdwfun <- function(x, f){
  return(f(x))
}
