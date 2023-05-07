
library(leaflet)
#helper function for choropleth animation
setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL, label = NULL,
                           options = NULL){

  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip, label = label
               )))

  options <- evalFormula(options, data = data)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))

  layerId <- options[[1]]
  style <- options[-1]
  if("label" %in% colnames(style)){
    labelData = style[,"label", FALSE]
    style = style[,-which(colnames(style)=="label"), FALSE]
    leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label)
  }
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

cumsumNA <- function(x) {
  x <- cumsum(replace(x, is.na(x), 0))
  x <- cumsum(x)
  x[x == 0] <- NA
  x
}

library(sf)
library(ggplot2)
plot_map <- function(data, diff_to = NULL, strokecolor = NA, fillcolor = c('#097FB3', '#A13675'),
                     alpha = 1, graticules = FALSE, zoom_to = NULL, zoom_level = NULL) {
  target_crs <- st_crs(data)

  if (!is.null(zoom_level)) {
    if (is.null(zoom_to)) {
      zoom_to_xy <- st_sfc(st_point(c(0, 0)), crs = target_crs)
    } else {
      zoom_to_xy <- st_transform(st_sfc(st_point(zoom_to), crs = 4326), crs = target_crs)
    }

    C <- 40075016.686   # ~ circumference of Earth in meters
    x_span <- C / 2^zoom_level
    y_span <- C / 2^(zoom_level+1)

    disp_window <- st_sfc(
      st_point(st_coordinates(zoom_to_xy - c(x_span / 2, y_span / 2))),
      st_point(st_coordinates(zoom_to_xy + c(x_span / 2, y_span / 2))),
      crs = target_crs
    )

    xlim <- st_coordinates(disp_window)[,'X']
    ylim <- st_coordinates(disp_window)[,'Y']
  } else {
    xlim <- NULL
    ylim <- NULL
  }

  if (!is.null(diff_to)) {
    shapebase <- diff_to
    shapediff <- st_sym_difference(data, diff_to)
  } else {
    shapebase <- data
    shapediff <- NULL
  }

  p <- ggplot() +
    geom_sf(data = shapebase, color = strokecolor, fill = fillcolor[1], alpha = alpha)

  if (!is.null(shapediff)) {
    p <- p + geom_sf(data = shapediff, color = strokecolor, fill = fillcolor[2], alpha = alpha)
  }

  p +
    coord_sf(xlim = xlim,
             ylim = ylim,
             crs = target_crs,
             datum = ifelse(graticules, target_crs$input, NA)) +
    theme_bw()
}