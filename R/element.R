#' @title Add a canvas specification to a ggplot object
#' @description A canvas specification essentially comprises a set of parameters
#'   from [ggplot2::ggsave()]. When a plot with this canvas specification is
#'   printed, it is rendered as it would appear if saved to a file with the
#'   specified dimensions.
#'
#'   Additional viewer parameters control how the preview looks inside the
#'   RStudio / Positron / VS Code viewer panel.  All viewer parameters read
#'   their defaults from `getOption("ggview.<param>")`, so you can set them
#'   once in your `.Rprofile` and forget about them.
#'
#' @inheritParams ggplot2::ggsave
#'
#' @param monitor_dpi Integer or `NULL`. Physical pixels-per-inch of your
#'   monitor.  `NULL` (default) uses the CSS standard of 96 px/in, which is
#'   fine for most screens.  Set to your screen's actual PPI for exact
#'   physical-size calibration.  Find your PPI at https://www.sven.de/dpi/
#'   Reads `getOption("ggview.monitor_dpi", NULL)`.
#'
#' @param viewer_scale Numeric.  Fine-tuning multiplier applied after the
#'   DPI calculation.  Open an image of known physical size at 100\%, hold a
#'   real ruler to the screen, then set
#'   `viewer_scale = expected / measured`.  1.0 means no correction.
#'   Reads `getOption("ggview.viewer_scale", 1.0)`.
#'
#' @param show_ruler Logical.  Whether to show the physical ruler overlay
#'   when the viewer first opens.  The ruler can always be toggled with the
#'   **R** key inside the viewer.  Default `FALSE`.
#'   Reads `getOption("ggview.show_ruler", FALSE)`.
#'
#' @param theme Character.  Viewer colour theme.  One of
#'   `"auto"` (follow the IDE theme), `"dark"`, or `"light"`.
#'   Reads `getOption("ggview.theme", "auto")`.
#'
#' @return An object of class `canvas` that can be added to a `ggplot` object
#'   to specify the plot dimensions and viewer behaviour.
#'
#' @examplesIf rstudioapi::isAvailable()
#' library(ggplot2)
#' p <-
#'   ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   ggtitle("My awesome plot")
#'
#' # Basic: physical size determined by dpi + dimensions
#' p + canvas(3, 3)
#' p + canvas(5, 3, dpi = 400)
#'
#' # With viewer options
#' p + canvas(100, 100, units = "mm", dpi = 300,
#'            show_ruler = TRUE, viewer_scale = 1.02)
#'
#' @export
canvas <- function(
  width,
  height,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  scale = 1,
  bg = "white",
  # ── viewer-only parameters ──────────────────────────────
  monitor_dpi = getOption("ggview.monitor_dpi", NULL),
  viewer_scale = getOption("ggview.viewer_scale", 1.0),
  show_ruler = getOption("ggview.show_ruler", FALSE),
  theme = getOption("ggview.theme", "auto")
) {
  units <- match.arg(units)
  theme <- match.arg(theme, c("auto", "dark", "light"))

  structure(
    list(
      # ggsave parameters
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      scale = scale,
      bg = bg,
      # viewer-only parameters
      monitor_dpi = monitor_dpi,
      viewer_scale = viewer_scale,
      show_ruler = isTRUE(show_ruler),
      theme = theme
    ),
    class = c("canvas", "gg")
  )
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.canvas <- function(object, plot, object_name, ...) {
  plot$canvas <- object
  class(plot) <- unique(c("ggview", class(plot)))
  plot
}

#' @export
print.ggview <- function(x, ...) {
  ggview(
    plot = x,
    width = x$canvas$width,
    height = x$canvas$height,
    units = x$canvas$units,
    dpi = x$canvas$dpi,
    scale = x$canvas$scale,
    bg = x$canvas$bg,
    monitor_dpi = x$canvas$monitor_dpi,
    viewer_scale = x$canvas$viewer_scale,
    show_ruler = x$canvas$show_ruler,
    theme = x$canvas$theme
  )
}

#' @export
plot.ggview <- print.ggview

# Strip the "ggview" class before passing to ggplot2 internals so that
# print dispatch does not recurse.
drop_ggview_class <- function(x) {
  class(x) <- setdiff(class(x), "ggview")
  x
}
