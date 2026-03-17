#' @title ggplot picture previewer (internal renderer)
#' @description Renders a ggplot to a temporary PNG, injects parameters into
#'   the HTML template, and opens the result in the IDE viewer panel.
#'
#'   This function is called automatically by [print.ggview()] when a plot
#'   carries a [canvas()] specification.  You can also call it directly to
#'   preview a plain ggplot without attaching a canvas.
#'
#' @param plot A ggplot object.  Defaults to the last plot.
#' @param device Raster device: `"png"` (default), `"jpeg"`, or `"bmp"`.
#'   SVG is not supported because the viewer displays a raster image.
#' @inheritParams canvas
#' @param width,height,units,dpi,scale,bg  Passed to [ggplot2::ggsave()].
#'   If the plot already has a [canvas()] attached, those values are used;
#'   these arguments override the canvas.
#'
#' @section Options cascade (most-specific wins):
#' ```
#' per-call argument
#'   → canvas() parameter
#'     → getOption("ggview.<param>")
#'       → package default
#' ```
#'
#' Persistent options — place in `~/.Rprofile` (user-wide) or a project
#' `.Rprofile`:
#' ```r
#' options(
#'   ggview.monitor_dpi  = 220,     # your monitor's physical PPI
#'   ggview.viewer_scale = 1.0,     # calibration multiplier
#'   ggview.show_ruler   = FALSE,   # ruler visible on open
#'   ggview.theme        = "auto"   # "auto" | "dark" | "light"
#' )
#' ```
#'
#' @noRd
ggview <- function(
  plot = ggplot2::last_plot(),
  device = c("png", "jpeg", "bmp"),
  scale = 1,
  width,
  height,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  bg = NULL,
  # viewer-only parameters
  monitor_dpi = getOption("ggview.monitor_dpi", NULL),
  viewer_scale = getOption("ggview.viewer_scale", 1.0),
  show_ruler = getOption("ggview.show_ruler", FALSE),
  theme = getOption("ggview.theme", "auto")
) {
  plot <- drop_ggview_class(plot)
  device <- match.arg(device)
  units <- match.arg(units)
  theme <- match.arg(theme, c("auto", "dark", "light"))

  # ── Temp directory ──────────────────────────────────────────────────────────
  path_dir <- file.path(tempdir(), "ggview")
  if (!dir.exists(path_dir)) {
    dir.create(path_dir)
  }
  ggview_cleanup(path_dir)

  random_stem <- tempfile(pattern = "ggview_", tmpdir = path_dir)
  path_img <- paste0(random_stem, ".", device)
  path_html <- paste0(random_stem, ".html")

  # ── Render image ────────────────────────────────────────────────────────────
  ggplot2::ggsave(
    filename = path_img,
    plot = plot,
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    bg = bg
  )

  # ── Resolve is_dark from theme argument ─────────────────────────────────────
  #   "auto"  → ask rstudioapi / Positron / VS Code R extension; default FALSE
  #   "dark"  → always dark
  #   "light" → always light
  is_dark <- switch(
    theme,
    auto = tryCatch(
      isTRUE(rstudioapi::getThemeInfo()$dark),
      error = function(e) FALSE
    ),
    dark = TRUE,
    light = FALSE
  )

  # ── Build substitution list ──────────────────────────────────────────────────
  #   Each key maps to a {{key}} placeholder in ggview.html.
  #   monitor_dpi and viewer_scale override the viewer's built-in defaults.
  #   An empty string for monitor_dpi tells the viewer to use CSS 96 px/in.
  subs <- list(
    file = basename(path_img),
    dpi = as.character(dpi),
    monitor_dpi = if (!is.null(monitor_dpi)) as.character(monitor_dpi) else "",
    scale = as.character(viewer_scale), # viewer calibration, not ggsave scale
    units = units,
    show_ruler = tolower(as.character(isTRUE(show_ruler))),
    is_dark = tolower(as.character(is_dark))
  )

  # ── Inject into HTML template ────────────────────────────────────────────────
  template <- readLines(
    system.file("ggview.html", package = "ggview"),
    warn = FALSE
  )
  html <- template
  for (key in names(subs)) {
    html <- gsub(paste0("{{", key, "}}"), subs[[key]], html, fixed = TRUE)
  }
  writeLines(html, path_html)

  # ── Open in viewer ───────────────────────────────────────────────────────────
  rstudioapi::viewer(path_html)
  invisible(plot)
}

#' @description Removes old plot+HTML pairs from the ggview temp directory,
#'   keeping only the `n` most recently created.
#' @noRd
ggview_cleanup <- function(dir, n = 50) {
  files <- list.files(dir, full.names = TRUE)
  if (length(files) == 0L) {
    return(invisible(NULL))
  }
  info <- file.info(files)
  info <- info[order(info$ctime, decreasing = TRUE), ]
  to_remove <- rownames(info)[seq_along(rownames(info)) > n]
  if (length(to_remove) > 0L) {
    file.remove(to_remove)
  }
  invisible(NULL)
}
