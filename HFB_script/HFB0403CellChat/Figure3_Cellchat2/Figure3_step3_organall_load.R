prepare_data <- function(object, signaling = NULL, slot.name = "netP", group = NULL, x.measure = "outdeg", y.measure = "indeg") {
  if (length(slot(object, slot.name)$centr) == 0) {
    stop("Please run `netAnalysis_computeCentrality` to compute the network centrality scores! ")
  }
  if (sum(c(x.measure, y.measure) %in% names(slot(object, slot.name)$centr[[1]])) != 2) {
    stop(paste0("`x.measure, y.measure` should be one of ", paste(names(slot(object, slot.name)$centr[[1]]), collapse = ", "), "\n", "`outdeg_unweighted` is only supported for version >= 1.1.2"))
  }
  centr <- slot(object, slot.name)$centr
  outgoing <- matrix(0, nrow = nlevels(object@idents), ncol = length(centr))
  incoming <- matrix(0, nrow = nlevels(object@idents), ncol = length(centr))
  dimnames(outgoing) <- list(levels(object@idents), names(centr))
  dimnames(incoming) <- dimnames(outgoing)
  for (i in 1:length(centr)) {
    outgoing[, i] <- centr[[i]][[x.measure]]
    incoming[, i] <- centr[[i]][[y.measure]]
  }
  if (is.null(signaling)) {
    message("Signaling role analysis on the aggregated cell-cell communication network from all signaling pathways")
  } else {
    message("Signaling role analysis on the cell-cell communication network from user's input")
    signaling <- signaling[signaling %in% object@netP$pathways]
    if (length(signaling) == 0) {
      stop("There is no significant communication for the input signaling. All the significant signaling are shown in `object@netP$pathways`")
    }
    outgoing <- outgoing[, signaling, drop = FALSE]
    incoming <- incoming[, signaling, drop = FALSE]
  }
  outgoing.cells <- rowSums(outgoing)
  incoming.cells <- rowSums(incoming)
  num.link <- aggregateNet(object, signaling = signaling, return.object = FALSE, remove.isolate = FALSE)$count
  num.link <- rowSums(num.link) + colSums(num.link) - diag(num.link)
  df <- data.frame(x = outgoing.cells, y = incoming.cells, labels = names(incoming.cells), Count = num.link)
  df$labels <- factor(df$labels, levels = names(incoming.cells))
  if (!is.null(group)) {
    df$Group <- group
  }
  return(df)
}


plot_data <- function(df, color.use = NULL,
                      dot.size = c(2, 6),
                      point.shape = c(21, 22, 24, 23, 25, 8, 3),
                      label.size = 6, dot.alpha = 0.6,
                      xlabel = "Outgoing interaction strength",
                      ylabel = "", title = NULL,
                      font.size = 10,
                      font.size.title = 30,
                      do.label = T,
                      show.legend = T,
                      show.axes = T,
                      weight.MinMax = c(0, 1000)) {
  if (is.null(color.use)) {
    color.use <- scPalette(nlevels(df$labels))
  }
  # if ("Group" %in% colnames(df)) {
    gg <- ggplot(data = df, aes(x, y)) + geom_point(aes(size = Count, colour = labels, fill = labels, shape = Group))
  } 
# else {
#     gg <- ggplot(data = df, aes(x, y)) + geom_point(aes(size = Count, colour = labels, fill = labels))
#   }
#   gg <- gg + CellChat_theme_opts() + theme(text = element_text(size = font.size), legend.key.height = grid::unit(0.15, "in")) + labs(title = title, x = xlabel, y = ylabel) + theme(plot.title = element_text(size = font.size.title, face = "plain")) + theme(axis.line.x = element_line(size = 0.25), axis.line.y = element_line(size = 0.25))
#   gg <- gg + scale_fill_manual(values = ggplot2::alpha(color.use, alpha = dot.alpha), drop = FALSE) + guides(fill = FALSE)
#   gg <- gg + scale_colour_manual(values = color.use, drop = FALSE) + guides(colour = FALSE)
#   if ("Group" %in% colnames(df)) {
#     gg <- gg + scale_shape_manual(values = point.shape[1:length(unique(df$Group))])
#   }
#   if (is.null(weight.MinMax)) {
#     gg <- gg + scale_size_continuous(range = dot.size)
#   } else {
#     gg <- gg + scale_size_continuous(limits = weight.MinMax, range = dot.size)
#   }
#   if (do.label) {
#     gg <- gg + ggrepel::geom_text_repel(mapping = aes(label = labels, colour = labels), size = label.size, show.legend = F, segment.size = 0.2, segment.alpha = 0.5)
#   }
#   if (!show.legend) {
#     gg <- gg + theme(legend.position = "none")
#   }
#   if (!show.axes) {
#     gg <- gg + theme_void()
#   }
#   gg <- gg + ylab("") + theme(
#     axis.title.x = element_text(size = 18),
#     axis.text.x = element_text(size = 18),
#     axis.ticks.y = element_line(size = 2),
#     axis.text.y = element_text(size = 14)
#   ) +
#     scale_y_continuous(breaks = seq(0, 60, by = 10)) +
#     coord_cartesian(ylim = c(0, 60), xlim = c(0, 18)) +
#     scale_x_continuous(breaks = seq(0, 18, by = 3)) +
#     scale_color_manual(values = colors_dict)
# 
#   return(gg)
# }
