#' Draw a M-bias DAG
#'
#' Draw a Directed Acyclic Graph (DAG), either before or after conditioning on the
#' collider M, for selection bias caused by M bias.
#'
#' @param x 'mbias' object to plot.
#' @param dec Number of digits displayed.
#' @param ... Other unused arguments.
#'
#' @return A DAG for selection bias caused by M bias.
#' @rdname draw_mdag
#' @keywords internal
#'
#' @export
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph create_layout ggraph geom_edge_link geom_node_point geom_node_label circle theme_graph
#'
draw_mdag_before <- function(x,
                             dec = 2,
                             ...) {
    obj <- x
    assoc <- name <- NULL

    .mbias_edgesb <- data.frame(from = c("a", "a", "b", "b", "x"),
                                to = c("x", "m", "m", "y", "y"),
                                assoc = obj[[4]])
    .mbias_nodesb <- data.frame(name = c("a", "x", "m", "b", "y"),
                                x = c(0, 0, 1, 2, 2),
                                y = c(2, 0, 1, 2, 0))
    .mbiasb <- igraph::graph_from_data_frame(.mbias_edgesb,
                                             directed = TRUE,
                                             vertices = .mbias_nodesb)
    .layoutb <- ggraph::create_layout(.mbiasb, layout = "manual",
                                      x = .mbias_nodesb[, 2], y = .mbias_nodesb[, 3])
    ggraph::ggraph(.layoutb) +
        geom_edge_link(aes(label= round(assoc, 2)),
                       arrow = arrow(angle = 30,
                                     length = unit(5, 'mm'),
                                     type = "closed"),
                       end_cap = circle(7, 'mm'),
                       lineend = "butt",
                       linejoin = "round",
                       linemitre = 1,
                       label_colour = "black",
                       label_alpha = 1,
                       label_parse = FALSE,
                       check_overlap = FALSE,
                       angle_calc = "along",
                       force_flip = TRUE,
                       label_dodge = unit(3, "mm"),
                       label_push = unit(3, "mm")) +
        geom_node_point(size = 12) +
        geom_node_label(aes(label = name), fill = "black", colour = "white",
                        size = 5, fontface = "bold", label.size = 0) +
        theme_graph()
}

#' @rdname draw_mdag
#'
#' @export
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph create_layout ggraph geom_edge_link geom_node_point geom_node_label circle theme_graph
#' @keywords internal
draw_mdag_after <- function(x,
                            dec = 2,
                            ...) {
    obj <- x
    assoc <- name <- NULL

    .mbias_edges <- data.frame(from = c("z", "z", "x"),
                               to = c("x", "y", "y"),
                               assoc = obj[[2]])
    .mbias_nodes <- data.frame(name = c("x", "z", "y"),
                               x = c(0, 1, 2),
                               y = c(0, 1, 0))
    .mbias <- igraph::graph_from_data_frame(.mbias_edges,
                                            directed = TRUE,
                                            vertices = .mbias_nodes)
    .layout <- ggraph::create_layout(.mbias, layout = "manual",
                                     x = .mbias_nodes[, 2], y = .mbias_nodes[, 3])
    ggraph::ggraph(.layout) +
        geom_edge_link(aes(label= round(assoc, 2)),
                       arrow = arrow(angle = 30,
                                     length = unit(5, 'mm'),
                                     type = "closed"),
                       end_cap = circle(7, 'mm'),
                       lineend = "butt",
                       linejoin = "round",
                       linemitre = 1,
                       label_colour = "black",
                       label_alpha = 1,
                       label_parse = FALSE,
                       check_overlap = FALSE,
                       angle_calc = "along",
                       force_flip = TRUE,
                       label_dodge = unit(3, "mm"),
                       label_push = unit(3, "mm")) +
        geom_node_point(size = 12) +
        geom_node_label(aes(label = name), fill = "black", colour = "white",
                        size = 5, fontface = "bold", label.size = 0) +
        theme_graph()
}
