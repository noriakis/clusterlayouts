#' layout_cluster_wise
#' 
#' split to subgraph, use merge_coords to merge the layouts
#' based on dla algorithm
#' @export
#' @return matrix of layout
#' 
layout_cluster_wise <- function(graph, cluster_col, per_layout=layout_in_circle) {
	V(graph)$id <- seq(vcount(graph))
    
    ## Factor level?
    all_clusters <- unique(vertex_attr(graph, cluster_col))

    clusters <- lapply(all_clusters, function(cl) {
      subg <- induced_subgraph(graph, which(vertex_attr(graph, cluster_col)==cl))
      subg
    })
    coords <- lapply(clusters, function(clus) {
      do.call(per_layout, list(graph = clus))
    })
    all_coords <- merge_coords(
      clusters,
      coords
    )
    all_coords[unlist(sapply(clusters, vertex_attr, "id")), ] <- all_coords[]
    return(all_coords)
}