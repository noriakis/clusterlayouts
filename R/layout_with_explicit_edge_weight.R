#' layout_with_explicit_edge_weight
#' 
#' This is based on the stackoverflow answers
#' https://stackoverflow.com/questions/16390221/how-to-make-grouped-layout-in-igraph
#' 
#' @param g tbl_graph object
#' @param cluster cluster column in node table
#' @param use_layout should have weights argument
#' @param original_weight reset original weights
#' @export
#' @return matrix of layout
layout_with_explicit_edge_weight <- function(g, cluster,
    use_layout=layout_with_fr, within=10, between=1, original_weight=FALSE) {
	
    weight.community <- function(row, membership, within, between){
        if(as.numeric(membership[which(names(membership)==row[1])])==as.numeric(membership[which(names(membership)==row[2])])){
            return(within)
        } else {
            return(between)
        }
    }
    
    all_att <- names(vertex.attributes(g))
    cluster_col <- vertex_attr(g, cluster)
    if ("name" %in% all_att) {
	    names(cluster_col) <- V(g)$name	
    } else {
    	names(cluster_col) <- seq_len(length(V(g)))
    }
    E(g)$weight <- apply(get.edgelist(g), 1, weight.community, cluster_col, within, between)

    do.call(use_layout, list(graph=g, weights=E(g)$weight))
}