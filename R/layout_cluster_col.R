#' layout_cluster_panel_col
#' 
#' @param g tbl_graph object
#' @param cluster cluster column in node table
#' @export
#' @return matrix of layout
layout_cluster_panel_col <- function(g, cluster, ncol=2, per_col=NULL,
    per_layout=in_circle(), x_space=1, y_space=1, widths=NULL) {

    cluster_col <- vertex_attr(g, cluster)
    ncl <- length(unique(cluster_col))
    uniqcol <- levels(cluster_col)

    if (!is.null(per_col)) {
        cat("Overriding ncol option\n")
        if (sum(per_col)!=ncl) {stop("Cluster number mismatch")}
    } else {
        if (ncl %% 2) {
            per_col <- c(rep(as.integer(ncl / ncol), ncol), ncl %% ncol)
        } else {
            per_col <- rep(ncl / ncol, ncol)
        }
    }

    if (is.null(widths)) {
        widths <- rep(1, length(per_col))
    } else {
        if (length(widths)!=length(per_col)) {
            stop("widths number should be equal to per_col")
        }
    }

    ## First perform positioning of the longest col
    longest_col <- which.max(per_col)[1]
    other_cols <- seq_len(length(per_col))
    other_cols <- other_cols[other_cols!=longest_col]
    csum <- cumsum(per_col)

    if (longest_col!=1) {
        to <- csum[longest_col]
        from <- csum[longest_col-1]+1
        longest_list <- uniqcol[from:to]
    } else {
        longest_list <- uniqcol[1:csum[longest_col]]
    }

    graph_list <- list()
    for (tmp_cl in longest_list) {
        tmp_g <- induced.subgraph(g, which(cluster_col==tmp_cl))
        tmp_g_ind <- which(cluster_col==tmp_cl)
        V(tmp_g)$ind <- tmp_g_ind

        lyt <- layout_(tmp_g, per_layout)[,c(1:2)] |>
            data.frame() |>
            `colnames<-`(c("x","y"))

        graph_list[[tmp_cl]][["layout"]] <- lyt #* widths[longest_col]
        graph_list[[tmp_cl]][["tmp_g"]] <- tmp_g
    }
    for (gn in seq_along(names(graph_list))) {
    	
        if (is.null(graph_list[[names(graph_list)[gn+1]]]$layout$y)) {
            cur_y <- graph_list[[names(graph_list)[gn]]]$layout$y
            if (max(cur_y) > 0) {
                cur_y <- cur_y - abs(max(cur_y))
                graph_list[[names(graph_list)[gn]]]$layout$y <- cur_y
            }
            next
        }

        cur_y <- graph_list[[names(graph_list)[gn]]]$layout$y
        if (max(cur_y) > 0) {
            cur_y <- cur_y - abs(max(cur_y))
        }
        cur_max_y <- max(cur_y)
        cur_min_y <- min(cur_y)

        nex_y <- graph_list[[names(graph_list)[gn+1]]]$layout$y
        if (max(nex_y) > 0) {
            nex_y <- nex_y - abs(max(nex_y))
        }
        if (gn!=1) {
            graph_list[[names(graph_list)[gn+1]]]$layout$y <- nex_y + cur_min_y - y_space        
        } else {
            graph_list[[names(graph_list)[gn+1]]]$layout$y <- nex_y + 1 + cur_min_y - y_space
        }
    }

    longest_col_min <- min(graph_list[[length(graph_list)]]$layout$y)
    longest_col_max <- max(graph_list[[1]]$layout$y)


    longest_mat <- do.call(rbind, lapply(graph_list, function(x) {
        tmp <- cbind(x$layout[,c("x","y")],
            V(x$tmp_g)$ind) |> data.frame() |>
            `colnames<-`(c("x","y","ind"))
        tmp$x <- tmp$x * widths[longest_col]
        tmp$y <- tmp$y * widths[longest_col]
        return(tmp)

    }))


    ## Other shorter cols
    other_cols_res <- lapply(other_cols, function(i) {
        if (i!=1) {
            to <- csum[i]
            from <- csum[i-1]+1
            cl_list <- uniqcol[from:to]
        } else {
            cl_list <- uniqcol[1:csum[i]]
        }
        tmp_graph_list <- list()
        for (tmp_cl in cl_list) {
	        tmp_g <- induced.subgraph(g, which(cluster_col==tmp_cl))
	        tmp_g_ind <- which(cluster_col==tmp_cl)
	        V(tmp_g)$ind <- tmp_g_ind

	        lyt <- layout_(tmp_g, per_layout)[,c(1:2)] |>
    	        data.frame() |>
        	    `colnames<-`(c("x","y"))
            
            tmp_graph_list[[tmp_cl]][["layout"]] <- lyt #* widths[i]
            tmp_graph_list[[tmp_cl]][["tmp_g"]] <- tmp_g
        }
        for (gn in seq_along(names(tmp_graph_list))) {
	        if (is.null(tmp_graph_list[[names(tmp_graph_list)[gn+1]]]$layout$y)) {
                cur_y <- graph_list[[names(graph_list)[gn]]]$layout$y
                if (max(cur_y) > 0) {
                    cur_y <- cur_y - abs(max(cur_y))
                    graph_list[[names(graph_list)[gn]]]$layout$y <- cur_y
                }
	            next
	        }

	        cur_y <- tmp_graph_list[[names(tmp_graph_list)[gn]]]$layout$y
            if (max(cur_y) > 0) {
                cur_y <- cur_y - abs(max(cur_y))
            }
	        cur_max_y <- max(cur_y)
	        cur_min_y <- min(cur_y)

	        nex_y <- tmp_graph_list[[names(tmp_graph_list)[gn+1]]]$layout$y
            if (max(nex_y) > 0) {
                nex_y <- nex_y - abs(max(nex_y))
            }
	        if (gn!=1) {
	            tmp_graph_list[[names(tmp_graph_list)[gn+1]]]$layout$y <- nex_y + cur_min_y - y_space        
	        } else {
	            tmp_graph_list[[names(tmp_graph_list)[gn+1]]]$layout$y <- nex_y + 1 + cur_min_y - y_space
	        }
        }

        ## Centralize
        if (sum(widths==1)==length(per_col)) {
            max_in_col <- max(tmp_graph_list[[length(tmp_graph_list)]]$layout[,"y"])
            min_in_col <- min(tmp_graph_list[[length(tmp_graph_list)]]$layout[,"y"])

            for (gn in seq_along(names(tmp_graph_list))) {
                cent_y <- tmp_graph_list[[names(tmp_graph_list)[gn]]]$layout[,"y"]
                jitter <- (longest_col_min - min_in_col) / 2
                tmp_graph_list[[names(tmp_graph_list)[gn]]]$layout[,"y"] <- cent_y + jitter
            }
        }
        do.call(rbind, lapply(tmp_graph_list, function(x) {
            tmp <- cbind(x$layout[,c("x","y")],
                V(x$tmp_g)$ind) |> data.frame() |>
                `colnames<-`(c("x","y","ind"))
            tmp$x <- tmp$x * widths[i]
            tmp$y <- tmp$y * widths[i]
            return(tmp)
        }))

    })
    
    names(other_cols_res) <- as.character(other_cols)
    other_cols_res[[as.character(longest_col)]] <- longest_mat



    if (sum(widths==1)!=length(per_col)) {
        tmpn <- names(other_cols_res)
        other_cols_res <- lapply(other_cols_res, function(tmp) {
            if (max(tmp$y)>0) {
                tmp$y <- tmp$y - max(tmp$y)
            } else {
                tmp$y <- tmp$y + abs(max(tmp$y))
            }
            tmp
        })
        names(other_cols_res) <- tmpn
        cur_min <- 0
        for (i in names(other_cols_res)) {
            if (min(other_cols_res[[i]]$y)<cur_min) {
                cur_min <- i
            }
        }
        longest_col_min <- other_cols_res[[as.character(cur_min)]]$y |> min()
        for (i in names(other_cols_res)) {
            min_in_col <- min(other_cols_res[[as.character(i)]]$y)
            jitter <- (longest_col_min - (min_in_col)) / 2
            other_cols_res[[as.character(i)]]$y <- other_cols_res[[as.character(i)]]$y + jitter
        }
    }

    ## Reposition X
    for (i in seq_len(length(per_col))) {
        if (is.null(other_cols_res[[as.character(i+1)]])) {
            next
        }
        cur_lyt_x <- other_cols_res[[as.character(i)]]$x
        cur_maximum_x <- max(cur_lyt_x)
        cur_minimum_x <- min(cur_lyt_x)
        if (cur_minimum_x < 0) {
            cur_lyt_x <- cur_lyt_x + abs(cur_minimum_x)
            cur_maximum_x <- max(cur_lyt_x)
            other_cols_res[[as.character(i)]]$x <- cur_lyt_x
        }
        next_lyt_x <- other_cols_res[[as.character(i+1)]]$x
        next_maximum_x <- max(next_lyt_x)
        next_minimum_x <- min(next_lyt_x)
        if (next_minimum_x < 0) {
            next_lyt_x <- next_lyt_x + abs(next_minimum_x)
            other_cols_res[[as.character(i+1)]]$x <- next_lyt_x
        }
        other_cols_res[[as.character(i+1)]]$x <- next_lyt_x + cur_maximum_x + x_space
    }

    ## Finalize
    res <- do.call(rbind, other_cols_res) |> data.frame()
    res <- res[order(res$ind),]
    row.names(res) <- res$ind
    res
}