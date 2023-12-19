#' layout_cluster_col
#' 
#' @param g tbl_graph object
#' @param cluster cluster column in node table
#' @export
#' @return matrix of layout
layout_cluster_col <- function(g, cluster, ncol=2, per_col=NULL,
    per_layout="circle", x_space=1, y_space=1, widths=NULL) {
    if (!("tbl_graph" %in% class(g))) {
        g <- as_tbl_graph(g)
    }

    cluster_col <- g |> activate(nodes) |> pull(.data[[cluster]])
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
        tmp_g <- g |> activate(nodes) |> filter(.data[[cluster]]==tmp_cl)
        tmp_g_ind <- which(cluster_col==tmp_cl)
        tmp_g <- tmp_g |> mutate(tmp_g_ind=tmp_g_ind)

        ## Not to call layout directory, or use igraph layout function
        lyt <- ggraph(tmp_g, layout=per_layout)$data[,c("x","y")]
        graph_list[[tmp_cl]][["layout"]] <- lyt * widths[longest_col]
        graph_list[[tmp_cl]][["tmp_g"]] <- tmp_g
    }
    for (gn in seq_along(names(graph_list))) {
    	
        if (is.null(graph_list[[names(graph_list)[gn+1]]]$layout$y)) {
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
            graph_list[[names(graph_list)[gn+1]]]$layout$y <- nex_y + (1*widths[longest_col]) + cur_min_y - y_space
        }
    }

    longest_col_min <- min(graph_list[[length(graph_list)]]$layout$y)
    longest_col_max <- max(graph_list[[1]]$layout$y)

    longest_mat <- do.call(rbind, lapply(graph_list, function(x) {
        cbind(x$layout[,c("x","y")],
            x$tmp_g |> activate(nodes) |> pull(.data[["tmp_g_ind"]])) |> data.frame() |>
            `colnames<-`(c("x","y","ind"))
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
            tmp_g <- g |> activate(nodes) |> filter(.data[[cluster]]==tmp_cl)
            tmp_g_ind <- which(cluster_col==tmp_cl)
            tmp_g <- tmp_g |> mutate(tmp_g_ind=tmp_g_ind)

            ## Not to call layout directory, or use igraph layout function
            lyt <- ggraph(tmp_g, layout=per_layout)$data[,c("x","y")]
            tmp_graph_list[[tmp_cl]][["layout"]] <- lyt * widths[i]
            tmp_graph_list[[tmp_cl]][["tmp_g"]] <- tmp_g
        }
        for (gn in seq_along(names(tmp_graph_list))) {
	        if (is.null(tmp_graph_list[[names(tmp_graph_list)[gn+1]]]$layout$y)) {
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
	            tmp_graph_list[[names(tmp_graph_list)[gn+1]]]$layout$y <- nex_y + (1*widths[i]) + cur_min_y - y_space
	        }
        }

        ## Centralize
        max_in_col <- max(tmp_graph_list[[length(tmp_graph_list)]]$layout[,"y"])
        min_in_col <- min(tmp_graph_list[[length(tmp_graph_list)]]$layout[,"y"])

        for (gn in seq_along(names(tmp_graph_list))) {
            cent_y <- tmp_graph_list[[names(tmp_graph_list)[gn]]]$layout[,"y"]
            jitter <- (longest_col_min - min_in_col) / 2
            tmp_graph_list[[names(tmp_graph_list)[gn]]]$layout[,"y"] <- cent_y + jitter
        }

        do.call(rbind, lapply(tmp_graph_list, function(x) {
            cbind(x$layout[,c("x","y")],
                x$tmp_g |> activate(nodes) |> pull(.data[["tmp_g_ind"]])) |> data.frame() |>
                `colnames<-`(c("x","y","ind"))
        }))
    })
    
    names(other_cols_res) <- as.character(other_cols)
    other_cols_res[[as.character(longest_col)]] <- longest_mat
    
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
    res <- do.call(rbind, other_cols_res) |> data.frame() |> dplyr::arrange(ind)
    row.names(res) <- res$ind
    res
}