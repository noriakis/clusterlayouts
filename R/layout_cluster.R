#' layout_cluster
#' 
#' @param g tbl_graph object
#' @param cluster cluster column in node table
#' @export
#' @return matrix of layout
layout_cluster <- function(g, cluster, nrow=2, per_row=NULL,
    per_layout="circle", x_space=1, y_space=1, heights=NULL) {
    # if ((is.null(nrow)&(is.null(ncol)))) {stop("Please specify either of nrow or ncol")}
    # if ((!is.null(nrow)&(!is.null(ncol)))) {stop("Please specify either of nrow or ncol")}    

    if (!("tbl_graph" %in% class(g))) {
        g <- as_tbl_graph(g)
    }
    cluster_col <- g |> activate(nodes) |> pull(.data[[cluster]])
    ncl <- length(unique(cluster_col))
    uniqcol <- levels(cluster_col)

    if (!is.null(per_row)) {
        cat("Overriding nrow option\n")
        if (sum(per_row)!=ncl) {stop("Cluster number mismatch")}
    } else {
        if (ncl %% 2) {
            per_row <- c(rep(as.integer(ncl / nrow), nrow), ncl %% nrow)
        } else {
            per_row <- rep(ncl / nrow, nrow)
        }
    }

    if (is.null(heights)) {
        heights <- rep(1, length(per_row))
    } else {
        if (length(heights)!=length(per_row)) {
            stop("heights number should be equal to per_row")
        }
    }

    ## First perform positioning of the longest row
    longest_row <- which.max(per_row)[1]
    other_rows <- seq_len(length(per_row))
    other_rows <- other_rows[other_rows!=longest_row]
    csum <- cumsum(per_row)

    if (longest_row!=1) {
        to <- csum[longest_row]
        from <- csum[longest_row-1]+1
        longest_list <- uniqcol[from:to]
    } else {
        longest_list <- uniqcol[1:csum[longest_row]]       
    }

    graph_list <- list()
    for (tmp_cl in longest_list) {
        tmp_g <- g |> activate(nodes) |> filter(.data[[cluster]]==tmp_cl)
        tmp_g_ind <- which(cluster_col==tmp_cl)
        tmp_g <- tmp_g |> mutate(tmp_g_ind=tmp_g_ind)

        ## Not to call layout directory, or use igraph layout function
        lyt <- ggraph(tmp_g, layout=per_layout)$data[,c("x","y")]
        graph_list[[tmp_cl]][["layout"]] <- lyt * heights[longest_row]
        graph_list[[tmp_cl]][["tmp_g"]] <- tmp_g
    }
    for (gn in seq_along(names(graph_list))) {
        if (is.null(graph_list[[names(graph_list)[gn+1]]])) {
            next
        }
        cur_lyt_x <- graph_list[[names(graph_list)[gn]]]$layout[,"x"]
        cur_maximum_x <- max(cur_lyt_x)
        cur_minimum_x <- min(cur_lyt_x)
        if (cur_minimum_x < 0) {
            cur_lyt_x <- cur_lyt_x + abs(cur_minimum_x)
            cur_maximum_x <- max(cur_lyt_x)
            graph_list[[names(graph_list)[gn]]]$layout[,"x"] <- cur_lyt_x
        }
        next_lyt_x <- graph_list[[names(graph_list)[gn+1]]]$layout[,"x"]
        next_maximum_x <- max(next_lyt_x)
        next_minimum_x <- min(next_lyt_x)
        if (next_minimum_x < 0) {
            next_lyt_x <- next_lyt_x + abs(next_minimum_x)
            graph_list[[names(graph_list)[gn+1]]]$layout[,"x"] <- next_lyt_x
        }
        graph_list[[names(graph_list)[gn+1]]]$layout[,"x"] <- next_lyt_x + cur_maximum_x + x_space
    }

    longest_row_min <- min(graph_list[[1]]$layout$x)
    longest_row_max <- max(graph_list[[length(graph_list)]]$layout$x)

    longest_mat <- do.call(rbind, lapply(graph_list, function(x) {
        cbind(x$layout[,c("x","y")],
            x$tmp_g |> activate(nodes) |> pull(.data[["tmp_g_ind"]])) |> data.frame() |>
            `colnames<-`(c("x","y","ind"))
    }))

    ## Other shorter rows
    other_rows_res <- lapply(other_rows, function(i) {
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
            tmp_graph_list[[tmp_cl]][["layout"]] <- lyt * heights[i]
            tmp_graph_list[[tmp_cl]][["tmp_g"]] <- tmp_g
        }
        for (gn in seq_along(names(tmp_graph_list))) {
            if (is.null(tmp_graph_list[[names(tmp_graph_list)[gn+1]]])) {
                next
            }
            cur_lyt_x <- tmp_graph_list[[names(tmp_graph_list)[gn]]]$layout[,"x"]
            cur_maximum_x <- max(cur_lyt_x)
            cur_minimum_x <- min(cur_lyt_x)
            if (cur_minimum_x < 0) {
                cur_lyt_x <- cur_lyt_x + abs(cur_minimum_x)
                cur_maximum_x <- max(cur_lyt_x)
                tmp_graph_list[[names(tmp_graph_list)[gn]]]$layout[,"x"] <- cur_lyt_x
            }
            next_lyt_x <- tmp_graph_list[[names(tmp_graph_list)[gn+1]]]$layout[,"x"]
            next_maximum_x <- max(next_lyt_x)
            next_minimum_x <- min(next_lyt_x)
            if (next_minimum_x < 0) {
                next_lyt_x <- next_lyt_x + abs(next_minimum_x)
                tmp_graph_list[[names(tmp_graph_list)[gn+1]]]$layout[,"x"] <- next_lyt_x
            }
            tmp_graph_list[[names(tmp_graph_list)[gn+1]]]$layout[,"x"] <- next_lyt_x + cur_maximum_x + x_space
        }

        ## Centralize
        max_in_row <- max(tmp_graph_list[[length(tmp_graph_list)]]$layout[,"x"])
        for (gn in seq_along(names(tmp_graph_list))) {
            cent_x <- tmp_graph_list[[names(tmp_graph_list)[gn]]]$layout[,"x"]
            jitter <- (longest_row_max - (max_in_row)) / 2
            tmp_graph_list[[names(tmp_graph_list)[gn]]]$layout[,"x"] <- cent_x + jitter
        }

        do.call(rbind, lapply(tmp_graph_list, function(x) {
            cbind(x$layout[,c("x","y")],
                x$tmp_g |> activate(nodes) |> pull(.data[["tmp_g_ind"]])) |> data.frame() |>
                `colnames<-`(c("x","y","ind"))
        }))
    })
    
    names(other_rows_res) <- as.character(other_rows)
    other_rows_res[[as.character(longest_row)]] <- longest_mat
    
    ## Reposition y
    for (i in seq_len(length(per_row))) {
        if (is.null(other_rows_res[[as.character(i+1)]])) {
            next
        }
        cur_y <- other_rows_res[[as.character(i)]]$y
        if (max(cur_y) > 0) {
            cur_y <- cur_y - abs(max(cur_y))
        }
        cur_max_y <- max(cur_y)
        cur_min_y <- min(cur_y)

        nex_y <- other_rows_res[[as.character(i+1)]]$y
        if (max(nex_y) > 0) {
            nex_y <- nex_y - abs(max(nex_y))
        }
        if (i!=1) {
            other_rows_res[[as.character(i+1)]]$y <- nex_y + cur_min_y - y_space        
        } else {
            other_rows_res[[as.character(i+1)]]$y <- nex_y + 1 + cur_min_y - y_space
        }
    }

    ## Finalize
    res <- do.call(rbind, other_rows_res) |> data.frame() |> dplyr::arrange(ind)
    row.names(res) <- res$ind
    res
}