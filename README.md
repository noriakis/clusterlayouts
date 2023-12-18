
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clusterlayouts

Graph layouts using cluster information.

``` r
## Make an example graph and assign a random category
library(clusterlayouts)
library(igraph)
test <- igraph::random.graph.game(n=100, p=0.01)
V(test)$category <- as.factor(sample(1:4, 100, replace=TRUE))
lyt <- layout_cluster(test, "category")
ggraph(test, layout="manual", x=lyt$x, y=lyt$y)+
    geom_edge_link0(color="grey80")+
    geom_node_point(aes(color=category))+
    theme_graph()
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="1500" style="display: block; margin: auto;" />

``` r
## Can change per cluster layout
test <- igraph::random.graph.game(n=500, p=0.001)
panel <- c(2,3,5,6,6,3,2,2)
V(test)$category <- as.factor(sample(1:sum(panel), 500, replace=TRUE))
lyt <- layout_cluster(test, "category", per_row=panel, per_layout="drl")
#> Overriding nrow option
ggraph(test, layout="manual", x=lyt$x, y=lyt$y)+
    geom_edge_link0(color="grey80")+
    geom_node_point(aes(color=category))+
    ggfx::with_outer_glow(
        geom_node_point(aes(color=category, filter=category==29)),
        colour="gold", expand=10
    )+
    theme_graph()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="3600" style="display: block; margin: auto;" />