# Libraries ----
library(shiny)
library(tidyverse)
library(igraph)
library(caret)
library(PerformanceAnalytics)
library(networkD3)

# Constants
TRAIN <- FALSE
theme_set(theme_minimal(base_size = 20))

# Create the RDS ----
if (TRAIN) {

    ## Load data ----
    data_music <- read_csv('data/full_music_data.csv')
    data_influence <- read_csv('data/influence_data.csv')
    data_artist <- read_csv('data/data_by_artist.csv')
    
    ### Reformat ----
    data_influence <- data_influence %>% 
        mutate(follower_main_genre = str_remove_all(follower_main_genre, ';'))
    
    artist_genre <- data_influence %>% 
        select(`follower_name`, `follower_main_genre`) %>% 
        unique() %>% 
        rename(`artist_names`=`follower_name`, `main_genre`=`follower_main_genre`)
    
    data_music_genre <- data_music %>%
        transform(artists_id = strsplit(str_trim(str_remove_all(artists_id, '[\\[\\]]'), side='both'), ","),
                  artist_names = strsplit(str_trim(str_remove_all(artist_names, '[\\[\\]]'), side='both'), ",")) %>%
        unnest(artist_names) %>% 
        mutate(artist_names = gsub('^.|.$', '', artist_names)) %>% 
        inner_join(artist_genre, by="artist_names")
    
    ### Problem with unknown artist ----
    unique_a <- unique(data_artist['artist_id'])
    unique_b <- unique(data_influence['influencer_id'])
    unique_c <- unique(data_influence['follower_id'])
    
    table(unlist(unique_b) %in% unlist(unique_a))
    table(unlist(unique_c) %in% unlist(unique_a))
    
    unknown_artist <- unique_c[!(unlist(unique_c) %in% unlist(unique_a)),]
    
    ### Calculate `PCA`----
    columns_to_normalize <- c("danceability", "energy", "valence", "tempo", "loudness", "mode", "key", "acousticness", "instrumentalness", "liveness", "speechiness", "duration_ms", "popularity", "count")
    
    data <- data_artist[, columns_to_normalize]
    tranformation_1 <- preProcess(data, method=c("scale", "pca"), pcaComp=3)
    data_artist$PC1 <- predict(tranformation_1, data)[['PC1']]
    data_artist$PC2 <- predict(tranformation_1, data)[['PC2']]
    data_artist$PC3 <- predict(tranformation_1, data)[['PC3']]
    
    data_influence_transformed <- data_influence %>% 
        inner_join(data_artist[, c('artist_id', 'PC1', 'PC2', 'PC3')], by = c("influencer_id" = "artist_id")) %>% 
        inner_join(data_artist[, c('artist_id', 'PC1', 'PC2', 'PC3')], by = c("follower_id" = "artist_id"), suffix = c("_influencer", "_follower")) %>% 
        rowwise() %>%
        mutate(`weight` = dist(matrix(c(`PC1_influencer`, `PC1_follower`, `PC2_influencer`, `PC2_follower`, `PC3_influencer`, `PC3_follower`), nrow = 2)))
    
    ### Create the graph ----
    data_influence_edges <- data_influence_transformed %>% 
        rename(from=influencer_id, to=follower_id) %>% 
        relocate(from, to, weight)
    
    data_artist_vertices <- data_artist %>% 
        rename(name=artist_name) %>% 
        relocate(artist_id)
    
    g <- graph_from_data_frame(d=data_influence_edges, vertices=data_artist_vertices, directed=TRUE) 
    
    ## Save RDS ----
    saveRDS(object = data_music_genre, file = file.path('shiny', 'storage', 'data_music_genre.RData'))
    saveRDS(object = data_influence, file = file.path('shiny', 'storage', 'data_influence.RData'))
    saveRDS(object = data_artist_vertices, file = file.path('shiny', 'storage', 'data_artist_vertices.RData'))
    saveRDS(object = g, file = file.path('shiny', 'storage', 'g.RData'))

} else {

    ## Load RDS ----
    data_music_genre <- readRDS(file = file.path('storage', 'data_music_genre.RData'))
    data_influence <- readRDS(file = file.path('storage', 'data_influence.RData'))
    data_artist_vertices <- readRDS(file = file.path('storage', 'data_artist_vertices.RData'))
    g <- readRDS(file = file.path('storage', 'g.RData'))

}

# EDA ----

# top_ <- 5
# top_influencers_ <- 5

sorted_music_genre <- data_music_genre %>% 
    group_by(`main_genre`) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    select(`main_genre`) %>% 
    unlist(use.names=FALSE)

plot_duration <- function(top_=5, data_music_genre){
    ## Duration evolution
    plot1 <- data_music_genre %>% 
        mutate(main_genre = factor(main_genre, levels=sorted_music_genre)) %>% 
        filter(main_genre %in% sorted_music_genre[1:top_]) %>% 
        group_by(`year`, `main_genre`) %>% 
        summarise(mean=mean(duration_ms)/1000/60) %>% 
        ggplot(aes(x=`year`, y=`mean`, color=`main_genre`)) +
        geom_line() +
        ylim(c(1.5, 5)) +
        labs(title='Song duration over time per genre', x=NULL, y='Minutes', color='Genre')
    
    plot2 <- data_music_genre %>% 
        mutate(main_genre = factor(main_genre, levels=sorted_music_genre)) %>% 
        filter(main_genre %in% sorted_music_genre[1:top_]) %>% 
        ggplot(aes(x=`year`, y=`duration_ms`/1000/60)) +
        geom_smooth(aes(color=`main_genre`), method="lm", formula=y~poly(x, 3, raw=TRUE)) +
        ylim(c(0, 5)) +
        labs(title='Song duration over time', x=NULL, y='Minutes', color='Genre')
    
    plot3 <- data_music_genre %>% 
        mutate(main_genre = factor(main_genre, levels=sorted_music_genre)) %>% 
        filter(main_genre %in% sorted_music_genre[1:top_]) %>% 
        ggplot(aes(x=`year`, y=`tempo`)) +
        geom_smooth(aes(color=`main_genre`), method="lm", formula=y~poly(x, 3, raw=TRUE)) +
        # ylim(c(0, 5)) +
        labs(title='Song tempo over time', x=NULL, y='Tempo', color='Genre')
    
    return(list(duration_genre=plot1, duration=plot2, tempo=plot3))
}

# Top influencer artists
data_influence_sorted <- data_influence %>% 
    group_by(`influencer_name`) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    select(`influencer_name`) %>% 
    unlist(use.names=FALSE)

plot_influencer <- function(top_=5, top_influencers_=5, data_influence){
    plot1 <- data_influence %>% 
        mutate(influencer_name = factor(influencer_name, levels=data_influence_sorted)) %>% 
        dplyr::filter(influencer_name %in% data_influence_sorted[1:top_]) %>% 
        group_by(`influencer_name`) %>% 
        tally() %>% 
        ggplot(aes(x=`influencer_name`, y=`n`)) +
        geom_col() +
        coord_flip() +
        scale_x_discrete(limits = rev) +
        labs(title='Top influencer artists', x=NULL, y='Number of influenced')
    
    plot2 <- data_influence %>% 
        mutate(follower_main_genre = factor(follower_main_genre, levels=sorted_music_genre)) %>% 
        filter(follower_main_genre %in% sorted_music_genre[1:top_]) %>% 
        group_by(`follower_main_genre`, `influencer_name`) %>%
        tally() %>% 
        arrange(desc(n)) %>% 
        slice(1:top_influencers_) %>% 
        ggplot(aes(x=reorder(`influencer_name`, -n, sum), y=`n`)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_x_discrete(limits = rev) +
        facet_wrap(`follower_main_genre` ~ ., scales="free_y") +
        labs(title='Top influencer artists by genre', x=NULL, y='Number of influenced')
    
    return(list(influencer=plot1, influencer_genre=plot2))
}

plot_inspiration <- function(top_=5, data_influence){
    ## Source of inspiration by genre
    plot1 <- data_influence %>% 
        mutate(influencer_main_genre = factor(influencer_main_genre, levels=sorted_music_genre),
               follower_main_genre = factor(follower_main_genre, levels=sorted_music_genre)) %>% 
        filter(influencer_main_genre %in% sorted_music_genre[1:top_], follower_main_genre %in% sorted_music_genre[1:top_]) %>% 
        group_by(`influencer_main_genre`, `follower_main_genre`) %>%
        tally() %>% 
        ggplot(aes(x=reorder(`influencer_main_genre`, -n, sum), y=reorder(`follower_main_genre`, -n, sum), fill=log(`n`))) +
        geom_tile() +
        scale_x_discrete(guide = guide_axis(angle = 90)) +
        labs(title='Source of inspiration by genre', x='Influencer', y='Follower', fill=NULL) +
        scale_fill_gradient(low="#F2D544", high="#025959")
    
    return(plot1)
}

# Description ----
graph_stats <- function(g) {
    deg <- degree(g, mode="all", loop=FALSE)
    deg_ <- ggplot(as.data.frame(deg), aes(x=deg)) + 
        geom_histogram() +
        labs(title='Total degree distribution', x='Degree', y='Count')
    
    df <- tribble(
        ~Metric,  ~Value,
        'Order (number of vertices)', gorder(g),
        'Size of the graph (number of edges)', gsize(g),
        'Average degree', mean(deg),
        'Network Diameter', diameter(g),
        'Average path length', mean_distance(g)
    )
    
    return(list(
        df=df,
        plot=deg_
))
}

# Centrality Measurements ----
centrality_measurements <- function(g, top_=5) {
    # Closeness
    g_components <- components(g, mode="strong")
    biggest_component <- which.max(g_components$csize)
    g_main <- induced_subgraph(g, which(g_components$membership == biggest_component))
    cl_fix <- closeness(g_main, normalized=TRUE)
    
    # Rest
    cl <- closeness(g, normalized=TRUE)
    dg <- degree(g, mode = "total", loop=FALSE, normalized=TRUE)
    bt <- betweenness(g, normalized=TRUE)
    pr <- page_rank(g)$vector
    
    # Output
    top_values <- function(df) {
        df_ <- tibble(Name=names(df), Value=as.numeric(df))
        df_ %>% 
            arrange(desc(Value)) %>% 
            slice_head(n = top_)
    }    
    
    # Plot
    corr_df <- data.frame(Degree=dg, Betweenness=bt, Closeness=cl, `Page Rank`=pr) 

    return(list(
        dg_top=top_values(dg),
        bt_top=top_values(bt),
        cl_top=top_values(cl),
        cl_top_fix=top_values(cl_fix),
        pr_top=top_values(pr),

        dg=dg,
        bt=bt,
        cl=cl,
        cl_fix=cl_fix,
        pr=pr,

        corr_df=corr_df
    ))
}

# Top`Betweenness` or any other----

top_by_metric <- function(metric, data_artist_vertices, top_n_=10) {
    top_artists <- head(sort(metric, decreasing=TRUE), top_n_)
    
    data_artist_vertices$metric <- metric
    
    out <- data_artist_vertices %>% 
        filter(`name` %in% names(top_artists)) %>% 
        select(`name`, `popularity`, `count`) %>% 
        arrange(factor(`name`, levels = names(top_artists))) %>% 
        rename(`Artist name` = `name`, `Popularity` = `popularity`, `Number of songs` = `count`)
    
    return(out)
}

# as_data_frame(g, what='vertices')

# metric <- cm$dg
# top_by_metric(cm$dg, data_artist_vertices, 10)

# Plot network in 3D ----
plot_network_3D <- function(network, arrow=TRUE) {
    # Store the degree.
    V(network)$degree <- strength(graph = network)
    # Create networkD3 object.
    network.D3 <- igraph_to_networkD3(g = network)
    # Define node size.
    network.D3$nodes <- network.D3$node %>% mutate(Degree = V(network)$degree, Group = 1)
    # Define edges width. 
    network.D3$links$Width <- 1
    
    forceNetwork(
        Links = network.D3$links, 
        Nodes = network.D3$nodes, 
        Source = 'source', 
        Target = 'target',
        NodeID = 'name',
        Group = 'Group',
        opacity = 0.9,
        Value = 'Width',
        Nodesize = 'Degree',
        # We input a JavaScript function.
        linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
        fontSize = 12,
        arrows = arrow,
        zoom = TRUE, 
        opacityNoHover = 1
    )
}

# Community detection ----

community_detection <- function(g) {
    # Communities can only be calculated for undirected graphs
    g_undirected <- as.undirected(g, mode='collapse', edge.attr.comb="max")
    
    cluster_test <- list(
        # cluster_edge_betweenness=cluster_edge_betweenness(g_undirected), 
        # cluster_fast_greedy=cluster_fast_greedy(g_undirected), # Could be used, remove for speed
        # cluster_label_prop=cluster_label_prop(g_undirected),
        # cluster_leading_eigen=cluster_leading_eigen(g_undirected), # Could be used, remove for speed
        cluster_louvain=cluster_louvain(g_undirected)  # Could be used
        # cluster_optimal=cluster_optimal(g_undirected) # Super slow
        # cluster_spinglass=cluster_spinglass(g), # Cannot work with unconnected graph, Invalid value
        # cluster_walktrap=cluster_walktrap(g_undirected), # Could be used, remove for speed
        # walktrap.community=walktrap.community(g_undirected) # Could be used, remove for speed
    )
    
    modularity_test <- lapply(cluster_test, modularity)
    return(list(
        cluster=cluster_test, 
        modularity=modularity_test
    ))
}

community_measurements <- function(g_base, g_communities){
    
    # Metrics
    sizes_ <- sizes(g_communities)
    
    df <- tribble(
        ~Metric,  ~Value,
        'Modularity', modularity(g_communities),
        'Number of communities', length(g_communities),
        'Number of edges inside a community', mean(sizes_)
    )
    
    # Histogram of the `size`
    plot_size <- ggplot(as.data.frame(sizes_), aes(x=sizes_)) + 
        geom_histogram() +
        labs(title='Number of actors inside a community', x='Members', y='Count')
    
    # Color boundary for communities
    # https://stackoverflow.com/questions/37374355/how-can-i-plot-igraph-community-with-defined-colors
    # g_undirected <- as.undirected(g_base, mode='collapse', edge.attr.comb="max")
    layout <-layout.fruchterman.reingold(g_base)
    # plot_communities <- plot(g_communities, g_base, layout=layout, vertex.size=5,  edge.arrow.size=.2)
    
    return(list(
        df=df,
        plot_size=plot_size,
        plot_communities=list(g_communities, g_base, layout)
    ))
}

# g_ego <- g_ego
# artist = input$artist
# year_start <- input$year_start
# year_end <- input$year_end

ego_evolution <- function(g_ego, artist, year_start, year_end){
    
    # Evolution of the degree
    artist_degree_history = data.frame(year=numeric(0), degree=numeric(0))
    
    for (year_ in seq(year_start, year_end, by=5)) {

        g1 <- subgraph.edges(g_ego, E(g_ego)[E(g_ego)$follower_active_start <= year_])
        deg <- degree(g1, mode="all", loop=FALSE)

        artist_degree_history = artist_degree_history %>%
            add_row(year = year_, degree = as.numeric(deg[names(deg) == artist_]))

    }
    
    # Current `end_year` plot
    g1 <- subgraph.edges(g_ego, E(g_ego)[E(g_ego)$follower_active_start <= year_end])
    deg <- degree(g1, mode="all", loop=FALSE)
    
    # Plots
    plot_histogram <- ggplot(as.data.frame(deg), aes(x=deg)) + 
        geom_histogram() +
        labs(title=paste('Total degree distribution before year', year_end), x='Degree', y='Count')
    
    plot_history <- ggplot(artist_degree_history, aes(x=year, y=degree)) +
        geom_line() +
        xlim(c(year_start, year_end))
    
    return(list(
        graph=g1,
        plot_histogram=plot_histogram,
        plot_history=plot_history
    ))
}

# # g_ego <- make_ego_graph(g, order = 1, nodes = 'Ricky Martin', mode = "all", mindist = 0)
# # g_ego <- make_ego_graph(g, order = 1, nodes = 'Madonna', mode = "all", mindist = 0)
# g_ego <- make_ego_graph(g, order = 1, nodes = 'Enrique Iglesias', mode = "all", mindist = 0)
# g_base <- g_ego[[1]]
# g_communities <- community_detection(g_base)$cluster$cluster_louvain
# aa <- renderPlot(community_measurements(g_base, g_communities)$plot_communities)

# More ----
## Ego ----
# g_ego <- make_ego_graph(g, order = 1, nodes = 'Enrique Iglesias', mode = "all", mindist = 0)
# 
# ## Short ----
# short <- all_shortest_paths(g, from='Enrique Iglesias', to = 'Wolfgang Amadeus Mozart', mode = "all")
# g1 <- induced_subgraph(g, short$res[[1]])

list_of_possible_artist <- data_artist_vertices %>% 
    arrange(desc(count)) %>% 
    slice(1:50) %>% 
    select(`name`) %>% 
    unlist(use.names = FALSE)
list_of_possible_artist <- c(list_of_possible_artist, 'Enrique Iglesias', 'Spice Girls')

# UI ----
ui <- navbarPage(title = "Music Network",
                 tabPanel("General description",
                          sidebarLayout(
                              sidebarPanel(
                                  div(img(src="MicrosoftTeams-image.png",weight=1282/4,height=424/4), style="text-align: center;"),
                                  br(),
                                  span("General descriptive information from the database"),
                                  br(),
                                  sliderInput('top', 'Number of top', min = 1, max = 20, value = 5),
                                  sliderInput('top_influencer', 'Number of top influencers', min = 1, max = 20, value = 5),
                                  selectInput('metric', 'Metric', choices=c('closeness', 'degree', 'betweenness', 'page_rank'), selected = 'betweenness')
                              ),
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Statistics", 
                                                  fluidRow(
                                                      column(6,
                                                          span('General metrics'),
                                                          tableOutput('graph_stats__df__g')),
                                                      column(6,
                                                          textOutput('metric__g'),
                                                          tableOutput('top_by_metric__df__g'))
                                                  ),
                                                  br(),
                                                  span('Distribution of degree'),
                                                  plotOutput('graph_stats__plot__g'),
                                                  br(),
                                                  fluidRow(
                                                      column(6,
                                                          span('Degree'),
                                                          tableOutput('centrality_measurements__dg_top__g')),
                                                      column(6,
                                                          span('Betweeness'),
                                                          tableOutput('centrality_measurements__bt_top__g'))
                                                  ),
                                                  fluidRow(
                                                      column(6,
                                                          span('Closeness'),
                                                          tableOutput('centrality_measurements__cl_top__g')),
                                                      column(6,
                                                          span('Page Rank'),
                                                          tableOutput('centrality_measurements__pr_top__g'))
                                                  ),
                                                  br(),
                                                  span('Correlation plot'),
                                                  plotOutput('centrality_measurements__corr_plot__g')
                                              ),
                                              tabPanel("Evolution", 
                                                       span('Evolution of duration'),
                                                       plotOutput('plot_duration__g__duration'),
                                                       br(),
                                                       span('Evolution of tempo'),
                                                       plotOutput('plot_duration__g__tempo')
                                              ),
                                              tabPanel("Influencer", 
                                                       span('Influencer artists'),
                                                       plotOutput('plot_inspiration__g__influencer'),
                                                       br(),
                                                       span('Influencer artists by genre'),
                                                       plotOutput('plot_inspiration__g__influencer_genre')
                                              ),
                                              tabPanel("Inspiration", 
                                                       span('Inspiration by genre'),
                                                       plotOutput('plot_inspiration__g', height = 800)
                                              )
                                  )
                              )
                          )
                 ),
                 tabPanel("Artist ego",
                          sidebarLayout(
                              sidebarPanel(
                                  div(img(src="MicrosoftTeams-image.png",weight=1282/4,height=424/4), style="text-align: center;"),
                                  br(),
                                  span("Select your favorite artist"),
                                  br(),
                                  selectInput('artist', 'Artist', choices=sort(list_of_possible_artist), selected = 'Enrique Iglesias'),
                                  sliderInput('top', 'Number of top', min = 1, max = 20, value = 5)
                              ),
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Visualization", 
                                                  span('Interactive plot'),
                                                  forceNetworkOutput('plot_network_3D__ego', height = 1200)
                                              ),
                                              tabPanel("Statistics", 
                                                  span('General metrics'),
                                                  tableOutput('graph_stats__df__ego'),
                                                  br(),
                                                  span('Distribution of degree'),
                                                  plotOutput('graph_stats__plot__ego'),
                                                  br(),
                                                  fluidRow(
                                                      column(6,
                                                             span('Degree'),
                                                             tableOutput('centrality_measurements__dg_top__ego')),
                                                      column(6,
                                                             span('Betweeness'),
                                                             tableOutput('centrality_measurements__bt_top__ego'))
                                                  ),
                                                  fluidRow(
                                                      column(6,
                                                             span('Closeness'),
                                                             tableOutput('centrality_measurements__cl_top__ego')),
                                                      column(6,
                                                             span('Page Rank'),
                                                             tableOutput('centrality_measurements__pr_top__ego'))
                                                  ),
                                                  br(),
                                                  span('Correlation plot'),
                                                  plotOutput('centrality_measurements__corr_plot__ego')
                                              ),
                                              tabPanel("Community", 
                                                   span('Communities'),
                                                   plotOutput('community_measurements__ego__plot_communities', height = 800),
                                                   br(),
                                                   span('Metrics'),
                                                   tableOutput('community_measurements__ego__df'),
                                                   br(),
                                                   span('Community size'),
                                                   plotOutput('community_measurements__ego__plot_size')
                                              ),
                                              tabPanel("Evolution", 
                                                   sliderInput('year_start', 'Start year', min = 1960, max = 2021, value = 1960, animate =TRUE, step=5),
                                                   sliderInput('year_end', 'End year', min = 1960, max = 2021, value = 2021, animate =TRUE, step=5),
                                                   br(),
                                                   br(),
                                                   span('Evolution of the degree for your favorite artist'),
                                                   plotOutput('ego_evolution__list__plot_g_ego', height = 500),
                                                   # forceNetworkOutput('ego_evolution__list__plot_g_ego', height = 500),
                                                   br(),
                                                   span('General metrics'),
                                                   tableOutput('ego_evolution__list__graph_stats'),
                                                   br(),
                                                   fluidRow(
                                                       column(6,
                                                           span('Histogram of the degree'),
                                                           plotOutput('ego_evolution__list__plot_histogram')),
                                                       column(6,
                                                           span('Evolution of the degree'),
                                                           plotOutput('ego_evolution__list__plot_history'))
                                                   )
                                              )
                                  )
                              )
                          )
                 ),
                 tabPanel("Shortest path",
                          sidebarLayout(
                              sidebarPanel(
                                  div(img(src="MicrosoftTeams-image.png",weight=1282/4,height=424/4), style="text-align: center;"),
                                  br(),
                                  span("Select your favorite artists"),
                                  br(),
                                  selectInput('artist_from', 'Artist from', choices=sort(list_of_possible_artist), selected = 'Enrique Iglesias'),
                                  selectInput('artist_to', 'Artist to', choices=sort(list_of_possible_artist), selected = 'Wolfgang Amadeus Mozart'),
                                  sliderInput('top', 'Number of top', min = 1, max = 20, value = 5)
                              ),
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Visualization", 
                                                  span('Interactive plot'),
                                                  forceNetworkOutput('plot_network_3D__short', height = 1200)
                                              ),
                                              tabPanel("Statistics", 
                                                  span('General metrics'),
                                                  tableOutput('graph_stats__df__short'),
                                                  br(),
                                                  span('Distribution of degree'),
                                                  plotOutput('graph_stats__plot__short'),
                                                  br(),
                                                  fluidRow(
                                                      column(6,
                                                             span('Degree'),
                                                             tableOutput('centrality_measurements__dg_top__short')),
                                                      column(6,
                                                             span('Betweeness'),
                                                             tableOutput('centrality_measurements__bt_top__short'))
                                                  ),
                                                  fluidRow(
                                                      column(6,
                                                             span('Closeness'),
                                                             tableOutput('centrality_measurements__cl_top__short')),
                                                      column(6,
                                                             span('Page Rank'),
                                                             tableOutput('centrality_measurements__pr_top__short'))
                                                  ),
                                                  br(),
                                                  span('Correlation plot'),
                                                  plotOutput('centrality_measurements__corr_plot__short')
                                              )
                                  )
                              )
                          )
                 )
)

# Server ----
server <- function(input, output) {
    
    # General
    graph_stats__g <- graph_stats(g)
    centrality_measurements__g <- centrality_measurements(g, 5)

    # Render
    output$graph_stats__df__g <- renderTable(graph_stats__g$df)
    output$graph_stats__plot__g <- renderPlot(graph_stats__g$plot)
    
    # Top for General 
    observeEvent(c(input$top, input$top_influencer), ignoreNULL = FALSE, ignoreInit = FALSE, {
        centrality_measurements__g <- centrality_measurements(g, input$top)
        
        plot_duration__g <- plot_duration(input$top, data_music_genre)
        plot_influencer__g <- plot_influencer(input$top, input$top_influencer, data_influence)
        plot_inspiration__g <- plot_inspiration(input$top, data_influence)

        output$plot_duration__g__duration_genre <- renderPlot(plot_duration__g$duration_genre)
        output$plot_duration__g__duration <- renderPlot(plot_duration__g$duration)
        output$plot_duration__g__tempo <- renderPlot(plot_duration__g$tempo)
        
        output$plot_inspiration__g__influencer <- renderPlot(plot_influencer__g$influencer)
        output$plot_inspiration__g__influencer_genre <- renderPlot(plot_influencer__g$influencer_genre)
        
        output$plot_inspiration__g <- renderPlot(plot_inspiration__g)
        
        output$centrality_measurements__dg_top__g <- renderTable(centrality_measurements__g$dg_top)
        output$centrality_measurements__bt_top__g <- renderTable(centrality_measurements__g$bt_top)
        output$centrality_measurements__cl_top__g <- renderTable(centrality_measurements__g$cl_top)
        output$centrality_measurements__cl_top_fix__g <- renderTable(centrality_measurements__g$dg_top_fix)
        output$centrality_measurements__pr_top__g <- renderTable(centrality_measurements__g$pr_top)
        output$centrality_measurements__corr_plot__g <- renderPlot(chart.Correlation(centrality_measurements__g$corr_df, histogram=TRUE, pch=19))
    })
    
    # Top for General 
    observeEvent(c(input$top, input$metric), ignoreNULL = FALSE, ignoreInit = FALSE, {
        if (input$metric=='closeness') {
            metric_ <- centrality_measurements__g$cl
        } else if (input$metric=='degree') {
            metric_ <- centrality_measurements__g$dg
        } else if (input$metric=='betweenness') {
            metric_ <- centrality_measurements__g$bt
        } else if (input$metric=='page_rank') {
            metric_ <- centrality_measurements__g$pr
        } else {
            metric_ <- NULL
        }
        top_by_metric__g <- top_by_metric(metric_, data_artist_vertices, input$top)
        output$top_by_metric__df__g <- renderTable(top_by_metric__g)
        
        output$metric__g <- renderText(paste('Top artist considering', input$metric))
    })
    
    # For ego
    observeEvent(c(input$artist, input$top, input$top_influencer), ignoreNULL = FALSE, ignoreInit = FALSE, {
        
        # Subgraph
        g_ego <- make_ego_graph(g, order = 1, nodes = input$artist, mode = "all", mindist = 0)
        g_ego <- g_ego[[1]]
        
        # Community
        community_detection__ego <- community_detection(g_ego)
        community_measurements__ego <- community_measurements(g_ego, community_detection__ego$cluster$cluster_louvain)
        
        # Calculation
        graph_stats__ego <- graph_stats(g_ego)
        centrality_measurements__ego <- centrality_measurements(g_ego, input$top)
        
        # Animated plot
        plot_network_3D__ego <- plot_network_3D(g_ego)
        
        # Render
        output$graph_stats__df__ego <- renderTable(graph_stats__ego$df)
        output$graph_stats__plot__ego <- renderPlot(graph_stats__ego$plot)
        
        output$centrality_measurements__dg_top__ego <- renderTable(centrality_measurements__ego$dg_top)
        output$centrality_measurements__bt_top__ego <- renderTable(centrality_measurements__ego$bt_top)
        output$centrality_measurements__cl_top__ego <- renderTable(centrality_measurements__ego$cl_top)
        output$centrality_measurements__cl_top_fix__ego <- renderTable(centrality_measurements__ego$dg_top_fix)
        output$centrality_measurements__pr_top__ego <- renderTable(centrality_measurements__ego$pr_top)
        output$centrality_measurements__corr_plot__ego <- renderPlot(chart.Correlation(centrality_measurements__ego$corr_df, histogram=TRUE, pch=19))
        
        output$community_measurements__ego__df <- renderTable(community_measurements__ego$df)
        output$community_measurements__ego__plot_size <- renderPlot(community_measurements__ego$plot_size)
        # Need to plot here, if not goes to other `device`
        p <- community_measurements__ego$plot_communities
        output$community_measurements__ego__plot_communities <- renderPlot(plot(p[[1]], p[[2]], layout=p[[3]], vertex.size=5,  edge.arrow.size=.2), height = 800, width = 1200)
        
        output$plot_network_3D__ego <- renderForceNetwork(plot_network_3D__ego)
        
    })
    
    # input <- NULL
    # input$artist <- 'Enrique Iglesias'
    # input$year_start <- 1990
    # input$year_end <- 2020
    
    # For ego evolution
    observeEvent(c(input$artist, input$year_start, input$year_end), ignoreNULL = FALSE, ignoreInit = FALSE, {
        # Subgraph
        g_ego <- make_ego_graph(g, order = 1, nodes = input$artist, mode = "all", mindist = 0)
        g_ego <- g_ego[[1]]
        
        # g_ego_filter <- subgraph.edges(g_ego, eids = E(g_ego)[E(g_ego)$follower_active_start <= year_], delete.vertices = TRUE)
        
        ego_evolution__list <- ego_evolution(g_ego, input$artist, input$year_start, input$year_end)
        
        g_ego_filtered <- ego_evolution__list$graph
        
        ego_evolution__list__graph_stats <- graph_stats(g_ego_filtered)
        evo_evolution__plot_3D <- plot_network_3D(g_ego_filtered)
        
        # Outputs
        output$ego_evolution__list__plot_g_ego <- renderPlot(plot(g_ego_filtered, vertex.size=5,  edge.arrow.size=.2), height = 500, width = 1000)
        # output$ego_evolution__list__plot_g_ego <- renderForceNetwork(evo_evolution__plot_3D)
        output$ego_evolution__list__plot_histogram <- renderPlot(ego_evolution__list$plot_histogram)
        output$ego_evolution__list__plot_history <- renderPlot(ego_evolution__list$plot_history)
        output$ego_evolution__list__graph_stats <- renderTable(ego_evolution__list__graph_stats$df)
        
    })
    
    # For short
    observeEvent(c(input$artist_from, input$top, input$artist_to), ignoreNULL = FALSE, ignoreInit = FALSE, {   

        short <- all_shortest_paths(g, from=input$artist_from, to = input$artist_to, mode = "all")
        g_short <- induced_subgraph(g, short$res[[1]])
        
        # Calculation
        graph_stats__short <- graph_stats(g_short)
        centrality_measurements__short <- centrality_measurements(g_short, input$top)
        
        # Animated plot
        plot_network_3D__short <- plot_network_3D(g_short)
        
        # Render
        output$graph_stats__df__short <- renderTable(graph_stats__short$df)
        output$graph_stats__plot__short <- renderPlot(graph_stats__short$plot)
        
        output$centrality_measurements__dg_top__short <- renderTable(centrality_measurements__short$dg_top)
        output$centrality_measurements__bt_top__short <- renderTable(centrality_measurements__short$bt_top)
        output$centrality_measurements__cl_top__short <- renderTable(centrality_measurements__short$cl_top)
        output$centrality_measurements__cl_top_fix__short <- renderTable(centrality_measurements__short$dg_top_fix)
        output$centrality_measurements__pr_top__short <- renderTable(centrality_measurements__short$pr_top)
        output$centrality_measurements__corr_plot__short <- renderPlot(chart.Correlation(centrality_measurements__short$corr_df, histogram=TRUE, pch=19))
        
        output$plot_network_3D__short <- renderForceNetwork(plot_network_3D__short)
    
    })
}

# Run ----
shinyApp(ui = ui, server = server)
