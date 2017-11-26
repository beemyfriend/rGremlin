#' Create a Gremlin object
#' 
#' @param graph graph object
#' @param last default query status of Gremlin
#' @export
makeGremlin <-function(graph, last = 'vertices'){
  list(
    graph = graph,
    vertices = V(graph),
    edges = E(graph),
    operations = list(
      if(last == 'vertices'){
        V(graph)
      } else if (last == 'edges'){
        E(graph)
      }
    ),
    last = last
  )
}

#'What does the last edgelist or nodelist contain?
#'
#'@param gremlin a gremlin object
#'@param key string of attribute to query
#'@param query string that returns a boolean when evaluated
#'@export
has <- function(gremlin, key, query){
  query_obj <- gremlin[[gremlin$last]]
  
  if(gremlin$last == 'vertices'){
    query_start <- get.vertex.attribute(gremlin$graph, key, index = gremlin$vertices)
  } else if(gremlin$last == 'edges'){
    query_start <- get.edge.attribute(gremlin$graph, key, index = gremlin$edges)
  }
  
  full_query <- parse(text = str_c('query_start', query))
  results <- query_obj[eval(full_query)]
  gremlin[[gremlin$last]] <- results
  gremlin$operations[[gremlin$operations %>% length + 1]] <- list(
    query = full_query,
    results = results
  )
  gremlin
}

query_record <- function(gremlin, query, results){
  gremlin$operations[[gremlin$operations %>% length + 1]] <- list(
    query = query,
    results = results
  )
  gremlin
}