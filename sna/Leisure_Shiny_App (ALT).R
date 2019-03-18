############################################# Leisure App ################################################## 
############################################################################################################
############################################################################################################ 
############################################################################################################ 

#Load for MBD Cleaning File
load("leisure_edges.Rda")
load("leisure_key.Rda")
graph <- graph.data.frame(leisure_edges, vertices = leisure_key, directed=F)
nodes <- data.frame(id = V(graph)$name, Name = V(graph)$Full.name, Region = V(graph)$Region,
                    Country = V(graph)$Country.of.Birth, Program = V(graph)$Academic.Program)
#Set color-encoding to region
nodes$group <- nodes$Region
#Calculates node degree for size encoding
degree_value <- degree(graph, mode="total")
nodes$degree <- degree_value[match(nodes$id, names(degree_value))]
nodes$value <- nodes$degree
get_similar_intrests <- function(choice, row1, row2) {
  # c(name1, name2, similarity)
  # "Overall", "Region", "Country", "Program", "Music_Genre", "Artist", "Sport_Genre", "Athlete", "Movie_Genre", "Movie"
  strings_to_return <- c()
  if((choice == "Region") | (choice == "Overall")) {
    if((row1$Region == row2$Region) & row1$Region != "") {
      string <- paste("Same region: ", as.character(row1$Region),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
  }
  if((choice == "Country") | (choice == "Overall")) {
    if((row1$Country.of.Birth == row2$Country.of.Birth) & row1$Country.of.Birth != "") {
      string <- paste("Same country: ", as.character(row1$Country.of.Birth),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
  }
  if((choice == "Program") | (choice == "Overall")) {
    if((row1$Academic.Program == row2$Academic.Program) & row1$Academic.Program != "") {
      string <- paste("Same program: ", as.character(row1$Academic.Program),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
  }
  if((choice == "Music_Genre") | (choice == "Overall")) {
    if((row1$Music.Genre0 != "") & ((row1$Music.Genre0 == row2$Music.Genre0) | (row1$Music.Genre0 == row2$Music.Genre1) | (row1$Music.Genre0 == row2$Music.Genre2))) {
      string <- paste("Same favourite music genre: ", as.character(row1$Music.Genre0),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
    if((row1$Music.Genre1 != "") & ((row1$Music.Genre1 == row2$Music.Genre0) | (row1$Music.Genre1 == row2$Music.Genre1) | (row1$Music.Genre1 == row2$Music.Genre2))) {
      string <- paste("Same favourite music genre: ", as.character(row1$Music.Genre1),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
    if((row1$Music.Genre2 != "") & ((row1$Music.Genre2 == row2$Music.Genre0) | (row1$Music.Genre2 == row2$Music.Genre1) | (row1$Music.Genre2 == row2$Music.Genre2))) {
      string <- paste("Same favourite music genre: ", as.character(row1$Music.Genre2),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
  }
  if((choice == "Artist") | (choice == "Overall")) {
    if((row1$Artist0 != "") & ((row1$Artist0 == row2$Artist0) | (row1$Artist0 == row2$Artist1) | (row1$Artist0 == row2$Artist2))) {
      string <- paste("Same favourite artist: ", as.character(row1$Artist0),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
    if((row1$Artist1 != "") & ((row1$Artist1 == row2$Artist0) | (row1$Artist1 == row2$Artist1) | (row1$Artist1 == row2$Artist2))) {
      string <- paste("Same favourite artist: ", as.character(row1$Artist1),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
    if((row1$Artist2 != "") & ((row1$Artist2 == row2$Artist0) | (row1$Artist2 == row2$Artist1) | (row1$Artist2 == row2$Artist2))) {
      string <- paste("Same favourite artist: ", as.character(row1$Artist2),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
  }
  if((choice == "Sport_Genre") | (choice == "Overall")) {
    if((row1$Sport0 != "") & ((row1$Sport0 == row2$Sport0) | (row1$Sport0 == row2$Sport1) | (row1$Sport0 == row2$Sport2))) {
      string <- paste("Same favourite sport: ", as.character(row1$Sport0),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
    if((row1$Sport1 != "") & ((row1$Sport1 == row2$Sport0) | (row1$Sport1 == row2$Sport1) | (row1$Sport1 == row2$Sport2))) {
      string <- paste("Same favourite sport: ", as.character(row1$Sport1),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
    if((row1$Sport2 != "") & ((row1$Sport2 == row2$Sport0) | (row1$Sport2 == row2$Sport1) | (row1$Sport2 == row2$Sport2))) {
      string <- paste("Same favourite sport: ", as.character(row1$Sport2),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
  }
  if((choice == "Athlete") | (choice == "Overall")) {
    if((row1$Athlete0 != "") & ((row1$Athlete0 == row2$Athlete0) | (row1$Athlete0 == row2$Athlete1) | (row1$Athlete0 == row2$Athlete2))) {
      string <- paste("Same favourite athlete: ", as.character(row1$Athlete0),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
    if((row1$Athlete1 != "") & ((row1$Athlete1 == row2$Athlete0) | (row1$Athlete1 == row2$Athlete1) | (row1$Athlete1 == row2$Athlete2))) {
      string <- paste("Same favourite athlete: ", as.character(row1$Athlete1),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
    if((row1$Athlete2 != "") & ((row1$Athlete2 == row2$Athlete0) | (row1$Athlete2 == row2$Athlete1) | (row1$Athlete2 == row2$Athlete2))) {
      string <- paste("Same favourite athlete: ", as.character(row1$Athlete2),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
  }
  if((choice == "Movie_Genre") | (choice == "Overall")) {
    if((row1$Movie.Genre0 != "") & ((row1$Movie.Genre0 == row2$Movie.Genre0) | (row1$Movie.Genre0 == row2$Movie.Genre1) | (row1$Movie.Genre0 == row2$Movie.Genre2))) {
      string <- paste("Same favourite movie genre: ", as.character(row1$Movie.Genre0),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
    if((row1$Movie.Genre1 != "") & ((row1$Movie.Genre1 == row2$Movie.Genre0) | (row1$Movie.Genre1 == row2$Movie.Genre1) | (row1$Movie.Genre1 == row2$Movie.Genre2))) {
      string <- paste("Same favourite movie genre: ", as.character(row1$Movie.Genre1),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
    if((row1$Movie.Genre2 != "") & ((row1$Movie.Genre2 == row2$Movie.Genre0) | (row1$Movie.Genre2 == row2$Movie.Genre1) | (row1$Movie.Genre2 == row2$Movie.Genre2))) {
      string <- paste("Same favourite movie genre: ", as.character(row1$Movie.Genre2),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
  }
  if((choice == "Movie") | (choice == "Overall")) {
    if((row1$Movie0 != "") & ((row1$Movie0 == row2$Movie0) | (row1$Movie0 == row2$Movie1) | (row1$Movie0 == row2$Movie2))) {
      string <- paste("Same favourite movie: ", as.character(row1$Movie0),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
    if((row1$Movie1 != "") & ((row1$Movie1 == row2$Movie0) | (row1$Movie1 == row2$Movie1) | (row1$Movie1 == row2$Movie2))) {
      string <- paste("Same favourite movie: ", as.character(row1$Movie1),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
    if((row1$Movie2 != "") & ((row1$Movie2 == row2$Movie0) | (row1$Movie2 == row2$Movie1) | (row1$Movie2 == row2$Movie2))) {
      string <- paste("Same favourite movie: ", as.character(row1$Movie2),sep="")
      strings_to_return <- c(strings_to_return, string)
    }
  }
  # c(name1, name2, similarity)
  name1 <-  as.character(row1$Full.name)
  name2 <- as.character(row2$Full.name)
  df <- data.frame(stringsAsFactors = FALSE)
  if(length(strings_to_return) < 1) {
    df1 <- data.frame(Name1 = name1, Name2 = name2, Similarities = "No similarities")
    df <- rbind(df, df1)
  }
  if((length(strings_to_return) == 1) & strings_to_return[1] != "") {
    df1 <- data.frame(Name1 = name1, Name2 = name2, Similarities = as.character(strings_to_return[1]))
    #df <- rbind(df, c(name1, name2, as.character(strings_to_return[1])))
    df <- rbind(df, df1)
  }
  if(length(strings_to_return) > 1) {
    for(i in 1:length(strings_to_return)) {
      if(strings_to_return[i] != "") {
        df1 <- data.frame(Name1 = name1, Name2 = name2, Similarities = as.character(strings_to_return[i]))
        #df <- rbind(df, c(name1, name2, as.character(strings_to_return[i])))
        df <- rbind(df, df1)
      }
    }
  }
  colnames(df) <- c("Name1", "Name2", "Similarities")
  return(df)
}
############################################################################################################ 
############################################################################################################ 
############################################################################################################ 
ui <- fluidPage(
  titlePanel("Leisure Network Analysis"),
  sidebarPanel( 
    selectInput("CONNECTION", "Select connections of interest", choices=c("Overall", "Region", "Country", 
                                                                          "Program", "Music_Genre", "Artist", "Sport_Genre", "Athlete", "Movie_Genre", "Movie")
                , selected ="Overall")),
  visNetworkOutput("network"), 
  dataTableOutput("nodes_data_from_shiny"),
  uiOutput('dt_UI1'), 
  uiOutput('dt_UI2'),
  uiOutput('dt_UI3')
)
server <- function(input, output, session) {
  #Allows user to choose which connection type to analyze
  changing_data <- reactive({
    req(input$CONNECTION)
    edges <- leisure_edges
    selection<-c('from','to', paste(input$CONNECTION, "weight", sep="_"))
    edges<-edges[,selection]
    
    #Remove connections with 0 weight
    edges<-edges[edges[length(edges)] > 0,]
    
    #Set thickness encoding based on edge weight
    edges$value<-edges[,length(edges)]
    edges
  })
  
  #Update degree values based on user filter
  changing_data2 <- reactive({
    req(input$CONNECTION)
    if(input$CONNECTION != "Overall"){
      graph <- graph.data.frame(changing_data(), vertices = leisure_key, directed=F)
    } else{
      graph <- graph.data.frame(leisure_edges, vertices = leisure_key, directed=F)
    }  
    
    nodes <- data.frame(id = V(graph)$name, Name = V(graph)$Full.name, Region = V(graph)$Region,
                        Country = V(graph)$Country.of.Birth, Program = V(graph)$Academic.Program)
    
    nodes$group <- nodes$Region
    degree_value <- degree(graph, mode="total")
    nodes$degree <- degree_value[match(nodes$id, names(degree_value))]
    nodes$value <- nodes$degree
    
    nodes
  })
  
  output$network <- renderVisNetwork({
    visNetwork(changing_data2(), changing_data(), height = "100%", width = "100%") %>%
      visOptions(highlightNearest = TRUE) %>%
      visNodes(scaling = list(min = 10, max = 50)) %>%
      visEdges(scaling = list(min = 1, max = 20)) %>%
      visIgraphLayout(layout = "layout_in_circle") %>%
      visLegend(position = "right", main = "Group") %>%
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
})
  
  myNode <- reactiveValues(selected = '')
  
  observeEvent(input$current_node_id, {
    myNode$selected <<- input$current_node_id
  })
  
  #Creates table of summary info of selected node
  changing_data3 <- reactive({
    req(input$CONNECTION)
    output_edges <- changing_data()
    
    #Check if node has any connections on given filter
    if (sum(myNode$selected == output_edges$from) > 0){
      connections <- output_edges[which(myNode$selected == output_edges$from),]
      top_connections <- connections[connections[length(connections)] == max(connections[length(connections)]),]
      connection_summary <- merge(top_connections, nodes, by.x = "to", by.y = "id", all.x = T)
      connection_summary$value.x <- NULL #Remove value.x column from merge
      connection_summary[,3:ncol(connection_summary)-1]
    } else{
      #Otherwise return a blank table
      setNames(data.frame(matrix(ncol = 8, nrow = 0)), 
               c(colnames(output_edges[1]), colnames(nodes[,2:length(nodes)]-1)))
    }
  })
  
  changing_data4 <- reactive({
    req(input$CONNECTION)
    output_edges <- changing_data()
    
    #Check if node has any connections on given filter
    if (sum(myNode$selected == output_edges$from) > 0){
      
      connections <- output_edges[which(myNode$selected == output_edges$from),]
      top_connections <- connections[connections[length(connections)] == max(connections[length(connections)]),]
      
      connection_summary <- merge(top_connections, leisure_key, by.x = "to", by.y = "Key", all.x = T)
      
      mynode_summary <- leisure_key[which(leisure_key$Key == myNode$selected),]
      
      #print(myNode$selected)
      #print(connection_summary)
      df <- data.frame()
      for (row in 1:nrow(connection_summary)){
        similar_intrests <- get_similar_intrests(input$CONNECTION, mynode_summary, connection_summary[row,])
        #name1 <- mynode_summary$Full.name
        #name2 <- connection_summary$Full.name[row]
        #df[row,] <- c(name1, name2, similar_intrests)
        df <- rbind(df,similar_intrests)
      }
      
      df
    } else{
      #Otherwise return a blank table
      setNames(data.frame(matrix(ncol = 8, nrow = 0)), 
               c(colnames(output_edges[1]), colnames(nodes[,2:length(nodes)]-1)))
    }
  })
  
  #Prints the node summary table
  output$table1 <- renderDataTable({
    nodes[which(myNode$selected == nodes$id), 1:length(nodes)-1]
  })
  
  #Prints the connection summary table
  output$table2 <- renderDataTable({
    changing_data3()
  })
  
  output$table3 <- renderDataTable({
    changing_data4()
  })
  
  output$dt_UI1 <- renderUI({
    if(nrow(nodes[which(myNode$selected == nodes$id),])!=0){
      dataTableOutput('table1')
    } else{}
  })
  
  output$dt_UI2 <- renderUI({
    if(nrow(nodes[which(myNode$selected == nodes$id),])!=0){
      dataTableOutput('table2')
    } else{}
  })
  
  output$dt_UI3 <- renderUI({
    if(nrow(nodes[which(myNode$selected == nodes$id),])!=0){
      dataTableOutput('table3')
    } else{}
  })
  
}
shiny::shinyApp(ui = ui, server = server, options = list(height = 600, width = 1000))

