#Loading the rvest package
library('rvest')
library('dplyr')

#PAgina a revisar como vemos tiene el nuemro de pagina "page=1" 
# para nuestro ejemplo solo tomaremos las cienprimeros ranks, por lo tanto tomaremos 
# desde la pagina uno hasta la pagina 3(cada pagina tiene 36 lenguajes)
url <- 'https://stackoverflow.com/tags?page=1&tab=popular'

url2 <- 'https://stackoverflow.com/questions/tagged/javascript'

#Leemos el html



#usamos un selectos css para cada variable


first_result <- rank_data_html[2]
#lo convertimos a texto
rank_lenguaje_name <- html_text(rank_data_html)

#Let's have a look at the rankings
strsplit <- strsplit(rank_data_html,"+") %>% unlist()
grep <- grep("href=\"/questions/tagged/javascript+",strsplit, value = T)
grep <- gsub(c("c(","\""),"",grep)
grep <- gsub("×",",",grep)
grep <- gsub(" × ","",rank_lenguaje_name)
strsplit2 <- strsplit(grep,",")%>% unlist()
grep <- gsub(" ","",strsplit2)  


head(strsplit(rank_lenguaje_name,"\r\n ") %>% gsub(" x "))

webpage <- read_html(url2)

# numero de preguntas
rank_number_question_html <- html_nodes(webpage,'.item-multiplier-count')

#lo convertimos a texto
rank_number_question <- html_text(rank_number_question_html)

#Let's have a look at the rankings
head(rank_number_question)


### nombre de la url
url <- 'https://stackoverflow.com/tags?page=1&tab=popular'

### nombre de las columnas que crearemos
names <- c("rank_lenguaje_name","rank_number_question")

### nombre de los nodos en el archivo html
node_name <- c('.post-tag','.item-multiplier-count')

## Creamos la funcion info_extract la cual se encargara de estraer
# y convertir en un data frame los nodos que busquemos

info_extract <- function(url,names,node_name) {
  
  #leemos la url
  webpage <- read_html(url)
  
  # ciclo for para cada nodo
  for (i in 1:length(node_name)) {
    #estraemos la infomacion del nodo
    html <- html_nodes(webpage,node_name[i])
    #lo convertimos en texto
    name <- html_text(html)
    
    if(i==1){
      result <- name
      
    }else {
      result <- cbind(result,name)
    }
    
    
  }
  #lo convertimos en un dataframe
  result <- data.frame(result,stringsAsFactors = F)
  names(result) <- names
  result
}

info_extract <- info_extract(url,names,node_name)



lenguaje <- c("javascript","java")

# creamos la funcion related_tags para obtener un data.frame
# con los lenguajes que mas se relacionan con los lenguajes populares
related_tags <- function(lenguaje) {
  # creamos esta bandera para identificar que es el primer ciclo for
  flag_first <- T
  for (i in lenguaje) {
    cat("----------------------\n",
        "Extrallendo: ",i,"\n",
        "======================\n")
    #en caso de que el nombre sea "c#" la url de stackoverflow la combierte en el tag
    # "c%223" lo mismo sucede con "c++" lo vuelve "c%2b%2b" por lo tanto haremos lo mismo
    i <- ifelse(i=="c#","c%23",i)
    i <- ifelse(i=="c++","c%2b%2b",i)
    
    # creamos la url con el nombre del tag
    url <- paste0('https://stackoverflow.com/questions/tagged/',i)
    webpage <- read_html(url)
    # buscamos cuantas veces el tag secundario se asocia con el tag popular
    count_html <- html_nodes(webpage,'.item-multiplier-count')
    count <- html_text(count_html)
    
    # buscamos el nombre de los lenguajes que mas se taggean con los lengujes populares
    rank_data_html <- html_nodes(webpage,'.js-gps-related-tags')
    character<- as.character(rank_data_html)
    strsplit <- strsplit(character," ")%>% unlist()
    
    #despues de separar buscamos en que filas se encuentran los tag que nos importan 
    grep <- grep(paste0("/questions/tagged/",i),strsplit, value = T)
    grep <- gsub(paste0("href=\"/questions/tagged/",i,"+"),"",grep)
    grep <- gsub("\"","",grep)
    second_tag <- substr(grep,2,nchar(grep))
    popular <- rep(i,length(substr))
    cbind <-  cbind(popular,second_tag) %>% cbind(count)
    
    if(flag_first)  {
      related_tags <- cbind
      flag_first <- F
      }else {related_tags <- rbind(related_tags,cbind)}
  }
  related_tags <- related_tags %>% data.frame(stringsAsFactors = F)
  related_tags$count <-  related_tags$count %>% as.numeric()
  related_tags
}

tabla_result <- related_tags(info_extract$rank_lenguaje_name)

muestra <- tabla_result %>% arrange(desc(count)) %>% head(150)

###
library(ggplot2)


group_by(tabla_result,second_tag) %>% summarise(n =n()) %>%
  filter(n>10) %>% arrange(desc(n)) %>%
  ggplot( aes(x=second_tag, y=n)) +
  geom_segment( aes(x=second_tag, xend=second_tag, y=0, yend=n) , size=1, color="blue", linetype="dotted" ) +
  geom_point()


ggplot(muestra, aes(x=as.factor(second_tag) )) + geom_bar()

vertices<-
  paste(muestra$popular,muestra$second_tag) %>% strsplit(" ") %>% unlist()


library(igraph)

g1<-
  graph(vertices)

plot.igraph(g1,edge.arrow.size = 0.3,
            arrow.width = 0.1,
            vertex.size = 1,
            vertex.label.cex = 0.8)

plot(g1, 
     layout = layout_with_graphopt, 
     edge.arrow.size = 0.2)

library(tidygraph)
library(ggraph)
routes_igraph_tidy <- as_tbl_graph(g1)
routes_igraph_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

ggraph(routes_igraph_tidy) + geom_edge_link() + 
  geom_node_point() + theme_graph()

ggraph(routes_igraph_tidy, layout = "graphopt") + 
  geom_node_point() +
  #geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  #geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()


ggraph(routes_igraph_tidy, layout = "linear") + 
  #geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  #scale_edge_width(range = c(0.2, 2)) +
  #geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()

library(networkD3)
library(webshot)

# create data:
set.seed(101)
links=data.frame(
  source=muestra$popular,
  target=muestra$second_tag
)
# Plot
graph <- simpleNetwork(muestra, "popular" ,"second_tag",
              zoom = T,
              #height =NULL,
              linkDistance = 150,
              fontSize = 25,
              opacity = .9,
              nodeColour = "#3730bd",
              fontFamily = "sans-serif",
              width = 1,
              charge = -30)
graph
saveNetwork(graph,file = '#252_interactive_network_chart1.html')



# Usually what you have is a connection data frame: a list of flows with intensity for each flow
links=data.frame(source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), value=c(2,3, 2, 3, 1, 3))

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes=data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource=match(links$source, nodes$name)-1 
links$IDtarget=match(links$target, nodes$name)-1

# Make the Network
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE)


##### purebas de graficas

library(tidyverse)
edge_list <- tibble(from = c(1, 2, 2, 3, 4), to = c(2, 3, 4, 2, 1))
node_list <- tibble(id = 1:4)

unique <- unique(muestra$popular)

for (i in 1:length(id_frame$name)) {
  muestra$popular[muestra$popular %in% id_frame$name[i]] <- i
}

for (i in 1:length(id_frame$name)) {
  muestra$second_tag[muestra$second_tag %in% id_frame$name[i]] <- i
}


routes_tidy <- tbl_graph(nodes = nodes,
                         edges = edges,
                         directed = TRUE)
###################



sources <- muestra %>%
  distinct(popular) %>%
  rename(label = popular)

destinations <- muestra %>%
  distinct(second_tag) %>%
  rename(label = second_tag)

nodes <- full_join(sources, destinations, by = "label")
nodes
nodes <- nodes %>% rowid_to_column("id")
nodes


per_route <- muestra %>%  
  group_by(popular, second_tag) %>%
  summarise(weight = sum(count)) %>% 
  ungroup()
per_route


edges <- per_route %>% 
  left_join(nodes, by = c("popular" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("second_tag" = "label")) %>% 
  rename(to = id)


edges <- select(edges, from, to, weight)
edges

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
routes_tidy

routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()

visNetwork(nodes, edges)

edges <- mutate(edges, width = weight/(max(weight)-min(weight))*10 )
visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")


nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)
forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "id", Value = "width", 
             opacity = 1, fontSize = 16, zoom = TRUE)

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")
