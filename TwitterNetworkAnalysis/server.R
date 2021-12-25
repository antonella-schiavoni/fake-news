library(shiny)
library(shinydashboard)
library(igraph)
library(visNetwork)
library(dplyr)
library(RColorBrewer)

set.seed(5)

user_to_user <- read.csv("./DatosKaggle/BuzzFeed/sample/NewsFollowing_20210815.csv")

BF_news_users <- read.csv("./DatosKaggle/BuzzFeed/sample/BipartiteUserNewsComplete_20210815.csv")

users_users_raw <- read.csv("./DatosKaggle/BuzzFeed/sample/UsersUsers.csv")

users_cluster <- read.csv("./DatosKaggle/BuzzFeed/sample/ClusterResults.csv")

fake_news_relationship <- read.csv("./DatosKaggle/BuzzFeed/sample/FakeNewsRelationship.csv")

qual_color_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
color_vector = sample(unlist(mapply(brewer.pal, qual_color_pals$maxcolors, rownames(qual_color_pals))))


whitelist = fake_news_relationship$Nombre # Necesario para coincidir con otro sample
users_users_raw = users_users_raw[users_users_raw$id_source %in% whitelist & users_users_raw$id_target %in% whitelist, ]

function(input, output) {
  output$network <- renderVisNetwork({
    BF_news_users_filtrado = data.frame(BF_news_users)
    # if (input$tipo_noticia_seleccionada != 'Todas') {
    #  BF_news_users_filtrado = BF_news_users[BF_news_users$news_type == input$tipo_noticia_seleccionada, ]
    # }
    
    cantidad_usuarios = length(unique(BF_news_users_filtrado$user_id))
    
    
    # Creamos el grafo bipartito
    grafo = BF_news_users_filtrado[,c("user_id", "news_title_hash")]
    grafo <- graph.data.frame(grafo)
    
    # Definimos de que tipo es cada nodo
    V(grafo)$type <- bipartite_mapping(grafo)$type
    
    # Hacemos un mapeo entre el hash de la noticia y el tipo de noticia
    tipo_noticia_a_hash = unique(BF_news_users_filtrado[,c(4,5)])
    
    # Creamos un vector para crear los labels de los nodos de tipo noticia (Fake / Real)
    label_tipo_noticia = c(vector(mode="character", length=cantidad_usuarios), tipo_noticia_a_hash$news_type)
    
    # Definimos el color de los nodos de tipo noticia
    color_noticia = ifelse(tipo_noticia_a_hash$news_type == "Fake", "red", "limegreen")
    
    # Definimos el color de los nodos restantes (usuarios), concatenados con los de color_noticia
    color_nodos = c(rep("lightsteelblue", 1, cantidad_usuarios), color_noticia)
    
    # Establecemos tipo de nodo para poder referenciarlo despues desde la parte de javascript
    tipo_nodos = c(rep("user", 1, cantidad_usuarios), rep("news", 1, length(color_noticia)))
    
    
    df_nodes <- data.frame(
      id = as.character(V(grafo)$name),
      title = as.character(V(grafo)$name),
      color = color_nodos,
      label = label_tipo_noticia,
      following = "",
      font.size = 10,
      shape='circle',
      type=tipo_nodos
    )
    
    # Creamos campo following para cada nodo. Si es una noticia queda vacio,
    # sino como un string "[id, id, ...]"
    following = c()
    for (i in 1:nrow(df_nodes)) { 
      if (df_nodes[i,"type"] == "news"){
        following[i] = ""
      } else {
        following[i] = user_to_user[user_to_user$user == as.integer(df_nodes[i,"id"]),]$following
      }
    }
    
    df_nodes$following = following
    
    df_edges <- data.frame(
      from = as.integer(as_edgelist(grafo)[,1]),
      to = as.character(as_edgelist(grafo)[,2])
    )
    
    visNetwork(df_nodes, df_edges) %>% 
      visPhysics(stabilization = FALSE) %>% # Mejora performance
      visEdges(
        smooth = FALSE, # Mejora performance
        color = list(color= "lightsteelblue", highlight ="black",hover = "blue")
      ) %>%
      visInteraction(hover = T) %>% 
      visEvents(
        startStabilizing = "function() {
          this.moveTo({ scale: 0.2 });
        }",
        selectNode = "function(properties) {
          // Poner color lightsteelblue para todos los nodos de tipo user para resetear estado
          this.body.nodeIndices
            .filter(id => this.body.data.nodes.get(id).type === 'user')
            .forEach(id => this.nodesHandler.body.nodes[id].setOptions({ color: 'lightsteelblue'}));
  
          const currentNode = this.body.data.nodes.get(properties.nodes[0]);
          
          // Resalta nodo seleccionado
          this.nodesHandler.body.nodes[currentNode.id].setOptions({
            color: {
              highlight: {
                border: 'yellow'
              }
            },
            borderWidth: 2
          });
  
          // Convierte el campo following, de string ('[id, id, ...]') a array
          const followingAsArray = currentNode.following
            .replace('[', '')
            .replace(']', '')
            .split(',')
            .map(n => Number(n));
            
          // Pinta de azul oscuro todos los usuarios a los que el usuario clickeado sigue
          followingAsArray.forEach(id => {
              const node = this.nodesHandler.body.nodes[id];
              if (node) {
                node.setOptions({ color: '#0000ff'});
              }
            });
  
          // Mandamos valores de javascript a shiny
          Shiny.onInputChange('current_node_id', currentNode.id);
          Shiny.onInputChange('node_type', currentNode.type);
          Shiny.onInputChange('following', followingAsArray);
        }"
      )
  })
  
  output$current_node_id <- renderText({
    if (is.null(input$current_node_id))
      expr = "No hay ningun nodo seleccionado"
    else
      expr = input$current_node_id
  })
  
  output$node_type <- renderText({
    if (is.null(input$node_type))
      expr = "No hay ningun nodo seleccionado"
    else
      expr = input$node_type
  })
  
  output$data_selected_user = renderDataTable({
    filtrado = data.frame(BF_news_users[BF_news_users$user_id == input$current_node_id,])
    filtrado$news_id = NULL
    filtrado$user_id = NULL
    expr = filtrado
  })
  
  output$news_title = renderText({
    BF_news_users[BF_news_users$news_title_hash == input$current_node_id,3][1]
  })
  
  output$data_selected_news = renderDataTable({
    filtrado = data.frame(BF_news_users[BF_news_users$news_title_hash == input$current_node_id,])
    filtrado$news_id = NULL
    expr = filtrado
  })
  
  output$users_who_shared_the_same_news = renderDataTable({
    user_id = BF_news_users[BF_news_users$news_title_hash == input$current_node_id,]$user_id
    
    output$amount_of_users_who_also_shared_this_news = renderText({ length(user_id) })
    total_shared_news = c()
    fake_news_shared_ratio = c()
    
    df_filtered = BF_news_users[BF_news_users$user_id %in% user_id,]
    for (id in user_id) {
      user_rows = df_filtered[df_filtered$user_id == id,]
      real_freq = length(user_rows[user_rows$news_type == 'Real',]$user_id)
      fake_freq = length(user_rows[user_rows$news_type == 'Fake',]$user_id)
      total = real_freq + fake_freq
      total_shared_news = append(total_shared_news, total)
      fake_news_shared_ratio = append(fake_news_shared_ratio, fake_freq / total)
    }
    expr = data.frame(user_id, total_shared_news, fake_news_shared_ratio)
  })
  
  output$table = renderDataTable({
    BF_news_users[BF_news_users$user_id %in% input$following,]
  })
  
  # TODO: Meter un if entre tabs para que no explote el servidor: https://stackoverflow.com/questions/38838571/changing-value-of-element-based-on-selected-tab-in-shinydashboard
  output$users_users_network <- renderVisNetwork({
    
    whitelist = fake_news_relationship$Nombre # Necesario para coincidir con otro sample
    users_users_raw[users_users_raw$id_source %in% whitelist & users_users_raw$id_target %in% whitelist, ]
    
    sample_users_users = sample_n(users_users_raw, 1000) # TODO: Tomar este 5000 desde un input slider
    
    node_ids = unique(c(sample_users_users$id_source, sample_users_users$id_target))
    
    if (input$cluster_type != 'fake_real_news') {
      classes = unique(as.vector(users_cluster[input$cluster_type]))
    } else {
      classes = c(1,2)
    }

    colors = c()
    for (id in node_ids) {
      if (input$cluster_type != 'fake_real_news') {
        cluster_class = as.integer(users_cluster[users_cluster$user_id == id,][input$cluster_type])
        colors = append(colors, color_vector[cluster_class])
      } else {
        is_fake = fake_news_relationship[fake_news_relationship$Nombre == id,]$Etiqueta == 'FakeCluster'
        if (is_fake) {
          colors = append(colors, 'red')
        } else {
          colors = append(colors, 'green')
        }
      }
    }
    
    df_nodes = data.frame(
      id = node_ids,
      title = node_ids,
      color = colors,
      font.size = 10,
      shape='circle'
    )

    df_edges <- data.frame(
      from = sample_users_users$id_source,
      to = sample_users_users$id_target,
      arrows = c("to", "from")
    )
    
    visNetwork(df_nodes, df_edges) %>% 
      visPhysics(stabilization = FALSE) %>% # Mejora performance
      visEdges(
        smooth = FALSE, # Mejora performance
      ) %>%
      visInteraction(hover = T) %>%
      visEvents(
        startStabilizing = "function() {
          this.moveTo({ scale: 0.2 });
        }",
        selectNode = "function(properties) { console.log(properties); }"
      )
  })
  
}