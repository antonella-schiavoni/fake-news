library(shiny)
library(shinydashboard)
library(igraph)
library(visNetwork)
library(dplyr)
set.seed(42)
# newsTypes <- c("Todas", "Fake","Real")

dashboardPage(
  dashboardHeader(title = "Analisis de fake news"),
  dashboardSidebar({
    # selectInput('tipo_noticia_seleccionada', 'Tipo de noticias', newsTypes, selected = "Todas")
    sidebarMenu(
      menuItem("Noticias - Usuarios", tabName = "news_users", icon = icon("newspaper")),
      menuItem("Usuarios - Usuarios", tabName = "users_users", icon = icon("users"))
    )
  }),
  dashboardBody(
    tabItems(
      
      # Tab Noticias - Usuarios
      
      tabItem(tabName = "news_users",
        fluidRow(
          includeCSS("www/styles.css"),
          box(
            h2("Noticias compartidas por usuarios"),
            markdown("
              Red bipartita que muestra en `rojo` los nodos que son fake news y en `verde` a los de real news.
              El resto de los nodos (`celeste`) son usuarios que comparten dichas noticias.
            "),
            markdown("La red es interactiva. Se puede hacer zoom, clickear nodos y arrastrar"),
            markdown("Se indica con borde `amarillo` el nodo seleccionado y con `azul oscuro` los nodos que siguen al usuario seleccionado"),
            visNetworkOutput("network", width = "100%")
          ) %>% tagAppendAttributes(class = 'mainBox'),
          box(
            h2("Información sobre nodo seleccionado"),
            fluidRow(
              box(
                title = "Id de nodo seleccionado",
                solidHeader = TRUE,
                status = "primary",
                markdown(textOutput("current_node_id"))
              ),
              box(
                title = "Tipo de nodo seleccionado",
                solidHeader = TRUE,
                status = "primary",
                markdown(textOutput("node_type"))
              ),
            ),
            fluidRow(
              column(
                width = 12,
                conditionalPanel(
                  condition = "input.node_type == 'user'",
                  box(
                    width = "100%",
                    title = "Noticias compartidas por el usuario seleccionado",
                    solidHeader = TRUE,
                    status = "primary",
                    dataTableOutput("data_selected_user")
                  )
                ),
                conditionalPanel(
                  condition = "input.node_type == 'news'",
                  box(
                    width = "100%",
                    title = "Titulo",
                    solidHeader = TRUE,
                    status = "primary",
                    markdown(textOutput("news_title"))
                  ),
                  box(
                    width = "100%",
                    title = "Data de todos los usuarios que compartieron la noticia seleccionada",
                    solidHeader = TRUE,
                    status = "primary",
                    markdown(paste("- Esta noticia fue compartida por", textOutput("amount_of_users_who_also_shared_this_news", inline = TRUE), "usuarios", sep= " ")),
                    markdown("---"),
                    dataTableOutput("users_who_shared_the_same_news")
                  )
                )
              )
            )
          ) %>% tagAppendAttributes(class = 'mainBox'),
        ),
        width = "100%"
      ),
      
      # Tab Usuarios - Usuarios
      
      tabItem(tabName = "users_users", 
        fluidRow(
          box(
            h2("Red de Usuarios conectados con usuarios"),
            markdown("Este es un sample del ~10% de la data porque con toda la data nunca termina de cargar"),
            selectInput(
              'cluster_type',
              'Tipo de clustering',
              c("fake_real_news","leading_eigen","cluster_louvain","label_propagation"),
              selected = "fake_real_news"
            ),
            conditionalPanel(
              condition = "input.cluster_type == 'fake_real_news'",
              markdown("Aquellos usuarios que compartieron mas del 50% de fake news se muestran en `rojo`, los demas en `verde`"),
              markdown("El objetivo de esta visualizacion es poder analizar si los usuarios que comparten muchas fakes news estan muy conectados entre ellos.
                       Si bien la red tiene una alta conectividad, se da que hay muchos usuarios que comparten una mayor proporción de `real news` conectados
                       con usuarios que comparten una alta proporción de `fake news`. Este gráfico no es el mejor para visualizar la hipótesis planteada,
                       hubiera sido mejor analizarlo mediante un gáfico de homofilia y Assortativity Mixing."),
            ),
            conditionalPanel(
              condition = "input.cluster_type == 'leading_eigen'",
              markdown("Al ejecutar este método de comunidades en R, se encontraron 16 comunidades diferentes. Se puede visualizar facilmente que dentro de esas
                       16 comunidades existen dos grandes grupos de usuarios, que parecen bastantes razonables. El grupo de usuarios coloreados en tonos de `rojo`
                       luce más densamente conectado que el grupo de usuarios coloreados en `verde`."),
            ),
            conditionalPanel(
              condition = "input.cluster_type == 'cluster_louvain'",
              markdown("El algorítmo de Cluster Louvain detectó 25 comunidades. Visualmente se pueden apreciar 5 grandes grupos. Los grupos de usuarios coloreados
                       en `negro` y `rosa` dan la impresión de ser los grupos predominantes, siendo los `negros` los más densamente conectados. En tercer lugar
                       predominan los usuarios coloreados de color `naranja` que forman un cluster bastante llamativo, el cual se conecta en su mayoria con usuarios
                       de color `rosa`. Finalmente también se observan nodos `verdes` y `marrones` que aparecen con muchisima menor frecuencia que los antes mencionados."),
            ),
            conditionalPanel(
              condition = "input.cluster_type == 'label_propagation'",
              markdown("Label Propagation logro diferenciar entre 32 tipos de nodos diferentes.Visualmente se aprecian dos grandes grupos.
                       Los nodos coloreados en `rojo`, los cuales esta altamente conectados y los nodos `azules`, los cuales tienen una
                       densidad de conexión menor y se encuentran rodeando a la componente de nodos `rojos`."),
            ),
            visNetworkOutput("users_users_network", width = "100%"),
            width = "100%"
          ),
          width = "100%"
        ),
        width = "100%"
      )
    )
  )
)