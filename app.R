library(RMySQL)
library(shiny)
library(DT)
library(shinythemes)


funcDispoProd<-function(n, mydb){
  args <- switch(n,
         "Total" = "SELECT pays, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1
                     group by pays
                     ORDER BY dispo_prot DESC LIMIT 10",
         "2018" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2018
                     group by pays, année
                     ORDER BY dispo_prot DESC LIMIT 10",
         "2017" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2017
                     group by pays, année
                     ORDER BY dispo_prot DESC LIMIT 10",
         "2016" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2016
                     group by pays, année
                     ORDER BY dispo_prot DESC LIMIT 10",
         "2015" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2015
                     group by pays, année
                     ORDER BY dispo_prot DESC LIMIT 10",
         "2014" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2014
                     group by pays, année
                     ORDER BY dispo_prot DESC LIMIT 10")
  dbGetQuery(mydb, args)
         
}

funcDispoProdKcal<-function(n, mydb){
  args <- switch(n,
                 "Total" = "SELECT pays, SUM(dispo_alim_kcal_p_j) AS dispo_alim_kcal_p_j FROM fao_project.dispo_alim
                     WHERE dispo_alim_kcal_p_j != -1
                     group by pays
                     ORDER BY dispo_alim_kcal_p_j DESC LIMIT 10",
                 "2018" = "SELECT pays, année, SUM(dispo_alim_kcal_p_j) AS dispo_alim_kcal_p_j FROM fao_project.dispo_alim
                     WHERE dispo_alim_kcal_p_j != -1 AND année = 2018
                     group by pays, année
                     ORDER BY dispo_alim_kcal_p_j DESC LIMIT 10",
                 "2017" = "SELECT pays, année, SUM(dispo_alim_kcal_p_j) AS dispo_alim_kcal_p_j FROM fao_project.dispo_alim
                     WHERE dispo_alim_kcal_p_j != -1 AND année = 2017
                     group by pays, année
                     ORDER BY dispo_alim_kcal_p_j DESC LIMIT 10",
                 "2016" = "SELECT pays, année, SUM(dispo_alim_kcal_p_j) AS dispo_alim_kcal_p_j FROM fao_project.dispo_alim
                     WHERE dispo_alim_kcal_p_j != -1 AND année = 2016
                     group by pays, année
                     ORDER BY dispo_alim_kcal_p_j DESC LIMIT 10",
                 "2015" = "SELECT pays, année, SUM(dispo_alim_kcal_p_j) AS dispo_alim_kcal_p_j FROM fao_project.dispo_alim
                     WHERE dispo_alim_kcal_p_j != -1 AND année = 2015
                     group by pays, année
                     ORDER BY dispo_alim_kcal_p_j DESC LIMIT 10",
                 "2014" = "SELECT pays, année, SUM(dispo_alim_kcal_p_j) AS dispo_alim_kcal_p_j FROM fao_project.dispo_alim
                     WHERE dispo_alim_kcal_p_j != -1 AND année = 2014
                     group by pays, année
                     ORDER BY dispo_alim_kcal_p_j DESC LIMIT 10")
  dbGetQuery(mydb, args)
  
}

funcDispoProdF<-function(n, mydb){
  args <- switch(n,
                 "Total" = "SELECT pays, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1
                     group by pays
                     ORDER BY dispo_prot LIMIT 10",
                 "2018" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2018
                     group by pays, année
                     ORDER BY dispo_prot LIMIT 10",
                 "2017" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2017
                     group by pays, année
                     ORDER BY dispo_prot LIMIT 10",
                 "2016" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2016
                     group by pays, année
                     ORDER BY dispo_prot LIMIT 10",
                 "2015" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2015
                     group by pays, année
                     ORDER BY dispo_prot LIMIT 10",
                 "2014" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2014
                     group by pays, année
                     ORDER BY dispo_prot LIMIT 10")
  dbGetQuery(mydb, args)
  
}


ui <-
  
  navbarPage(
    title = 'FAO PROJECT',
    windowTitle = 'Navigation Bar', 
    position = 'fixed-top', 
    collapsible = TRUE, 
    theme = shinytheme('cosmo'), 
    tabPanel(title = 'Data',
            fluidPage(
              titlePanel("censusVis"),
             sidebarLayout(
               sidebarPanel(
                 helpText("Visualisation des données contenue dans les tables SQL."),
                 
                 selectInput("var", 
                             label = "Choose a table",
                             choices = list("Population", 
                                            "Population sous nutrition",
                                            "disponibilite alimentaire", 
                                            "equilibre production"),
                             selected = "Population"),
                 
               ),
               mainPanel(DT::dataTableOutput("data_table"))
    ))),
    tabPanel(title = 'Requête SQL',
             fluidPage(
               titlePanel("censusVis"),
               tabsetPanel( 
                 tabPanel("haut ratio",
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Sélectionnez une disponibilité (Protéine ou Kcal),
                                       puis sélectionnez une année pour connaitre les pays ayant les plus haut ratio de disponibilité/habitant"),
                              
                              selectInput("var1", 
                                          label = "Disponibilité",
                                          choices = list("Proteine",
                                                         "Kcal"),
                                          selected = "Protéine"),
                              
                              selectInput("var2", 
                                          label = "Année",
                                          choices = list("Total",
                                                         "2018", 
                                                         "2017",
                                                         "2016", 
                                                         "2015",
                                                         "2014"),
                                          selected = "Total"),
                          
                                        ),
                          mainPanel(DT::dataTableOutput("sql1"))
                                      )
                                    ),
                 tabPanel("Faible ratio",
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Sélectionnez une année pour connaitre les pays avec le plus faible ratio de disponibilité/habitant"),
                              selectInput("var3", 
                                          label = "Année",
                                          choices = list("Total",
                                                         "2018", 
                                                         "2017",
                                                         "2016", 
                                                         "2015",
                                                         "2014"),
                                          selected = "Total"),
                              
                            ),
                            mainPanel(DT::dataTableOutput("sql2"))
                          )
                 ), 
                 tabPanel("Pertes",
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Cette requête permet de visualiser la quantité totale (en kg) de produits perdus par pays et par année")
                            ),
                            mainPanel(DT::dataTableOutput("sql3"))
                          )
                 ),  
                 tabPanel("Sous nutrition",
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Permet de visualiser les 10 pays pour lesquels la proportion de personnes sous-alimentées est la plus forte.")
                            ),
                            mainPanel(DT::dataTableOutput("sql4"))
                          )
                 ),
                 tabPanel("ratio Autres utilisations/Disponibilité intérieure",
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Permet de visualiser les 10 produits pour lesquels le ratio Autres utilisations/Disponibilité intérieure est le plus élevé.")
                            ),
                            mainPanel(DT::dataTableOutput("sql5"))
                          )
                 )
               )))
  )



server <- function(input, output) {
  
    mydb = dbConnect(MySQL(),
                 user='cyrille',
                 password='*Shekil602*!',
                 dbname='FAO_Project',
                 port=3306)

    output$data_table <- renderDataTable({
      args <- switch(input$var,
                     "Population" = "SELECT * FROM population",
                     "Population sous nutrition" = "SELECT * FROM population_sous_nutrition",
                     "disponibilite alimentaire" = "SELECT * FROM dispo_alim",
                     "equilibre production" = "SELECT * FROM equilibre_prod")
      dbGetQuery(mydb, args)
    })
    output$sql1 <- renderDataTable({
      args <- switch(input$var1,
                     "Proteine" = funcDispoProd(input$var2, mydb),
                     "Kcal" = funcDispoProdKcal(input$var2, mydb))
    })
    output$sql2 <- renderDataTable({
      args <- switch(input$var3,
                     "Total" = "SELECT pays, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1
                     group by pays
                     ORDER BY dispo_prot LIMIT 10",
                     "2018" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2018
                     group by pays, année
                     ORDER BY dispo_prot LIMIT 10",
                     "2017" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2017
                     group by pays, année
                     ORDER BY dispo_prot LIMIT 10",
                     "2016" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2016
                     group by pays, année
                     ORDER BY dispo_prot LIMIT 10",
                     "2015" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2015
                     group by pays, année
                     ORDER BY dispo_prot LIMIT 10",
                     "2014" = "SELECT pays, année, SUM(dispo_prot / 1000) AS dispo_prot FROM fao_project.dispo_alim
                     WHERE dispo_prot != -1 AND année = 2014
                     group by pays, année
                     ORDER BY dispo_prot LIMIT 10")
      dbGetQuery(mydb, args)
    })
    output$sql3 <- renderDataTable({
      args <- "SELECT pays, année, SUM(pertes * 1000000) AS pertes_tot FROM fao_project.equilibre_prod
      WHERE pertes != -1
      group by pays, année"
      dbGetQuery(mydb, args)
    })
    
    output$sql4 <- renderDataTable({
    args <- "SELECT Country, Year ,Population FROM fao_project.population_sous_nutrition
    WHERE Population != -1
    group by Country, Year
    ORDER BY Population desc LIMIT 10"
    dbGetQuery(mydb, args)
    })
    
    output$sql5 <- renderDataTable({
    args <- "SELECT pays, année, produit, (autres_utilisations/dispo_int) as ratio FROM fao_project.equilibre_prod
    WHERE autres_utilisations != -1  AND dispo_int != -1
    group by pays, année, produit
    ORDER BY ratio desc LIMIT 10"
    dbGetQuery(mydb, args)
    })
}

shinyApp(ui = ui, server = server)
    