library(shiny)
library(tidyverse)
library(tibble)
library(stringr)
library(ggplot2)
library(igraph)
library(visNetwork)

load("Global.RData")

ageToIndex <- function(AgeCat){
  data.frame(ageCat = c("All Dogs","Young Adult", "Middle Adult", 
                        "Mature Adult", "Senior"),
             index = c(1:5)) %>% 
    filter(ageCat == AgeCat) %>%
    pull(index) %>%
    return()
}

for(i in seq_along(VLList)){
  colnames(VLList[[i]]) <- c("label", "value", "ID", "color", "group")
}

for(i in seq_along(ELList)){
  colnames(ELList[[i]]) <- c("from", "to", "fromCount", "toCount", "combinedCount","relativeRisk",
                            "ChiSq", "Chi P", "Chi Q")
  ELList[[i]] <- ELList[[i]] %>%
    mutate(width = log10(relativeRisk))
}

allVL <- VLList[[1]]
yaVL <- VLList[[2]]
miVL <- VLList[[3]]
maVL <- VLList[[4]]
sVL <- VLList[[5]]

allEL <- ELList[[1]]
yaEL <- ELList[[2]]
miEL <- ELList[[3]]
maEL <- ELList[[4]]
sEL <- ELList[[5]]

# user interface elements and layout
ui <- fluidPage(
  # App title ----
  titlePanel("DAP Comorbidity"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    #Age Group Selection (access using label AgeGroup)
    selectInput("AgeChosen", label = "Age Group", c("All Dogs","Young Adult", "Middle Adult", 
                                                    "Mature Adult", "Senior")),
    #Slider for rel risk cutoff (Access using RRCutoff)
    numericInput("RRCutoff", "Relative Risk Cutoff", value = 1, min = 0, max = 150),
    
    
    
    #Radio Button for choosing mode
    radioButtons("appMode", "Analysis Mode", choices = c("All Conditions", "By Index Condition")),
    
    conditionalPanel(
      condition = "input.appMode == 'By Index Condition'",
      selectInput("indexCondition", "Index Condition", choices = NULL)
    ),
    
    #Type Select
    checkboxInput("typeStratOnOff", "Stratify By Type", FALSE),
    
    conditionalPanel(
      condition = "input.typeStratOnOff == 1",
      checkboxGroupInput("typeStrat", "Types to include", choices = types, selected = types)
    )
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Network", visNetworkOutput("netPlot")),
                tabPanel("Edge List", DT::dataTableOutput("edgeList")),
                tabPanel("Vertex List", DT::dataTableOutput("vertexList")))
    
  )
)

# server-side computations
server <- function(input, output) {
  #Reactive expression for pulling which age group was selected
  AgeGroup <- reactive(ageToIndex(input$AgeChosen))
  
  #Reactive expression for type picking
  typeList <- reactive(input$typeStrat)
  
  #Reactive expression assigning proper Edge List
  EL <- reactive(ELList[[AgeGroup()]] %>%
                   filter(relativeRisk > input$RRCutoff)) 
  #Reactive expression assigning proper Vertex List
  VL <- reactive(VLList[[AgeGroup()]] %>%
                   filter(label %in% c(EL()$from, EL()$to)))
  #Reactive expression creating network
  net <- reactive(graph_from_data_frame(EL(), vertices = VL(), directed = FALSE))
  
  #Input Update for Index condition availability
  switchAgeOrMode <- reactive({list(input$appMode, input$AgeChosen)})
  
  observeEvent(switchAgeOrMode(),{
    #freezeReactiveValue(input, "indexCondition")
    updateSelectInput(inputId = "indexCondition", choices = VL() %>%
                        arrange(label) %>%
                        pull(label))
  })
  
  # observeEvent(reactive(typeList),{
  #   
  # })
  
  #Find vertices that are connected to index condition - Messy!
  indexConditionVertices <- reactive(unique(c(EL() %>% 
                                                filter(from == input$indexCondition | to == input$indexCondition) %>%
                                                pull(from),
                                              EL() %>% 
                                                filter(from == input$indexCondition | to == input$indexCondition) %>%
                                                pull(to))))
  
  #Make EL of edges that connect two vertices in the above group
  indexEL <- reactive(EL() %>% filter(from %in% indexConditionVertices() & to %in% indexConditionVertices()))
  indexVL <- reactive(VL() %>% filter(label %in% c(indexEL()$from,indexEL()$to)))
  indexNet <- reactive(graph_from_data_frame(indexEL(), vertices = indexVL(), directed = FALSE))
  
  #Plot output
  output$netPlot <- renderVisNetwork({
    
    #Set parameters (Margin of 0)
    par(mar = c(0,3.5,0,3.5))
    
    #If statement where first condition is for all conditions, second is for index conditions
    if(input$appMode == "All Conditions"){
      visIgraph(induced_subgraph(net(), V(net())[group %in% input$typeStrat]))
    }else{
      visIgraph(induced_subgraph(indexNet(), V(indexNet())[group %in% input$typeStrat])) %>%
        visIgraphLayout(layout = "layout_in_circle")
    }
  }
  #Plot parameters
  # width = 800, height = 800, res = 200
  )
  
  #EdgeList output
  output$edgeList <- DT::renderDataTable({
    
    if(input$appMode == "All Conditions"){
      EL() %>%
        select(from,to,fromCount,toCount,combinedCount,relativeRisk,`Chi Q`) %>%
        rename("Condition 1" = from,
               "Condition 2" = to,
               "Freq. 1" = fromCount,
               "Freq. 2" = toCount,
               "Comorbid Count" = combinedCount,
               "Relative Risk" = relativeRisk,
               "Q value" = `Chi Q`)
    }else{
      indexEL() %>%
        select(from,to,fromCount,toCount,combinedCount,relativeRisk,`Chi Q`) %>%
        rename("Condition 1" = from,
               "Condition 2" = to,
               "Freq. 1" = fromCount,
               "Freq. 2" = toCount,
               "Comorbid Count" = combinedCount,
               "Relative Risk" = relativeRisk,
               "Q value" = `Chi Q`)
    }
    
  })
  
  #VertexList output
  output$vertexList <- DT::renderDataTable({
    if(input$appMode == "All Conditions"){
      VL() %>%
        select(label, value, group) %>%
        mutate(CMI = sapply(1:nrow(VL()),
                            function(x) sum(EL() %>%
                                              filter(from == VL()$label[x] | to == VL()$label[x]) %>%
                                              pull(relativeRisk)))) %>%
        mutate(deg = sapply(1:nrow(VL()),
                            function(x) nrow(EL() %>%
                                               filter(from == VL()$label[x] | to == VL()$label[x])))) %>%
        rename("Condition" = label,
               "Frequency" = value,
               "Disease Type" = group,
               "Comorbidity Index" = CMI)
    }else{
      indexVL() %>%
        select(label, value, group) %>%
        mutate(CMI = sapply(1:nrow(VL()),
                            function(x) sum(EL() %>%
                                              filter(from == VL()$label[x] | to == VL()$label[x]) %>%
                                              pull(relativeRisk)))) %>%
        mutate(deg = sapply(1:nrow(VL()),
                            function(x) nrow(EL() %>%
                                               filter(from == VL()$label[x] | to == VL()$label[x])))) %>%
        rename("Condition" = label,
               "Frequency" = value,
               "Disease Type" = group,
               "Comorbidity Index" = CMI)
    }
  }
  )
  
}
# run it all
shinyApp(ui = ui, server = server)