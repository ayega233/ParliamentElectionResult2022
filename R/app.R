#' @import shiny
#' @import httr
#' @import ggplot2
#' @import maps

sweden_counties <- c(
  "Country Level" ="00",
  "Blekinge County" ="10",
  "Dalarna County"="24",
  "G\u00E4vleborg County"="25",
  "Gotland County"="09",
  "Halland County"="15",
  "J\u00E4mtland County"="27",
  "J\u00F6nk\u00F6ping County"="06",
  "Kalmar County"="08",
  "Kronoberg County"="07",
  "Norrbotten County"="29",
  "Sk\u00E5ne County"="14",
  "Sk\u00E5ne South County"="13",
  "Sk\u00E5ne vastra"="12",
  "Stockholm County"="02",
  "S\u00F6dermanland County"="04",
  "Uppsala County"="03",
  "V\u00E4rmland County"="21",
  "V\u00E4sterbotten County"="28",
  "V\u00E4sternorrland County"="26",
  "V\u00E4stmanland County"="23",
  "\u00D6rebro County"="22",
  "\u00D6sterg\u00F6tland County"="05",
  "V\u00E4stra G\u00F6taland - North County"="18",
  "V\u00E4stra G\u00F6taland - South County"="19",
  "V\u00E4stra G\u00F6taland - West County"="17",
  "V\u00E4stra G\u00F6taland - East County"="20"
)

show_count <-9

####UI part #############
ui <- fluidPage(
  tags$head(

    tags$style(HTML("
      .shiny-table {
        width: 800px !important;
      }"))
  ),
  titlePanel("Parliament Election in Sweden 2022"),
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("Summary",
               plotOutput(outputId = "summary_plot",height = "400px",
                          width = "800px"),
               tableOutput("summary_data")
      ),
      tabPanel("Graph",
               selectInput("constituency", "Select Parliamentary Constituency:",
                           sweden_counties),

               plotOutput(outputId = "county_plot",height = "400px",
                          width = "800px")
      ),
      tabPanel("Data",
               selectInput("data_constituency", "Select Parliamentary Constituency:",
                           sweden_counties),
               tableOutput("county_data"),
      ),
      tabPanel(
        "Reference",
        tags$p(
          "There data were obtained from",
          tags$a("IMDB", href = "http://www.imdb.com/"), "and",
          tags$a("Rotten Tomatoes", href = "https://www.rottentomatoes.com/"), "."
        )
      )
    )

  )
)

liu_theme <- theme(
  plot.title =element_text(colour = "#6a7e91",size = 12),
  axis.title.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text = element_text(size = 14),
  plot.margin = margin(1, 1, 1, 1, "cm"),
  aspect.ratio=0.5,
  panel.grid.major = element_line(colour = "grey80"),
  panel.background = element_rect(
    fill = "white",
    colour = "black",
    linewidth = 1
  )
)

getTableData <- function(data){

  lst <- sort(sapply(data, function(x) x$andel), index.return=TRUE, decreasing=TRUE)
  names <-sapply(data, function(x) x$partibeteckning)
  results <-sapply(data, function(x) x$andel)

  table_data <-  data.frame(list(
    "Party Name"=names[lst$ix[1:show_count]],
    "Total Votes"=sapply(data, function(x) x$antal)[lst$ix[1:show_count]],
    "Vote Percentage"=results[lst$ix[1:show_count]]
  ))
  return(table_data)
}


getPlot <- function(data){
  lst <- sort(sapply(data, function(x) x$andel), index.return=TRUE, decreasing=TRUE)
  names <-sapply(data, function(x) x$partibeteckning)
  results <-sapply(data, function(x) x$andel)
  p_names<-names[lst$ix[1:show_count]]
  Party_name <-list()
  Vote_percentage<-list()
  plot_data <-  data.frame(list(
    Party_name=p_names,
    Vote_percentage=results[lst$ix[1:show_count]]
  ))

  p <- ggplot2::ggplot(data=plot_data)+ (mapping = aes(x = factor(Party_name, levels = names),y = Vote_percentage ))+coord_flip()+geom_bar(stat="identity",)
  p <- p+geom_text(label=plot_data$Vote_percentage,hjust = -1,color="red")+ylim(0,50)+liu_theme
  return(p)
}
################ server#################
server <- function(input, output) {

  #here output diagram functions
  output$summary_plot <- renderPlot({
    return(getPlot(election_summary()))
  })

  output$summary_data<- renderTable({
    return(getTableData(election_summary()))
  }, rownames = TRUE)

  output$county_plot <-renderPlot({
    if (input$constituency != '')  {
      return(getPlot(election_result(input$constituency)))
    }
  })

  output$county_data<- renderTable({
    if (input$data_constituency != '')  {
      return(getTableData(election_result(input$data_constituency)))
    }
  }, rownames = TRUE)

}

####out put
shinyApp(ui = ui, server = server)
