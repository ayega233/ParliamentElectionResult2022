#' @import shiny
#' @import httr
#' @import ggplot2
#' @import maps

sweden_counties <- c(
  "Country Level" ="00",
  "Blekinge County" ="10",
  "Dalarna County"="24",
  "Gävleborg County"="25",
  "Gotland County"="09",
  "Halland County"="15",
  "Jämtland County"="27",
  "Jönköping County"="06",
  "Kalmar County"="08",
  "Kronoberg County"="07",
  "Norrbotten County"="29",
  "Skåne County"="14",
  "Skåne South County"="13",
  "Skåne vastra"="12",
  "Stockholm County"="02",
  "Södermanland County"="04",
  "Uppsala County"="03",
  "Värmland County"="21",
  "Västerbotten County"="28",
  "Västernorrland County"="26",
  "Västmanland County"="23",
  "Örebro County"="22",
  "Östergötland County"="05",
  "Västra Götaland - North County"="18",
  "Västra Götaland - South County"="19",
  "Västra Götaland - West County"="17",
  "Västra Götaland - East County"="20"
)

show_count <-10

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
################ server#################
server <- function(input, output) {

  #here output diagram functions
  output$summary_plot <- renderPlot({

    summary <- election_summary()
    lst <- sort(sapply(summary, function(x) x$andel), index.return=TRUE, decreasing=TRUE)
    names <-sapply(summary, function(x) x$partibeteckning)
    results <-sapply(summary, function(x) x$andel)

    plot_data <-  data.frame(list(
      name=names[lst$ix[1:show_count]],
      value=results[lst$ix[1:show_count]]
    ))

    p <- ggplot2::ggplot(data=plot_data)+ (mapping = aes(x = factor(name, level = names),y = value ))+coord_flip()+geom_bar(stat="identity",)
    p <- p+geom_text(label=plot_data$value,hjust = -1,color="red")+ylim(0,50)+liu_theme
    return(p)
  })

  output$summary_data<- renderTable({
    summary <- election_summary()
    lst <- sort(sapply(summary, function(x) x$andel), index.return=TRUE, decreasing=TRUE)
    names <-sapply(summary, function(x) x$partibeteckning)
    results <-sapply(summary, function(x) x$andel)

    plot_data <-  data.frame(list(
      "Party Name"=names[lst$ix[1:show_count]],
      "Total Votes"=sapply(summary, function(x) x$antal)[lst$ix[1:show_count]],
      "Vote Percentage"=results[lst$ix[1:show_count]]
    ))
    return(plot_data)
  }, rownames = TRUE)

  output$county_plot <-renderPlot({
    if (input$constituency != '')  {
      country_summary <- election_result(input$constituency)
      lst <- sort(sapply(country_summary, function(x) x$andel), index.return=TRUE, decreasing=TRUE)

      names <-sapply(country_summary, function(x) x$partibeteckning)
      results <-sapply(country_summary, function(x) x$andel)

      plot_data <-  data.frame(list(
        name=names[lst$ix[1:show_count]],
        value=results[lst$ix[1:show_count]]
      ))

      p <- ggplot2::ggplot(data=plot_data)+ (mapping = aes(x = factor(name, level = names),y = value ))+coord_flip()+geom_bar(stat="identity",)
      p <- p+geom_text(label=plot_data$value,hjust = -1,color="red")+ylim(0,50)+liu_theme
      return(p)
    }

  })

  output$county_data<- renderTable({
    if (input$data_constituency != '')  {
      country_summary <- election_result(input$data_constituency)
      lst <- sort(sapply(country_summary, function(x) x$andel), index.return=TRUE, decreasing=TRUE)
      names <-sapply(country_summary, function(x) x$partibeteckning)
      results <-sapply(country_summary, function(x) x$andel)

      plot_data <-  data.frame(list(
        "Party Name"=names[lst$ix[1:show_count]],
        "Total Votes"=sapply(country_summary, function(x) x$antal)[lst$ix[1:show_count]],
        "Vote Percentage"=results[lst$ix[1:show_count]]
      ))
      return(plot_data)
    }
  }, rownames = TRUE)

}

####out put
shinyApp(ui = ui, server = server)
