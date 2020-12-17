#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rethinking)
library(splines)

load(file = 'sea_level.rda')

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Fun with basis splines"),

    sidebarLayout(
        sidebarPanel(
            fileInput(
                "rds_in"
                , "Pick an .rds file"
                , multiple = FALSE
                , accept = ".rds"
            )
            , selectInput(
                "time_index"
                , "Select the time index"
                , choices = NULL
            )
            , selectInput(
                "response_variable"
                , "Select the response variable"
                , choices = NULL
            )
            , sliderInput(
                "num_knots"
                , "Number of knots:"
                , min = 3
                , max = 10
                , value = 5
            )
            # sliderInput(
            #   ""
            # ),
            , p(
                "American Academy of Actuaries, Canadian Institute of Actuaries, Casualty Actuarial Society and Society of Actuaries. Actuaries Climate Index. http://actuariesclimateindex.org (accessed Dec. 15, 2020)"
            )
        ),

        mainPanel(
           plotOutput("out_plot")
           , tableOutput("tbl_time_series")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    tbl_time_series <- reactiveVal()
    
    output$tbl_time_series <- renderTable({
        req(input$rds_in)
        
        tbl_time_series(readRDS(input$rds_in$datapath))
        
        max_knots <- max(10, nrow(tbl_time_series()) - 10)
        
        updateSliderInput(session, "num_knots", max = max_knots)
        updateSelectInput(session, "time_index", choices = names(tbl_time_series()))
        updateSelectInput(session, "response_variable", choices = names(tbl_time_series()))
        
        tbl_time_series()
    })
    
    output$out_plot <- renderPlot({
        
        req(input$rds_in)
        
        tbl_mojo <- tbl_time_series()
        
        knot_list <- quantile(
            tbl_mojo[[input$time_index]]
            , probs = seq(0, 1, length.out = input$num_knots)
        )

        B = bs(
            tbl_mojo[[input$time_index]]
            , knots = knot_list %>% head(-1) %>% tail(-1)
            , degree = 3
            , intercept = TRUE
        )

        mdl_sea_level <- quap(
            alist(
                response ~ dnorm(mu, sigma),
                mu <- a + B %*% w,
                a ~ dnorm(0.5, 0.5),
                w ~ dnorm(0, 10),
                sigma ~ dexp(1)
            )
            , data = list(response = tbl_mojo[[input$response_variable]], B=B)
            , start = list(w = rep(0, ncol(B)))
        )

        post <- extract.samples(mdl_sea_level)
        w <- apply(post$w, 2, mean)


        mu <- link(mdl_sea_level)
        mu_PI <- apply(mu, 2, PI, 0.97)
        plot(
            tbl_mojo[[input$time_index]]
            , tbl_mojo[[input$response_variable]]
            , col = col.alpha(rangi2, 0.3)
            , pch = 16
            , type = "l"
        )
        shade(
            mu_PI
            , tbl_mojo[[input$time_index]]
            , col = col.alpha("black", 0.5)
        )
    })

}

shinyApp(ui = ui, server = server)
