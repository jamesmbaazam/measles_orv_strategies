library('shiny')

ui <- navbarPage(
    "ORV",
    tabPanel(
        "Intervention day",
        sidebarLayout(
            sidebarPanel(
                sliderInput("vaccine_target", "target:", 0.7,
                            min = 0, max = 1
                ),
                sliderInput("vaccine_efficacy", "efficacy:", 0.9,
                            min = 0, max = 1
                ),
                numericInput("intervention_length", "duration:", 14,
                             min = 1, max = 100
                ),
                numericInput("N", "N:", 1E5,
                             min = 1E2, max = 1E6
                ),
                numericInput("mtime", "endtime:", 120,
                             min = 10, max = 3 * 365
                ),
                sliderInput("R", "R",
                            min = 1, max = 20, value = 4
                ),
                sliderInput("IP", "Infectious period (days)", 5,
                            min = 1, max = 100
                ),
                sliderInput("LP", "Latent period (days):", 8,
                            min = 1, max = 100
                )
            ),
            mainPanel(plotOutput("plot1"))
        )
    ),
    tabPanel(
        "R sensitivty",
        sidebarLayout(
            sidebarPanel(
                sliderInput("R", "R",
                            min = 1, max = 20, value = 4
                ),
                numericInput("pm", "+/-:", 0.5,
                             min = 1, max = 10
                ),
                sliderInput("vaccine_target", "target:", 0.7,
                            min = 0, max = 1
                ),
                sliderInput("vaccine_efficacy", "efficacy:", 0.9,
                            min = 0, max = 1
                ),
                numericInput("intervention_length", "duration:", 14,
                             min = 1, max = 100
                ),
                numericInput("N", "N:", 1E5,
                             min = 1E2, max = 1E6
                ),
                numericInput("mtime", "endtime:", 120,
                             min = 10, max = 3 * 365
                ),
                sliderInput("IP", "Infectious period (days)", 5,
                            min = 1, max = 100
                ),
                sliderInput("LP", "Latent period (days):", 8,
                            min = 1, max = 100
                )
            ),
            mainPanel(plotOutput("plot2"))
        )
    ),
    tabPanel(
        "Duration sensitivity",
        sidebarLayout(
            sidebarPanel(
                numericInput("intervention_length", "duration:", 14,
                             min = 1, max = 100
                ),
                numericInput("pm2", "+/-:", 7,
                             min = 1, max = 21
                ),
                sliderInput("vaccine_target", "target:", 0.7,
                            min = 0, max = 1
                ),
                sliderInput("vaccine_efficacy", "efficacy:", 0.9,
                            min = 0, max = 1
                ),
                sliderInput("R", "R",
                            min = 1, max = 20, value = 4
                ),
                numericInput("N", "N:", 1E5,
                             min = 1E2, max = 1E6
                ),
                numericInput("mtime", "endtime:", 120,
                             min = 10, max = 3 * 365
                ),
                sliderInput("IP", "Infectious period (days)", 5,
                            min = 1, max = 100
                ),
                sliderInput("LP", "Latent period (days):", 8,
                            min = 1, max = 100
                )
            ),
            mainPanel(plotOutput("plot3"))
        )
    ),
    tabPanel(
        "Cover sensitivty",
        sidebarLayout(
            sidebarPanel(
                sliderInput("vaccine_target", "target:", 0.7,
                            min = 0, max = 1
                ),
                numericInput("pm3", "+/-:", 0.1,
                             min = 0, max = .9
                ),
                sliderInput("vaccine_efficacy", "efficacy:", 0.9,
                            min = 0, max = 1
                ),
                numericInput("intervention_length", "duration:", 14,
                             min = 1, max = 100
                ),
                sliderInput("R", "R",
                            min = 1, max = 20, value = 4
                ),
                numericInput("N", "N:", 1E5,
                             min = 1E2, max = 1E6
                ),
                numericInput("mtime", "endtime:", 120,
                             min = 10, max = 3 * 365
                ),
                sliderInput("IP", "Infectious period (days)", 5,
                            min = 1, max = 100
                ),
                sliderInput("LP", "Latent period (days):", 8,
                            min = 1, max = 100
                )
            ),
            mainPanel(plotOutput("plot4"))
        )
    ),
    tabPanel(
        "Retrospective analysis",
        sidebarLayout(
            sidebarPanel(
                numericInput("day", "Start day:", 60,
                             min = 10, max = 3 * 365
                ),
                sliderInput("vaccine_target", "target:", 0.7,
                            min = 0, max = 1
                ),
                sliderInput("vaccine_efficacy", "efficacy:", 0.9,
                            min = 0, max = 1
                ),
                numericInput("intervention_length", "duration:", 14,
                             min = 1, max = 100
                ),
                sliderInput("R", "R",
                            min = 1, max = 20, value = 4
                ),
                numericInput("N", "N:", 1E5,
                             min = 1E2, max = 1E6
                ),
                numericInput("mtime", "endtime:", 120,
                             min = 10, max = 3 * 365
                ),
                sliderInput("IP", "Infectious period (days)", 5,
                            min = 1, max = 100
                ),
                sliderInput("LP", "Latent period (days):", 8,
                            min = 1, max = 100
                )
            ),
            mainPanel(plotOutput("plot5"))
        )
    ),
    tabPanel("Summary")
)

server <- function(input, output) {
    source('../scripts/epi_functions.r')
    output$plot1 <- renderPlot({
        out <- p_red(R = input$R, input$vaccine_efficacy, input$vaccine_target, input$intervention_length, input$mtime, input$LP, input$IP, input$N, step = 1)
        plot(out)
    })
    
    output$plot2 <- renderPlot({
        R2 <- c(input$R - input$pm, input$R, input$R + input$pm)
        R2[R2 < 0] <- 0
        out2 <- R_compare(R = R2, input$vaccine_efficacy, input$vaccine_target, input$intervention_length, input$mtime, input$LP, input$IP, input$N, step = 1)
        plot(out2)
    })
    
    output$plot3 <- renderPlot({
        il <- c(
            input$intervention_length - input$pm2,
            input$intervention_length, input$intervention_length + input$pm2
        )
        il[il < 0] <- 0
        out3 <- Int_compare(R = input$R, input$vaccine_efficacy, input$vaccine_target, il, input$mtime, input$LP, input$IP, input$N, step = 1)
        plot(out3)
    })
    
    output$plot4 <- renderPlot({
        vt <- c(
            input$vaccine_target - input$pm3,
            input$vaccine_target, input$vaccine_target + input$pm3
        )
        vt[vt < 0] <- 0
        vt[vt > 1] <- 1
        out4 <- Vacc_compare(R = input$R, input$vaccine_efficacy, vt, input$intervention_length, input$mtime, input$LP, input$IP, input$N, step = 1)
        plot(out4)
    })
    
    output$plot5 <- renderPlot({
        out5 <- retro(R = input$R, day = input$day, input$vaccine_efficacy, input$vaccine_target, input$intervention_length, input$mtime, input$LP, input$IP, input$N)
        plot(out5)
    })
}


shinyApp(ui, server)