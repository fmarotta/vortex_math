library(shiny)
library(ggplot2)
library(ggforce)

mod <- function(a, b) a %% b

ui <- fluidPage(
    fluidRow(
        column(
            4,
            sliderInput(
                inputId = "n",
                label = "Choose n",
                min = 2,
                max = 32,
                value = 2
            ),
            sliderInput(
                inputId = "mod",
                label = "Choose mod",
                min = 2,
                max = 2048,
                value = 9
            )
        ),
        column(
            8,
            plotOutput("plot")
        )
    )
)

server <- function(input, output) {
    output$plot <- renderPlot({
        d <- data.frame(
            theta = cumsum(rep(2*pi/(input$mod), input$mod)),
            label = as.character(1:input$mod)
        )
        d$y <- cos(d$theta)
        d$x <- sin(d$theta)

        p <- ggplot(d, aes(x, y)) +
            geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = F) +
            geom_point() +
            geom_label(aes(label = label)) +
            coord_fixed() +
            theme_void() +
            scale_color_viridis_c()# +
            #theme(legend.pos = "none")

        i <- 0
        pool <- c()
        nmod <- input$n %% input$mod
        while (i < input$mod - 1) {
            i <- i + 1
            if (i %in% pool)
                next()
            m <- vector(mode = "integer", length = input$mod)
            m[1] <- i
            j <- 1
            while (!(m[j] %in% pool) && !(m[j] %in% m[-j])) {
                m[j+1] <- (m[j] * nmod) %% input$mod
                j <- j + 1
            }
            m <- m[1:j]
            pool <- unique(c(pool, m))
            m[m == 0] <- input$mod
            p <- p + geom_path(aes(color = atan2(x, y)), data = d[m, ])
        }
        p
    })
}

shinyApp(ui = ui, server = server)
