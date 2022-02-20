library(shiny)
library(ggplot2)
library(ggforce)
library(RColorBrewer)

options(shiny.port = 2711)

mod <- function(a, b) a %% b

col_vector <- grDevices::colors()[grep('(gr(a|e)y)|(white)', grDevices::colors(), invert = T)]

ui <- fluidPage(
    fluidRow(
        column(
            4,
            sliderInput(
                inputId = "n",
                label = "Choose the multiplier",
                min = 2,
                max = 32,
                value = 2
            ),
            sliderInput(
                inputId = "mod",
                label = "Choose the modulus",
                min = 2,
                max = 2048,
                value = 9
            ),
            checkboxInput(
                inputId = "arrows",
                label = "Arrows?",
                value = F
            ),
            selectInput(
                inputId = "colorBy",
                label = "Color by...",
                choices = list("Nothing", "Segment angle", "Segment length", "Path"),
                selected = "Nothing"
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
            if (input$colorBy != "Path") scale_color_viridis_c() else scale_color_viridis_d()# +
            #theme(legend.pos = "none")

        i <- 0
        pool <- c()
        nmod <- input$n %% input$mod
        q <- 1
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
            if (input$colorBy == "Segment angle")
                p <- p + geom_path(
                    aes(color = atan2(x, y)),
                    data = d[m, ],
                    arrow = if (input$arrows) grid::arrow() else NULL
                )
            else if (input$colorBy == "Segment length") {
                dd <- d[m, ]
                dd$dist <- sqrt(
                    (dd$x[c(2:nrow(dd), nrow(dd))] - dd$x)^2 +
                    (dd$y[c(2:nrow(dd), nrow(dd))] - dd$y)^2
                )
                p <- p + geom_path(
                    aes(color = dist),
                    data = dd,
                    arrow = if (input$arrows) grid::arrow() else NULL
                )
            } else if (input$colorBy == "Path") {
                dd <- d[m, ]
                dd$path = q
                q <- q + 1
                p <- p + geom_path(
                    aes(color = as.character(path)),
                    data = dd,
                    arrow = if (input$arrows) grid::arrow() else NULL
                )
            } else if (input$colorBy == "Nothing") 
                p <- p + geom_path(
                    data = d[m, ],
                    color = "black",
                    arrow = if (input$arrows) grid::arrow() else NULL
                )
        }
        p
    })
}

shinyApp(ui = ui, server = server)
