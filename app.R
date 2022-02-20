library(shiny)
library(ggplot2)
library(ggforce)
library(data.table)

mod <- function(a, b) a %% b

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
    p <- reactive({
        d <- data.table(
            theta = cumsum(rep(2*pi/(input$mod), input$mod)),
            label = as.character(1:input$mod),
            path = 0,
            dist = 0
        )
        d$y <- cos(d$theta)
        d$x <- sin(d$theta)

        i <- 0
        q <- 1
        pool <- c()
        nmod <- input$n %% input$mod
        e <- data.table()
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
            e <- rbind(e, d[m][, path := q][, dist := sqrt(
                (x[c(2:nrow(.SD), nrow(.SD))] - x)^2 +
                (y[c(2:nrow(.SD), nrow(.SD))] - y)^2
            )])
            q <- q + 1
        }

        ggplot(e, aes(x, y)) +
            geom_circle(aes(x0 = 0, y0 = 0, r = 1), data = d, inherit.aes = FALSE) +
            geom_point(data = d) +
            geom_label(aes(label = label), data = d) +
            coord_fixed() +
            theme_void()
    })

    output$plot <- renderPlot({
        if (input$colorBy == "Segment angle")
            p <- p() + geom_path(
                aes(color = atan2(x, y), group = path),
                arrow = if (input$arrows) grid::arrow() else NULL
            ) +
            scale_color_viridis_c() +
            theme(legend.pos = "none")
        else if (input$colorBy == "Segment length")
            p <- p() + geom_path(
                aes(color = dist, group = path),
                arrow = if (input$arrows) grid::arrow() else NULL
            ) +
            scale_color_viridis_c() +
            theme(legend.pos = "none")
        else if (input$colorBy == "Path")
            p <- p() + geom_path(
                aes(color = as.factor(path), group = path),
                arrow = if (input$arrows) grid::arrow() else NULL
            ) +
            scale_color_viridis_d(name = "Path")
        else if (input$colorBy == "Nothing")
            p <- p() + geom_path(
                aes(group = path),
                color = "black",
                arrow = if (input$arrows) grid::arrow() else NULL
            )
        p
    })
}

shinyApp(ui = ui, server = server)
