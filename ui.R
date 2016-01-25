# pkgTest <- function(x) {
#     if (!require(x,character.only = TRUE)) {
#         install.packages(x,dep=TRUE)
#         if(!require(x,character.only = TRUE)) {
#             stop("Package not found")
#         }
#     }
# }
require("shiny")
require("ggplot2")
require("gridExtra")
#pkgTest("shiny")
#pkgTest("ggplot2")
#pkgTest("gridExtra")

# Film/noFilm temp data in aberystwyth may be the wromg way round

set.list <- list('Temperature', 'Moisturer(WV)')
place.list <- list('Location 1', 'Location 2')
FNF.list <- list('Treatment', 'No Treatment')


shinyUI(fluidPage(
    
    title = "Seedling Trial Data",
    
    plotOutput('plot2'),
    plotOutput('plot'),
    
    fluidRow(
        column(3,
            h4("Data Specification"),
            selectInput('Data.place', 'Loci 1 or Loci 2', place.list, place.list[1]),
            selectInput('Data.FNF', 'Treatment or No Treatment', FNF.list, FNF.list[1])
            
        ),
        column(4,
            h4("Graph"),
            checkboxInput('Poly', 'Add 5th order polynomial', value = F),
            checkboxInput('Points', 'Add Points', value = T),
            dateRangeInput("Dates", "Range", start = "2013-05-06", end = "2013-09-01", min = "2013-03-01",
                           max = "2015-05-01", format = "yyyy-mm-dd", startview = "month",
                           weekstart = 0, language = "en", separator = " to "),
            radioButtons("Meanpoints", label = "Mean Points (Rolling)",
                         choices = list("1h Data" = 'a', "2h Mean" = 'b', "3h Mean" = 'c', "4h Mean" = 'd',
                                        "6h Mean" = 'e', "12h Mean" = 'f', "24h Mean" = 'g'),selected = 'g')
            
        ),
        column(5,
            h4("Extras"),
            checkboxInput('Color', 'Color Points', value = F),
            checkboxInput('SowingDates', 'Show sowings & Biomass Index Comparison', value = T),
            actionButton("makeG2", "Copy Graph to Top Graph"),
            hr(),
            br()
#             h4("Saving"),
#             textInput("name", 'Name to save graph as', value = "Test_Graph.wmf"),
#             actionButton("save", label = "Save")

    )
)))
