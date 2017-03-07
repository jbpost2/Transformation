###########################################################################
##R Shiny App to look at transformations
##Justin Post - Fall 2016
###########################################################################

#Load package
library(shiny)
library(shinydashboard)
library(plotly)

# Define UI for application that draws the prior and posterior distributions given a value of y
dashboardPage(skin="red",
    dashboardHeader(title="Visualization of a Transformation of a Random Variable",titleWidth=1000),
    dashboardSidebar(disable = TRUE),
    dashboardBody(withMathJax(),
        column(
            width=3,
            box(background="red",width=12,title="Description of Example",solidHeader=TRUE,
                h5("This applet is intended to help visualization of a transformation of a random variable."),
                h5("Specifically, suppose \\(X\\sim gamma(\\alpha.\\lambda)\\).  (\\(\\alpha\\) is the 'shape' parameter and \\(\\lambda\\) is the 'rate' parameter.)  We want to know the distribution of \\(Y=1/X\\)."), 
                h5("The distribuiton of \\(Y\\) is known to be an inverse gamma distribution with parameters \\(\\alpha\\) and \\(1/\\lambda\\)."),
                h5("The pdfs are given by"),
                h5("\\(f_X(x)=\\frac{\\lambda^{\\alpha}}{\\Gamma(\\alpha)}x^{\\alpha-1}e^{-\\lambda x}~~~~~\\mbox{ for }x>0\\)"),
                h5("and"),
                h5("\\(f_Y(y)=\\frac{\\lambda^{\\alpha}}{\\Gamma(\\alpha)}y^{-\\alpha-1}e^{-\\lambda /y}~~~~~\\mbox{ for }y>0\\)"),
                h5("The relationship between the two random variables can be described using CDFs"),
                h5("\\(F_Y(y)=P(Y\\leq y)=P(1/X\\leq y) = 1-F_X(1/y)\\)")
            ),
            box(width=12,
                title="Parameters of the Gamma distribution",
                background="red",
                solidHeader=TRUE,
                h5("(Set to 1 if blank.)"),
                numericInput("alpha",label=h5("Alpha Value (> 0, 'Shape')"),value=1,min=0,step=0.25),
                numericInput("lambda",label=h5("Lambda Value (> 0, 'Rate')"),value=1,min=0,step=0.25)
            ),
            box(width=12,background="red",
                sliderInput(label="Animate", inputId="Animate", 
                            min = 0, max = 100, value = 0, step = 1,
                            animate=list(TRUE, interval=500,loop=FALSE))
            )
        ),
        column(width=9,
            fluidRow(
                  box(
                   width=6,
                   plotlyOutput(outputId = "invgammaPDF")
                 ),
                 box(
                   width=6,
                   plotlyOutput(outputId = "invgammaCDF")
                 ) 
               ),
            fluidRow(
                box(
                    width=6,
                    plotlyOutput(outputId = "gammaPDF")
                ),
                box(
                  width=6,
                  plotlyOutput(outputId = "gammaCDF")
                )
                
            )
        )
    )
)

