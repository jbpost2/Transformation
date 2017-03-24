###########################################################################
##R Shiny App to look at transformations
##Justin Post - Fall 2016
###########################################################################

#Load package
library(shiny)
library(shinydashboard)
library(plotly)

dashboardPage(skin="red",
  dashboardHeader(title="Visualization of a Transformation of a Random Variable",titleWidth=750),
  
  #define sidebar items
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("archive")),
    menuItem("Application", tabName = "app", icon = icon("laptop"))
  )),

  #define the body of the app
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "about",
        fluidRow(
          #add in latex functionality if needed
          withMathJax(),
              
          #two columns for each of the two items
          column(6,
            #Description of App
            h1("What does this app do?"),
            #box to contain description
            box(background="red",width=12,
              h4("This applet is intended to help visualization of a transformation of a random variable using the CDF method.  For many, the first intuition in trying to find the distribution of a transformation is to plug the transformation directly into the PDF.  The goal of this applet is to show that is not the case!"),
              h4("Specifically, suppose \\(X\\sim Gamma(\\alpha.\\lambda)\\).  (\\(\\alpha\\) is the 'shape' parameter and \\(\\lambda\\) is the 'rate' parameter.)  We want to know the distribution of \\(Y=1/X\\)."), 
              h4("The distribuiton of \\(Y\\) is known to be an Inverse Gamma distribution with parameters \\(\\alpha\\) and \\(1/\\lambda\\)."),
              h4("The pdfs are given by"),
              h4("\\(f_X(x)=\\frac{\\lambda^{\\alpha}}{\\Gamma(\\alpha)}x^{\\alpha-1}e^{-\\lambda x}~~~~~\\mbox{ for }x>0\\)"),
              h4("and"),
              h4("\\(f_Y(y)=\\frac{\\lambda^{\\alpha}}{\\Gamma(\\alpha)}y^{-\\alpha-1}e^{-\\lambda /y}~~~~~\\mbox{ for }y>0\\)"),
              h4("The relationship between the two random variables can be described using CDFs"),
              h4("\\(F_Y(y)=P(Y\\leq y)=P(1/X\\leq y) = P(X > 1/y) = 1-F_X(1/y)\\)"),
              h4("The purpose of this applet is to show this last relationship graphically.")
            )
          ),
              
          column(6,
            #How to use the app
            h1("How to use the app?"),
            #box to contain description
            box(background="red",width=12,
              h4("The controls for the app are located on the left and the visualizations are given on the right."),
              h4("The boxes on the top right allow the user to change the parameters of the Gamma distribution."),
              h4("The top row of graphs display the corresponding PDF and CDF for this Gamma distribution."),
              h4("On the bottom left, there is a slider bar with play button that can be used to visualize the transformation process via the CDF.  As the slider bar moves to the right, the value of \\(y\\) in \\(F_Y(y)=P(Y\\leq y)\\) increases.  This corresponds to \\(F_Y(y)=P(X>1/y)\\)."),
              h4("The PDF and CDf of Y (Inverse Gamma distribution) are given by the bottom two graphs.  The appropriate area is shown corresponding to \\(F_Y(y)\\) with regard to the slider.")
            )
          )
        )
      ),
      
      #actual app layout      
      tabItem(tabName = "app",  
        column(width=3,
          fluidRow(
            box(width=12,
              title="Parameters of the Gamma distribution",
              background="red",
              solidHeader=TRUE,
              h5("(Set to 1 if blank.)"),
              numericInput("alpha",label=h5("Alpha Value (> 0, 'Shape')"),value=1,min=0,step=0.25),
              numericInput("lambda",label=h5("Lambda Value (> 0, 'Rate')"),value=1,min=0,step=0.25)
            ),
            box(width=12,background="red",
              sliderInput(label="Animate", inputId="Animate", min = 0, max = 100, value = 0, step = 1,animate=list(TRUE, interval=500,loop=FALSE))
            )
          )
        ),
        column(width=9,
          fluidRow(
            box(width=6,
              plotlyOutput(outputId = "gammaPDF")
            ),
            box(width=6,
              plotlyOutput(outputId = "gammaCDF")
            ) 
          ),
          fluidRow(
            box(width=6,
              plotlyOutput(outputId = "invgammaPDF")
            ),
            box(width=6,
              plotlyOutput(outputId = "invgammaCDF")
            )
          )
        )
      )
    )
  )
)


