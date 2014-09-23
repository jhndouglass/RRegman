load("moddat.rdata")
site <- as.list(as.character(moddat$CATCHMENT))

shinyUI(pageWithSidebar(
  # title
  headerPanel(
    list(HTML('<img src="CSF.png"/>'), "   CSF regression model viewer"),
    windowTitle="CSF regresion models shiny app"),

  # sidebar
  sidebarPanel(
    tabsetPanel(
      tabPanel("Main",
        helpText("This is the CSF regression model viewer. It displays predictions
                 of water quality improvements from the third CSF evaluation.
                 Select graphs, tables or maps and then have a play with the options
                 in the sidebar to explore the results.
                 If you're interested you can also model diagnostics and download
                 the R model objects by choosing model, below. Sadly we can't make
                 the training or prediction data sets avialable on the web."),

        radioButtons(
          "choice",
          "Select view",
          choices = c("Graphs" , "Tables", "Maps", "Models")
        ),


        conditionalPanel(
          condition = "input.choice == 'Tables' |
          input.choice == 'Graphs'",
          selectInput(
            "site",
            "Select Target Area",
            choices = site)),

        selectInput(
          "det",
          "Select Determinand",
          choices = c("Orthophosphate", "Total Phosphorus", "Ammoniacal Nitrogen",
                      "TON", "Suspended Solids")),

        conditionalPanel(
          condition = "input.choice == 'Maps'",
          selectInput(
            "sce",
            "Select Scenario to map",
            choices = c("Baseline" = "base.out", "Current" = "curr.out",
                        "Extrapolated" = "extrap.out", "Optimised" = "optim.out",
                        "Maximum CSF" = "max.out",
                        "Maximum agricultural benefit" = "maxben.out",
                        "Headroom" = "head.out",
                        "Current reductions" = "curr.reduct",
                        "Extrapolated reductions" = "extrap.reduct",
                        "Optimised Reductions" = "optim.reduct",
                        "Maximum CSF reductions" = "max.reduct",
                        "Maximum agricultural benefit reductions" = "maxben.reduct",
                        "Headroom reductions" = "head.reduct"))),

        conditionalPanel(
          condition = "input.choice == 'Graphs'",
          checkboxInput(
            "lim",
            "Toggle Environmental Quality Standard on graph"),
          numericInput(
            "eqs",
            "Set Environmental Quality Standard value (mg/l)",
            0)),

        div(style = "margin-top: 25px; width = 50px; ", HTML('<img src="EA.png"/>')),

        div(style = "margin-top: 25px; width = 50px; ", HTML('<img src="NE.png"/>'))
          ),

    tabPanel("Scenarios",
             h5("Baseline"),
             p("This is is predicted concentration (mg/l) of a given pollutant
               as an average ", em("across"), " the target area"),

             h5("Current"),
             p("This is designed to answer the question ‘what is the impact of
               CSF to date on water quality?’.  To estimate this, we model all
               the measures that CSFOs have entered onto the CSF RD until June
               2013, and we will compare this to various different indicators of
               water quality.  We assume that not all measures recommended will
               actually be implemented, and that the likelihood of a measure
               being successfully implemented will vary by measure and by
               catchment.  We use data gathered as part of the ongoing CSF
               audits to determine this relationship."),

             h5("Extrapolated"),
             p("This scenarios aims to simulate the effect of CSFO activity were
               it repeated until all farms within their target areas and
               priority catchments were engaged. Data from the Current activity
               scenario is summarised to average number of measures and
               associated total reduction per farm source within that catchment.
               If a source on a particular type of farm has had no measures
               recommended during the entire period covered by the CSF RD, then
               we assume that there are no reductions from this sector in the
               extrapolated version."),

             h5("Optimised"),
             p("This, as the name implies, is designed to indicate what we think
               is a realistic maximum reduction achievable by CSF.  It involves
               applying the ten most effective measures per pollutant to each
               farm in our catchments and estimating the combined effect of
               these measures.  The likelihood of measure implementation, and
               therefore the likely effectiveness of each measure is based on
               the results of the CSF audits and therefore measures that CSF has
               not been successful in recommending to date will not be
               considered effective in this scenario.  The fact that this
               scenario assumes 100% coverage of CSF in a target area or
               catchment makes this unlikely to happen, however we will use this
               both to identify where concerted CSF effort may deliver real
               water quality benefits.  We will also examine the relationship
               between current CSF activity and the results from this scenario
               as an indicator that CSF has achieved as much as it can in a
               catchment and that it is time to consider targeting elsewhere"),

             h5("Maximum CSF"),
             p("This is the same as optimised – but assumes 100%
               implementation"),

             h5("Maximum Agricultural"),
             p("This scenario applies all measures to all farms, and assumes a
               95% implementation rate for all measures.  This means that each
               farm receives up to fifty measure per pollutant.  We use this
               scenario to estimate the greatest change to water quality
               achievable without catchment scale land use change.  If water
               quality is still worse than the required levels after this
               scenario then we would question whether these levels are
               achievable at all."),

             h5("Headroom"),
             p("Redcues agricultural input to the regression models to zero.
               In some cases this will be very similar to Maximum Agricultural"),

             div(style = "margin-top: 25px; width = 50px; ", HTML('<img src="EA.png"/>')),

             div(style = "margin-top: 25px; width = 50px; ", HTML('<img src="NE.png"/>'))
             )
    )),

  mainPanel(
    conditionalPanel(
      condition = "input.choice == 'Graphs'",
      textOutput("graph_title_c"),
      textOutput("graph_title_a"),
      textOutput("graph_title_b"),
      plotOutput("rect_graph"),
      downloadButton("graph_dl", "Save image")),
    conditionalPanel(
      condition = "input.choice == 'Tables'",
      textOutput("tab_title1c"),
      textOutput("tab_title1a"),
      textOutput("tab_title1b"),
      tableOutput("tab_dat1"),
      textOutput("tab_title2c"),
      textOutput("tab_title2a"),
      textOutput("tab_title2b"),
      tableOutput("tab_dat2"),
      downloadButton("tab_dl", "Download all TA predictions for detminand"),
      downloadButton("datbook_dl", "Download description of columns")),
    conditionalPanel(
      condition = "input.choice == 'Maps'",
      textOutput("map_title_c"),
      textOutput("map_title_a"),
      textOutput("map_title_b"),
      plotOutput("map"),
      downloadButton("map_dl", "Save map")),
    conditionalPanel(
      condition = "input.choice == 'Models'",
      textOutput("mod_title"),
      plotOutput("mod_diag"),
      verbatimTextOutput("mod_summ"),
      downloadButton("mod_dl", "Download R model object"))
  )
)
)
