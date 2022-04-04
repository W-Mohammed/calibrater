################################################################################
#
# Script Name:        ui.R
# Module Name:        WB and DHS demo app
# Script Description: Defines the UI part of the healthsites api example
#                     shiny app.
# Author:             Wael Mohammed
# Organisation:       Dark Peak Analytics
# Email:              wmohammed@darkpeakanalytics.com
#
################################################################################

# Ground work: ----
dark <- bslib::bs_theme(bg = "black", fg = "white", primary = "purple")
countries <- sort(spData::world$name_long)

# UI: ----
ui <- fluidPage(
  titlePanel(h5("Example app to showcase API package functionality")),
  theme = dark,
  waiter::use_waiter(),
  fluidRow(
    column(
      width = 2, offset = 0,
      shinyWidgets::prettySwitch("light_mode", "Light mode"),
    ),
    column(
      width = 2, offset = 0,
      selectInput(
        inputId = "add_country",
        label = "Select to add:",
        choices = countries,
        selected = "Kenya",
        selectize = TRUE)
    ),
    column(
      width = 1, offset = 0, style = "margin-top: 40px;",
      actionButton(
        inputId = "add",
        label = "Add")
    ),
    column(
      width = 2, offset = 0,
      uiOutput(
        outputId = "remove_list")
    ),
    column(
      width = 1, offset = 0, style = "margin-top: 40px;",
      uiOutput(
        outputId = "remove_button")
    ),
    column(
      width = 2, offset = 0,
      uiOutput(
        outputId = "api_key_ui")
    )
  ),
  fluidRow(
    column(
      width = 12, offset = 0,
      navlistPanel(
        id = "countries",
        widths = c(2, 10),
        tabPanel(
          title = "World",
          tagList(
            "Please choose a country and click \"Add\" to query the api",
            bslib::navs_pill_card(
              id = "world_pill_card",
              navbarMenu(
                title = "Health facilities", menuName = "Extras",
                bslib::nav(title = "Map",
                           uiOutput(outputId = "world_mapUI")
                ),
                bslib::nav(title = "Statistics",
                           uiOutput(outputId = "world_statsUI")
                )
              )
            )
          )
        )
      )
    )
  )
)

