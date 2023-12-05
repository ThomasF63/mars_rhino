# app/view/raw_outputs.R

box::use(
  DT[DTOutput,renderDT],
  shiny[h3, moduleServer, NS, tagList, selectInput, checkboxInput, checkboxGroupInput, reactive],
  shinyWidgets[pickerInput],
  rlang[...],
  dplyr[...],
  shinycssloaders[withSpinner]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Raw outputs"),

    selectInput(ns("aggs"),
                "Aggregate by:",
                choices=c("Month"="t",
                          "Year"="year",
                          "Cycle"="planting",
                          "Simulation"="scenario",
                          "Crop"="crop"),
                multiple=T),

    DTOutput(ns("raw_dat")) %>% withSpinner()
  )
}

#' @export
server <- function(id,sim_dat) {

  moduleServer(id, function(input, output, session) {

    agged_dat = reactive({
      if(is.null(input$aggs)) {return(sim_dat())}
      else {
        sim_dat() %>%
          mutate(across(t:area_ha,as.factor)) %>%
          group_by_at(vars(input$aggs)) %>%
          summarise(across(is.numeric, sum, na.rm=T),cumul_yield=cumsum(yield),cumul_rev=cumsum(revenue)) %>%
          # recalculate cumulative variables
          return(.)
        }
      })

    # Eventually replace the DT button with a Shiny one, easier to customise and doesn't require client-side rendering
    output$raw_dat = renderDT(agged_dat() %>% select(-any_of(c('crop_planting','timestep'))),
                              extensions = 'Buttons', rownames=T, server=F,
                              options = list(dom = "Blfrtip", buttons=c('colvis','csv')))

  })

}
