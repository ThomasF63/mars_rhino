# app/view/dashboard_widgets/density_ts.R

box::use(
  dplyr[...],
  shiny[h3, moduleServer, NS, tagList],
  echarts4r,
)
box::use(
  app/logic/kpis,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    echarts4r$echarts4rOutput(ns("chart"))
  )
}

#' @export
server <- function(id,processed_sim_dat) {

  moduleServer(id, function(input, output, session) {

    output$chart = echarts4r$renderEcharts4r(
      processed_sim_dat() %>%
        # Doesn't have easy ways to ignore NAs when connecting lines so just filter out 0s
        # Use dplyr group_by rather than echarts group_by, latter causes problems
        group_by(crop_planting) %>%
        echarts4r$e_chart(x = timestep) %>%
        echarts4r$e_line(density, symbol='none') %>%
        echarts4r$e_x_axis(min=1,max=max(processed_sim_dat()$timestep)) %>%
        echarts4r$e_title("Crop Density","Plants/ha") %>%
        echarts4r$e_tooltip() %>%
        echarts4r$e_datazoom(show=FALSE) %>%
        echarts4r$e_group("grp")
    )

  })

}
