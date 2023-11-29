# app/view/dashboard_widgets/revenue_ts.R

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
server <- function(id,processed_sim_dat,grp="grp") {

  moduleServer(id, function(input, output, session) {

    output$chart = echarts4r$renderEcharts4r(
      processed_sim_dat() %>%
        # Doesn't have easy ways to ignore NAs when connecting lines so just filter out 0s
        # Use dplyr group_by rather than echarts group_by, latter causes problems
        filter(revenue > 0) %>%
        group_by(crop_planting) %>%
        echarts4r$e_chart(x = t) %>%
        echarts4r$e_line(revenue) %>%
        #echarts4r$e_x_axis(t) %>%
        echarts4r$e_title("Revenue","USD - points show harvest events") %>%
        echarts4r$e_tooltip(trigger="axis") %>%
        echarts4r$e_datazoom(show=FALSE) %>%
        echarts4r$e_group(grp)
    )

  })

}
