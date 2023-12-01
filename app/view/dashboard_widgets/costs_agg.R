# app/view/dashboard_widgets/costs_agg.R

box::use(
  dplyr[...],
  shiny[h3, moduleServer, NS, tagList],
  rlang[...],
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
server <- function(id,processed_sim_dat,cost_type,grp="grp") {

  moduleServer(id, function(input, output, session) {

    if(cost_type=='materials'){
      series="mat_total"
      title="Material costs"
    } else if(cost_type=='labor'){
      series="labor_total"
      title="Labor costs"
    }

    output$chart = echarts4r$renderEcharts4r(
      processed_sim_dat() %>%
        # Doesn't have easy ways to ignore NAs when connecting lines so just filter out 0s
        # Use dplyr group_by rather than echarts group_by, latter causes problems
        mutate(selected_cost = !!sym(series)) %>%
        group_by(crop_planting) %>%
        echarts4r$e_chart(x = t) %>%
        echarts4r$e_line(selected_cost) %>%
        #echarts4r$e_x_axis(t) %>%
        echarts4r$e_title(title,"USD, activities aggregated") %>%
        echarts4r$e_tooltip(trigger="axis") %>%
        echarts4r$e_datazoom(show=FALSE) %>%
        echarts4r$e_group(grp)
    )

  })

}
