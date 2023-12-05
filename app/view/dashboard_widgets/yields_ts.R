# app/view/dashboard_widgets/yields_ts.R

box::use(
  dplyr[...],
  shiny[h3, moduleServer, NS, tagList],
  echarts4r,
  shinycssloaders[withSpinner]
)
box::use(
  app/logic/kpis,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    echarts4r$echarts4rOutput(ns("chart")) %>% withSpinner()
  )
}

#' @export
server <- function(id,processed_sim_dat,show_timeline=F,grp="grp") {

  moduleServer(id, function(input, output, session) {

    output$chart = echarts4r$renderEcharts4r(
      processed_sim_dat() %>%
        # Doesn't have easy ways to ignore NAs when connecting lines so just filter out 0s
        # Use dplyr group_by rather than echarts group_by, latter causes problems
        group_by(crop_planting) %>%
        mutate(cum_yields = cumsum(yield)) %>%
        echarts4r$e_chart(x = timestep) %>%
        echarts4r$e_line(cum_yields, symbol="none") %>%
        echarts4r$e_x_axis(min=1,max=max(processed_sim_dat()$timestep)) %>%
        echarts4r$e_title("Yields","t/ha, cumulative") %>%
        echarts4r$e_tooltip() %>%
        echarts4r$e_datazoom(show=show_timeline) %>%
        echarts4r$e_group(grp) #%>% echarts4r$e_toolbox_feature(feature = c("saveAsImage"))
    )

  })

}
