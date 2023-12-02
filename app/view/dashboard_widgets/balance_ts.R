# app/view/dashboard_widgets/balance_ts.R

box::use(
  dplyr[...],
  shiny[h3, moduleServer, NS, tagList, reactive],
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
server <- function(id,processed_sim_dat,th,grp="grp",show_timeline=F) {
  # should be possible to colour based on value but couldn't get it to work on first try
  # https://echarts4r.john-coene.com/reference/e_visual_map


  moduleServer(id, function(input, output, session) {

    th_ = reactive({
      # See if we've aggregated to year
      if(identical(processed_sim_dat()$timestep,processed_sim_dat()$year)) {
        th*12
      } else {
        th
      }
    })

    output$chart = echarts4r$renderEcharts4r(
      processed_sim_dat() %>%
        mutate(
          total_costs = rowSums(across(contains('labor_time'))) + rowSums(across(contains('mat_costs'))),
          net_income = revenue - total_costs
        ) %>%
        # Doesn't have easy ways to ignore NAs when connecting lines so just filter out 0s
        # Use dplyr group_by rather than echarts group_by, latter causes problems
        group_by(timestep) %>%
        summarise(balance = sum(net_income,na.rm=T)) %>%
        #mutate(t=as.factor(t)) %>%
        echarts4r$e_charts(x = timestep) %>%
        echarts4r$e_bar(balance) %>% #,symbol='none'
        echarts4r$e_legend(show=F) %>%
        echarts4r$e_visual_map(
          type="piecewise",
          show=FALSE,
          right='5',
          top='15%',
          pieces=list(
            list(gt = th, color="green"),
            list(lte = th, color="red")
          )
        ) %>%
        echarts4r$e_x_axis(min=0,max=max(processed_sim_dat()$timestep)) %>%
        echarts4r$e_mark_line(data=list(yAxis=th_()), title="", symbol="none") %>%
        echarts4r$e_title("Balance","USD, red below income threshold") %>%
        echarts4r$e_tooltip(trigger="axis") %>%
        echarts4r$e_datazoom(show=show_timeline) %>%
        echarts4r$e_group(grp) %>%
        echarts4r$e_connect_group(grp)
    )

  })

}
