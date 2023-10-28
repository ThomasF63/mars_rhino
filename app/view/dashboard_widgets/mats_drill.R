# app/view/dashboard_widgets/mats_drill.R
# timeline currently appears to be a dead-end due to lack of support for nesting

box::use(
  dplyr[...],
  tidyr[pivot_longer],
  rlang[...], #parse_expr, and !! but not sure how to pick just latter
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

    expr = c(
      'processed_sim_dat()',
      'group_by(crop,planting)',
      'echarts4r$e_chart(x=t, timeline=T)',
      'echarts4r$e_line(yield)',
      'echarts4r$e_line(...)',
      'echarts4r$e_line(density)',
      'echarts4r$e_tooltip()'
    )

    build_pipeline <- function(commands, op=`%>%`) {
      # not sure why ensym(`%>%`) doesnt work directly but it doesnt
      pipe = ensym(op)
      exprs = parse_exprs(commands)
      new_call = exprs[[1]]
      for(x in exprs[-1]) {
        new_call = expr((!!pipe)(!!new_call,!!x))
      }
      new_call
    }


    output$chart = echarts4r$renderEcharts4r(
      #processed_sim_dat() %>%
        # Doesn't have easy ways to ignore NAs when connecting lines so just filter out 0s
        # Use dplyr group_by rather than echarts group_by, latter causes problems
        #mutate(
        #  mat_costs_total = rowSums(across(contains('mat_costs')))
        #) %>%
        #pivot_longer(contains('mat_costs'),names_to="activity",values_to="mat_cost") %>%
        #group_by(crop,planting) %>%
        #{eval(join_exprs(parse_exprs(expr,'%>%')))}
        #echarts4r$e_chart(x = t, timeline=T) %>%
        # This approach works. Now I need to figure out how to do it programmatically
        #echarts4r$e_line(yield) %>%
        #echarts4r$e_add_nested("color",activity) %>%
        #echarts4r$e_x_axis(t) %>%
        #echarts4r$e_tooltip()
      eval(build_pipeline(expr))
    )

  })

}
