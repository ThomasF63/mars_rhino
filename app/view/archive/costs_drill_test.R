# app/view/dashboard_widgets/costs_drill_test.R
# timeline currently appears to be a dead-end due to lack of support for nesting
# may provide a decent way to handle adding/removing series: https://echarts4r.john-coene.com/articles/proxies

box::use(
  dplyr[...],
  tidyr[pivot_longer],
  rlang[...], #parse_expr, and !! but not sure how to pick just latter
  shiny[h3, moduleServer, NS, tagList, reactive, observeEvent, updateTextInput],
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
server <- function(id,sim_dat,cost_type) {

  moduleServer(id, function(input, output, session) {

    output$chart = echarts4r$renderEcharts4r({

      # level 1
      if (is.null(input$drill_elements$level) || input$drill_elements$level == "level1") {

        # prep data, agg to total cost
        chart_data = sim_dat() %>%
          group_by(t) %>%
          summarise(total_cost = sum(mat_total))

        chart_data %>%
          echarts4r$e_chart(x = t) |>
          echarts4r$e_bar(total_cost, name = "Total Materials") |>
          echarts4r$e_on(
            query = "series.bar",
            handler = paste0("function(params){
           Shiny.setInputValue('",id,"-drill_elements', {level: 'level2', drill1: params.name}, {priority: 'event'});
          }"),
            event = "click"
          )

        # level 2
      } else if (input$drill_elements$level == "level2") {
        chart_data = sim_dat() %>%
          group_by(crop,planting)

        chart_data %>%
          echarts4r$e_chart(x = t) |>
          echarts4r$e_bar(total_mat, name = "Total material costs per crop") |>
          echarts4r$e_title("Back", triggerEvent = TRUE) |>
          echarts4r$e_on(
            query = "series.bar",
            handler = "function(params){
           Shiny.setInputValue('drill_elements', {level: 'level3', drill2: params.name}, {priority: 'event'});
         }",
            event = "click"
          ) %>%
          echarts4r$e_on(
            query = "title",
            handler = paste0("function(params){
               Shiny.setInputValue('chart_drill_elements', {level: 'level1', drill1: '", unique(chart_data$t), "'}, {priority: 'event'});
             }"),
            event = "click"
          )

        # level 3
      } else if (input$drill_elements$level == "level3") {
        chart_data = sim_dat() %>%
          filter(crop == input$drill_elements$drill2) %>%
          pivot_longer(cols = starts_with("mat_costs"),names_to="activity",values_to="cost") %>%
          mutate(tf=as.factor(t)) %>%
          group_by(activity)

        chart_data |>
          echarts4r$e_chart(x = t) |>
          echarts4r$e_bar(cost, name = "Material costs by activity") |>
          echarts4r$e_title("Back", triggerEvent = TRUE) |>
          echarts4r$e_on(
            query = "title",
            handler = paste0("function(params){
               Shiny.setInputValue('drill_elements', {level: 'level2',
                                                      drill1: '", unique(chart_data$t), "',
                                                      drill2: '", unique(chart_data$crop), "'}, {priority: 'event'});
             }"),
            event = "click"
          )
      }

    })

  })

}
