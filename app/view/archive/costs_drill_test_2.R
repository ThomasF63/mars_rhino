# app/view/dashboard_widgets/costs_drill_test.R
# timeline currently appears to be a dead-end due to lack of support for nesting
# may provide a decent way to handle adding/removing series: https://echarts4r.john-coene.com/articles/proxies

box::use(
  dplyr[...],
  tidyr[pivot_longer],
  rlang[...], #parse_expr, and !! but not sure how to pick just latter
  glue[glue],
  shiny[h3, moduleServer, NS, tagList, observeEvent],
  echarts4r[...],
)
box::use(
  app/logic/kpis,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    echarts4rOutput(ns("chart"))
  )
}

#' @export
server <- function(id,sim_dat,cost_type) {

  moduleServer(id, function(input, output, session) {


    plot_sales_data <- function(id,group,
    chart_data, chart_title, chart_color,
    chart_drill_to, chart_back_drill_to, filtered_place) {

      #id = paste0('app-sim_dashboard-',id,'-')

      sales_chart <- chart_data |>
        dplyr::group_by(pick({{group}})) |>
        e_chart(x = t) |>
        e_bar(cost)

      # Adding the click observer only when drill_to is passed
      if (!is.null(chart_drill_to)) {
        sales_chart <- sales_chart |>
          e_on(
            query = "series.bar",
            # Set input values
            handler = glue(
              "function(params){
             Shiny.setInputValue(
              'drill_test-custom_bar_click',
              {clicked_level: '<>', drilled_place: params.name}, {priority: 'event'}
             );
             console.log(drill_test-custom_bar_click);
           }",
              .open = "<<", .close = ">>"
            ),
            event = "click"
          )
      }
      if (!is.null(chart_back_drill_to)) {
        if (is.null(filtered_place)) {
          observe_handler = glue(
            "function(params){
        Shiny.setInputValue(
          'custom_bar_click',
          {clicked_level: '<>'},
          {priority: 'event'}
        );
        console.log(custom_bar_click);
      }",
            .open = "<<", .close = ">>"
          )
        } else {
          observe_handler = glue(
            "function(params){
          Shiny.setInputValue(
            'custom_bar_click',
            {clicked_level: '<>', drilled_place: '<>'},
            {priority: 'event'}
          );
          console.log(custom_bar_click);
        }",
            .open = "<<", .close = ">>"
          )
        }
        sales_chart <- sales_chart |>
          e_title("Back", triggerEvent = TRUE) |>
          e_on(
            query = "title",
            # Set input values
            handler = observe_handler,
            event = "click"
          )
      }
      return(sales_chart)
    }

    # testing

    observeEvent(input$custom_bar_click, {
      print(input$custom_bar_click)
    })

    output$chart <- renderEcharts4r({
      # Our custom input value that we send from the bar click
      print(input$custom_bar_click)
      print(paste0('id is: ',id))

      if (is.null(input$custom_bar_click$clicked_level) || input$custom_bar_click$clicked_level == "level1") {
        # Prepare data for chart
        chart_data <- sim_dat() |>
          mutate(cost = mat_total)

        # Create chart
        plot_sales_data(id,'crop',
          chart_data = chart_data,
          chart_title = "By Crop",
          chart_color = "#5470C6",
          chart_drill_to = "level2",
          chart_back_drill_to = NULL,
          filtered_place = NULL
        )
      } else if (input$custom_bar_click$clicked_level == "level2") {
        # Prepare data for chart

        chart_data <- sim_dat() |>
          filter(crop == input$custom_bar_click$drilled_place) |>
          pivot_longer(cols = starts_with("mat_costs"),names_to="activity",values_to="cost") |>
          dplyr::group_by(activity)

        # Create chart
        plot_sales_data(id,'activity',
          chart_data = chart_data,
          chart_title = glue(
            "Sales by Activity (Filtered for {input$custom_bar_click$drilled_place})"
          ),
          chart_color = "#91CC75",
          chart_drill_to = NULL,
          chart_back_drill_to = "level1",
          filtered_place = NULL
        )
      }

    })



  })

}
