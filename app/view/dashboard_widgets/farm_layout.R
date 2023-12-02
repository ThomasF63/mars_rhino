# app/view/dashboard_widgets/farm_layout.R

box::use(
  dplyr[...],
  shiny[h3, moduleServer, NS, tagList],
  echarts4r,
  #data.tree[FromDataFrameNetwork]
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
server <- function(id,sim_dat,grp="grp") {

  moduleServer(id, function(input, output, session) {

    # for a treemap, must build out the df as a reactive:
    # parents:  ""     "Farm"             "Food crops"                 "Cocoa"
    # children: "Farm" "Food crops"       "Cocoa"                      "A"
    # value     0      <sum of crop vals> <sum of all cocoa plantings> <size of planting A>
    # Will have to do several rounds of aggregation, then drop crop parents where there's only one planting
    # Will be time consuming, just use a pie chart for now


    output$chart = echarts4r$renderEcharts4r(
      sim_dat() %>%
        group_by(crop_planting) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        mutate(layout = ifelse(type=='timber',density,area_ha)) %>%
        #group_by(type) %>%
        echarts4r$e_charts(crop_planting) %>%
        echarts4r$e_pie(layout, radius = c("50%", "70%")) %>%
        echarts4r$e_title("Farm Layout","Food crops in ha, Timber in trees/ha") %>%
        echarts4r$e_tooltip() %>%
        echarts4r$e_group(grp) %>%
        echarts4r$e_connect_group(grp)
    )

  })

}
