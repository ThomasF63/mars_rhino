# app/view/dashboard_widgets/ftes_ts.R

box::use(
  dplyr[...],
  shiny[h3, moduleServer, NS, tagList, reactive],
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
    echarts4r$echarts4rOutput(ns("chart")) %>% withSpinner(type=7)
  )
}

#' @export
server <- function(id,sim_dat,sim_params,grp="grp") {

  moduleServer(id, function(input, output, session) {

    labour_price_day_rp = reactive({ sim_params %>% filter(parameter=="labour_price_day_rp") %>% pull() })
    rp_to_usd = reactive({ sim_params %>% filter(parameter=="rp_to_usd") %>% pull() })

    #labour_price_day_rp = sim_params %>% filter(parameter=="labour_price_day_rp") %>% pull()
    #rp_to_usd = sim_params %>% filter(parameter=="rp_to_usd") %>% pull()

    output$chart = echarts4r$renderEcharts4r(

      sim_dat() %>%
        # Use dplyr group_by rather than echarts group_by, latter causes problems
        mutate(ftes = labor_total / (labour_price_day_rp() * rp_to_usd()),
               timestep=as.character(timestep)) %>%
        group_by(crop_planting) %>%
        echarts4r$e_chart(x = timestep) %>%
        echarts4r$e_bar(ftes,stack="stk") %>%
        echarts4r$e_x_axis(min=0,max=max(sim_dat()$timestep)) %>%
        echarts4r$e_title("Full time equivalents","1 FTE = 40h per week") %>%
        echarts4r$e_tooltip(trigger="axis") %>%
        echarts4r$e_datazoom(show=FALSE) %>%
        echarts4r$e_group(grp) #%>% echarts4r$e_toolbox_feature(feature = c("saveAsImage"))
    )

  })

}
