# app/view/dashboard_widgets/costs_drill.R
# timeline currently appears to be a dead-end due to lack of support for nesting
# may provide a decent way to handle adding/removing series: https://echarts4r.john-coene.com/articles/proxies

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
server <- function(id,processed_sim_dat,cost_type) {

  moduleServer(id, function(input, output, session) {


    build_expression <- function(sim_dat){

      if(cost_type=='materials'){
        prefix="mat_costs_"
        title="Material costs"
      } else if(cost_type=='labor'){
        prefix="labor_time_"
        title="Labor costs"
      }

      activities = sim_dat %>%
        select(matches(!!prefix)) %>%
        colnames()

      series = paste0('echarts4r$e_bar(',activities,',stack="stk")')

      beginning = c(
        'processed_sim_dat()',
        'group_by(crop,planting)',
        'mutate(t=as.character(t))',
        'echarts4r$e_chart(x=t, timeline=T)'
      )
      end = c('echarts4r$e_legend(type="scroll",top=30)',
              'echarts4r$e_x_axis(min=0,max=max(processed_sim_dat()$t))',
              paste0('echarts4r$e_title("',title,'","")'),
              'echarts4r$e_tooltip()',
              'echarts4r$e_datazoom(show=FALSE)',
              'echarts4r$e_group("grp")')

      return(c(beginning,series,end))
    }

    build_pipeline <- function(commands, op=`%>%`) {
      # not sure why ensym(`%>%`) doesnt work directly but it doesnt
      pipe = ensym(op)
      exprs = parse_exprs(commands)
      pipeline = exprs[[1]]
      for(x in exprs[-1]) {
        pipeline = expr((!!pipe)(!!pipeline,!!x))
      }
      pipeline
    }


    output$chart = echarts4r$renderEcharts4r(
      eval(build_pipeline(
        build_expression(processed_sim_dat())
        ))
    )

  })

}
