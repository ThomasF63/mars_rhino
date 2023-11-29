# app/view/dashboard_widgets/costs_drill.R
# timeline currently appears to be a dead-end due to lack of support for nesting
# may provide a decent way to handle adding/removing series: https://echarts4r.john-coene.com/articles/proxies

box::use(
  dplyr[...],
  tibble[deframe],
  tidyr[pivot_longer],
  rlang[...], #parse_expr, and !! but not sure how to pick just latter
  shiny[h3, moduleServer, NS, tagList],
  utils[read.csv],
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

    if(cost_type=='materials'){
      prefix="mat_costs_"
      title="Material costs"
      suffix='_m'
    } else if(cost_type=='labor'){
      prefix="labor_time_"
      title="Labor costs"
      suffix='_l'
    }

    tidynames = read.csv("app/data/column_names_crosswalk.csv") %>%
      mutate(oldname = paste0(prefix,oldname)) %>%
      relocate(tidyname) %>%
      deframe()

    build_expression <- function(sim_dat){

      # Get lookup table for tidier column names

      activities = sim_dat() %>%
        select(matches(!!prefix)) %>%
        rename_with(tolower) %>%
        rename(any_of(tidynames)) %>%
        colnames()

      series = paste0('echarts4r$e_bar(`',activities,'`,stack="stk")')

      beginning = c(
        'sim_dat()',
        paste0('rename_with(tolower, starts_with("',prefix,'"))'),
        'rename(any_of(tidynames))',
        'mutate(t=as.character(t))',
        'group_by(crop_planting)',
        'echarts4r$e_chart(x=t, timeline=T)'
      )
      end = c('echarts4r$e_legend(type="scroll",top=30)',
              'echarts4r$e_x_axis(min=0,max=max(sim_dat()$t))',
              paste0('echarts4r$e_title("',title,'","")'),
              'echarts4r$e_tooltip(trigger="axis")',
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
        build_expression(sim_dat())
        ))
    )

  })

}
