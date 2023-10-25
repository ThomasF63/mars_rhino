# app/view/import_params.R

box::use(
  dplyr[...],
  purrr[map],
  janitor[clean_names],
  shiny[moduleServer, NS, tagList, reactive, reactiveVal, fileInput],
  readxl[read_excel],
)
box::use(
  app/logic/file_io[get_params,get_crop_cals],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    fileInput(ns("param_input"), "Choose parameter file",
              multiple = F, accept = c(".xlsx")),
    fileInput(ns("cals_input"), "Choose crop calendars file",
              multiple = F, accept = c(".xlsx"))
  )
}

#' @export
server <- function(id,sim_dat) {

  moduleServer(id, function(input, output, session) {

    # get_params = reactive({
    #
    #   # Specify default behaviour if nothing provided
    #   path = ifelse(is.null(input$param_input), "app/data/parameters.xlsx", input$param_input$datapath)
    #
    #   print('test1')
    #
    #   out = list(sim_params = read_excel(path,sheet="simulation_control"),
    #        crop_params = read_excel(path,sheet="crop_global_parameters"),
    #        farm_layout = read_excel(path,sheet="farm_layout"),
    #        cal_lookup = read_excel(path,sheet="crop sheet lookup")
    #   )
    #
    #   return(out)
    #
    # })
    #
    # get_cals = reactive({
    #
    #   # Specify default behaviour if nothing provided
    #   path = ifelse(is.null(input$cals_input), "app/data/Crop Calendars - Prod, Labor and Materials .xlsx", input$cals_input$datapath)
    #
    #   # fetch crop cals
    #   extracted_crop_cals = get_params$cal_lookup %>%
    #     left_join(sheet_lookup) %>%
    #     mutate(calendar = purrr::map(sheet,
    #                                  \(s) read_excel(path,sheet=s) %>%
    #                                    #drop the filler and plot columns
    #                                    select(-contains(c('...','Graph','Grafik'))) %>%
    #                                    #also weed out any empty columns
    #                                    clean_names()
    #     )
    #     ) %>%
    #     select(-sheet)
    #
    #   out = list(cal_path = path,
    #        crop_cals = extracted_crop_cals
    #   )
    #
    #   return(out)
    #
    # })

    get_params = function(path){
      output = list(
        sim_params = read_excel(path,sheet="simulation_control"),
        crop_params = read_excel(path,sheet="crop_global_parameters"),
        farm_layout = read_excel(path,sheet="farm_layout"),
        cal_lookup = read_excel(path,sheet="crop sheet lookup")
      )
      return(output)
    }

    get_crop_cals = function(path,cal_lookup){

      output = cal_lookup %>%
        mutate(calendar = purrr::map(sheet,
                                     \(s) read_excel(path,sheet=s) %>%
                                       #drop the filler and plot columns
                                       select(-contains(c('...','Graph','Grafik'))) %>%
                                       #also weed out any empty columns
                                       clean_names()
        )
        ) %>%
        select(-sheet)
      return(output)
    }

    cur_params = reactiveVal(get_params("app/data/parameters.xlsx"))

    print('test')

    cur_cals = reactiveVal(get_crop_cals("app/data/Crop Calendars - Prod, Labor and Materials .xlsx"))

    observeEvent(input$param_input, {
      new_params = get_params(input$param_input$datapath)
      if(!identical(
        get_params$farm_layout %>% select(crop,planting) %>% distinct(),
        new_params$farm_layout %>% select(crop,planting) %>% distinct())) {
        cur_cals(get_crop_cals(input$param_input$datapath))
      }
      cur_params(new_params)
    })

    observeEvent(input$cals_input, {
      cur_cals(get_crop_cals(input$param_input$datapath))
    })

    return(append(cur_params,cur_cals))

  })

}
