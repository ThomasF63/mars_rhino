# app/logic/file_io.R

box::use(
  dplyr[...],
  purrr[map],
  readxl[read_excel],
  janitor[clean_names]
)

#### Function 1: Get parameters from parameters.xslx ####
#' @export
get_params = function(path){
  output = list(
    sim_params = read_excel(path,sheet="simulation_control"),
    crop_params = read_excel(path,sheet="crop_global_parameters"),
    farm_layout = read_excel(path,sheet="farm_layout"),
    cal_lookup = read_excel(path,sheet="crop sheet lookup")
  )
  return(output)
}


#### Function 2: Get crop calendars ####
#' @export
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
