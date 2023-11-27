# app/logic/sim_run.R

#### SETUP ####

# Libraries
box::use(
  dplyr[...],
  tidyr[unnest,pivot_longer,replace_na],
  purrr[map,map2],
  janitor[clean_names],
  readxl[read_excel],
)
# Local functions
box::use(
  app/logic/sim_functions[...],
)



#' Run the simulation with a given parameter set
#' @export
run_sim = function(sim_params,crop_params,farm_layout,all_crop_cals,scenario){

  #### INITIALIZATION ####

  #' Pivot all the parameters longer for easier extraction
  crop_params = pivot_longer(crop_params,!crop,names_to="parameter",values_to="value")
  farm_layout = pivot_longer(farm_layout,!crop:planting,names_to="parameter",values_to="value")

  #' Extract the crop calendars
  #' import crop calendar sheet, per crop in the layout
  crop_cals = farm_layout %>%
    dplyr::select(crop,planting) %>%
    distinct() %>%
    left_join(all_crop_cals)


  # build farm timeseries (shell), farm(t)
  farm_ts = data.frame(
    t = seq(from=1,
            to=sim_params %>% filter(parameter=="simulation_length") %>% pull(),
            by=1)
  ) %>%
    mutate(year = 1+floor((t-1)/12))

  farm = farm_layout %>%
    dplyr::select(crop,planting) %>%
    distinct() %>%
    mutate(
      lifecycles = purrr::map2(crop,planting,
                               \(c,p) build_lifecycle(c,p,farm_ts,crop_params,farm_layout))
    ) %>%
    select(lifecycles) %>%
    unnest(lifecycles) %>%
    filter(!is.na(age)) # decide later whether I want these, depends on plotting


  #### CALCULATIONS ####

  #' population density & mortality
  densities = farm_layout %>%
    dplyr::select(crop,planting) %>%
    distinct() %>%
    mutate(
      density = purrr::map2(crop,planting,
                            \(c,p) calc_densities(c,p,farm,crop_cals,crop_params,farm_layout,sim_params))
    ) %>%
    select(density) %>%
    unnest(density)

  #' carbon & biomass
  carbon_biomass = farm_layout %>%
    dplyr::select(crop,planting) %>%
    distinct() %>%
    mutate(
      carbon = purrr::map2(crop,planting,
                           \(c,p) calc_carbon(c,p,densities,crop_params,sim_params))
    ) %>%
    select(carbon) %>%
    unnest(carbon)

  #' yield
  yields = farm_layout %>%
    dplyr::select(crop,planting) %>%
    distinct() %>%
    mutate(
      yield = purrr::map2(crop,planting,
                          \(c,p) calc_yields(c,p,carbon_biomass,crop_cals,crop_params))
    ) %>%
    select(yield) %>%
    unnest(yield)

  #' labour costs
  labour_costs = farm_layout %>%
    dplyr::select(crop,planting) %>%
    distinct() %>%
    mutate(
      labour = purrr::map2(crop,planting,
                           \(c,p) calc_labour(c,p,densities,crop_cals,crop_params,sim_params))
    ) %>%
    select(labour) %>%
    unnest(labour)

  #' material costs
  material_costs = farm_layout %>%
    dplyr::select(crop,planting) %>%
    distinct() %>%
    mutate(
      mats = purrr::map2(crop,planting,
                         \(c,p) calc_materials(c,p,densities,crop_cals,crop_params,sim_params))
    ) %>%
    select(mats) %>%
    unnest(mats)

  #' revenue
  revenues = farm_layout %>%
    dplyr::select(crop,planting) %>%
    distinct() %>%
    mutate(
      rev = purrr::map2(crop,planting,
                        \(c,p) calc_revenue(c,p,yields,crop_params,sim_params))
    ) %>%
    select(rev) %>%
    unnest(rev)


  #### OUTPUTS ####

  #' return a list containing production+revenue, and costs
  #' all production+revenue is in revenues df
  #' combine the two costs
  return(
  #  list(
  #    production = revenues,
  #    costs = left_join(labour_costs,material_costs,by=c("t","year","age","cycle","crop","planting","density"))
  #  )
    revenues %>%
      left_join(
        left_join(labour_costs,material_costs,by=c("t","year","age","cycle","crop","planting","density")),
        by=c("t","year","age","cycle","crop","planting","density")
      ) %>%
      mutate(
        scenario=scenario,
        across(contains('mat_costs') | contains('labor_time'),
               ~ replace_na(.x,0)),
        mat_total = rowSums(across(contains('mat_costs'))),
        labor_total = rowSums(across(contains('labor_time')))
      )
  )


}
