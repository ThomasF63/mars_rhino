# app/logic/sim_functions.R

box::use(
  dplyr[...],
  tidyr[...],
  purrr[accumulate],
  utils[head],
)


#### Function 1: Build crop lifecycle ####
#' @export
build_lifecycle = function(c,p,farm_ts,crop_params,farm_layout){

  # fetch relevant parameters
  p_ = farm_layout %>% filter(crop==c & planting==p) %>% pivot_wider(names_from=parameter,values_from=value)
  max_age_ = crop_params %>% filter(crop==c & parameter=="max_age") %>% pull()

  # calculate lifecycle
  life_cycle = farm_ts %>%
    mutate(age = t - p_$planting_time,
           cycle = 1+floor(age/(max_age_+p_$post_cycle+p_$planting_time)),
           age = age - (cycle - 1)*(max_age_+p_$post_cycle+p_$planting_time)
    ) %>%
    # Filter down to the lifecycle specified in the farm layout
    filter(cycle > 0 & cycle <= p_$cycles) %>%
    # Re-add missing rows with dummy entries, and label with crop and planting
    right_join(farm_ts) %>%
    mutate(crop = c,
           planting = p)

  return(life_cycle)
}


#### Function 2: von Bertalanffy growth ####
#' max_sz = maximum size, k = growth rate, a = age
#' @export
v_b = function(max_sz,k,a){
  return(max_sz - (max_sz * exp(-k*a)))
}


#### Function 3: Desity & mortality ####
#' includes mortality and the code for timber thinning
#' the crop calendar is only needed to check for thinning rates in timber
#' @export
calc_densities = function(c,p,farm,crop_cal,crop_params,farm_layout,sim_params){

  # fetch relevant parameters
  cal_ = crop_cal %>% filter(crop==c & planting==p) %>% unnest(calendar)
  D_0_ = farm_layout %>% filter(crop==c & planting==p & parameter=="starting_density") %>% pull()
  D_ref_ = crop_params %>% filter(crop==c & parameter=="reference_density") %>% pull()
  plot_size_ = sim_params %>% filter(parameter=="plot_size_ha") %>% pull()
  k_ = crop_params %>% filter(crop==c & parameter=="mortality_rate") %>% pull()
  # the code for timber is '2', so if 2 is_timber_ is true
  is_timber_ = crop_params %>% filter(crop==c & parameter=="type") %>% pull() %>% {. == 2}

  g = farm %>%
    filter(crop == c & planting == p) %>%
    # needed for timber thinning
    left_join(cal_ %>% select(age=month_nb,timber_harvest=harvest)) %>%
    mutate(
      # density = D_0_ * exp(-k_*age), # replaced by the accumulate() approach below
      # we only care about harvest in timber, so we keep those values (coalesce replaces NA with 0 as needed) and all non-timber sets to 0
      timber_harvest = if(is_timber_) coalesce(timber_harvest,0) else 0,
      # quite a lot going on here
      # purrr:accumulate lets us iteratively build a column of values using previous values in the same column
      # basically it's cramming a for-loop inside the dplyr pipe
      # we iteratively calculate density using the previous value of density (.x), applying mortality (.x*k_), and timber harvesting (.x*.y)
      # the timber harvest is wrapped in coalesce() to replace NA with 0 when timber_harvest = NA
      # using .init lets us specify the starting density (D_0_) for age = 0 to begin the loop, but this adds one extra element to the column
      # finally, piping to head(-1) drops the final value, effectively lagging all values by 1 (so the impact on densities is seen in the month AFTER harvest)
      density = purrr::accumulate(timber_harvest, ~ .x - .x*k_ - coalesce(.x*.y,0), .init=D_0_) %>% head(-1),
      density = if_else(density < 0,0,density),
      thinning = timber_harvest * density,
      area_ha = (density/D_ref_) * plot_size_
    )

  return(g)

}


#### Function 4: Carbon & biomass ####
#' includes the code to calculate timber regrowth
#' @export
calc_carbon = function(c,p,densities,crop_params,sim_params){
  max_bm_ = crop_params %>% filter(crop==c & parameter=="max_ag_biomass") %>% pull()
  k_ = crop_params %>% filter(crop==c & parameter=="growth_rate") %>% pull()
  r_s_r_ = crop_params %>% filter(crop==c & parameter=="root_shoot_ratio") %>% pull()
  c_content_ = crop_params %>% filter(crop==c & parameter=="c_content") %>% pull()
  D_ref_ = crop_params %>% filter(crop==c & parameter=="reference_density") %>% pull()
  plot_size_ = sim_params %>% filter(parameter=="plot_size_ha") %>% pull()
  # the code for timber is '2', so if 2 is_timber_ is true
  is_timber_ = crop_params %>% filter(crop==c & parameter=="type") %>% pull() %>% {. == 2}
  regrowth_on_ = sim_params %>% filter(parameter=="regrowth_on") %>% pull() %>% as.logical()

  carbon = densities %>%
    filter(crop == c & planting == p) %>%
    # Here we are just calculating regrowth
    mutate(thinning_ = if_else(timber_harvest > 0,1,0)) %>%
    fill(thinning_) %>%
    mutate(thinning_ = lag(cumsum(thinning_),default=0)) %>%
    group_by(thinning_) %>%
    mutate(
      time_since_thinning_ = row_number(),
      regrowth = ( v_b(max_bm_,k_,a=time_since_thinning_) - v_b(max_bm_,k_,a=time_since_thinning_-1) ) * c_content_ * plot_size_ * (density/D_ref_),
      regrowth = if_else(thinning_ > 0, regrowth, 0)
    ) %>%
    ungroup() %>%
    # Done calculating regrowth
    # Now calculate the base level of biomass and add regrowth if applicable
    mutate(
      ag_c = v_b(max_bm_,k_,age) * c_content_ * plot_size_ * (density/D_ref_) + if(regrowth_on_) cumsum(regrowth) else 0,
      bg_c = ag_c * r_s_r_,
      c_total = ag_c+bg_c
    ) %>%
    select(-thinning,-thinning_,-time_since_thinning_)

  return(carbon)
}


#### Function 5: Yields ####
#' @export
calc_yields = function(c,p,carbon_biomass,crop_cal,crop_params){
  cal_ = crop_cal %>% filter(crop==c & planting==p) %>% unnest(calendar)
  D_ref_ = crop_params %>% filter(crop==c & parameter=="reference_density") %>% pull()
  # the code for timber is '2', so if 2 is_timber_ is true
  is_timber_ = crop_params %>% filter(crop==c & parameter=="type") %>% pull() %>% {. == 2}

  y = carbon_biomass %>%
    filter(crop == c & planting == p) %>%
    left_join(cal_ %>% select(age=month_nb,harvest)) %>%
    mutate(
      harvest = replace_na(harvest,0),
      # for timber trees, yield is calculated from biomass and density adjustment has already been applied
      # for crop plants, yield is directly provided in crop calendar but must be adjusted for densities
      yield = if(is_timber_) harvest * ag_c else harvest * density/D_ref_,
      type = ifelse(is_timber_,'timber','crop')
    )

  return(y)
}


#### Function 6: Revenues ####
#' @export
calc_revenue = function(c,p,yields,crop_params,sim_params){
  price_ = crop_params %>% filter(crop==c & parameter=="price_rp_t") %>% pull()
  rp_usd_ = sim_params %>% filter(parameter=="rp_to_usd") %>% pull()
  plot_size_ = sim_params %>% filter(parameter=="plot_size_ha") %>% pull()

  is_timber_ = crop_params %>% filter(crop==c & parameter=="type") %>% pull() %>% {. == 2}
  expansion_factor_ = ifelse(is_timber_,
                             crop_params %>% filter(crop==c & parameter=="expansion_factor") %>% pull(),
                             1)

  rev = yields %>%
    filter(crop == c & planting == p) %>%
    mutate(
      revenue = yield * price_ * rp_usd_ * plot_size_ * expansion_factor_
    ) %>%
    select(-timber_harvest)

  return(rev)
}


#### Function 7: Labour costs ####
#' currently converts labour hours to rp, then to $, check with tom if this is desired from raw outputs
#' @export
calc_labour = function(c,p,densities,crop_cal,crop_params,sim_params){
  cal_ = crop_cal %>% filter(crop==c & planting==p) %>% unnest(calendar)
  D_ref_ = crop_params %>% filter(crop==c & parameter=="reference_density") %>% pull()
  rp_usd_ = sim_params %>% filter(parameter=="rp_to_usd") %>% pull()
  labour_price_day_rp_ = sim_params %>% filter(parameter=="labour_price_day_rp") %>% pull()


  labour = densities %>%
    filter(crop == c & planting == p) %>%
    left_join(cal_ %>% select(age=month_nb,contains('labor_time'))) %>%
    mutate(
      across(contains('labor_time'),
             # adjust all costs based on density
             ~ .x * density/D_ref_ * rp_usd_ * labour_price_day_rp_)
    ) %>%
    select(-timber_harvest,-thinning,-area_ha)

  return(labour)
}


#### Function 8: Materials costs ####
#' currently converts material costs from rp to $
#' @export
calc_materials = function(c,p,densities,crop_cal,crop_params,sim_params){
  cal_ = crop_cal %>% filter(crop==c & planting==p) %>% unnest(calendar)
  D_ref_ = crop_params %>% filter(crop==c & parameter=="reference_density") %>% pull()
  rp_usd_ = sim_params %>% filter(parameter=="rp_to_usd") %>% pull()

  materials = densities %>%
    filter(crop == c & planting == p) %>%
    left_join(cal_ %>% select(age=month_nb,contains('mat_costs'))) %>%
    mutate(
      across(contains('mat_costs'),
             # adjust all costs based on density
             ~ .x * density/D_ref_ * rp_usd_)
    ) %>%
    select(-timber_harvest,-thinning,-area_ha)

  return(materials)
}
