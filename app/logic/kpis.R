# app/logic/kpis.R

box::use(
  dplyr[...],
  tidyr[...],
)


#### Function 1: Total income ####
#' @export
total_income = function(sim_results){

  temp = sim_results %>%
    mutate(
      total_costs = rowSums(across(contains('labor_time'))) + rowSums(across(contains('mat_costs'))),
      net_income = revenue - total_costs
    )

  result = sum(temp$net_income)

  return(result)
}


#### Function 2: Total revenue ####
#' @export
total_revenue = function(sim_results){

  result = sum(sim_results$revenue)

  return(result)
}


#### Function 3: Total materials costs ####
#' @export
total_materials = function(sim_results){

  temp = sim_results %>%
    mutate(
      total_mats = rowSums(across(contains('mat_costs')))
    )

  result = sum(sim_results$total_mats)

  return(result)
}


#### Function 4: Total labour costs ####
#' @export
total_labour = function(sim_results){

  temp = sim_results %>%
    mutate(
      total_mats = rowSums(across(contains('labor_time')))
    )

  result = sum(sim_results$total_mats)

  return(result)
}


#### Function 5: Total months (time) of simulation ####
#' @export
total_months = function(sim_results){

  result = max(sim_results$t)

  return(result)
}


#### Function 6: Time to first profitable month ####
#' @export
months_to_profit = function(sim_results){

  temp = sim_results %>%
    mutate(
      total_costs = rowSums(across(contains('labor_time'))) + rowSums(across(contains('mat_costs'))),
      net_income = revenue - total_costs
    ) %>%
    group_by(t) %>%
    summarise(cumulative_total_income = cumsum(net_income)) %>%
    filter(cumulative_total_income > 0) %>%
    arrange(t)

  result = temp$cumulative_total_income[1]

  return(result)
}


#### Function 7: Total carbon ####
#' @export
total_carbon = function(sim_results){

  result = sum(sim_results$total_c)

  return(result)
}


