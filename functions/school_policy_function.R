# Model of the school policy

# modeling from intervention → practice → food environment → health → economic outcome

# make variables for testing our model (only for construction)
library(decisionSupport)

make_variables <- function(est, n = 1)
  #take one time #
{
  x <- decisionSupport::random(rho = est, n = n)
  for (i in colnames(x))
    assign(i, as.numeric(x[1, i]), envir = .GlobalEnv)
}

#Just one run#
make_variables(decisionSupport::estimate_read_csv(paste
("data/inputs_school_policy.csv", sep ="")))

# Model function
school_policy_function <- function(
){
  
  # Apply toggles to adjust input parameters
  # transform to yes no binary
  # a probability around 0.5 (i.e., Bernoulli distribution)
  use_staff_training_foodsafety <- chance_event(use_staff_training_foodsafety)
  if (!use_staff_training_foodsafety) training_costs_foodsafety_annual <- 0
  if (!use_staff_training_foodsafety) training_costs_foodsafety_1st_year <- 0
  use_staff_training_nutrition <- chance_event(use_staff_training_nutrition)
  if (!use_staff_training_nutrition) training_costs_nutrition_annual <- 0
  if (!use_staff_training_nutrition) training_costs_nutrition_1st_year <- 0
  use_physical_activity <- chance_event(use_physical_activity)
  if (!use_physical_activity) training_costs_physical_activity_annual <- 0
  if (!use_physical_activity) training_costs_physical_activity_1st_year <- 0
  use_menu_change_rda <- chance_event(use_menu_change_rda)
  if (!use_menu_change_rda) change_menu_costs_annual <- 0
  use_limit_unhealthy_canteen_food <- chance_event(use_limit_unhealthy_canteen_food)
  if (!use_limit_unhealthy_canteen_food) monitoring_canteen_cost <- 0
  
  # Composite indicators (simplified)
  staff_knowledge_index <- mean(c(staff_knowledge_food_safety, staff_knowledge_nutrition))
  meal_practices_score <- mean(c(meal_nutrition_practices_staff, food_safety_practices_staff))
  unhealthy_food_exposure <- max(c(unhealthy_canteen_foods, unhealthy_school_gate_foods, advertisement_exposure))
  oversight_index <- mean(c(practice_school_management_board, parents_monitor_school_meal_practices))
  
  # Policy impacts (attenuated by barriers)
  health_lessons_effective <- health_lessons_class * (1 - teaching_time)
  staff_practice_effective <- meal_practices_score * (1 - resistance_existing_staff_practices)
  food_access_score <- children_access_healthy_food * (1 - unhealthy_food_exposure) * (1 - resistance_child_preferences_attitude)
  food_consumption_score <- children_consume_healthy_food * (1 - unhealthy_food_exposure) * (1 - resistance_child_preferences_attitude)
  
  # Peer influence (bonus)
  if (chance_event(peer_influence_factor)) {
    food_consumption_score <- food_consumption_score * (1 + peer_influence_factor)
  }
  
  # Physical activity program effect on BMI
  if (physical_activity_program_effective == 1) {
    bmi_risk_modifier <- 1 - physical_activity_effect
  } else {
    bmi_risk_modifier <- 1
  }
  
  # BMI and disease outcomes
  if ((food_access_score + food_consumption_score) >= food_access_and_consumption_threshold) {
    bmi_high <- nutrition_status_bmi_high * (1 - food_access_and_consumption_reduce_overweight) * bmi_risk_modifier
    bmi_low <- nutrition_status_bmi_low * (1 - food_access_and_consumption_reduce_underweight)
  } else {
    bmi_high <- nutrition_status_bmi_high
    bmi_low <- nutrition_status_bmi_low
  }
  
  # Health costs with and without policy
  # Health utility  ####
  # nutrition policy would reduce treatment and diagnosis costs
  # How many fewer health events per VND spent?
  # A discussion among school nurses indicated that approximately 3-4% of the student body visits the school nurse each day
  # 50% to 70% of nurse visits result in some form of treatment or medication
  baseline_health_costs <- ((baseline_disease_diagnosis * disease_diagnosis_cost) + 
    (baseline_disease_treatment * disease_treatment_cost)) * n_student
  
  policy_health_costs <- (((baseline_disease_diagnosis - n_reduce_disease_diagnosis) * 
                            disease_diagnosis_cost) + 
                            ((baseline_disease_treatment - n_reduce_disease_treatment) *
                               disease_treatment_cost)) * 
                                              n_student
  
  # Net health benefit (VND)
  net_health_benefit <- baseline_health_costs - policy_health_costs
  
  # Education benefit (difference from baseline)
  education_benefit_policy <- student_performance_improvement * n_student * value_of_learning_per_student
  education_benefit_baseline <- education_benefit_policy * 0.75  # assume 25% lower without intervention
  education_benefit <- education_benefit_policy
  
  # Annual policy cost
  annual_policy_costs <- training_costs_foodsafety_annual + training_costs_nutrition_annual +
    training_costs_physical_activity_annual + change_menu_costs_annual +
    monitoring_canteen_cost
  policy_cost <- vv(annual_policy_costs, var_CV = CV_value, n = number_of_years)
  policy_cost[1] <- policy_cost[1] + training_costs_foodsafety_1st_year + training_costs_nutrition_1st_year + training_costs_physical_activity_1st_year
  
  # Benefits under policy
  annual_policy_benefit <- net_health_benefit + education_benefit
  policy_benefit <- vv(annual_policy_benefit, var_CV = CV_value, n = number_of_years)
  
  # Do-nothing (baseline) cost and benefit estimation from health outcomes only
  annual_no_policy_costs <- vv(baseline_health_costs, var_CV = CV_value, n = number_of_years)
  annual_no_policy_benefit <- vv(education_benefit_baseline, var_CV = CV_value, n = number_of_years)
  no_policy_result <- annual_no_policy_benefit - annual_no_policy_costs
  
  # Net Present Values
  npv_policy <- discount(policy_benefit - policy_cost, discount_rate, TRUE)
  npv_no_policy <- discount(no_policy_result, discount_rate, TRUE)
  
  return(
    list(
      decision_value = npv_policy - npv_no_policy,
      net_health_benefit = net_health_benefit,
      education_benefit = education_benefit
    )
  )
}

