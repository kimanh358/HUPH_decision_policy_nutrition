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
  # Staff training: Food safety
  staff_training_foodsafety <- chance_event(use_staff_training_foodsafety)
  if (!staff_training_foodsafety) {
    # if not then no costs
    training_costs_foodsafety_annual <- 0
    training_costs_foodsafety_1st_year <- 0
  } else {
    n_reduce_disease_diagnosis <- n_reduce_disease_diagnosis * (1 + staff_training_foodsafety_nutrition_effect)
    n_reduce_disease_treatment <- n_reduce_disease_treatment * (1 + staff_training_foodsafety_nutrition_effect)
  }
  
  # Staff training: Nutrition
  staff_training_nutrition <- chance_event(use_staff_training_nutrition)
  if (!staff_training_nutrition) {
    training_costs_nutrition_annual <- 0
    training_costs_nutrition_1st_year <- 0
  } else {
    student_performance_improvement <- student_performance_improvement * (1 + staff_training_nutrition_effect)
    staff_knowledge_nutrition <- staff_knowledge_nutrition * (1 + staff_training_nutrition_effect)
  }
  
  # Physical education
  use_physical_activity <- chance_event(use_physical_activity)
  if (!use_physical_activity) {
    training_costs_physical_activity_annual <- 0
    training_costs_physical_activity_1st_year <- 0
  } else {
    student_performance_improvement <- student_performance_improvement * (1 + physical_activity_effect)
    n_reduce_disease_diagnosis <- n_reduce_disease_diagnosis * (1 + physical_activity_effect)
    n_reduce_disease_treatment <- n_reduce_disease_treatment * (1 + physical_activity_effect)
    staff_knowledge_food_safety <- staff_knowledge_food_safety * (1 + physical_activity_effect)
  }
  
  # Menu change to meet RDA
  use_menu_change_rda <- chance_event(use_menu_change_rda)
  if (!use_menu_change_rda) {
    change_menu_costs_annual <- 0
  } else {
    n_reduce_disease_diagnosis <- n_reduce_disease_diagnosis * (1 + menu_change_rda_nutrition_effect)
    n_reduce_disease_treatment <- n_reduce_disease_treatment * (1 + menu_change_rda_nutrition_effect)
    unhealthy_canteen_foods <- unhealthy_canteen_foods *(1 +  menu_change_rda_nutrition_effect)
    children_consume_healthy_food <- children_consume_healthy_food * (1 + menu_change_rda_nutrition_effect)
    children_access_healthy_food <- children_access_healthy_food * (1 + menu_change_rda_nutrition_effect)
  }
  
  # Limit unhealthy canteen food
  use_limit_unhealthy_canteen_food <- chance_event(use_limit_unhealthy_canteen_food)
  if (!use_limit_unhealthy_canteen_food) {
    monitoring_canteen_cost <- 0
  } else {
    n_reduce_disease_diagnosis <- n_reduce_disease_diagnosis * (1 + limit_unhealthy_canteen_food_nutrition_effect)
    n_reduce_disease_treatment <- n_reduce_disease_treatment * (1 + limit_unhealthy_canteen_food_nutrition_effect)
    unhealthy_canteen_foods <- unhealthy_canteen_foods * (1 + limit_unhealthy_canteen_food_nutrition_effect)
    children_consume_healthy_food <- children_consume_healthy_food * (1 + limit_unhealthy_canteen_food_nutrition_effect)
    children_access_healthy_food <- children_access_healthy_food * (1 + limit_unhealthy_canteen_food_nutrition_effect)
  }
  
  # Composite indicators (simplified)
  unhealthy_food_exposure <- max(c(unhealthy_canteen_foods, unhealthy_school_gate_foods, advertisement_exposure))
  
  # Policy impacts (attenuated by barriers)
  food_access_score <- children_access_healthy_food * (1 - unhealthy_food_exposure) * (1 - resistance_child_preferences_attitude)
  food_consumption_score <- children_consume_healthy_food * (1 - unhealthy_food_exposure) * (1 - resistance_child_preferences_attitude)
  
  # Peer influence (negative)
  if (chance_event(peer_influence_factor)) {
    food_consumption_score <- food_consumption_score * (1 - peer_influence_factor)
  }
  
  # BMI and disease outcomes
  if ((food_access_score + food_consumption_score) >= food_access_and_consumption_threshold) {
    bmi_high <- nutrition_status_bmi_high * (1 - food_access_and_consumption_reduce_overweight) 
    bmi_low <- nutrition_status_bmi_low * (1 - food_access_and_consumption_reduce_underweight)
  } else {
    bmi_high <- nutrition_status_bmi_high
    bmi_low <- nutrition_status_bmi_low
  }
  
  # Both high and low BMI levels are associated with increased disease risk
  
  # Initialize multipliers for diagnosis and treatment
  diagnosis_multiplier <- 1
  treatment_multiplier <- 1
  
  # Overweight increases risk
  if (bmi_high > overweight_threshold) {
    excess_overweight <- bmi_high - overweight_threshold
    diagnosis_multiplier <- diagnosis_multiplier + nutrition_status_bmi_high * 
      overweight_diagnosis_risk_multiplier
    treatment_multiplier <- treatment_multiplier + nutrition_status_bmi_high * 
      overweight_treatment_risk_multiplier
  }
  
  # Underweight increases risk
  if (bmi_low > underweight_threshold) {
    excess_underweight <- bmi_low - underweight_threshold
    diagnosis_multiplier <- diagnosis_multiplier + excess_underweight * 
      underweight_diagnosis_risk_multiplier
    treatment_multiplier <- treatment_multiplier + excess_underweight * 
      underweight_treatment_risk_multiplier
  }
  
  # Adjust baseline disease burden based on BMI-related risk
  baseline_disease_diagnosis <- baseline_disease_diagnosis * 
    diagnosis_multiplier
  baseline_disease_treatment <- baseline_disease_treatment * 
    treatment_multiplier
  
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
      education_benefit = education_benefit, 
      use_staff_training_foodsafety = use_staff_training_foodsafety,
      use_staff_training_nutrition = use_staff_training_nutrition,
      use_physical_activity = use_physical_activity,
      use_menu_change_rda = use_menu_change_rda,
      use_limit_unhealthy_canteen_food = use_limit_unhealthy_canteen_food
    )
  )
}

