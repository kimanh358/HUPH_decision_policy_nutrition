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

#Just one run# for testing
make_variables(decisionSupport::estimate_read_csv(paste
("data/input_school_policy_post.csv", sep ="")))

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
 n_reduce_disease_diagnosis <- n_reduce_disease_diagnosis* (1 + staff_training_foodsafety_nutrition_effect)
 n_reduce_disease_treatment <- n_reduce_disease_treatment* (1 + staff_training_foodsafety_nutrition_effect)
 n_sick_days_avoided_per_student <- n_sick_days_avoided_per_student* (1 + staff_training_foodsafety_nutrition_effect)
 # Food safety reduces foodborne illness, fewer sick days missed → marginal learning gain
 student_performance_improvement <- student_performance_improvement* (1 + staff_training_foodsafety_nutrition_effect)
 }
 
 # Staff training: Nutrition
 staff_training_nutrition <- chance_event(use_staff_training_nutrition)
 if (!staff_training_nutrition) {
 training_costs_nutrition_annual <- 0
 training_costs_nutrition_1st_year <- 0
 } else {
 # Nutrition training directly improves dietary knowledge → better concentration and performance
 student_performance_improvement <- student_performance_improvement* (1 + staff_training_nutrition_effect)
 staff_knowledge_nutrition <- staff_knowledge_nutrition* (1 + staff_training_nutrition_effect)
 n_sick_days_avoided_per_student <- n_sick_days_avoided_per_student* (1 + staff_training_nutrition_effect)
 }
 
 # Physical education
 use_physical_activity <- chance_event(use_physical_activity)
 if (!use_physical_activity) {
 training_costs_physical_activity_annual <- 0
 training_costs_physical_activity_1st_year <- 0
 } else {
 student_performance_improvement <- student_performance_improvement* (1 + physical_activity_effect)
 n_reduce_disease_diagnosis <- n_reduce_disease_diagnosis* (1 + physical_activity_effect)
 n_reduce_disease_treatment <- n_reduce_disease_treatment* (1 + physical_activity_effect)
 n_sick_days_avoided_per_student <- n_sick_days_avoided_per_student* (1 + physical_activity_effect)
 }
 
 # Menu change to meet RDA
 use_menu_change_rda <- chance_event(use_menu_change_rda)
 if (!use_menu_change_rda) {
 change_menu_costs_annual <- 0
 } else {
 n_reduce_disease_diagnosis <- n_reduce_disease_diagnosis* (1 + menu_change_rda_nutrition_effect)
 n_reduce_disease_treatment <- n_reduce_disease_treatment* (1 + menu_change_rda_nutrition_effect)
 n_sick_days_avoided_per_student <- n_sick_days_avoided_per_student* (1 + menu_change_rda_nutrition_effect)
 # RDA-compliant meals improve nutrient intake → better cognitive function and learning
 student_performance_improvement <- student_performance_improvement* (1 + menu_change_rda_nutrition_effect)
 unhealthy_canteen_foods <- unhealthy_canteen_foods* (1 - menu_change_rda_nutrition_effect)
 children_consume_healthy_food <- children_consume_healthy_food* (1 + menu_change_rda_nutrition_effect)
 children_access_healthy_food <- children_access_healthy_food* (1 + menu_change_rda_nutrition_effect)
 }
 
 # Limit unhealthy canteen food
 use_limit_unhealthy_canteen_food <- chance_event(use_limit_unhealthy_canteen_food)
 if (!use_limit_unhealthy_canteen_food) {
 monitoring_canteen_cost <- 0
 } else {
 n_reduce_disease_diagnosis <- n_reduce_disease_diagnosis* (1 + limit_unhealthy_canteen_food_nutrition_effect)
 n_reduce_disease_treatment <- n_reduce_disease_treatment* (1 + limit_unhealthy_canteen_food_nutrition_effect)
 n_sick_days_avoided_per_student <- n_sick_days_avoided_per_student* (1 + limit_unhealthy_canteen_food_nutrition_effect)
 # Removing energy-dense/nutrient-poor foods reduces sugar-driven energy crashes → sustained attention
 student_performance_improvement <- student_performance_improvement* (1 + limit_unhealthy_canteen_food_nutrition_effect)
 unhealthy_canteen_foods <- unhealthy_canteen_foods* (1 - limit_unhealthy_canteen_food_nutrition_effect)
 children_consume_healthy_food <- children_consume_healthy_food* (1 + limit_unhealthy_canteen_food_nutrition_effect)
 children_access_healthy_food <- children_access_healthy_food* (1 + limit_unhealthy_canteen_food_nutrition_effect)
 }
 
 #
 # Off-campus food environment: competition with school food
 #
 # Cheap, energy-dense junk food sold by street vendors at school gates
 # competes directly with the school meal program. None of the five
 # interventions controls the vendors themselves, so gate food is a
 # background risk that cannot be eliminated by school actors alone.
 #
 # However, three of the five interventions make the school food environment
 # more competitive, reducing how much gate food displaces healthy eating:
 #
 #  Nutrition training → children learn to value nutritious food;
 #  staff reinforce healthy choices at mealtimes,
 #  making gate junk food a less attractive option.
 #  Menu change to RDA → school meals become nutritionally complete
 #  and more filling, reducing the appetite for
 #  gate top-ups after lunch.
 #  Limit unhealthy → removes the easy on-campus junk-food option,
 #  canteen food so gate vendors become conspicuously "the
 #  unhealthy alternative" rather than a canteen
 #  substitute; also reinforces healthy eating norms.
 #
 # Food safety training and physical activity influence the general health
 # and behavioral environment but do not directly affect the school-vs-gate
 # food competition, so they do not reduce the effect (below as 'attenuation')
 #
 # WITHOUT the five actions (do-nothing):
 #  gate food harm operates at full strength — no school food is attractive
 #  enough to displace vendor purchases, and health gains are maximally eroded.
 #
 # WITH actions active:
 #  gate food harm is attenuated in proportion to the combined effect of the
 #  competitive interventions, so more of the policy gains survive.
 #
 # This is the key mechanism that makes the with/without policy comparison
 # meaningful in the presence of the off-campus food environment.
 #
 # Gate food harm index: expected fraction of students experiencing
 # nutritional harm from off-campus vendors (prevalence × risk × impact).
 #  unhealthy_school_gate_foods  = prevalence of exposure
 #  unhealthy_schoolgate_food_risk  = P(harm | exposure)
 #  impact_risk_unhealthy_schoolgate_food = damage fraction if harm occurs
 gate_food_harm <- unhealthy_school_gate_foods*
 unhealthy_schoolgate_food_risk*
 impact_risk_unhealthy_schoolgate_food

 # Baseline disease burden (no-policy arm): gate food adds illness even
 # without any school intervention — used in the do-nothing counterfactual.
 baseline_disease_diagnosis <- baseline_disease_diagnosis* (1 + gate_food_harm)
 baseline_disease_treatment <- baseline_disease_treatment * (1 + gate_food_harm)

 # Gate food attenuation from the three competitive interventions.
 # Each active intervention reduces the effective harm by its own effect size.
 # Summed and clamped to [0, 1]; zero when no relevant action is taken.
 gate_attenuation <- 0
 if (staff_training_nutrition)
 gate_attenuation <- gate_attenuation + staff_training_nutrition_effect
 if (use_menu_change_rda)
 gate_attenuation <- gate_attenuation + menu_change_rda_nutrition_effect
 if (use_limit_unhealthy_canteen_food)
 gate_attenuation <- gate_attenuation + limit_unhealthy_canteen_food_nutrition_effect
 gate_attenuation <- min(1, gate_attenuation)

 # Effective gate food harm under the policy scenario.
 # No actions active → gate_food_harm_policy = gate_food_harm (full erosion)
 # All three active  → gate_food_harm_policy approaches 0  (minimal erosion)
 gate_food_harm_policy <- gate_food_harm* (1 - gate_attenuation)

 # Policy gains are scaled down by the residual (policy-scenario) harm.
 # Contrast with the baseline above, which uses the full harm: this asymmetry
 # is what produces the NPV difference between acting and not acting.
 gate_food_offset <- 1 - gate_food_harm_policy
 n_reduce_disease_diagnosis <- n_reduce_disease_diagnosis* gate_food_offset
 n_reduce_disease_treatment <- n_reduce_disease_treatment* gate_food_offset
 n_sick_days_avoided_per_student <- n_sick_days_avoided_per_student* gate_food_offset

 # Composite unhealthy food exposure: mean across on-campus (canteen) and
 # off-campus (gate vendors, advertising) sources. Gate food prevalence
 # enters here as the exposure score — kept separate from the harm index above.
 unhealthy_food_exposure <- mean(c(unhealthy_canteen_foods, unhealthy_school_gate_foods, advertisement_exposure))
 
 # Policy impacts (attenuated by barriers)
 food_access_score <- children_access_healthy_food* (1 - unhealthy_food_exposure)* (1 - resistance_child_preferences_attitude)
 food_consumption_score <- children_consume_healthy_food* (1 - unhealthy_food_exposure)* (1 - resistance_child_preferences_attitude)
 
 # Peer influence (negative)
 if (chance_event(peer_influence_factor)) {
 food_consumption_score <- food_consumption_score* (1 - peer_influence_factor)
 }
 
 # BMI and disease outcomes
 if ((food_access_score + food_consumption_score) >= food_access_and_consumption_threshold) {
 bmi_high <- nutrition_status_bmi_high* (1 - food_access_and_consumption_reduce_overweight) 
 bmi_low <- nutrition_status_bmi_low* (1 - food_access_and_consumption_reduce_underweight)
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
 diagnosis_multiplier <- diagnosis_multiplier + excess_overweight* 
 overweight_diagnosis_risk_multiplier
 treatment_multiplier <- treatment_multiplier + excess_overweight* 
 overweight_treatment_risk_multiplier
 }
 
 # Underweight increases risk
 if (bmi_low > underweight_threshold) {
 excess_underweight <- bmi_low - underweight_threshold
 diagnosis_multiplier <- diagnosis_multiplier + excess_underweight* 
 underweight_diagnosis_risk_multiplier
 treatment_multiplier <- treatment_multiplier + excess_underweight* 
 underweight_treatment_risk_multiplier
 }
 
 # Adjust baseline disease burden based on BMI-related risk
 baseline_disease_diagnosis <- baseline_disease_diagnosis* 
 diagnosis_multiplier
 baseline_disease_treatment <- baseline_disease_treatment* 
 treatment_multiplier
 
 # Health costs with and without policy
 # Health utility ####
 # nutrition policy would reduce treatment and diagnosis costs
 # How many fewer health events per VND spent?
 # A discussion among school nurses indicated that approximately 3-4% of the student body visits the school nurse each day
 # 50% to 70% of nurse visits result in some form of treatment or medication

 # Clamp costs to non-negative (norm distribution can sample below zero)
 disease_diagnosis_cost <- max(0, disease_diagnosis_cost)
 disease_treatment_cost <- max(0, disease_treatment_cost)

 # Clamp reductions so they cannot exceed the baseline rate (toggle compounding can push n_reduce above baseline)
 n_reduce_disease_diagnosis <- min(n_reduce_disease_diagnosis, baseline_disease_diagnosis)
 n_reduce_disease_treatment <- min(n_reduce_disease_treatment, baseline_disease_treatment)
 n_sick_days_avoided_per_student <- min(n_sick_days_avoided_per_student, baseline_sick_days_per_student)

 baseline_health_costs <- ((baseline_disease_diagnosis* disease_diagnosis_cost) +
 (baseline_disease_treatment* disease_treatment_cost))* n_student

 policy_health_costs <- (((baseline_disease_diagnosis - n_reduce_disease_diagnosis)*
 disease_diagnosis_cost) +
 ((baseline_disease_treatment - n_reduce_disease_treatment)*
  disease_treatment_cost))*
   n_student
 
 # Net health benefit (VND)
 # Includes direct cost savings from reduced nurse visits AND
 # absenteeism benefit: fewer sick days lost = more learning time + less admin disruption
 sick_days_avoided <- n_sick_days_avoided_per_student* n_student
 absenteeism_benefit <- sick_days_avoided* value_per_sick_day_lost
 net_health_benefit <- (baseline_health_costs - policy_health_costs) + absenteeism_benefit
 
 # Education benefit (incremental gain above baseline)
 # Baseline learning value without any intervention (independent input parameter)
 education_benefit_baseline <- baseline_student_performance* n_student* value_of_learning_per_student
 # Policy improves performance above baseline by student_performance_improvement (fractional uplift)
 education_benefit_policy <- education_benefit_baseline* (1 + student_performance_improvement)
 # Incremental gain attributable to the intervention only
 education_benefit <- education_benefit_policy - education_benefit_baseline
 # simplifies to: baseline_student_performance* student_performance_improvement* n_student* value_of_learning_per_student
 
 # Annual policy cost
 annual_policy_costs <- training_costs_foodsafety_annual + training_costs_nutrition_annual +
 training_costs_physical_activity_annual + change_menu_costs_annual +
 monitoring_canteen_cost
 policy_cost <- vv(annual_policy_costs, var_CV = CV_value, n = number_of_years)
 policy_cost[1] <- policy_cost[1] + training_costs_foodsafety_1st_year + training_costs_nutrition_1st_year + training_costs_physical_activity_1st_year
 
 # Benefits under policy: education value + absenteeism savings
 # Health costs are kept on the cost side (below) so both NPVs are interpretable
 annual_policy_benefit <- education_benefit_policy + absenteeism_benefit
 policy_benefit <- vv(annual_policy_benefit, var_CV = CV_value, n = number_of_years)
 
 # Health costs as annual time series on the cost side
 annual_policy_health_cost <- vv(policy_health_costs, var_CV = CV_value, n = number_of_years)
 # Total outflows: implementation costs + ongoing health costs under policy
 total_policy_outflow <- policy_cost + annual_policy_health_cost
 
 # Do-nothing: education value minus full baseline health burden
 # Both NPVs now reflect health costs, so npv_no_policy is interpretable as the value of inaction
 annual_no_policy_benefit <- vv(education_benefit_baseline, var_CV = CV_value, n = number_of_years)
 annual_no_policy_health_cost <- vv(baseline_health_costs, var_CV = CV_value, n = number_of_years)
 no_policy_result <- annual_no_policy_benefit - annual_no_policy_health_cost
 
 # Cashflows
 cashflow_policy <- policy_benefit - total_policy_outflow
 cashflow_health <- vv(net_health_benefit, var_CV = CV_value, n = number_of_years)
 cashflow_education <- vv(education_benefit, var_CV = CV_value, n = number_of_years)
 
 # Net Present Values
 npv_policy <- discount(policy_benefit - total_policy_outflow, discount_rate, TRUE)
 npv_no_policy <- discount(no_policy_result, discount_rate, TRUE)
 
 return(list(
 decision_value = npv_policy - npv_no_policy,
 net_health_benefit = net_health_benefit,
 education_benefit = education_benefit,
 cashflow_policy = cashflow_policy,
 cashflow_health = cashflow_health,
 cashflow_education = cashflow_education,
 use_staff_training_foodsafety = use_staff_training_foodsafety,
 use_staff_training_nutrition = use_staff_training_nutrition,
 use_physical_activity = use_physical_activity,
 use_menu_change_rda = use_menu_change_rda,
 use_limit_unhealthy_canteen_food = use_limit_unhealthy_canteen_food))
}

