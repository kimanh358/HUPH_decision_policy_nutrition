# Model of the school policy

# make variables for testing our model (only for construction)
library(decisionSupport)

make_variables <- function(est,n=1)
{x <- decisionSupport::random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}


make_variables(decisionSupport::estimate_read_csv(paste("inputs_school_policy.csv",sep="")))


# The model #### 

school_policy_function <- function(x, varnames){
  
  # Costs####
  # annual costs for the policy 
annual_policy_costs <- training_costs_foodsafety_annual + 
  training_costs_nutrition_annual + # for staff
  training_costs_physical_activity_annual + #for sports
  change_menu_costs_annual + # for RDA menu
    monitoring_cost # for the sugar and salt

  # establishing the policy at school
policy_1st_year_cost <- training_costs_foodsafety_1st_year + 
  training_costs_nutrition_1st_year + 
  training_costs_physical_activity_1st_year
  
  policy_cost <-vv(annual_policy_costs,
                    var_CV = CV_value , 
                    n = number_of_years)
  
  # add establishment to first year
  policy_cost[1] <- policy_1st_year_cost + policy_cost[1]

  # Risks ####
  unhealthy_risk = chance_event(unhealthy_schoolgate_food_risk)
  
  # Add up all benefits and risks ####

  annual_benefit_policy <-  (disease_diagnosis * (n_disease_diagnosis * n_student)) +  # paying for doctor visits
  (disease_treatment * (n_disease_treatment * n_student)) # paying for treatement (hospital or home)
 
  if (unhealthy_risk == 1) { 
    # reduce benefit because of the unhealthy school gate food
  policy_benefit <- vv(annual_benefit_policy,
                                var_CV = CV_value,
                                n = number_of_years) * 1-unhealthy_schoolgate_food_risk
  } else {
   # or keep the full benefit 
   policy_benefit <- vv(annual_benefit_policy,
                        var_CV = CV_value,
                        n = number_of_years) }
                        
  # from the input table for now
  no_intervention_result <- vv(total_benefit_no_policy - total_costs_no_policy, 
                               var_CV = CV_value,
                               n = number_of_years)
  
  # Final result of the costs and benefits policy
  policy_intervention_result <- policy_benefit - policy_cost  # etc. 
    
  # calculate the Net Present Value (NPV) with with the specified discount rate

  NPV_interv_policy <-
    discount(x = policy_intervention_result, 
             discount_rate = discount_rate, 
             calculate_NPV = TRUE)
  
  # NPV no intervention ####
  NPV_no_interv <-
    discount(x = no_intervention_result, 
             discount_rate = discount_rate, 
             calculate_NPV = TRUE)
  
  ### END of policy model script ###
  
  # Beware, if we do not name our outputs (left-hand side of the equal sign) in the return section, 
  # the variables will be called output_1, _2, etc.
  return(list(NPV_interv_policy = NPV_interv_policy, 
              NPV_no_interv = NPV_no_interv,
              decision = NPV_interv_policy - NPV_no_interv,
              Cashflow_policy = policy_intervention_result, 
              Cashflow_no_policy = no_intervention_result))
}


# Run the model 

set.seed(84) 

policy_simulation_results <- mcSimulation(
  estimate = estimate_read_csv("inputs_school_policy.csv"),
  model_function = school_policy_function,
  numberOfModelRuns = 1000, #run 1000 times
  functionSyntax = "plainNames"
)

# Plot comparative results

plot_distributions(mcSimulation_object = policy_simulation_results, 
                   vars = c("NPV_interv_policy","NPV_no_interv"),
                   method = 'hist_simple_overlay', 
                   base_size = 7, 
                   x_axis_name = "Comparative NPV outcomes (million VND)")

plot_distributions(mcSimulation_object = policy_simulation_results, 
                   vars = c("decision"), # NPV_interv - NPV_no_interv
                   method = 'hist_simple_overlay', 
                   base_size = 7,  
                   x_axis_name = "Expected gains with school policy (policy - no policy)")

mcSimulation_table <- data.frame(policy_simulation_results$x,
                                 policy_simulation_results$y[1:3])

EVPI <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_interv_policy")

plot_evpi(EVPI, decision_vars = "NPV_interv_policy") 



