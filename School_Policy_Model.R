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
  trainingcost <-vv(training_costs_foodsafety_annual,var_CV=CV_value,n = number_of_years)
  trainingcost[1]<-training_costs_foodsafety_1st_year
  training_costs_nutrition_1st_year
  training_costs_nutrition_annual
  training_costs_physical_activity_1st_year
  training_costs_physical_activity_annual
  change_menu_costs_annual
  mornitoring_cost
  
  
  # Risks ####
  unhealthy_schoolgate_food_risk
  unhealthy_risk=chance_event(unhealthy_schoolgate_food_risk)
  # These are 'ex-ante' risks, or risks understood when making a decision
  
  ####
  
  # Add up all benefits and risks ####
  total_benefit_no_policy
  disease_diagnosis
  n_disease_diagnosis
  disease_diagnosis_benefit<-vv(disease_diagnosis,var_CV=CV_value,n = number_of_years)*
    n_disease_diagnosis*n_student
  disease_treatment
  n_disease_treatment
  
   
  no_intervention_result <- total_benefit_no_policy - total_costs_no_policy
  
  # Final result of the costs and benefits policy
  policy_intervention_result <- (disease_diagnosis_benefit) - trainingcost  # etc. 
    
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
              NPV_no_policy = NPV_no_interv,
              decision = NPV_interv_policy - NPV_no_interv,
              Cashflow_policy = policy_intervention_result, 
              Cashflow_no_policy = no_intervention_result))
}


# Run the model 

set.seed(84) 

garden_simulation_results <- mcSimulation(
  estimate = estimate_read_csv("inputs_school_policy.csv"),
  model_function = school_policy_function,
  numberOfModelRuns = 1000, #run 1000 times
  functionSyntax = "plainNames"
)

# Plot comparative results

plot_distributions(mcSimulation_object = garden_simulation_results, 
                   vars = c("NPV_interv_policy","NPV_interv_policy"),
                   method = 'hist_simple_overlay', 
                   base_size = 7, 
                   x_axis_name = "Comparative NPV outcomes")

plot_distributions(mcSimulation_object = garden_simulation_results, 
                   vars = c("decision"), # NPV_interv - NPV_no_interv
                   method = 'hist_simple_overlay', 
                   base_size = 7,  
                   x_axis_name = "Expected gains with school policy (policy - no policy)")




