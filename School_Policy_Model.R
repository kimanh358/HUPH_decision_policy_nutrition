# Model of the school policy

# make variables for testing our model (only for construction)
library(decisionSupport)

make_variables <- function(est,n=1) #take one time #  
{x <- decisionSupport::random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

#Just for one time# 
make_variables(decisionSupport::estimate_read_csv(paste("inputs_school_policy.csv",sep="")))


# The model #### 

school_policy_function <- function(x, varnames){
  
 #positive nutrition attitude practice
 positive_board <- chance_event(practice_school_management_board) #thuc hanh cua ban giam hieu
 if (positive_board == 1) {  #neu thuc hanh 100% thì se lay duoc nhieu benefit, hoac se giam tat ca loi ich 
   # getting more benefit because of positive board 
   staff_knowledge_food_safety
   staff_knowledge_nutrition
   health_lessons_class
   food_safety_practices_staff
   meal_nutrition_practices_staff
 } else {
   # or reduce all the  benefit 
   staff_knowledge_food_safety <- staff_knowledge_food_safety* (1-practice_school_management_board)
   staff_knowledge_nutrition <- staff_knowledge_nutrition* (1-practice_school_management_board)
   health_lessons_class <- health_lessons_class* (1-practice_school_management_board)
   food_safety_practices_staff <- food_safety_practices_staff* (1-practice_school_management_board)
   meal_nutrition_practices_staff <- meal_nutrition_practices_staff* (1-practice_school_management_board)
 } 
     
  #parent attitude practice abt nutrition and foodsafety at school
  #kien thuc thuc hanh cua bo me ve dinh duong va attp tai truong
  positive_parents <- chance_event(parents_monitor_school_meal_practices)
  if (positive_parents == 1) {  #neu cha me giam sat dung  
    # getting more benefit because of positive parents 
    #thi cac nhan vien se thuc hanh dung, hoac nguoc lai 
    food_safety_practices_staff <- food_safety_practices_staff* 
      food_safety_practices_staff_if_parents_monitor #increase benefit
    meal_nutrition_practices_staff <- meal_nutrition_practices_staff* 
      meal_nutrition_practices_staff_if_parents_monitor
  } else {
    # or keep all the  benefit 
    #giu nguyen cac loi ich
    food_safety_practices_staff 
    meal_nutrition_practices_staff 
  }
  
  #overwork teacher reduce education benefit
  #gv qua nhieu viec dan den giam loi ich cua cac lop giang day ve suc khoe
  overwork_teacher <- chance_event(teaching_time) #teachers have no time to teach health lessons 
  if (overwork_teacher == 1) {  #neu gv 
    # reduce the  benefit 
    health_lessons_class <- health_lessons_class * (1-teaching_time)
  } else {
    # keep the benefit because of positive teachers 
    #giu nguyen cac loi ich 
    health_lessons_class
  }
  
  #resistance from the staff
  #can tro tu nhan vien 
  bad_belief_staff <- chance_event(resistance_existing_staff_belief) # nhan vien co niem tin k dung
  bad_practice_staff <- chance_event(resistance_existing_staff_practices) #nhan vien co thuc hanh k dung
  if (bad_belief_staff == 1 | bad_practice_staff==1) {  #neu nhan vien co kien thuc hoac thuc hanh k dung
    # reduce benefit because of bad staff  #thi se giam loi ich
    food_safety_practices_staff <- food_safety_practices_staff * 
      (1-max(resistance_existing_staff_belief,resistance_existing_staff_practices))
    meal_nutrition_practices_staff <- meal_nutrition_practices_staff * 
      (1-max(resistance_existing_staff_belief,resistance_existing_staff_practices))
  } else {  # hoac nguoc lai, 
    # keep the benefit because of positive staff 
    food_safety_practices_staff
    meal_nutrition_practices_staff
  }
  
  #risk to children access and consume unhealthy food
  #nguy co tu tre em 
  bad_gate_food <- chance_event(unhealthy_school_gate_foods)
  bad_canteen_food <- chance_event(unhealthy_canteen_foods)
  if (bad_gate_food == 1 | bad_canteen_food==1) { 
    # reduce access and consume healthy food
    children_access_healthy_food <- children_access_healthy_food *  #
      (1-max(unhealthy_canteen_foods,unhealthy_school_gate_foods))
    children_consume_healthy_food <- children_consume_healthy_food * 
      (1-max(unhealthy_canteen_foods,unhealthy_school_gate_foods))
  } else {
    # access and consume healthy food
    children_access_healthy_food
    children_consume_healthy_food
  }
  
  #risk of not enough budget for Min. requirement school meal
  #nguy co tu viec khong du budget 
  budget_lacking <- chance_event(costs_exceed_budget)
  if (budget_lacking == 1) { 
    # reduce the  benefit 
    school_meet_mealrequirement <- (school_meet_mealrequirement +
      food_safety_practices_staff +
      meal_nutrition_practices_staff) * (1-costs_exceed_budget) #how much will it be reduce  
  } else {
  # getting more benefit because of meeting requirement 
    school_meet_mealrequirement <- school_meet_mealrequirement +
      food_safety_practices_staff +
    meal_nutrition_practices_staff
  }
  
  #children resistance
child_resistance <- chance_event(resistance_child_preferences_attitude)
 if (child_resistance == 1) {  #neu su can tro =1 tuc la se giam su tiep can va tieu thu healthy food
   # reduce access and consume healthy food
   children_access_healthy_food <- children_access_healthy_food * 
     (1-resistance_child_preferences_attitude)
   children_consume_healthy_food <- children_consume_healthy_food * 
     (1-resistance_child_preferences_attitude)
 } else {  # hoac la nguoc lai 
   # access and consume healthy food
   children_access_healthy_food
   children_consume_healthy_food
 }
  
#neu tre em tiep can va tieu thu unhealthy food vuot nguong, bmi se rat cao hoac thap, hoac giu nguyen 
if ((children_access_healthy_food + children_consume_healthy_food) >= food_access_and_consumption_threshold ) {
nutrition_status_bmi_high <- nutrition_status_bmi_high * 
  (1 - food_access_and_consumption_reduce_overweight)  # phan tram cua reduction 
nutrition_status_bmi_low <- nutrition_status_bmi_low * 
  (1 - food_access_and_consumption_reduce_underweight)
} else {  # keep nutrition status
  nutrition_status_bmi_high
  nutrition_status_bmi_low
}
  


  # Costs####
  # annual costs for the policy 
# tinh tong cost cho cac hoat dong hang nam 
annual_policy_costs <- training_costs_foodsafety_annual + 
  training_costs_nutrition_annual + # for staff 
  training_costs_physical_activity_annual + #for physical activities 
  change_menu_costs_annual + # for RDA menu assumming slightly more 
  monitoring_canteen_cost # for the monitoring the canteeen

  # establishing the policy at school
# Establishment cost in 1st year#
#cost for training nutrition, foodsafety and activities 
#cost cho cac hoat dong nam dau tien 
policy_1st_year_cost <- training_costs_foodsafety_1st_year + 
  training_costs_nutrition_1st_year + 
  training_costs_physical_activity_1st_year
  
  policy_cost <-vv(annual_policy_costs, #tao ra chuoi bien thien ve thoi gian cua policy cost 
                    var_CV = CV_value , 
                    n = number_of_years)
  
  # add establishment to first year
  #chon 1 gia tri trong 5 nam do#
  policy_cost[1] <- policy_1st_year_cost + policy_cost[1] 

  # neu bmi vuot qua nguong thi cost for disease dignosis se tang cao, neu khong thi se giu nguyen 
  if ((nutrition_status_bmi_high + nutrition_status_bmi_low) >= bmi_disease_threshold) {
    disease_diagnosis <- disease_diagnosis * (1 + bmi_disease_increase) 
  } else {
    disease_diagnosis
  }
  
   
  # Add up all benefits and risks ####
  #reduce cost of paying for doctor visits#
  #think about it lately#
  #them tat ca risk and benefit 
  #

 annual_benefit_policy <-  (disease_diagnosis * #cost of a student getting diagnosis #
                               (n_reduce_disease_diagnosis * #how many fewer dígnosis per student year with policy  #
                                  n_student)) +  # the number of student in school# 
    # reduce cost of paying for treatment (hospital or home)
    (disease_treatment * #cost of a student getting treatment 
     (n_reduce_disease_treatment *#how many fewer treatment  per student  year with policy  #
        n_student)) #the number of student in school
  
  # or keep the full benefit 
    policy_benefit <-  vv(annual_benefit_policy,
                        var_CV = CV_value,
                        n = number_of_years) 
                        
  # from the input table for now
  no_policy_result <- vv(total_benefit_no_policy - total_costs_no_policy, 
                               var_CV = CV_value,
                               n = number_of_years)
  
  # Final result of the costs and benefits policy
  policy_intervention_result <- policy_benefit - policy_cost  # etc. 
    
  # calculate the Net Present Value (NPV) with with the specified discount rate
#NPV with policy#
  NPV_interv_policy <-  # npv khi co can thiep 
    discount(x = policy_intervention_result, 
             discount_rate = discount_rate, 
             calculate_NPV = TRUE)
  
  # NPV no policy ####
  NPV_no_policy <-     #npv khi khong co can thiep chinh sach
    discount(x = no_policy_result, 
             discount_rate = discount_rate, 
             calculate_NPV = TRUE)
  
  ### END of policy model script ###
  
  # Beware, if we do not name our outputs (left-hand side of the equal sign) in the return section, 
  # the variables will be called output_1, _2, etc.
  return(list(NPV_interv_policy = NPV_interv_policy,  
              NPV_no_policy = NPV_no_policy,
              decision = NPV_interv_policy - NPV_no_policy,
              Cashflow_policy = policy_intervention_result, 
              Cashflow_no_policy = no_policy_result))
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
                   vars = c("NPV_interv_policy","NPV_no_policy"),
                   method = 'hist_simple_overlay', 
                   base_size = 7, 
                   x_axis_name = "Comparative NPV outcomes (million VND)")

plot_distributions(mcSimulation_object = policy_simulation_results, 
                   vars = c("decision"), # NPV_interv - NPV_no_policy
                   method = 'hist_simple_overlay', 
                   base_size = 7,  
                   x_axis_name = "Expected gains with school policy (policy - no policy)")
#create 1 table#
mcSimulation_table <- data.frame(policy_simulation_results$x,
                                 policy_simulation_results$y[1:3])

EVPI <- multi_EVPI(mc = mcSimulation_table,
                   first_out_var = "NPV_interv_policy")

plot_evpi(EVPI, decision_vars = "decision") 

plot_cashflow(policy_simulation_results,cashflow_var_name="Cashflow_policy")


