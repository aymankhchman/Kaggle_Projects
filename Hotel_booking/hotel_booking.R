library("tidyverse")
library("tidymodels")

detach("tidymodels")

df <- read_csv(here::here(filename = "hotel_bookings.csv"))



df %>%
  count(children)




# We'll try to transform the variable children into a factor .


df <- df %>%
  mutate(children = case_when(children + babies > 0 ~ "children",
                              TRUE ~ "no")) 


df_numeric <- df %>%
            select_if(is.numeric)

df %>%
  view()


### Do parents with children tend to take children to different type of hotel ?


df %>%
  ggplot(mapping = aes(hotel,fill = hotel))+
  geom_bar()+
  theme_minimal()

#Our database contain more people going to city hotel than resort hotel

df %>%
  group_by(hotel)%>%
  mutate(children = ifelse(children == "children",1,0))%>%
  summarise(avg = mean(children))%>%
  ggplot(mapping = aes(hotel,avg))+
  geom_point(aes(color = hotel,size = 2))+
  geom_segment(aes(x = hotel,xend = hotel  , y =0 ,yend =avg),color = "gray50")+
  theme_minimal()+
  labs(title =  "Proportion of people at the hotel with children given the type of hotel",
       y = "Proportion")+
  theme(legend.position = "")



 ### How about check days people stays the weekend or week ? 

# Make this into one plot

df %>%
  pivot_longer(cols = c(stays_in_week_nights,stays_in_weekend_nights),
               names_to = "type_day",
               values_to = "value")%>%
  filter(value == 0)%>%
  ggplot(mapping = aes(children,fill = children))+
  geom_bar()+
  facet_wrap(~type_day)+
  theme_minimal()+
  theme(legend.position = "",
        panel.grid = element_blank())

#We can see here a big disproportion , most people with no children comes on the weekend nights


#Then this should be an interesting variable,since we can stay on weekend and week nights we can't make only one
#variable


df <- df %>%
  mutate(stays_in_weekend_nights = ifelse(stays_in_weekend_nights > 0 , "yes","no"),
         stays_in_week_nights = ifelse(stays_in_week_nights > 0 , "yes","no"))





## Will the lead variable be important ? Do people with children reserve much earlier ?



df %>%
  mutate(country = fct_lump(country,10))%>%
  ggplot(mapping = aes(children,lead_time,fill = children))+
  geom_boxplot()+
  theme_minimal()+
  coord_flip()+
  facet_wrap(~country)



#Different type of result depending on the country .

# Let's check the country variable


df %>%
  mutate(country = fct_lump(country,10))%>%
  group_by(country)%>%
  mutate(children = ifelse(children == "children" , 1 , 0))%>%
  summarise(avg = mean(children))%>%
  mutate(country = fct_reorder(country,avg))%>%
  ggplot(mapping = aes(country,avg))+
  geom_point()+
  coord_flip()+
  theme_minimal()

### Does month can give us an insght on the people who have children ? 


df %>%
  count(arrival_date_month,sort = TRUE)

library("tidytext")

df %>%
  group_by(arrival_date_month,children)%>%
  add_count()%>%
  ungroup()%>%
  mutate(arrival_date_month = reorder_within(arrival_date_month,n,children,fun= sum))%>%
  ggplot(mapping = aes(arrival_date_month,fill = arrival_date_month))+
  geom_bar()+
  facet_wrap(~children,scales = "free_y")+
  scale_x_reordered()+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        legend.position = "none")+
  coord_flip()

#We can see that month can be an interesting variable


# Is adr an important variable ? 


df %>%
  ggplot(mapping = aes(children,adr,fill = children))+
  geom_boxplot()+
  theme_minimal()+
  coord_flip()

# So adr is an important variable !

# What about the meal variable ? 

df %>%
  count(meal,children)

df %>%
  ggplot(mapping = aes(children,fill = meal))+
  geom_bar()+
  facet_wrap(~meal)

#meal is an important varibale ! 




# Parking variable ! 

df %>%
  count(required_car_parking_spaces,children)

df %>%
  ggplot(mapping = aes(children ,fill = children))+
  geom_bar()+
  facet_wrap(~required_car_parking_spaces,scales = "free_y")


#We can turn this into a dummy variable

df <- df %>%
  mutate(required_car_parking_spaces = ifelse(required_car_parking_spaces > 0 , "parking","no_parking"))


# Lets check special_request since children are full of surprises ! 

df %>%
  count(total_of_special_requests)

df %>%
  mutate(total_of_special_requests = ifelse(total_of_special_requests >0,1,0))%>%
  group_by(children)%>%
  summarise(avg = mean(total_of_special_requests))%>%
  ggplot(mapping = aes(children , avg))+
  geom_point(aes(color = children),size = 4)+
  geom_segment(aes(x = children , xend = children , y = 0 , yend = avg),color = "gray50")+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        legend.position = "")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "", y = "Moyenne",title = "Pourcentage de ceux qui utlisent le parking")


df_model <- df %>%
  select(children , arrival_date_month , hotel, stays_in_week_nights,
         stays_in_weekend_nights,total_of_special_requests,required_car_parking_spaces,meal,adr)%>%
  mutate_if(is.character,factor)

  df_model <- df_model %>%
  mutate(children = ifelse(children == "children",1,0),
         children = factor(children))

#### Let's build our model using  the tidymodel  packages

set.seed(129)
df_model_split <- initial_split(df_model,strata = children)
df_model_train <- training(df_model_split)
df_mdeol_test <- testing(df_model_split)

####
model_rec <- recipe(children ~ . ,data = df_model_train) %>%
          step_zv(all_numeric())%>%
          step_dummy(all_nominal(),-all_outcomes())%>%
          step_naomit(everything())%>%
          step_smote(children)

model_prep <- prep(model_rec)

model_2rec <- recipe(children ~ . , data = df_model_train) %>%
  step_dummy(all_nominal(),-all_outcomes())%>%
  step_smote(children)
 
juice(model_prep) %>%
  count(children)


# Let's build an xgboost model

xgb_spec <- boost_tree(
trees = 1000,
tree_depth = tune(),
min_n = tune(),
loss_reduction = tune(),
sample_size = tune(),
mtry = tune(),
learn_rate = tune()
) %>%
  set_engine("xgboost")%>%
  set_mode("classification")


xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(),df_model_train),
  learn_rate(),
  size = 50
)


xgb_wf <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(model_rec)




#### V-fold in order to choose the best parameter



set.seed(161)


hotel_folds <- vfold_cv(df_model_train,strata = children,v = 3)

#To make the process faster

doParallel::registerDoParallel()


xgb_res <- tune::tune_grid(
  xgb_wf,
  resamples = hotel_folds,
  grid = xgb_grid,
  control = tune::control_grid(save_pred = TRUE)
  )

xgb_res


best_auc <- select_best(xgb_res,"roc_auc")


final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)

install.packages("vip")
library("vip")

final_xgb %>%
  fit(data = df_model_train)%>%
  pull_workflow_fit()%>%
  vip(geom = "col")


result <- last_fit(final_xgb,df_model_split)

result %>%
  collect_predictions()%>%
    conf_mat(.pred_class,truth = children)

result %>%
  collect_metrics()

results <- final_res$pre$mold$outcomes


table(results,df_mdeol_test$children)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")


tune_wf <- workflow() %>%
  add_recipe(model_rec) %>%
  add_model(tune_spec)


doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = hotel_folds,
  grid = 20
)


