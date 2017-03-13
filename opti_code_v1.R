
options(scipen=999)
rm(list = ls())
library(dplyr)
library(ggplot2)
library(scatterplot3d)
library(plotly)
library(KRLS)
library(rdetools)
library(dplyr)
library(OpenMx)
library(e1071)
setwd("C:/Users/Akash Rastogi/Desktop/UMich_study_material/winter_2017/optimisation_project/")
data_sim <- read.csv("opti_data_input.csv")
data_sim_2 <- read.csv("opti_sim_input_2.csv")
data_sim_2$team <- as.character(data_sim_2$team)
data_sim$team <- as.character(data_sim$team)


# Merging the two datasets
data_sim_v1 <- data_sim %>%
  group_by(team, Tag) %>%
  summarise(Value = mean(Value),
            Ball.Control = mean(Ball.Control),
            Dribbling = mean(Dribbling),
            Marking = mean(Marking),
            Slide.Tackle = mean(Slide.Tackle),
            Stand.Tackle = mean(Stand.Tackle),
            Aggression = mean(Aggression),
            Reactions = mean(Reactions),
            Att..Position = mean(Att..Position),
            Interceptions = mean(Interceptions),
            Vision = mean(Vision),
            Composure = mean(Composure),
            Crossing = mean(Crossing),
            Short.Pass = mean(Short.Pass),
            Long.Pass = mean(Long.Pass),
            Acceleration = mean(Acceleration),
            Stamina = mean(Stamina),
            Strength = mean(Strength),
            Balance = mean(Balance),
            Sprint.Speed = mean(Sprint.Speed),
            Agility = mean(Agility),
            Jumping = mean(Jumping),
            Heading = mean(Heading),
            Shot.Power = mean(Shot.Power),
            Finishing = mean(Finishing),
            Long.Shots = mean(Long.Shots),
            Curve = mean(Curve),
            FK.Acc. = mean(FK.Acc.),
            Penalties = mean(Penalties),
            Volleys = mean(Volleys),
            GK.Positioning = mean(GK.Positioning),
            GK.Diving = mean(GK.Diving),
            GK.Handling = mean(GK.Handling),
            GK.Kicking = mean(GK.Kicking),
            GK.Reflexes = mean(GK.Reflexes))

data_full <- left_join(data_sim_v1, data_sim_2)
data_full <- data_full %>%
  ungroup()

# Model for striker position
data_striker <- data_full %>%
  filter(Tag == 1)

# t <- step(glm(gf ~ Ball.Control+
#                Dribbling+ 
#                Aggression+
#                Acceleration+
#                Stamina+ 
#                Sprint.Speed+
#                Agility+
#                Jumping+
#                Heading+
#                Shot.Power+
#                FK.Acc.+
#                Penalties+
#                Volleys + 
#                Value, data = data_striker, family = poisson(link = "log")), direction = "both")
# 
# t.best.poisson <- glm(gf ~ Ball.Control + Dribbling  + 
#                         Heading + Value + Stamina + FK.Acc., data= data_striker, 
#                       family = poisson(link = "log"))  

t.best.lr <- lm(gf ~ Ball.Control + Dribbling  + 
                  Heading + Value + Stamina + FK.Acc., 
                      data = data_striker )  
summary(t.best.lr)


# Model for midfield position
data_midfield <- data_full %>%
  filter(Tag == 4)

t <- step(glm(asst ~ Ball.Control+
                Stamina+
                Agility+
                Shot.Power+
                Interceptions+
                Vision+
                Crossing+
                Short.Pass+
                Long.Pass+
                Balance+
                Long.Shots+
                Curve + Value, data = data_midfield, family = poisson(link = "log")), direction = "both")

t.best.poisson.midfield <- glm(asst ~ Ball.Control + Long.Pass + Curve + Value, 
                      data = data_midfield, family = poisson(link = "log"))  

t.best.lr.midfield <- lm(asst ~ Ball.Control + Stamina + Shot.Power + Vision + 
                  + Value, 
                data = data_midfield)  



# Model for defense position
data_defense <- data_full %>%
  filter(Tag == 2)

t <- step(glm(ga ~ Stamina+
                Interceptions+
                Acceleration+
                Jumping+
                Heading+
                Marking+
                Slide.Tackle+
                Stand.Tackle+
                Reactions + Value, data = data_defense, family = poisson(link = "log")), direction = "both")

t.best.poisson.defense <- glm(ga ~ Stamina + Acceleration + Jumping + Stand.Tackle + Heading +
                                Marking + Reactions + Value, 
                               data = data_defense, family = poisson(link = "log"))  
summary(t.best.poisson.defense)

t.best.lr.defense <- lm(ga ~ Stamina + Acceleration + Jumping + Stand.Tackle + Heading +
                          Marking + Reactions + Value, 
                         data = data_defense)  

summary(t.best.lr.defense)
plot(t.best.lr.defense)


summary(t.best.lr.midfield)


# Prediction for the striker position
player_data <- read.csv("matlab_player_data.csv")
budget  = 50000000 * 11
budget_striker = budget * 3 / 11

pred_striker_data <- player_data %>%
  filter(Tag == 1)

param_space <- data.frame(alpha = 0, beta = 0, gamma = 0)
alpha <- seq(0.35, 0.70, 0.02)
beta <- seq(0.05, 0.60, 0.02)

for(i in 1:length(alpha)) {
  for(j in 1:length(beta)){
    gamma = round(1 - alpha[i] - beta[j], digits = 2)
    if(alpha[i] >= beta[j] & beta[j] >= gamma & gamma > 0)
      param_space <- rbind(param_space, data.frame(alpha = alpha[i], beta = beta[j],
                                                   gamma = gamma))
  }
}

pred_striker_data$pred_y <- predict(t.best.lr, newdata = pred_striker_data)

for(i in 1:length(param_space)) {
  striker_1_d <- pred_striker_data %>%
    filter(Value <= param_space[i, "alpha"] * budget_striker) %>%
    arrange(desc(pred_y)) %>%
    dplyr::select(Final.row.number) %>%
    filter(row_number()==1)
  
  striker_2_d <- pred_striker_data %>%
    filter(Value <= param_space[i, "beta"] * budget_striker) %>%
    arrange(desc(pred_y)) %>%
    dplyr::select(Final.row.number) %>%
    filter(row_number()==1)
  
  
  striker_3_d <- pred_striker_data %>%
    filter(Value <= param_space[i, "gamma"] * budget_striker) %>%
    arrange(desc(pred_y)) %>%
    dplyr::select(Final.row.number) %>%
    filter(row_number()==1)
  
  
  
}

# flag = 0
# gf_predict <- 0
# best_strikers <- c(0, 0, 0, 0)
# for(i in 1:nrow(striker_data)) {
#   for(j in i+1:nrow(striker_data)){
#     for(k in j+1:nrow(striker_data)){
#       for(l in k+1:nrow(striker_data)){
#         temp_data = rbind(striker_data[i,] + striker_data[j,] + striker_data[k,] + 
#                             striker_data[l,])
#         temp_data = temp_data %>%
#           summarise(Value = mean(Value),
#                     Ball.Control = mean(Ball.Control),
#                     Dribbling = mean(Dribbling),
#                     Stamina = mean(Stamina),
#                     Heading = mean(Heading),
#                     FK.Acc. = mean(FK.Acc.))
#         gf_pred_temp <- predict(object = t.best.lr, data = temp_data)
#         if(gf_pred_temp > gf_predict){
#           gf_predict = gf_pred_temp
#           best_strikers <- c(i, j, k, l)
#         }
#         print(flag)
#         flag = flag +1
#       }
#     }
#    }
# }
