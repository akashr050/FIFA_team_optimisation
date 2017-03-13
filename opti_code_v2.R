
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
t.best.poisson <- glm(gf ~ Ball.Control + Dribbling  + 
                        Heading + Value + Stamina + FK.Acc., data= data_striker,
                      family = poisson(link = "log"))

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


player_data <- read.csv("matlab_player_data.csv")
budget  = 10000000 * 11

# Prediction for the striker position
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

param_space <- param_space[2:nrow(param_space),]

pred_striker_data$pred_y <- predict(t.best.lr, newdata = pred_striker_data)

striker_player_fnl <- data.frame(alpha = numeric(),
                                 beta = numeric(),
                                 gamma = numeric(),
                                 player_1 = numeric(),
                                 player_2 = numeric(),
                                 player_3 = numeric(),
                                 pred_y = numeric())

for(i in 1:nrow(param_space)) {
  striker_1_d <- pred_striker_data %>%
    filter(Value <= param_space[i, "alpha"] * budget_striker) %>%
    arrange(desc(pred_y)) %>%
    dplyr::select(Final.row.number, pred_y) %>%
    filter(row_number()==1)
  
  striker_2_d <- pred_striker_data %>%
    filter(Value <= param_space[i, "beta"] * budget_striker & 
             Final.row.number != striker_1_d$Final.row.number) %>%
    arrange(desc(pred_y)) %>%
    dplyr::select(Final.row.number, pred_y) %>%
    filter(row_number()==1)
  
  
  striker_3_d <- pred_striker_data %>%
    filter(Value <= param_space[i, "gamma"] * budget_striker & 
             Final.row.number != striker_1_d$Final.row.number & 
             Final.row.number != striker_2_d$Final.row.number) %>%
    arrange(desc(pred_y)) %>%
    dplyr::select(Final.row.number, pred_y) %>%
    filter(row_number()==1)

  striker_player_fnl <- rbind(striker_player_fnl, data.frame(alpha = param_space[i, "alpha"],
                                          beta = param_space[i, "beta"],
                                          gamma = param_space[i, "gamma"],
                                          player_1 = striker_1_d$Final.row.number,
                                          player_2 = striker_2_d$Final.row.number,
                                          player_3 = striker_3_d$Final.row.number,
                                          pred_y = mean(striker_1_d$pred_y,
                                                        striker_2_d$pred_y, 
                                                        striker_3_d$pred_y)))
}

striker_player_fnl <- striker_player_fnl %>%
  arrange(desc(pred_y)) %>%
  filter(row_number() == 1)


# Prediction for the midfield position
budget_mid = budget * 4 / 11

pred_mid_data <- player_data %>%
  filter(Tag == 4)

param_space_th <- data.frame(alpha = 0, beta = 0, gamma = 0, theta = 0)
alpha <- seq(0.25, 0.70, 0.05)
beta <- seq(0.05, 0.60, 0.05)
gamma <- seq(0.05, 0.60, 0.05)
for(i in 1:length(alpha)) {
  for(j in 1:length(beta)){
    for(k in 1:length(gamma)){
      theta = round(1 - alpha[i] - beta[j] - gamma[k], digits = 2)
      if(alpha[i] >= beta[j] & beta[j] >= gamma[k] & gamma[k] >= theta & theta > 0)
        param_space_th <- rbind(param_space_th, data.frame(alpha = alpha[i], beta = beta[j],
                                                     gamma = gamma[k],
                                                     theta = theta))
    }
  }
}

param_space_th <- param_space_th[2:nrow(param_space_th),]

pred_mid_data$pred_y <- predict(t.best.lr.midfield, newdata = pred_mid_data)

mid_player_fnl <- data.frame(alpha = numeric(),
                                 beta = numeric(),
                                 gamma = numeric(),
                                 theta = numeric(),
                                 player_1 = numeric(),
                                 player_2 = numeric(),
                                 player_3 = numeric(),
                                 pred_y = numeric())

for(i in 1:nrow(param_space_th)) {
  mid_1_d <- pred_mid_data %>%
    filter(Value <= param_space_th[i, "alpha"] * budget_mid) %>%
    arrange(desc(pred_y)) %>%
    dplyr::select(Final.row.number, pred_y) %>%
    filter(row_number()==1)
  
  mid_2_d <- pred_mid_data %>%
    filter(Value <= param_space_th[i, "beta"] * budget_mid & 
             Final.row.number != mid_1_d$Final.row.number) %>%
    arrange(desc(pred_y)) %>%
    dplyr::select(Final.row.number, pred_y) %>%
    filter(row_number()==1)
  
  
  mid_3_d <- pred_mid_data %>%
    filter(Value <= param_space_th[i, "gamma"] * budget_mid & 
             Final.row.number != mid_1_d$Final.row.number & 
             Final.row.number != mid_2_d$Final.row.number) %>%
    arrange(desc(pred_y)) %>%
    dplyr::select(Final.row.number, pred_y) %>%
    filter(row_number()==1)
  
  
  mid_4_d <- pred_mid_data %>%
    filter(Value <= param_space_th[i, "theta"] * budget_mid & 
             Final.row.number != mid_1_d$Final.row.number & 
             Final.row.number != mid_2_d$Final.row.number &
             Final.row.number != mid_3_d$Final.row.number) %>%
    arrange(desc(pred_y)) %>%
    dplyr::select(Final.row.number, pred_y) %>%
    filter(row_number()==1)
  
  mid_player_fnl <- rbind(mid_player_fnl, data.frame(alpha = param_space_th[i, "alpha"],
                                                             beta = param_space_th[i, "beta"],
                                                             gamma = param_space_th[i, "gamma"],
                                                             theta = param_space_th[i, "theta"],
                                                             player_1 = mid_1_d$Final.row.number,
                                                             player_2 = mid_2_d$Final.row.number,
                                                             player_3 = mid_3_d$Final.row.number,
                                                             player_4 = mid_4_d$Final.row.number,
                                                             pred_y = mean(mid_1_d$pred_y,
                                                               mid_2_d$pred_y,
                                                               mid_3_d$pred_y,
                                                               mid_4_d$pred_y)))
}

mid_player_fnl <- mid_player_fnl %>%
  arrange(desc(pred_y)) %>%
  filter(row_number() == 1)



# Prediction for the defense position
budget_defense = budget * 3 / 11

pred_defense_data <- player_data %>%
  filter(Tag == 2)

pred_defense_data$pred_y <- predict(t.best.lr.defense, newdata = pred_defense_data)

defense_player_fnl <- data.frame(alpha = numeric(),
                                 beta = numeric(),
                                 gamma = numeric(),
                                 player_1 = numeric(),
                                 player_2 = numeric(),
                                 player_3 = numeric(),
                                 pred_y = numeric())

for(i in 1:nrow(param_space)) {
  defense_1_d <- pred_defense_data %>%
    filter(Value <= param_space[i, "alpha"] * budget_defense) %>%
    arrange(pred_y) %>%
    dplyr::select(Final.row.number, pred_y) %>%
    filter(row_number()==1)
  
  defense_2_d <- pred_defense_data %>%
    filter(Value <= param_space[i, "beta"] * budget_defense & 
             Final.row.number != defense_1_d$Final.row.number) %>%
    arrange(pred_y) %>%
    dplyr::select(Final.row.number, pred_y) %>%
    filter(row_number()==1)
  
  
  defense_3_d <- pred_defense_data %>%
    filter(Value <= param_space[i, "gamma"] * budget_defense & 
             Final.row.number != defense_1_d$Final.row.number & 
             Final.row.number != defense_2_d$Final.row.number) %>%
    arrange(pred_y) %>%
    dplyr::select(Final.row.number, pred_y) %>%
    filter(row_number()==1)
  
  defense_player_fnl <- rbind(defense_player_fnl, data.frame(alpha = param_space[i, "alpha"],
                                                             beta = param_space[i, "beta"],
                                                             gamma = param_space[i, "gamma"],
                                                             player_1 = defense_1_d$Final.row.number,
                                                             player_2 = defense_2_d$Final.row.number,
                                                             player_3 = defense_3_d$Final.row.number,
                                                             pred_y = mean(defense_1_d$pred_y,
                                                                           defense_2_d$pred_y, 
                                                                           defense_3_d$pred_y)))
}

defense_player_fnl <- defense_player_fnl %>%
  arrange(pred_y) %>%
  filter(row_number() == 1)


