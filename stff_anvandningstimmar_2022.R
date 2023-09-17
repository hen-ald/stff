install.packages("plotly")
install.packages("openxlsx")

library(tidyverse)
library(openxlsx)
library(plotly)
#options(scipen=999)

df <- read.xlsx("Book (3).xlsx") #C:\corrydor\stff\stff_corrydor
glimpse(df)

df$behov_av_timmar <- df$`Behovtimmar` - df$`Befintligtid` #skapar ny variabel
df$behov_yta_fullstor_11_spel <- df$Behovtimmar / 44 - df$`Befintligytafullstor11-spelarplan`
df$behov_per_capita_2022 <- df$behov_yta_fullstor_11_spel / df$`Befolkning.7-20.år.2022`
df$capita_per_behov_2022 <- df$`Befolkning.7-20.år.2022` / df$behov_yta_fullstor_11_spel
df$nyckeltal_2022 <- df$`Befolkning.7-20.år.2022` / df$`Behovyta+befintligyta`
df$nyckeltal_2070 <- df$`Befolkning.7-20.år.2070` / df$`Behovyta+befintligyta`
df$lag_per_capita <- df$Summalag / df$`Befolkning.7-20.år.2022`
df$capita_per_lag <- df$`Befolkning.7-20.år.2022` / df$Summalag
df$behov_timmar_per_capita <- df$Behovtimmar / df$`Befolkning.7-20.år.2022`


graf_stff_summalag <- df %>% 
  ggplot(aes(
    x = reorder(Kommun, Summalag),
    y = Summalag, fill = Kommun
  )) +
  geom_col()+
  coord_flip() +
  geom_text(aes(label = Summalag), vjust = 0.5)+
  theme(legend.position="none")

graf_stff_summalag

graf_stff_behovytafullstor <- df %>% 
  ggplot(aes(
    x = reorder(Kommun, behov_yta_fullstor_11_spel),
    y = behov_yta_fullstor_11_spel, fill = Kommun
  )) +
  geom_col()+
  coord_flip()+
  theme(legend.position="none")

graf_stff_behovytafullstor

graf_stff_behov_per_capita <- df %>% 
  ggplot(aes(
    x = reorder(Kommun, behov_per_capita_2022),
    y = behov_per_capita_2022, fill = Kommun
  )) +
  geom_col()+
  coord_flip()+
  theme(legend.position="none")
graf_stff_behov_per_capita

graf_stff_nyckeltal2022 <- df %>% 
  ggplot(aes(
    x = reorder(Kommun, nyckeltal_2022),
    y = nyckeltal_2022,fill = Kommun
  )) +
  geom_col()+
  coord_flip()+
  theme(legend.position="none")
graf_stff_nyckeltal2022

graf_stff_behov_timmar_per_capita <- df %>% 
  ggplot(aes(
    x = reorder(Kommun, behov_timmar_per_capita),
    y = behov_timmar_per_capita, fill = Kommun
  )) +
  geom_col()+
  coord_flip()+
  theme(legend.position="none")
graf_stff_behov_timmar_per_capita

graf_stff_lag_per_capita <- df %>% 
  ggplot(aes(
    x = reorder(Kommun, lag_per_capita),
    y = lag_per_capita, fill = Kommun
  )) +
  geom_col()+
  coord_flip()+
  theme(legend.position="none")
graf_stff_lag_per_capita

graf_stff_behov_av_timmar <- df %>% 
  ggplot(aes(
    x = reorder(Kommun, behov_av_timmar),
    y = behov_av_timmar, fill = Kommun
  )) +
  geom_col()+
  coord_flip()+
  theme(legend.position="none")
graf_stff_behov_av_timmar

#geom_point

punkter_stff_nyckeltal_summalag <- df %>% 
  ggplot(aes(y = nyckeltal_2022,
             x = Summalag)) +
  geom_point()
punkter_stff_nyckeltal_summalag

# punkter_stff_behov_summalag <- df %>% 
#   ggplot(aes(y = Summalag,
#              x = behov_yta_fullstor_11_spel)) +
#   geom_point(aes(size = behov_per_capita_2022, color = df$Kommun))+
#   geom_text(aes(label = Kommun), vjust = 0.5)+
#   geom_smooth()
#   
# punkter_stff_behov_summalag

punkter_stff_behov_summalag <- df %>% 
  ggplot(aes(y = Summalag,
             x = behov_yta_fullstor_11_spel)) +
  geom_point(aes(size = behov_per_capita_2022, color = Kommun))+
  #geom_text(aes(label = Kommun), vjust = -3, size = 2 )+
  geom_smooth()+
  theme(legend.position="none")

ggplotly(punkter_stff_behov_summalag)

punkter_stff_nyckeltal_lagpercapita <- df %>% 
  ggplot(aes(y = lag_per_capita,
             x = nyckeltal_2022)) +
  geom_point(aes(size = behov_per_capita_2022, color = Kommun))+
  #geom_text(aes(label = Kommun), vjust = -3, size = 2 )+
  geom_smooth()+
  theme(legend.position="none")

ggplotly(punkter_stff_nyckeltal_lagpercapita)
