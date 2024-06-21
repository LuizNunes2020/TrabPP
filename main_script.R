library(tidyverse)
library(svglite)

#--- Metadados ---#
set.seed(42)
# usaremos essas cores durante todo o programa
cor_tipo <- c("#adb5bd", '#6c757d')
cor_oleo <- c("#70e000", "#38b000")
cor_fogo <- c("#d62728", "#ff7f0e")
cor_mexer <- c("#1f77b4", "#e377c2")

ext_graf <- 'png' # tipo de arquivo que os gráficos serão salvos
tam_graf <- c('w' = 2001, 'h' = 1500)
path_graf <- 'gráficos'
unit_graf <- 'px'


#--- Preparando os dados ---# <- serviço de corno
Tipo <- c(rep('Normal', 16), rep('Premium', 16))
Óleo <- c(rep(c(rep('Sem', 8), rep('Com', 8)), 2))
Fogo <- c(rep(c(rep('Baixo', 4), rep('Alto', 4)), 4))            
Mexer <- c(rep(c(rep('Sim', 2), rep('Não', 2)), 8))
Replicação <- rep(c(1, 2), 16)
Qtt_pirua <- c(14, 19, 52, 42, 11, 26)

Qtt_pirua <-c(14, 19,
              52, 42,
              11, 26,
              83, 158,
              10, 15,
              21, 23,
              15, 23,
              32, 13,
              16, 19,
              32, 25,
              22, 19,
              34, 147,
              18, 9,
              21, 23,
              14, 16,
              29, 15
)

Grama_pirua <- c(
  3, 4,
  10, 8,
  3, 5,
  15, 27,
  2, 3,
  5 , 5,
  3, 4,
  8, 3,
  5, 5,
  6, 5,
  4, 4,
  8, 20,
  5, 2,
  5, 4,
  2, 3, 
  5, 3
)

df <- tibble(Tipo = as.factor(Tipo),
             Óleo = as.factor(Óleo),
             Fogo = as.factor(Fogo),
             Mexer = as.factor(Mexer),
             Replicação = as.factor(Replicação),
             Qtt_pirua,
             Grama_pirua
) %>% mutate(Qtt_pirua = log(Qtt_pirua))


#--- Análise exploratória ---#
# Quantidade de piruá X Tipo de pipoca
ggplot(df, aes(x = Tipo, y = Qtt_pirua, fill = Tipo)) +
  geom_violin(alpha = 0.5) +
  scale_fill_manual(values=c(cor_tipo)) +
  labs(title = 'Quantidade de piruá por tipo de pipoca',
       y = 'Quantidade de piruá') +
  theme_bw() +
  theme(legend.position = 'none')
ggsave(paste('piruaXtipo.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# Quantidade de piruá X presença de Óleo
ggplot(df, aes(x = Óleo, y = Qtt_pirua, fill = Óleo)) +
  geom_violin(alpha = 0.5) +
  scale_fill_manual(values=c(cor_oleo)) +
  labs(title = 'Quantidade de piruá por presença de óleo',
       y = 'Quantidade de piruá') +
  theme_bw() +
  theme(legend.position = 'none') # trocar a ordem
ggsave(paste('piruaXoleo.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# Quantidade de piruá X Intensidade do fogo
ggplot(df, aes(x = Fogo, y = Qtt_pirua, fill = Fogo)) +
  geom_violin(alpha = 0.5) +
  scale_fill_manual(values=c(cor_fogo)) +
  labs(title = 'Quantidade de piruá por intensidade do fogo',
       y = 'Quantidade de piruá') +
  theme_bw() +
  theme(legend.position = 'none')
ggsave(paste('piruaXfogo.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# Quantidade de piruá X Mexer ou não
ggplot(df, aes(x = Mexer, y = Qtt_pirua, fill = Mexer)) +
  geom_violin(alpha = 0.5) +
  scale_fill_manual(values=c(cor_mexer)) +
  labs(title = 'Quantidade de piruá por mexer',
       y = 'Quantidade de piruá') +
  theme_bw() +
  theme(legend.position = 'none')
ggsave(paste('piruaXmexer.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)


#--- ANOVA ---#
lm_model <- lm(Qtt_pirua ~
              Tipo * Óleo * Fogo * Mexer +
              Replicação * Óleo +
              Replicação * Fogo +
              Replicação * Mexer,
            df) # Replicação aqui, essencialmente, faz o papel da blocagem


anova(lm_model)

# Utilizar peso dos piruás ao invés da quantidade gerou os mesmos resultados na ANOVA

# lm_model2 <- lm(Grama_pirua ~
#                Tipo * Óleo * Fogo * Mexer +
#                Replicação*Óleo +
#                Replicação * Fogo +
#                Replicação * Mexer,
#              df)
# anova(lm_model2)


# Tipo deu não significativo (enquanto os outros fatores deram).
# A interação Óleo:Fogo:Mexer é significativa
# A interação Bloco:Fator não é significativa pra nenhum dos fatores (amém)

#--- Gráficos da interação ---#
# As bolas maiores são as médias
# x-axis: Óleo | fill: Fogo | Mexer: não
ggplot(df %>%
         filter(Mexer == 'Não') %>%
         group_by(Óleo, Fogo) %>%
         mutate(media = mean(Qtt_pirua)),
       aes(x = Óleo, color = Fogo)) +
  geom_point(aes(y = Qtt_pirua), size = 3, alpha = 0.3) +
  geom_point(aes(y = media), size = 4) +
  geom_line(aes(y = media, group = Fogo), linewidth = 2) +
  scale_color_manual(values=c(cor_fogo)) +
  labs(title = 'Quantidade de piruá por presença de óleo,\nSEM MEXER e separado pela intensidade do fogo',
       y = 'Quantidade de piruá') +
  theme_bw()
ggsave(paste('interacaoOleo1.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# x-axis: Óleo | fill: Fogo | Mexer: sim
ggplot(df %>%
         filter(Mexer == 'Sim') %>%
         group_by(Óleo, Fogo) %>%
         mutate(media = mean(Qtt_pirua)),
       aes(x = Óleo, color = Fogo)) +
  geom_point(aes(y = Qtt_pirua), size = 3, alpha = 0.3) +
  geom_point(aes(y = media), size = 4) +
  geom_line(aes(y = media, group = Fogo), linewidth = 2) +
  scale_color_manual(values=c(cor_fogo)) +
  labs(title = 'Quantidade de piruá por presença de óleo,\nMEXENDO e separado pela intensidade do fogo',
       y = 'Quantidade de piruá') +
  theme_bw()
ggsave(paste('interacaoOleo2.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# x-axis: Fogo | fill: Mexer | Óleo: sem
ggplot(df %>%
         filter(Óleo == 'Sem') %>%
         group_by(Fogo, Mexer) %>%
         mutate(media = mean(Qtt_pirua)),
       aes(x = Fogo, color = Mexer)) +
  geom_point(aes(y = Qtt_pirua), size = 3, alpha = 0.3) +
  geom_point(aes(y = media), size = 4) +
  geom_line(aes(y = media, group = Mexer), linewidth = 2) +
  scale_color_manual(values=c(cor_mexer)) +
  labs(title = 'Quantidade de piruá por intensidade do fogo,\nSEM ÓLEO e separado por mexer',
       y = 'Quantidade de piruá') +
  theme_bw()
ggsave(paste('interacaoFogo1.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# x-axis: Fogo | fill: Mexer | Óleo: com
ggplot(df %>%
         filter(Óleo == 'Com') %>%
         group_by(Fogo, Mexer) %>%
         mutate(media = mean(Qtt_pirua)),
       aes(x = Fogo, color = Mexer)) +
  geom_point(aes(y = Qtt_pirua), size = 3, alpha = 0.3) +
  geom_point(aes(y = media), size = 4) +
  geom_line(aes(y = media, group = Mexer), linewidth = 2) +
  scale_color_manual(values=c(cor_mexer)) +
  labs(title = 'Quantidade de piruá por intensidade do fogo,\nCOM ÓLEO e separado por mexer',
       y = 'Quantidade de piruá') +
  theme_bw()
ggsave(paste('interacaoFogo2.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# x-axis: Mexer | fill: Óleo | Fogo: Baixo
ggplot(df %>%
         filter(Fogo == 'Baixo') %>%
         group_by(Mexer, Óleo) %>%
         mutate(media = mean(Qtt_pirua)),
       aes(x = Mexer, color = Óleo)) +
  geom_point(aes(y = Qtt_pirua), size = 3, alpha = 0.3) +
  geom_point(aes(y = media), size = 4) +
  geom_line(aes(y = media, group = Óleo), linewidth = 2) +
  scale_color_manual(values=c(cor_oleo)) +
  labs(title = 'Quantidade de piruá por mexer,\nFOGO BAIXO e separado por presença de óleo',
       y = 'Quantidade de piruá') +
  theme_bw()
ggsave(paste('interacaoMexer1.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# x-axis: Mexer | fill: Óleo | Fogo: Alto
ggplot(df %>%
         filter(Fogo == 'Alto') %>%
         group_by(Mexer, Óleo) %>%
         mutate(media = mean(Qtt_pirua)),
       aes(x = Mexer, color = Óleo)) +
  geom_point(aes(y = Qtt_pirua), size = 3, alpha = 0.3) +
  geom_point(aes(y = media), size = 4) +
  geom_line(aes(y = media, group = Óleo), linewidth = 2) +
  scale_color_manual(values=c(cor_oleo)) +
  labs(title = 'Quantidade de piruá por mexer,\nFOGO ALTO e separado por presença de óleo',
       y = 'Quantidade de piruá') +
  theme_bw()
ggsave(paste('interacaoMexer2.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

#--- Teste de Tukey ---#
# Fizemos o teste pras médias de cada um dos tratamentos, pra cada combinação dos outros dois fatores. Ou seja, tem quatro teste Tukey por fator.
vec_li <- c()
vec_ls <- c()
vec_cond <- c()

# Teste: Óleo
for(i in 1:2) {
  for(j in 1:2) {
    temp_model <- TukeyHSD(aov(Qtt_pirua ~ Óleo,
                               df %>% filter(Fogo == c('Baixo', 'Alto')[i], Mexer == c('Sim', 'Não')[j])))
    vec_li <- c(vec_li, temp_model$Óleo[1])
    vec_ls <- c(vec_ls, temp_model$Óleo[2])
    vec_cond <- c(vec_cond, paste('Fogo', '(',c('Baixo', 'Alto')[i], ')', ' | ', 'Mexer', '(',c('Sim', 'Não')[j], ')', sep = ''))
  }
}

# Teste: Fogo
for(i in 1:2) {
  for(j in 1:2) {
    temp_model <- TukeyHSD(aov(Qtt_pirua ~ Fogo,
                               df %>% filter(Óleo == c('Sem', 'Com')[i], Mexer == c('Sim', 'Não')[j])))
    vec_li <- c(vec_li, temp_model$Fogo[1])
    vec_ls <- c(vec_ls, temp_model$Fogo[2])
    vec_cond <- c(vec_cond, paste('Óleo', '(',c('Sem', 'Com')[i], ')', ' | ', 'Mexer', '(',c('Sim', 'Não')[j], ')', sep = ''))
  }
}

# Teste: Mexer
for(i in 1:2) {
  for(j in 1:2) {
    temp_model <- TukeyHSD(aov(Qtt_pirua ~ Mexer,
                               df %>% filter(Fogo == c('Baixo', 'Alto')[i], Óleo == c('Sem', 'Com')[j])))
    vec_li <- c(vec_li, temp_model$Mexer[1])
    vec_ls <- c(vec_ls, temp_model$Mexer[2])
    vec_cond <- c(vec_cond, paste('Fogo', '(',c('Baixo', 'Alto')[i], ')', ' | ', 'Óleo', '(',c('Sem', 'Com')[j], ')', sep = ''))
  }
}

ic_df <- tibble(
  Fator = c(
    rep('Óleo', 4),
    rep('Fogo', 4),
    rep('Mexer', 4)
  ),
  Nível = vec_cond,
  LI = vec_li,
  LS = vec_ls
)

# Gráficos do Tukey
# Óleo
ggplot(ic_df %>% filter(Fator == 'Óleo'), aes(x = Nível)) +
  geom_segment(aes(y = LI , yend = LS), linewidth = 1) +
  geom_point(aes(y = LI), size = 2) +
  geom_point(aes(y = LS), size = 2) +
  geom_hline(yintercept = 0) +
  labs(title = 'Intervalo de confiança do Óleo para combinações de níveis de Fogo e Mexer',
       x = 'Efeito do fator') +
  coord_flip() +
  theme_bw()
ggsave(paste('tukeyOleo.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# Fogo
ggplot(ic_df %>% filter(Fator == 'Fogo'), aes(x = Nível)) +
  geom_segment(aes(y = LI , yend = LS), linewidth = 1) +
  geom_point(aes(y = LI), size = 2) +
  geom_point(aes(y = LS), size = 2) +
  geom_hline(yintercept = 0) +
  labs(title = 'Intervalo de confiança do Fogo para combinações de níveis de Óleo e Mexer',
       x = 'Efeito do fator') +
  coord_flip() +
  theme_bw()
ggsave(paste('tukeyMexer.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# Mexer
ggplot(ic_df %>% filter(Fator == 'Mexer'), aes(x = Nível)) +
  geom_segment(aes(y = LI , yend = LS), linewidth = 1) +
  geom_point(aes(y = LI), size = 2) +
  geom_point(aes(y = LS), size = 2) +
  geom_hline(yintercept = 0) +
  labs(title = 'Intervalo de confiança de Mexer para combinações de níveis de Óleo e Fogo',
       x = 'Efeito do fator') +
  coord_flip() +
  theme_bw()
ggsave(paste('tukeyMexer.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

#--- Análise de Resíduos ---#
# novo modelo sem o fator Tipo e as interações que não são significativas 
lm_new_model <- lm(Qtt_pirua ~
            Óleo * Fogo * Mexer +
            Replicação,
          df)
# QQ PLOT
ggplot() +
  geom_qq(aes(sample = rstandard(lm_new_model))) +
  geom_abline(color = "red") + 
  labs(title = 'QQ plot',
       x = 'Teórico',
       y = 'Amostra') +
  theme_bw()
ggsave(paste('qqplot.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# eu sei que os dados não parecem muito normais, mas rode esse exemplo algumas vezes e veja cada absurdo que aparece:
ggplot() +
  geom_qq(aes(sample = rnorm(32))) +
  geom_abline(color = "red")

# Resíduos x Fitted
ggplot(lm_new_model, aes(x = .fitted,
           y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = 'Resíduos por valores ajustados',
       y = 'Resíduos',
       x = 'Ajustado') +
  theme_bw()
ggsave(paste('residuosXfitted.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# Resíduos por ordem de coleta de dado
# [a fazer]

# Resíduos x Óleo
ggplot() +
  geom_point(
    aes(x = lm_new_model$model$Óleo, y = lm_new_model$residuals),
    size = 3
  ) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = 'Níveis do fator Óleo por resíduos',
       x = 'Níveis',
       y = 'Residuos') +
  theme_bw()
ggsave(paste('residuosXoleo.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# Resíduos x Fogo
ggplot() +
  geom_point(
    aes(x = lm_new_model$model$Fogo, y = lm_new_model$residuals),
    size = 3
  ) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = 'Níveis do fator Fogo por resíduos',
       x = 'Níveis',
       y = 'Residuos') +
  theme_bw()
ggsave(paste('residuosXfogo.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

# Resíduos x Mexer
ggplot() +
  geom_point(
    aes(x = lm_new_model$model$Mexer, y = lm_new_model$residuals),
    size = 3
  ) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = 'Níveis do fator Mexer por resíduos',
       x = 'Níveis',
       y = 'Residuos') +
  theme_bw()
ggsave(paste('residuosXmexer.', ext_graf, sep = ''), path = path_graf, device = ext_graf, width = tam_graf[1], height = tam_graf[2], units = unit_graf)

