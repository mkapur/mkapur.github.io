## Tidy CPUE
require(lunar)
require(MuMIn)
require(ggplot2)
require(modelr)
require(tidyverse)
require(parsnip)

## generate fake data ----
## possibly get this from somwhere
landings <-  data.frame(expand.grid(YEAR = seq(1956,2015,1),   VESSEL = c('Ariel','Jasmine','Cinderella','Elsa'))) %>%
  arrange(.,YEAR) %>%
  mutate(LBS = c(rnorm(0.5*nrow(.),44,4),rnorm(0.1*nrow(.),24,4),
                 rnorm(0.1*nrow(.),34,4),rnorm(0.3*nrow(.),30,4)),
         HOOKS = c(rnorm(0.5*nrow(.),65,4),rnorm(0.1*nrow(.),55,4),
                   rnorm(0.1*nrow(.),70,4),rnorm(0.3*nrow(.),80,4)),
         WIND_SPEED = c(rnorm(0.5*nrow(.),8.5,2),rnorm(0.5*nrow(.),5,4)),
         MOON_PHASE = rep(lunar::lunar.8phases,nrow(.)/8),
         MEAN_TEMP = c(rnorm(0.5*nrow(.),14,4),rnorm(0.1*nrow(.),40,4),
                       rnorm(0.1*nrow(.),20,4),rnorm(0.3*nrow(.),25,4)))



## modelR::fit_with
cpue_fits <- landings %>% fit_with(lm, formulas(~LBS,
                                                base = ~YEAR + VESSEL ,
                                                interaction = ~VESSEL * MEAN_TEMP,
                                                phase = add_predictors(base, ~MOON_PHASE),
                                                full_no_int = add_predictors(base, ~.),
                                                full_int = add_predictors(interaction,~.)
))
cpue_fits
purrr::map(cpue_fits,parsnip::predict.model_fit, type = 'conf_int')

## extract formula one with the lowest AIC
best_formula <- cpue_fits[[which.min(purrr::map(cpue_fits,AIC))]]['call'][[1]]
best_mod <- linear_reg() %>% set_engine("lm") %>%
  fit(formula(best_formula), data = landings)
predict(lm_fit, landings, type = "conf_int")

## bind predicts and CI to original df
pred_df <- landings %>%
  bind_cols(.,PREDICTS = predict(best_mod, landings)) %>%
  bind_cols(., predict(best_mod ,landings, type = "conf_int")) 

## clean the DF to get mean annual values

pred_df_clean <- pred_df %>% group_by(YEAR) %>% summarise(meanLBS = mean(LBS), meanCPUE = mean(.pred),
                                                          meanCPUE.lwr = mean(.pred_lower),
                                                          meanCPUE.upr = mean(.pred_upper))


ggplot(pred_df_clean, aes(x = YEAR)) +
  theme_minimal()+
  geom_line(aes(y = meanLBS), col = 'black', lwd = 1.2) +
  geom_point(aes(y = meanCPUE),col = 'dodgerblue' ) + geom_errorbar(aes(ymin = meanCPUE.lwr, ymax =meanCPUE.upr),col = 'dodgerblue' ) +
  labs(x = 'Year', y = 'Catch (lbs, black line) or CPUE (blue points and 95%CI')


