source("prep-figures.R")

zoib2 <- custom_family(
  "zoib2", dpars = c("mu", "phi", "zipp", "coi"),
  links = c("logit", "log", "logit", "logit"), lb = c(NA, 0, NA, NA),
  type = "real"
)


## experiment 2 main model:
load("e2_roulette_study/model_fits/model_zoib.rda")

res_e2 <- mzoib 
summary(res_e2)

load("e3_football_study/model_fits/model_zoib.rda")
res_e3 <- mzoib 
summary(res_e3)

### posterior predictive plot:
posterior_predict_zoib2 <- function(i, prep, ...) {
  zi <- brms:::get_dpar(prep, "zipp", i)
  coi <- brms:::get_dpar(prep, "coi", i)
  mu <- brms:::get_dpar(prep, "mu", i = i)
  phi <- brms:::get_dpar(prep, "phi", i = i)
  hu <- runif(prep$nsamples, 0, 1)
  hu2 <- runif(prep$nsamples, 0, 1)
  #one_or_zero <- runif(prep$nsamples, 0, 1)
  ifelse(hu > zi, 0, 
         ifelse(hu2 < coi, 1, 
                rbeta(prep$nsamples, shape1 = mu * phi, shape2 = (1 - mu) * phi)
         ))
}

options(mc.cores = 1)

set.seed(6675433)
pp_e2 <- pp_check(res_e2, type = "hist", binwidth = 0.025, nsamples = 11) +
  coord_cartesian(ylim = c(0, 360)) +
  theme(legend.position = "none")  +
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400)) + 
  theme(panel.grid.minor.y = element_blank())
#pp_e2
pp_e3 <- pp_check(res_e3, type = "hist", binwidth = 0.025, nsamples = 11) +
  coord_cartesian(ylim = c(0, 470)) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400)) + 
  theme(panel.grid.minor.y = element_blank())
#pp_e3
pp_out <- plot_grid(pp_e2, pp_e3, ncol = 1, 
                    labels = c("Exp. 2", "Exp. 3"), 
                    vjust = 1.9, hjust = -0.55)
ggsave(plot = pp_out, filename = "figures/ppp.pdf", device = "pdf",
       width = 8, height = 10, units = "in")
ggsave(plot = pp_out, filename = "figures/ppp.png", device = "png",
       width = 8, height = 10, units = "in")

options(mc.cores = parallel::detectCores())

### main figure exp. 2


lpars_e2 <- res_e2 %>% 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  filter(.variable != "b_phi_Intercept") %>% 
  mutate(type = case_when(
    str_detect(.variable, "Intercept") ~ "Intercept",
    str_detect(.variable, "condyellow") ~ "yellow",
    str_detect(.variable, "condwhite") ~ "white"
  )) %>%  
  mutate(parameter = case_when(
    str_detect(.variable, "zipp") ~ "g",
    str_detect(.variable, "coi") ~ "f",
    TRUE ~ "mu"
  ))

condpars_e2 <- lpars_e2 %>% 
  pivot_wider(id_cols = c(.chain:.draw, parameter, parameter), 
              names_from = type, values_from = .value) %>% 
  mutate(None = Intercept, 
         White = Intercept + white,
         Yellow = Intercept + yellow) %>% 
  select(-Intercept, -white, -yellow) %>% 
  pivot_longer(cols = c(None, White, Yellow), 
               names_to = "condition", values_to = "estimate") %>% 
  mutate(estimate = plogis(estimate)) %>% 
  mutate(parameter = factor(
    x = parameter, 
    levels = c("g", "f", "mu"), 
    labels = par_labels))

## plots in which CIs sizes are relative to each other.
tmp <- condpars_e2 %>% 
  group_by(parameter, condition) %>% 
  mean_qi()
tmp <- tmp %>% 
  mutate(width = .upper - .lower) %>% 
  summarise(mwidth = mean(width))
pr1 <- condpars_e2 %>% 
  ggplot(aes(x = estimate, y = condition)) +
  #stat_histintervalh(breaks = 40) +
  stat_halfeye() +
  facet_wrap("parameter", scales = "free_x") +
  xlab("Probability/Proportion") + ylab(ylabel) +
  scale_x_continuous(labels = scales::percent_format(1))
tmp2 <- pr1$data %>% 
  group_by(parameter) %>% 
  summarise(min = min(estimate), 
            max = max(estimate)) %>% 
  mutate(fwidth = max - min) %>% 
  mutate(newmin = min, 
         newmax = max, 
         mid = min + fwidth * 0.5)
tmp2$fac <- tmp$mwidth/tmp2$fwidth
tmp2$ciwdith <- tmp$mwidth
tmp2 <- tmp2 %>% 
  mutate(newwidth = ciwdith * 1/min(fac)) %>% 
  mutate(newmin = mid - newwidth/2,
         newmax = mid + newwidth/2)
tmpl <- tmp2 %>% 
  select(parameter, newmin, newmax) %>% 
  pivot_longer(cols = c(newmin, newmax), 
               names_to = "bounds", values_to = "estimate") %>% 
  mutate(condition = NA)
pr1use <- pr1  + theme_bw(base_size = 17) + theme(
  panel.grid.minor.x = element_blank(), 
  panel.grid.major.x = element_blank())

comppars_e2 <- condpars_e2 %>% 
  group_by(parameter) %>% 
  compare_levels(estimate, by = condition) %>% 
  filter(condition != "Yellow - White") %>% 
  mutate(condition = factor(
    x = condition, 
    levels = c("White - None", "Yellow - None"), 
    labels = c("White", "Yellow")))

pr2 <- comppars_e2 %>% 
  ggplot(aes(x = estimate, y = condition)) +
  geom_vline(xintercept = 0, color = "grey", size = 0.6) +
  stat_halfeye() +
  facet_wrap("parameter", scales = "free_x") +
  xlab("Difference from no message condition (on probability/proportion scale)") + 
  ylab("No message vs.") +
  scale_x_continuous(labels = scales::percent_format(1)) 

ciwidths <- comppars_e2 %>% 
  group_by(parameter, condition) %>% 
  mean_qi() %>% 
  mutate(width = .upper - .lower) %>% 
  summarise(mwdith = mean(width))

tmp <- pr2$data %>% 
  group_by(parameter) %>% 
  summarise(min = min(estimate), 
            max = max(estimate)) %>% 
  mutate(pwidth = max-min) %>% 
  mutate(mid = min + (max-min)/2) %>% 
  mutate(ciwdith = ciwidths$mwdith) %>% 
  mutate(newwidth = ciwdith * max(pwidth/ciwdith)) %>% 
  mutate(newmin = mid - newwidth/2,
         newmax = mid + newwidth/2)
tmpl <- tmp %>% 
  select(parameter, newmin, newmax) %>% 
  pivot_longer(cols = c(newmin, newmax), 
               names_to = "bounds", values_to = "estimate") %>% 
  mutate(condition = NA)
pr2 <- pr2 +
  geom_blank(data = tmpl)
pr2use <- pr2  + theme_bw(base_size = 17) + theme(
  panel.grid.minor.x = element_blank(), 
  panel.grid.major.x = element_blank()
)
plot_res_e2 <- cowplot::plot_grid(pr1use, pr2use, ncol = 1, 
                   rel_heights = c(4.5, 4))



### exp 3


lpars_e3 <- res_e3 %>% 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  filter(.variable != "b_phi_Intercept") %>% 
  mutate(type = case_when(
    str_detect(.variable, "Intercept") ~ "Intercept",
    str_detect(.variable, "conditionYellow") ~ "effect",
  )) %>%  
  mutate(parameter = case_when(
    str_detect(.variable, "zipp") ~ "g",
    str_detect(.variable, "coi") ~ "f",
    TRUE ~ "mu"
  ))

condpars_e3 <- lpars_e3 %>% 
  pivot_wider(id_cols = c(.chain:.draw, parameter, parameter), 
              names_from = type, values_from = .value) %>% 
  mutate(None = Intercept, 
         Yellow = Intercept + effect) %>% 
  select(-Intercept, -effect) %>% 
  pivot_longer(cols = c(None, Yellow), 
               names_to = "condition", values_to = "estimate") %>% 
  mutate(estimate = plogis(estimate)) %>% 
  mutate(parameter = factor(
    x = parameter, 
    levels = c("g", "f", "mu"), 
    labels = par_labels))

tmp_e3 <- condpars_e3 %>% 
  group_by(parameter, condition) %>% 
  mean_qi()
tmp_e3 <- tmp_e3 %>% 
  mutate(width = .upper - .lower) %>% 
  summarise(mwidth = mean(width))
pr1_e3 <- condpars_e3 %>% 
  ggplot(aes(x = estimate, y = condition)) +
  #stat_histintervalh(breaks = 40) +
  stat_halfeye() +
  facet_wrap("parameter", scales = "free_x") +
  xlab("Probability/Proportion") + ylab(ylabel) +
  scale_x_continuous(labels = scales::percent_format(1))
tmp2_e3 <- pr1_e3$data %>% 
  group_by(parameter) %>% 
  summarise(min = min(estimate), 
            max = max(estimate)) %>% 
  mutate(fwidth = max - min) %>% 
  mutate(newmin = min, 
         newmax = max, 
         mid = min + fwidth * 0.5)
tmp2_e3$fac <- tmp_e3$mwidth/tmp2_e3$fwidth
tmp2_e3$ciwdith <- tmp_e3$mwidth
tmp2_e3 <- tmp2_e3 %>% 
  mutate(newwidth = ciwdith * 1/min(fac)) %>% 
  mutate(newmin = mid - newwidth/2,
         newmax = mid + newwidth/2)
tmpl_e3 <- tmp2_e3 %>% 
  select(parameter, newmin, newmax) %>% 
  pivot_longer(cols = c(newmin, newmax), 
               names_to = "bounds", values_to = "estimate") %>% 
  mutate(condition = NA)
pr1_e3 <- pr1_e3 +
  geom_blank(data = tmpl_e3)

pr1use_e3 <- pr1_e3  + theme_bw(base_size = 17) + theme(
  panel.grid.minor.x = element_blank(), 
  panel.grid.major.x = element_blank())

comppars_e3 <- condpars_e3 %>% 
  group_by(parameter) %>% 
  compare_levels(estimate, by = condition) %>% 
  mutate(condition = factor(
    x = condition, labels = "          "
  ))
pr2_e3 <- comppars_e3 %>% 
  ggplot(aes(x = estimate)) +
  geom_vline(xintercept = 0, color = "grey", size = 0.6) +
  stat_halfeye() +
  facet_wrap("parameter", scales = "free_x") +
  xlab("Difference yellow - no message condition (on probability/proportion scale)") + 
  ylab("   ") +
  scale_x_continuous(labels = scales::percent_format(1)) 

ciwidths_e3 <- comppars_e3 %>% 
  group_by(parameter, condition) %>% 
  mean_qi() %>% 
  mutate(width = .upper - .lower) %>% 
  summarise(mwdith = mean(width))

tmp_e3 <- pr2_e3$data %>% 
  group_by(parameter) %>% 
  summarise(min = min(estimate), 
            max = max(estimate)) %>% 
  mutate(pwidth = max-min) %>% 
  mutate(mid = min + (max-min)/2) %>% 
  mutate(ciwdith = ciwidths$mwdith) %>% 
  mutate(newwidth = ciwdith * max(pwidth/ciwdith)) %>% 
  mutate(newmin = mid - newwidth/2,
         newmax = mid + newwidth/2)
tmpl_e3 <- tmp_e3 %>% 
  select(parameter, newmin, newmax) %>% 
  pivot_longer(cols = c(newmin, newmax), 
               names_to = "bounds", values_to = "estimate") %>% 
  mutate(condition = NA)
pr2_e3 <- pr2_e3 +
  geom_blank(data = tmpl_e3)

pr2use_e3 <- pr2_e3  + theme_bw(base_size = 17) + theme(
  panel.grid.minor = element_blank(), 
  panel.grid.major = element_blank(), 
  axis.ticks.y = element_blank(), axis.text.y = element_blank()
)


plot_res_e3 <- cowplot::plot_grid(pr1use_e3, pr2use_e3, ncol = 1, 
                   rel_heights = c(6.5, 5), align = "hv")

plot_res_out <- plot_grid(plot_res_e2, plot_res_e3, ncol = 1,
                          labels = c("Exp. 2", "Exp. 3"), 
                          rel_heights = c(3, 2.5))

plot_res_out <- plot_grid(pr1use, pr2use, pr1use_e3, pr2use_e3, 
                          ncol = 1, align = "hv", 
                          labels =  c("(a) Exp. 2", "(b) Exp. 2", "(c) Exp. 3", "(d) Exp. 3"), 
                          hjust = -0.07,
                          rel_heights = c(3, 2, 2, 1) + 1.25)

ggsave(plot = plot_res_out, filename = "figures/main-res.pdf", device = "pdf",
       width = 10, height = 12, units = "in")
ggsave(plot = plot_res_out, filename = "figures/main-res.png", device = "png",
       width = 10, height = 12, units = "in")

