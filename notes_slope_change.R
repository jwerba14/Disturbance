` plot chl slope}
ggplot(dat_dist,
       aes(x=ExptDay,adj_chl,
           colour=animal,
           shape=disturb))+geom_point()+
  scale_y_log10() +
  facet_wrap(~disturb)+
  geom_smooth(method="gam",formula=y~s(x,k=10,bs="cs"),
              method.args=list(method="REML"))+
  geom_line(aes(group=TankNum),alpha=0.1)
```

Could do this with GAMs: https://rpubs.com/bbolker/ratgrowthcurves

Algal resistance, compare slope over time after disturbance
``{r algal slope, eval=FALSE}
## this might not work ... not evaluated ...
dat_dist1 <- transform(dat_dist,ExptDay0 = ExptDay-min(ExptDay,na.rm=TRUE))

mod_res <- lmer(log10(adj_chl) ~ animal*ExptDay0 + (1|TankNum),
                data = dat_dist1,
                subset=(disturb=="y" & ExptDay0<=5))
summary(mod_res)
plot(resid(mod_res))
qqnorm(resid(mod_res))
mod_res2 <- lmer(log10(adj_chl) ~ -1 + animal + animal:ExptDay0 +
                   (1|TankNum),  ## ExpDay0|TankNum ?
                 data = dat_dist,
                 subset=(disturb=="y" & ExptDay0<=5))

inv_cont2 <- rbind(
  cbind(inverse_cont,matrix(0,4,4)),
  cbind(matrix(0,4,4),inverse_cont)
)
dimnames(inv_cont2) <- list(paste(rep(c("int","slope"),each=4),
                                  rownames(inv_cont2)),
                            names(fixef(mod_res2)))

m2 <- multcomp::glht(mod_res2,inv_cont2)
summary(m2)
## need to double-check this
```