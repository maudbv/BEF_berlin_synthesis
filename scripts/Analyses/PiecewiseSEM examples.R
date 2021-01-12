#SEM examples

library(piecewiseSEM)
library(DiagrammeR)
library(MuMin)

# Random example
dat <- data.frame(x1 = runif(50),
                  y1 = runif(50),
                  y2 = runif(50),
                  y3 = runif(50))

model <- psem(lm(y1 ~ x1 + y2, dat), lm(y2 ~ x1, dat), lm(y3 ~ y1, dat))

summary(model, dat, .progressBar = F)

#nodes_1 <- create_node_df(
nodes_1 <- data.frame( 
    label = names(dat),
    type = "lower",
    style = "filled",
    color = c("aqua", "coral", "coral","coral"),
    shape = c("circle", "circle",
              "rectangle", "rectangle"),
    data = c(3.5, 2.6, 9.4, 2.7),
    x = c(2,1,3,2),
    y = c(1,2,2,3)
    )

plot(model,
     node_attrs = list(
       label = names(dat),
       shape = "rectangle",
       color = "black",
       fillcolor = "orange",
       x = 1:4,    ## SEEMS to plot in the opposite order
       y = 1:4
     )
)



# Pollinator application

## Pollinators
data.poll <- cbind(Env_data[EF_data$ID_plot,
                            c("Size_Patch",
                              "Road_Dist",
                              "Seal_500",
                              "ShHerb_500",
                              "mean_tempDay_summer",
                              "mean_tempNight_summer")],
                   Biodiv_data[EF_data$ID_plot,
                               c("Plant_SR","Plant_Cover",
                                 "Archeophyte_Cover",
                                 "Indigenous_Cover","Indigenous_SR",
                                 "NeophyteHerb_Prop",
                                 "NeophyteHerb_Cover",
                                 "Neophyte_Cover",
                                 "Pollinators_SR",
                                 "Pollinators_Abun")],
                   Poll =  EF_data$PollinationVisitCounts
)

data.poll = na.omit(data.poll)
## Maybe need to remove some outliers


## Try using dredge
(drdg1 <- dredge( f <- lm(log(Poll) ~  Seal_500 +
                      Size_Patch  +
                      Pollinators_Abun +
                      Pollinators_SR +
                      Indigenous_Cover +
                      Archeophyte_Cover +
                      NeophyteHerb_Cover +
                      NeophyteHerb_Prop ,
                   data = data.poll)))

# (drdg1 <- dredge(glm(Poll ~  Seal_500 +
#                       Size_Patch  +
#                       Pollinators_Abun +
#                       Pollinators_SR +
#                       Indigenous_Cover +
#                        Archeophyte_Cover +
#                        NeophyteHerb_Cover +
#                        Plant_Cover +
#                       NeophyteHerb_Prop ,
#                     data = data.poll,
#                     family = gaussian(link = "log"))))

(mavg1 <- model.avg(drdg1, subset = delta < 4))
 mavg1$sw
confint(mavg1)


## FULL MODEL 
model.full <- psem(
  
lm(log(Poll) ~ Pollinators_SR +  Pollinators_Abun + Seal_500 +
     Size_Patch + Indigenous_Cover + NeophyteHerb_Prop,
   data = data.poll),

lm( Pollinators_SR ~  Pollinators_Abun + Seal_500 + Size_Patch + Indigenous_Cover + NeophyteHerb_Prop,
    data = data.poll),

lm( Pollinators_Abun ~  Indigenous_Cover +  Seal_500 +  Size_Patch,
    data = data.poll),

lm(Indigenous_Cover ~ NeophyteHerb_Prop  +  Seal_500 +  Size_Patch,
   data = data.poll),

lm(NeophyteHerb_Prop ~ Size_Patch + Seal_500  , data.poll)
)

summary(model.full)
anova(model.full)
infCrit(model.full)

## reduced 
model.redux <- psem(
  
  lm(log(Poll) ~ Indigenous_Cover,
     data = data.poll),
  
  lm( Pollinators_SR ~ Pollinators_Abun,
      data = data.poll),
  
  lm( Pollinators_Abun ~  Seal_500 +
        Size_Patch,
      data = data.poll),
  
  lm(Indigenous_Cover ~ NeophyteHerb_Prop,
     data = data.poll),
  
  lm(NeophyteHerb_Prop ~ Size_Patch + Seal_500, data = data.poll)
)
(summary(model.redux))
basisSet(model.redux)

# Increased again based on Independent separation
model.redux2 <- update(model.redux,
      log(Poll) ~ Indigenous_Cover + Seal_500)
infCrit(model.redux2) #  AIC =  53.761     BIC = 78.372
plot(model.redux2)

# Increase again for logic 
model.redux3 <- update(model.redux2,
                       log(Poll) ~ Indigenous_Cover +
                         Seal_500 +
                         Pollinators_Abun +
                         Pollinators_SR
  )
infCrit(model.redux3) #AIC =  52.449    BIC = 79.795
# not worse !


# Increase again for logic 
model.redux4 <- psem(

  lm(log(Poll) ~ Indigenous_Cover + Seal_500,
     data = data.poll),
  
  Pollinators_SR ~ 1,
  
  Pollinators_Abun ~  1,
  
  lm(Indigenous_Cover ~ NeophyteHerb_Prop,
     data = data.poll),
  
  lm(NeophyteHerb_Prop ~ Size_Patch + Seal_500, data = data.poll)
)

model.redux4
summary(model.redux4)
plot(model.redux4)
infCrit(model.redux4) #AIC =  52.449    BIC = 79.795
# not worse !

# Select model:
anova(model.redux, model.redux2,model.redux3, model.redux4)

poll_psem <- model.redux3
sum_poll <- summary(poll_psem)

## Make into a graph:
source('/scripts/psem2graph.R')
poll_graph <- psem2graph(poll_psem)
poll_graph$nodes_df$fillcolor = c("coral","coral","coral",
                              "#ACDD8E","#ACDD8E",
                              "#B2D6E2","#B2D6E2")
poll_graph$nodes_df$color = c("coral","coral","coral",
                                  "#ACDD8E","#ACDD8E",
                                  "#B2D6E2","#B2D6E2")
poll_graph$nodes_df$width = 0.7
poll_graph$nodes_df$shape = "ellipse"
poll_graph$nodes_df$fontcolor ="black"
poll_graph$nodes_df$fontsize = 6
 
poll_graph$edges_df$color = c("black","red")[(as.numeric(poll_graph$edges_df$label)<0) +1]
poll_graph$edges_df$penwidth = abs(as.numeric(poll_graph$edges_df$label))*5

# Graphical output
quartz()
render_graph(poll_graph,
             as_svg = TRUE,
             title = "Pollination pSEM",
             layout  = "nicely")


## Partial residuals for Pollination visits
par(mfrow = c(2,2))
pR <- partialResid(log(Poll) ~ Indigenous_Cover, model.redux2)
plot(pR$x, pR$y,
     ylab = "log(Pollination)", 
     xlab = "% Indigenous Cover",
     main = "Partial residuals")
f <- lm(yresid ~ xresid, pR)
predf <- predict(f, 
                 newdata = pR[order(pR$xresid),],
                 se.fit = TRUE)
lines(x = sort(pR$xresid), predf$fit, col  ="red")
lines(x =sort(pR$xresid),predf$fit + predf$se.fit, col  ="red", lty = "dashed")
lines(x =sort(pR$xresid),predf$fit - predf$se.fit,  col  ="red", lty = "dashed")


pR <- partialResid(log(Poll) ~ Seal_500, model.redux2)
plot(pR$x, pR$y,
     ylab = "log(Pollination)", 
     xlab = "Seal_500",
     main = "Partial residuals")
f <- lm(yresid ~ xresid, pR)
predf <- predict(f, 
                 newdata = pR[order(pR$xresid),],
                 se.fit = TRUE)
lines(x = sort(pR$xresid), predf$fit, col  ="red")
lines(x =sort(pR$xresid),predf$fit + predf$se.fit, col  ="red", lty = "dashed")
lines(x =sort(pR$xresid),predf$fit - predf$se.fit,  col  ="red", lty = "dashed")


pR <- partialResid(log(Poll) ~ Pollinators_SR, model.redux2)
plot(pR$x, pR$y,
     ylab = "log(Pollination)", 
     xlab = "Pollinator Richness",
     main = "Partial residuals")
f <- lm(yresid ~ xresid, pR)
predf <- predict(f, 
                 newdata = pR[order(pR$xresid),],
                 se.fit = TRUE)
lines(x = sort(pR$xresid), predf$fit, col  ="red")
lines(x =sort(pR$xresid),predf$fit + predf$se.fit, col  ="red", lty = "dashed")
lines(x =sort(pR$xresid),predf$fit - predf$se.fit,  col  ="red", lty = "dashed")

pR <- partialResid(log(Poll) ~ Pollinators_Abun, model.redux2)
plot(pR$x, pR$y,
     ylab = "log(Pollination)", 
     xlab = "Pollinator Abundance",
     main = "Partial residuals")
f <- lm(yresid ~ xresid, pR)
predf <- predict(f, 
                 newdata = pR[order(pR$xresid),],
                 se.fit = TRUE)
lines(x = sort(pR$xresid), predf$fit, col  ="red")
lines(x =sort(pR$xresid),predf$fit + predf$se.fit, col  ="red", lty = "dashed")
lines(x =sort(pR$xresid),predf$fit - predf$se.fit,  col  ="red", lty = "dashed")

