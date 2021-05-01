# Linear models for pollination
quartz()
par(mfrow = c(2,2), mar = c(4,4,1,1))

## poll diversity vs. pollination:
plot(Poll.visits ~ Pollinators_SR, data= data.poll)
summary(f <-lm(Poll.visits ~  Pollinators_SR,
                data= data.poll))
add.stats(f)


## urban vs. pollination:
plot(Poll.visits ~ Seal_500, data= data.poll)
summary(f <-lm(Poll.visits ~ Seal_500,
                    data= data.poll))
add.stats(f)

## vegetation vs. pollination:
plot(Poll.visits ~ Plants_insect.poll_Cover, data= data.poll)
summary(f <-lm(Poll.visits ~ Plants_insect.poll_Cover,
                data= data.poll))
add.stats(f)


## connectivity vs. pollination:
plot(Poll.visits ~ ShDry_500, data= data.poll)
summary(f <-lm(Poll.visits ~ ShDry_500,
                data= data.poll))
add.stats(f)

