# import libraries
library(tidyverse)

# import data
redclaw_growth <- read.csv("data/redclaw_growth.csv")

# define parameters
bw_0 <- 0.0221
cray.max <- 1
juvenile.max <- 45

redclaw.growth <- redclaw_growth %>%
  mutate(wt.mid = (wt_2 + wt_1) / 2,
         g.gain.day = (wt_2 - wt_1) / (t_2 - t_1),
         geometric.weight = sqrt(wt_1 * wt_2))

# try base code log-log relationship - looks good!
redclaw.fit.twinlog <- lm(log(g.gain.day) ~ log(wt.mid), data = redclaw.growth)
summary(redclaw.fit.twinlog)

plot(log(g.gain.day) ~ log(wt.mid), data = redclaw.growth, col = "grey", pch = 20, cex = 1.5, main = "g per day by weight")
abline(redclaw.fit.twinlog, col = "darkorange", lwd = 2)

plot(fitted(redclaw.fit.twinlog), resid(redclaw.fit.twinlog), col = "grey", pch = 20,
     xlab = "fitted", ylab = "residuals", main = "fitted vs residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(redclaw.fit.twinlog), main = "normal Q-Q plot", col = "darkgrey")
qqline(resid(redclaw.fit.twinlog), col = "dodgerblue", lwd = 2)

plot(g.gain.day ~ wt.mid, data = redclaw.growth, col = "grey", pch = 20, cex = 1.5, main = "g per day gained")
curve(exp(redclaw.fit.twinlog$coef[1]) * x ^ redclaw.fit.twinlog$coef[2], from = 0, to = 100, add = TRUE, col = "darkorange", lwd = 2)

# calculate coefficients for growth curve bw_1 = [bw_0^c_1 + c_2 * days]^c_4
c_1 <- 1 - redclaw.fit.twinlog$coef[2]
c_2 <- c_1 * exp(redclaw.fit.twinlog$coef[1])
c_4 <- 1 / c_1

# plot relationship between weight and grams gained per day 
p_rc_gain <- redclaw.growth %>%
  ggplot(aes(wt.mid, g.gain.day, color = source)) +
  geom_point() +
  stat_function(fun = function(x) exp(redclaw.fit.twinlog$coef[1]) * x^redclaw.fit.twinlog$coef[2], color = 'dodgerblue')+
  stat_function(fun = function(x) exp(redclaw.geometric.twinlog$coef[1]) * x^redclaw.geometric.twinlog$coef[2], color = 'black') +
  xlab("Mass (g)") +
  ylab("Daily mass gain (g/cray/day)") +
  theme_minimal()

# print plot to file
pdf("plots/p_rc_gain.pdf", height = 4)
print(p_rc_gain)
dev.off()

# try base code log-log relationship for geometric weight
redclaw.geometric.twinlog <- lm(log(g.gain.day) ~ log(geometric.weight), data = redclaw.growth)
summary(redclaw.geometric.twinlog)

plot(log(g.gain.day) ~ log(geometric.weight), data = redclaw.growth, col = "grey", pch = 20, cex = 1.5, main = "g per day by weight")
abline(redclaw.geometric.twinlog, col = "darkorange", lwd = 2)

plot(g.gain.day ~ geometric.weight, data = redclaw.growth, col = "grey", pch = 20, cex = 1.5, main = "g per day gained")
curve(exp(redclaw.geometric.twinlog$coef[1]) * x ^ redclaw.geometric.twinlog$coef[2], from = 0, to = 100, add = TRUE, col = "darkorange", lwd = 2)

# calculate coefficients for growth curve bw_1 = [bw_0^c_1 + c_2 * days]^c_4
geometric_c_1 <- 1 - redclaw.geometric.twinlog$coef[2]
geometric_c_2 <- c_1 * exp(redclaw.geometric.twinlog$coef[1])
geometric_c_4 <- 1 / geometric_c_1

# plot relationship between weight and grams gained per day 
p_geometric_gain <- redclaw.growth %>%
  ggplot(aes(geometric.weight, g.gain.day)) +
  geom_point() +
  stat_function(fun = function(x) exp(redclaw.geometric.twinlog$coef[1]) * x^redclaw.geometric.twinlog$coef[2], color = 'dodgerblue') +
  xlab("Mass (g)") +
  ylab("Daily mass gain (g/cray/day)") +
  theme_minimal()

# print plot to file
pdf("plots/p_geometric_gain.pdf", height = 4)
print(p_geometric_gain)
dev.off()

# estimate body-weight over time
bodyweight <- tibble(day = seq(1,360,1)) %>%
  mutate(bw = (bw_0^c_1 + c_2 * day)^c_4)

# write csv
write.csv(bodyweight, "tabs/bodyweight.csv")

# plot estimated growth curve
cray.growth.plot <- bodyweight %>%
  ggplot(aes(day, bw)) +
  geom_line() +
  lims(x = c(0, 300), y = c(0, 50)) +
  labs(x = "Days", y = "Mass (g)") +
  theme_minimal()

# print plot to file
pdf("plots/cray_growth_plot.pdf", height = 4)
print(cray.growth.plot)
dev.off()

# estimate body-weight over time geometric model
bodyweight_geometric <- tibble(day = seq(1,240,1)) %>%
  mutate(bw = (bw_0^geometric_c_1 + geometric_c_2 * day)^geometric_c_4)

# write to csv
write.csv(bodyweight_geometric, "tabs/bodyweight_geometric.csv")

# plot estimated growth curve geometric
cray.geometric.plot <- bodyweight_geometric %>%
  ggplot(aes(day, bw)) +
  geom_line() +
  labs(x = "Days", y = "Mass (g)") +
  theme_minimal()

# print plot to file
pdf("plots/cray_geometric_plot.pdf", height = 4)
print(cray.geometric.plot)
dev.off()

# define feeding requirements

# import data
redclaw.feed_rate <- read.csv("data/redclaw_feed_rate.csv")

# plot to view
# plot estimated growth curve
cray.feed.plot <- redclaw.feed_rate %>%
  ggplot(aes(mass, feed_rate)) +
  geom_line() +
  labs(x = "Mass (g)", y = "Feed Rate") +
  theme_minimal()

# try base code log-log relationship - looks good!
redclaw.feed.twinlog <- lm(log(feed_rate) ~ log(mass), data = redclaw.feed_rate)
summary(redclaw.feed.twinlog)

plot(log(feed_rate) ~ log(mass), data = redclaw.feed_rate, col = "grey", pch = 20, cex = 1.5, main = "Feed Rate")
abline(redclaw.feed.twinlog, col = "darkorange", lwd = 2)

plot(feed_rate ~ mass, data = redclaw.feed_rate, col = "grey", pch = 20, cex = 1.5, main = "Feed Rate")
curve(exp(redclaw.feed.twinlog$coef[1]) * x ^ redclaw.feed.twinlog$coef[2], from = 0, to = 100, add = TRUE, col = "darkorange", lwd = 2)

# plot proposed feeding regime 
p_feed_rate <- redclaw.feed_rate %>%
  ggplot(aes(mass, feed_rate)) +
  geom_point() +
  stat_function(fun = function(x) exp(redclaw.feed.twinlog$coef[1]) * x^redclaw.feed.twinlog$coef[2], color = 'dodgerblue') +
  xlab("Crayfish Mass (g)") +
  ylab("Feed Rate (proportion of bodyweight)") +
  theme_minimal()

# print plot to file
pdf("plots/p_feed_rate.pdf", height = 4)
print(p_feed_rate)
dev.off()

g.model.filter <- bodyweight %>%
  filter(day %in% seq(0, 210,7))

write.csv(g.model.filter,"tabs/growth_model.csv")
