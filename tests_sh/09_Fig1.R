
## plots for the beta distributions for Figure 1

set.seed(3)

# s1 <- seq(0,1,0.01)

d1 <- rbeta(10e6, shape1 = 1, shape2 = 1)
dens_1 <- density(d1)
plot(dens_1)
p1 <- polygon(dens_1, col = "red", main = "")

title("A", adj = 0)

d2 <- rbeta(10e6, shape1 = 1, shape2 = 1.5)
#dens_2 <- density(d2)

library(ggplot2)

d1_df <- as.data.frame(d1)
p1a <- ggplot(d1_df, aes(x = d1)) + 
  geom_density(colour = "#7997FF", fill = "#7997FF", alpha = 0.4) +
  #geom_density(fill = "grey") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  ggtitle("A") + 
  ylab("Density") +
  xlab("") +
  xlim(0,1)
p1a

d2_df <- as.data.frame(d2)
#colnames(d2_df) <- "x"
p1b <- ggplot(d2_df, aes(x = d2)) + 
  geom_density(fill = "red", colour = "red") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  ggtitle("B") + 
  ylab("Density") +
  xlab("") + 
  xlim(0,1)


d3 <- rbeta(10e6, shape1 = 1.5, shape2 = 1)
d3_df <- as.data.frame(d3)
p1c <-   ggplot(d3_df, aes(x = d3)) + 
  geom_density(fill = "blue", colour = "blue") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  ggtitle("C") + 
  ylab("Density") +
  xlab("") + 
  xlim(0,1)
p1c


d4 <- rbeta(10e6, shape1 = 1, shape2 = 5)
d4_df <- as.data.frame(d4)
p1d <-   ggplot(d4_df, aes(x = d4)) + 
  geom_density(fill = "red", colour = "red") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  ggtitle("D") + 
  ylab("Density") +
  xlab("") + 
  xlim(0,1) +
  scale_y_continuous(breaks = c(0,1,2,3,4), labels=c("0.0", "1.0",
                             "2.0", "3.0", "4.0"))
p1d


d5 <- rbeta(10e6, shape1 = 5, shape2 = 1)
d5_df <- as.data.frame(d5)
p1e <-   ggplot(d5_df, aes(x = d5)) + 
  geom_density(colour = "blue", fill = "blue") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), axis.title = element_text(size = 12)) +
  ggtitle("E") + 
  ylab("Density") +
  xlab("") +
  xlim(0,1) +
  scale_y_continuous(breaks = c(0,1,2,3,4), labels=c("0.0", "1.0",
                                                     "2.0", "3.0", "4.0"))
p1e



gridExtra::grid.arrange(p1a, p1b, p1c, p1d, p1e, ncol = 2,  
             layout_matrix = cbind(c(1,2,4), c(1,3,5)))


install.packages("patchwork")
library(patchwork)

layout <- "
##AA##
BB#CCC
DD#EEE
"

p1a + p1b + p1c + p1d + p1e + plot_layout(design = layout)

pdf("tests_sh/figs/Fig1.pdf", height = 6, width = 6)
myplot <- p1a + p1b + p1c + p1d + p1e + plot_layout(design = layout)
print(myplot)
dev.off()


library("cowplot")

pdf("tests_sh/figs/Fig1.pdf", height = 9, width = 9)
#myplot <- 
ggdraw() +
  draw_plot(p1a, x = 0.25, y = 0.66, width = .5, height = 0.33) +
  draw_plot(p1b, x = 0, y = 0.33, width = .5, height = 0.33) +
  draw_plot(p1c, x = 0.5, y = 0.33, width = 0.5, height = 0.33) +
  draw_plot(p1d, x = 0.0, y = 0, width = .5, height = 0.33) +
  draw_plot(p1e, x = 0.5, y = 0, width = .5, height = 0.33) 
#print(myplot)
dev.off()
