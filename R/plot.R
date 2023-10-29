rm(list = ls())
library(ggplot2)
library(lamW)
library(reshape2)
library(tikzDevice)

f <- function(x)
{
  cg <- sqrt(2 * pi)
  sqrt(lambertW0((cg * x)^(-2)))
}

xname <- '$1 / \\alpha$'
y1name <- '$z_\\alpha$'
y2name <- '$f_1(\\alpha)$'

n <- 1000
alpha <- 1 - (ppoints(n) + 1) / 2
y1 <- qnorm(1 - alpha)
y2 <- f(alpha)

df <- data.frame(x = 1 / alpha, y1 = y1, y2 = y2)
names(df) <- c('x', y1name, y2name)
df <- melt(df, id = 'x')
names(df) <- c('x', 'curve', 'y')

p <- ggplot() + labs(x = xname, y = '', linetype = '')
p <- p + geom_line(data = df, aes(x = x, y = y, group = curve, linetype = curve))

tikz('../tex/figBounds.tex', height = 4, width = 6.5, timestamp = FALSE)
print(p)
dev.off()

commands <- c('maxAlpha', 'maxAlphaDiff')
values <- c(alpha[n], y2[n] - y1[n])
digits <- c(6, 3)
lines <- rep(NA, 2)

for (i in 1:2)
{
  lines[i] <- paste('\\newcommand{\\', commands[i], '}{\\ensuremath{',
                    round(values[i], digits[i]), '}}', sep = '')
}

fileConn <- file('../tex/dataCommands.tex')
writeLines(lines, fileConn)
close(fileConn)
