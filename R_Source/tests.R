#z-test
zTest <- function(n1, n2, m1, m2, alpha = 0.05)
{
	zd <- ((m1 + m2)/(n1 + n2))*((n1 + n2 - m1 - m2)/(n1 + n2))*(1/n1 + 1/n2)
	stat <- (m1/n1 + 1/(2*n1) - m2/n2 - 1/(2*n2))/sqrt(zd)
	pValue <- 2*(1 - pnorm(abs(stat)))
	
	return(list(stat = stat, pValue = pValue, isSignificant = pValue < alpha))
}
