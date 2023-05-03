t.otoczenia = 23
Ts = c(50, 55, 60, 70, 80, 95, 105, 115, 120)
T4s = (273.15 + Ts) ^ 4
mrs = c(0.07, 0.14, 0.15, 0.20, 0.28, 0.34, 0.44, 0.49, 0.56)
whs = c(1.89, 2.39, 2.64, 3.49, 4.48, 5.83, 6.93, 7.78, 8.59)
bls = c(1.94, 2.41, 2.69, 3.76, 4.56, 6.23, 7.24, 8.29, 8.96)
grs = c(0.44, 0.55, 0.61, 0.88, 1.04, 1.48, 1.72, 2.01, 2.15)
del = c(0.05, 0.07, 0.09, 0.11, 0.11, 0.13, 0.13, 0.14, 0.15)

df = data.frame(T4s = T4s, mrs = mrs, whs = whs, bls = bls, grs = grs, del = del)
ggplot(df) + 
  geom_point(aes(x = T4s, y = mrs), color="blue") + 
  geom_smooth(aes(x = T4s, y = mrs), method = "lm", color="blue") + 
  geom_point(aes(x = T4s, y = bls), color="black") + 
  geom_smooth(aes(x = T4s, y = bls), method = "lm", color="black") + 
  geom_point(aes(x = T4s, y = whs), color="yellow") + 
  geom_smooth(aes(x = T4s, y = whs), method = "lm", color="yellow") + 
  geom_point(aes(x = T4s, y = grs), color="brown") + 
  geom_smooth(aes(x = T4s, y = grs), method = "lm", color="brown")

cst = lm(bls ~ T4s, data=df)$coeff["T4s"] / 0.95
Amr = lm(mrs ~ T4s, data=df)$coeff["T4s"] / cst
Awh = lm(whs ~ T4s, data=df)$coeff["T4s"] / cst
Agr = lm(grs ~ T4s, data=df)$coeff["T4s"] / cst

Vs = c(0.722, 1.65, 2.581, 3.521, 4.46, 5.41, 6.37, 7.33, 8.29, 9.27)
As = c(0.889, 1.167, 1.407, 1.627, 1.827, 2.013, 2.189, 2.355, 2.508, 2.658)
Rs = Vs / As
alfas = 0.00407 * ((Rs / 0.277) ^ 0.11778)
Ts = (Rs - 0.277) / (alfas * 0.277) + 300
T4s = Ts ^ 4

mVs = c(0.02, 0.56, 1.71, 3.33, 5.23, 7.55, 10.07, 12.81, 15.88, 18.82)
df2 = data.frame(Vs = Vs, As = As, mVs = mVs, T4s = T4s)
ggplot(df2, aes(x = T4s, y = mVs)) + geom_point() + geom_smooth(method="lm")

ls = c(5, 10, 15, 18, 21, 24, 27, 30, 35, 40, 50, 60)
mVs = c(18.48, 6.58, 3.31, 2.41, 1.83, 1.44, 1.15, 0.94, 0.7, 0.54, 0.34, 0.28)
l2is = 1/ls ^ 2
df3 = data.frame(ls = ls, mVs = mVs, l2is = l2is)
ggplot(df3, aes(x = ls, y = mVs)) + geom_point() + geom_smooth()

df4 = data.frame(ls = ls[-1:-2], mVs = mVs[-1:-2], l2is = l2is[-1:-2])
ggplot(df4, aes(x = ls, y = mVs)) + geom_point() + geom_smooth()