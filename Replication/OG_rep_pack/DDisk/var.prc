@  VAR.PRC 
   Miscellaneous VAR Procedures
@

@ -- varest1.prc ------------------------------------------
  Basic VAR Estimation
@

/*
   varest1.prc
   mww, 2/8/01
*/
proc(3) = varest1(s,t1,t2,varlag);

@ -- Input and Output --
 

Model: 

y(t) = c + p1*y(t-1) + p2*y(t-2) + ...  Pvarlag*y(t-varlag) + e(t)
 
 
Input: 
  S == Data vector (Txn)
  t1 and t2 == sample period --
               VAR is estimated using S[t1:t2,.]
  varlag == number of lags

Output:
  Const == vector of constants
  phi == (n x (n*varlag)) matrix of VAR coefficients
         phi=[p1 p2 p3 ... pvarlag]
  seps = variance of e

@
     
local yv, x, i, beta, e, seps, const, phi;

@ -- Checks -- @
if t1 <= varlag;
 "t1 <= varlag in varest1.prc -- processing stops"; stop;
endif;

if ismiss(s[t1-varlag:t2,.]) .== 1;
  "Missing data in varest1.prc -- processing stops"; stop;
endif; 

@ Set Up VAR @
yv=s[t1:t2,.];
x=ones(rows(yv),1);
i=1; do while i<=varlag;
  x=x~s[t1-i:t2-i,.];
i=i+1; endo;

@ Estimate VAR @
beta=inv(x'x)*x'yv;
e=yv-x*beta;
seps=(e'e)/(rows(e)-cols(x));

@
  Transform the VAR so that it is written in Standard form as:
  s(t)=P1*s(t-1) + P2*s(t-2) + ... + Pvarlag*s(t-varlag) + e(t)
@
const=beta[1,.]';
phi=beta[2:rows(beta),.]';

retp(const,phi,seps);
endp;
@ -------------------------------------------------------------------- @
proc(1) = vargen1(const,phi,seps,t,nplus);

@ -- Input and Output --
 

Model: 

y(t) = const + p1*y(t-1) + p2*y(t-2) + ...  Pvarlag*y(t-varlag) + e(t)

     a zero initial condition is used to begin the iterations and the 
     the first "nplus" observations are discarded 

Input: 
  Const == vector of constants
  phi == (n x (n*varlag)) matrix of VAR coefficients
         phi=[p1 p2 p3 ... pvarlag]
  seps = variance of e
  t == number of time periods to generate
  nplus == number of additional "initial" conditions that are discarded
  
Ouput:
  S == t x n matrix of observations
       (t is time series dimension, n is cross section dimension)
@
local t1,n,c,e,y,svec,yt;

     
t1=t+nplus;
n=rows(phi);
c=chol(seps);
e=rndn(t1,n);
e=e*c;
y=zeros(t1,n);
svec=zeros(cols(phi),1);

for i (1,t1,1);
 yt=const+phi*svec+e[i,.]';
 y[i,.]=yt';
 if n .< rows(svec);
   svec[n+1:rows(svec)]=svec[1:rows(svec)-n];
 endif;
 svec[1:n]=yt;
endfor;
y=y[nplus+1:t1,.];
retp(y);
endp;

@ ----------------------- compest.prc ------------------------- @
/*
compest.prc
 Estimate Companion Form of a VAR
*/

proc(3) = compest(s,t1,t2,varlag);

@ -- Input and Output --
 

Model: 

y(t) = c + p1*y(t-1) + p2*y(t-2) + ...  Pvarlag*y(t-varlag) + e(t)
 
 
Input: 
  S == Data vector (Txn)
  t1 and t2 == sample period --
               VAR is estimated using S[t1:t2,.]
  varlag == number of lags

Output:
  Const == vector of constants
  Companion Matrix of VAR
  seps = variance of e

@
     
local yv, x, i, beta, e, seps, const, phi, temp;

@ -- Checks -- @
if t1 <= varlag;
 "t1 <= varlag in varest1.prc -- processing stops"; stop;
endif;

if ismiss(s[t1-varlag:t2,.]) .== 1;
  "Missing data in varest1.prc -- processing stops"; stop;
endif; 

@ Set Up VAR @
yv=s[t1:t2,.];
x=ones(rows(yv),1);
i=1; do while i<=varlag;
  x=x~s[t1-i:t2-i,.];
i=i+1; endo;

@ Estimate VAR @
beta=inv(x'x)*x'yv;
e=yv-x*beta;
seps=(e'e)/(rows(e)-cols(x));

@
  Transform the VAR so that it is written in Standard form as:
  s(t)=P1*s(t-1) + P2*s(t-2) + ... + Pvarlag*s(t-varlag) + e(t)
@
const=beta[1,.]';
temp=beta[2:rows(beta),.]';

@ ---- Calculate Companion Matrix ---- @
phi=zeros(cols(temp),cols(temp));
phi[1:rows(temp),.]=temp;
if cols(temp) .> rows(temp);
 phi[rows(temp)+1:rows(phi),1:cols(phi)-rows(temp)]=eye(cols(phi)-rows(temp));
endif;

retp(const,phi,seps);
endp;
