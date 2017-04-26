function y = BlackScholesCall(S0,K,r,q,v,T);

% Black Scholes call price
% Inputs
%   S0 = spot price
%   K  = vector of strike prices
%   r  = risk free rate
%   q  = dividend yield
%   v  = volatility
%   T  = maturity

d1 = (log(S0/K) + (r-q+v^2/2)*T)/v/sqrt(T);
d2 = d1 - v*sqrt(T);

y = S0*exp(-q*T)*normcdf(d1) - K*exp(-r*T)*normcdf(d2);

