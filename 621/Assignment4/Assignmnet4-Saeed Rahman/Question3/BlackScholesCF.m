function CF = BlackScholesCF(phi,S0,r,v,T);

% Black Scholes characteristic function for the log stock price
% ---------------------------------
% Inputs
%   phi = vector of arguments
%   SO  = spot price
%   r   = risk free rate
%   v   = volatility
%   T   = maturity
% ---------------------------------
% Output
%   CF = vector of values of the characteristic function, evaluated
%        at each point of the phi vector.

u  = log(S0) + (r-v.^2./2).*T;
s2 = v.^2.*T;

CF = exp(i.*u.*phi - 0.5.*s2.*phi.^2);

