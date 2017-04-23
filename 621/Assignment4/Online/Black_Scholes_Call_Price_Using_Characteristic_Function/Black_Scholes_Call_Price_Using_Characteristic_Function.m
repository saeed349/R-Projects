% Black-Scholes call price using the characteristic function
% Uses the Heston-like form for the call price
%     Call = S*exp(-qT)*P1 - K*exp(-rT)*P1
% where P1 and P2 are in-the-money probabilities obtained from the CF
% by Fabrice Douglas Rouah

clc; clear;

% Inline function for the Black Scholes call
C = inline('s*exp(-q*T)*normcdf((log(s/K) + (r-q+v^2/2)*T)/v/sqrt(T)) - K*exp(-r*T)*normcdf((log(s/K) + (r-q+v^2/2)*T)/v/sqrt(T) - v*sqrt(T))',...
	's','K','r','q','v','T');

% Spot, strike, risk free rate, dividend yield, maturity, and volatility
S = 100;
K = 100;
r = 0.04055;
q = 0.022;
T = .5;
v = 0.51;

% Load the Gauss Laguerre weights
load GaussLaguerreWeights32
x = GaussLaguerreWeights32(:,1);
w = GaussLaguerreWeights32(:,2);

% Integrate the Black-Scholes integrands to obtain P1 and P2
for k=1:32;
	int1(k) = w(k)*BSIntegrand(x(k),S,K,r,q,v,T,1);
	int2(k) = w(k)*BSIntegrand(x(k),S,K,r,q,v,T,2);
end

% The in-the-money probabilities P1 and P2
P1 = 1/2 + 1/pi*sum(int1);
P2 = 1/2 + 1/pi*sum(int2);

% The call price using the characteristic function
CallCF = S*exp(-q*T)*P1 - K*exp(-r*T)*P2

% The call price using the closed form
CallBS = C(S,K,r,q,v,T)

