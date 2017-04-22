function y = EstimateRhoAndVol(params,MktStrike,MktVol,ATMVol,F,T,b,FineTune)

% -------------------------------------------------------------------------
% Uses the Fine Tuned version of the SABR volatilities.
% Returns the following SABR parameters:
% r = rho
% v = vol-of-vol
% Uses ATM volatility to estimate alpha
% Required inputs:
% MktStrike = Vector of Strikes
% MktVol    = Vector of corresponding volatilities
% ATMVol = ATM volatility
% F = spot price
% T = maturity
% b = beta parameter
% Fine Tune?  'Y' or 'N'
% -------------------------------------------------------------------------
r = params(1);
v = params(2);
a = findAlpha(F,F,T,ATMVol,b,r,v);
N = length(MktVol);

% Define the model volatility and the squared error terms
for i=1:N
	ModelVol(i) = SABRvol_FineTune(a,b,r,v,F,MktStrike(i),T,FineTune);
	error(i) = (ModelVol(i) - MktVol(i))^2;
end;

% Return the SSE
y = sum(error);

% Impose the constraint that -1 <= rho <= +1 and that v>0
% via a penalty on the objective function
if abs(r)>1 | v<0
	y = 1e100;
end

	