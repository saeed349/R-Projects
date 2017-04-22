function y = EstimateAllParametersFine_Tune(params,MktStrike,MktVol,F,T,b,FineTune)

% -------------------------------------------------------------------------
% Uses the Fine Tuned version of the SABR volatilities.
% Returns the following SABR parameters:
% a = alpha
% r = rho
% v = vol-of-vol
% Required inputs:
% MktStrike = Vector of Strikes
% MktVol    = Vector of corresponding volatilities
% F = spot price
% T = maturity
% b = beta parameter
% FineTune? ('Y' or 'N')
% -------------------------------------------------------------------------
a = params(1);
r = params(2);
v = params(3);

N = length(MktVol);

% Define the model volatility and the squared error terms
for i=1:N
	ModelVol(i) = SABRvol_FineTune(a,b,r,v,F,MktStrike(i),T,FineTune);
	error(i) = (ModelVol(i) - MktVol(i))^2;
end;

% Return the SSE
y = sum(error);

% Impose the constraint that -1 <= rho <= +1 and that v>0
if abs(r)>1 | v<0
	y = 1e100;
end

	