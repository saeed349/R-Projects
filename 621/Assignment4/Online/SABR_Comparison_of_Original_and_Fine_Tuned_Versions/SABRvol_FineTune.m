function y = SABRvol_FineTune(a,b,r,v,F,K,T,FineTune);

% ---------------------------------------------
% Returns two possible SABR volatilities.
% (1) Original SABR formula by Hagan et al.
% (2) Fine Tuned formula summarized in 
%     "Fine-Tune Your Smile" by Jan Obloj
% Required inputs:
% a = alpha parameter
% b = beta parameter
% r = rho parameter
% v = vol of vol parameter
% F = spot price
% K = strike price
% T = maturity
% FineTune? ('Y' or 'N')
% ---------------------------------------------
% By Fabrice Douglas Rouah

x = log(F/K);

% Separate out into cases x=0, b=1, and b<1.
if abs(x) <= 0.0001
	I0H = a*K^(b-1);
	I0B = a*K^(b-1);
else
	if abs(v) <= 0.001  % ATM vol
		I0H = x*a*(1-b) / (F^(1-b)-K^(1-b));
		I0B = x*a*(1-b) / (F^(1-b)-K^(1-b));
	else
		if b == 1
			z = v*x/a;
			sq = sqrt(1 - 2*r*z + z^2);
			I0H = v*x / log((sq + z - r)/(1-r));
			I0B = v*x / log((sq + z - r)/(1-r));
		else
			z = v*(F^(1-b) - K^(1-b)) / a / (1-b);
			e = v*(F-K) / a / (F*K)^(b/2);
			sq = sqrt(1-2*r*e +e^2);
			I0H = v*x*e / z / log((sq + e - r)/(1-r));
			I0B = v*x / log((sq + z - r)/(1-r));
		end
	end
end

I1H = (b-1)^2*a^2/24/(F*K)^(1-b) + r*v*a*b/4/(F*K)^((1-b)/2) + (2-3*r^2)*v^2/24;

% Original Hagan SABR implied vol.
HaganVol   = I0H*(1 + I1H*T);

% Fine Tuned SABR implied vol.
RefinedVol = I0B*(1 + I1H*T);

if strcmp(FineTune,'N')
	y = HaganVol;
else
	y = RefinedVol;
end

