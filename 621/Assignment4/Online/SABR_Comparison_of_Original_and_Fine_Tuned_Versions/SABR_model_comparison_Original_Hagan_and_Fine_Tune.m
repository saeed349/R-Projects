% SABR model: Pat Hagan et al (2002) "Managing Smile Risk".
% Compare Hagan's original model to refinements proposed in the literature
% and summarized in "Fine-Tune Your Smile" by Jan Obloj.
% Compare also to SABR produced by vendor model.
% There are two ways to estimate the SABR parameters (r,v,a)
% (1) Estimate r and v, and obtain a as the cubic root of an equation,
%     as in Graeme West's paper "SABR in Illiquid Markets"
% (2) Estimate r, v, and a directly.  This is the method used by 
%     the vendor software.
% Refinements to the SABR volatilities are summarized by Jan Obloj.
% This Matlab code produces 4 sets of SABR parameters.
% (1) Original SABR parameters, estimation method 1.
% (2) Original SABR parameters, estimation method 2.
% (3) Fine Tuned SABR parameters, estimation method 1.
% (4) Fine Tuned SABR parameters, estimation method 2.
% See the Note on www.FRouah.com for details.
% By Fabrice Douglas Rouah.

clc; clear;

% Select the maturity (k=1 through 6).
k = 1;

% Input the market strikes and vols for the selected maturity.
oldMK    = xlsread('SABR vol surface from vendor.xls', 'Sheet1', 'C3:AZ3');
oldMV    = xlsread('SABR vol surface from vendor.xls', 'Sheet1', 'C4:AZ9');
VendorV  = xlsread('SABR vol surface from vendor.xls', 'Sheet1', 'C23:AZ28');

% Input the Maturity (T), ATM Strike (F), ATM Vol
T      = xlsread('SABR vol surface from vendor.xls', 'Sheet1', 'B4:B9');
F      = xlsread('SABR vol surface from vendor.xls', 'Sheet1', 'L13');
ATMvol = xlsread('SABR vol surface from vendor.xls', 'Sheet1', 'D13:D18');

% Select the k-th maturity
oldMV = oldMV(k,:);
VendorV = VendorV(k,:);
T = T(k);
ATMvol = ATMvol(k);

% Select only non-blank entries
Index = find(~isnan(oldMV));
MV = oldMV(Index);
MK = oldMK(Index);


% Create a grid of strikes for the SABR vols
Increment = 5;
MK2 = [oldMK(1):Increment:oldMK(end)];

% Define the starting values and options for fminsearch
start = [.3, .3];
options = optimset('MaxFunEvals', 1e5, 'TolFun', 1e-8, 'TolX', 1e-10);

% Parameter estimation method 1.  Set beta = 0.5.
% Estimate rho and v, and at each iteration step,
% find alpha as the cubic root using the findAlpha function.
% Original Hagan SABR model, estimation method 1.
b = .5;
FineTune = 'N';
[param1H, feval] = fminsearch(@(par)EstimateRhoAndVol_FineTune(par, MK, MV, ATMvol, F, T, b, FineTune), start, options);
r1H = param1H(1);
v1H = param1H(2);
a1H = findAlpha(F,F,T,ATMvol,b,r1H,v1H);
% Fined Tuned SABR model, estimation method 1.
FineTune = 'Y';
[param1F, feval] = fminsearch(@(par)EstimateRhoAndVol_FineTune(par, MK, MV, ATMvol, F, T, b, FineTune), start, options);
r1F = param1F(1);
v1F = param1F(2);
a1F = findAlpha(F,F,T,ATMvol,b,r1F,v1F);


% Parameter estimation method 2.  Set beta = 0.5.
% Estimate rho, v, and alpha directly
% Original Hagan SABR model, estimation method 2.
b = 0.5;
start = [.3 .3 .2];
FineTune = 'N';
[param2H , feval] = fminsearch(@(par)EstimateAllParameters_FineTune(par, MK, MV, F, T, b, FineTune), start, options);
a2H = param2H(1);
r2H = param2H(2);
v2H = param2H(3);
% Fined Tuned SABR model, estimation method 2.
FineTune = 'Y';
[param2F, feval] = fminsearch(@(par)EstimateAllParameters_FineTune(par, MK, MV, F, T, b, FineTune), start, options);
a2F = param2F(1);
r2F = param2F(2);
v2F = param2F(3);

% Create the SABR curve based on these 4 sets of parameters.
for j=1:length(MK2);
	FineTune = 'N';
	SABR_1H(j) = SABRvol_FineTune(a1H, b, r1H, v1H, F, MK2(j), T, FineTune); % Original SABR, method 1.
	SABR_2H(j) = SABRvol_FineTune(a2H, b, r2H, v2H, F, MK2(j), T, FineTune); % Original SABR, method 2.
	FineTune = 'Y';
	SABR_1F(j) = SABRvol_FineTune(a1F, b, r1F, v1F, F, MK2(j), T, FineTune); % Fine Tuned SABR, method 1.
	SABR_2F(j) = SABRvol_FineTune(a2F, b, r2F, v2H, F, MK2(j), T, FineTune); % Fine Tuned SABR, method 2.
end

% Plot the results of the four SABR curves against the market implied vols.
% Parameter estimation method 1.
subplot(1,2,1);
plot(oldMK, oldMV, 'kx', oldMK, VendorV, 'ko', MK2, SABR_1H, 'k-', MK2, SABR_1F, 'r-')
legend('Market Vols', 'Vendor SABR', 'Original SABR, Method 1', 'Fine Tuned SABR, Method 1');

% Parameter estimation method 2.
subplot(1,2,2);
plot(oldMK, oldMV, 'kx', oldMK, VendorV, 'ko', MK2, SABR_2H, 'k-', MK2, SABR_2F, 'r-')
legend('Market Vols', 'Vendor SABR', 'Original SABR, Method 2', 'Fine Tuned SABR, Method 2');

% Evaluate the goodness-of-fit in terms of MSE
% SSE for Vendor model.
VendorV = VendorV(Index);
for j=1:length(MV);
	e_v(j)  = abs((VendorV(j) - MV(j))/MV(j));
end

% SSE for Original and Fine Tuned SABR
for j=1:length(MK);	
	FineTune = 'N';
	e_1H(j) = abs((SABRvol_FineTune(a1H, b, r1H, v1H, F, MK(j), T, FineTune) - MV(j))/MV(j));
	e_2H(j) = abs((SABRvol_FineTune(a2H, b, r2H, v2H, F, MK(j), T, FineTune) - MV(j))/MV(j));
	FineTune = 'Y';
	e_1F(j) = abs((SABRvol_FineTune(a1F, b, r1F, v1F, F, MK(j), T, FineTune) - MV(j))/MV(j));
	e_2F(j) = abs((SABRvol_FineTune(a2F, b, r2F, v2H, F, MK(j), T, FineTune) - MV(j))/MV(j));
end

Error_Vendor  = mean(e_v)
Error_Hagan_Method_1 = mean(e_1H) 
Error_Hagan_Method_2 = mean(e_2H) 
Error_FineTuned_Method_1 = mean(e_1F) 
Error_FineTuned_Method_2 = mean(e_2F) 
