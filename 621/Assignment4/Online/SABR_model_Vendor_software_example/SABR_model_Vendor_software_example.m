% SABR model: Pat Hagan et al (2002) "Managing Smile Risk".
% Compare SABR volatilities to those produced by a vendor software
% There are two ways to estimate the SABR parameters (r,v,a)
% (1) Estimate r and v, and obtain a as the cubic root of an equation,
%     as in Graeme West's paper "SABR in Illiquid Markets"
% (2) Estimate r, v, and a directly.  This is the method used by 
%     the vendor software.
% See the Note on www.FRouah.com for details.
% By Fabrice Douglas Rouah.

clc; clear;

% Select the maturity (k=1 through 6);
k = 2;

% Input the market strikes and vols, and the vendor SABR volatilities
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
Increment = 10;
MK2 = [oldMK(1):Increment:oldMK(end)];

% Define the starting values and options for fminsearch
start = [.3, .3];
options = optimset('MaxFunEvals', 1e5, 'TolFun', 1e-8, 'TolX', 1e-10);

% Parameter estimation method 1.  Set beta = 0.5.
% Estimate rho and v, and at each iteration step,
% find alpha as the cubic root using the findAlpha function
b = .5;
[param, feval] = fminsearch(@(par)EstimateRhoAndVol(par, MK, MV, ATMvol, F, T, b), start, options);
r = param(1);
v = param(2);
a = findAlpha(F,F,T,ATMvol,b,r,v);

% Create the SABR curve based on these parameters.
for j=1:length(MK2);
	Vol(j) = SABRvol(a, b, r, v, F, MK2(j), T);
end

% Parameter estimation method 2.  Set beta = 0.5.
% Estimate rho, v, and alpha directly
start = [a r .2];
[param2, feval] = fminsearch(@(par)EstimateAllParameters(par, MK, MV, F, T, b), start, options);
a = param2(1);
r = param2(2);
v = param2(3);

% Create the SABR curve based on these parameters.
for j=1:length(MK2);
	Vol2(j) = SABRvol(a, b, r, v, F, MK2(j), T);
end

% Plot the results of both SABR curves against the curve from the vendor.
% The vendor software clearly uses estimation method 2.
plot(MK2, Vol, 'k-', MK2, Vol2, 'r-', oldMK, VendorV, 'ko', oldMK, oldMV, 'kx')
legend('SABR vol method 1 (r and v only)', 'SABR vol method 2 (all parameters)', 'Vendor SABR vol', 'Market vol')
