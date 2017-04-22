
clc; clear;


disp('%%-----------------Part1--------------------------------%%')

k = 1;
% Input the market strikes and vols, and the vendor SABR volatilities
oldMK    = xlsread('swap_data.xlsx', 'Sheet1', 'C3:U3');
oldMV    = xlsread('swap_data.xlsx', 'Sheet1', 'C4:U4');


% Input the Maturity (T), ATM Strike (F), ATM Vol
T      = xlsread('swap_data.xlsx', 'Sheet1', 'A4');
F      = xlsread('swap_data.xlsx', 'Sheet1', 'B6');

oldMV = oldMV(k,:);
T = T(k);
% Select only non-blank entries
Index = find(~isnan(oldMV));
MV = oldMV(Index);
MK = oldMK(Index);

% Create a grid of strikes for the SABR vols
MK2 = oldMK; 

%beta
b = .5;

% Define the starting values and options for fminsearch

options = optimset('MaxFunEvals', 1e5, 'TolFun', 1e-8, 'TolX', 1e-10);
start = [.3 ,.3 ,.2];
[param2, feval] = fminsearch(@(par)EstimateAllParameters(par, MK, MV, F, T, b), start, options);

a = param2(1);
r = param2(2);
v = param2(3);

% Create the SABR curve based on these parameters.
for j=1:length(MK2);
	Vol2(j) = SABRvol(a, b, r, v, F, MK2(j), T);
end
% Plot the results of both SABR curves against the curve from the market.

% 'alpha parameter     ::',a
% 'beta parameter      ::',b
% 'rho parameter       ::',r
% 'vol of vol parameter::',v

X = sprintf('Beta=%f, Alpha=%f, Rho=%f, Vol=%f',b,a,r,v);
disp(X)
P=sprintf('Mean Squared Error=%f',immse(Vol2,oldMV));
disp(P)

figure
plot(MK2, Vol2, 'r-',  oldMK, oldMV, 'kx-')
legend('SABR vol for 3 year maturity','Original Vol for 3 year maturity')
legend('boxoff')
title('BETA=.5')

disp('%%-----------------Part2--------------------------------%%')

%--------------beta=.7----------
b=.7;
start = [.3 ,.3 ,.2];
[param2, feval] = fminsearch(@(par)EstimateAllParameters(par, MK, MV, F, T, b), start, options);

a = param2(1);
r = param2(2);
v = param2(3);

% Create the SABR curve based on these parameters.
for j=1:length(MK2);
	Vol2(j) = SABRvol(a, b, r, v, F, MK2(j), T);
end
% Plot the results of both SABR curves against the curve from the market.

X = sprintf('Beta=%f, Alpha=%f, Rho=%f, Vol=%f',b,a,r,v);
disp(X)
P=sprintf('Mean Squared Error=%f',immse(Vol2,oldMV));
disp(P)

figure
plot(MK2, Vol2, 'b-',  oldMK, oldMV, 'kx-')

legend('SABR vol for 3 year maturity','Original Vol for 3 year maturity')
legend('boxoff')
title('BETA=.7')

%--------------beta=.4----------
b=.4;
start = [.3 ,.3 ,.2];
[param2, feval] = fminsearch(@(par)EstimateAllParameters(par, MK, MV, F, T, b), start, options);

a = param2(1);
r = param2(2);
v = param2(3);

% Create the SABR curve based on these parameters.
for j=1:length(MK2);
	Vol3(j) = SABRvol(a, b, r, v, F, MK2(j), T);
end
% Plot the results of both SABR curves against the curve from the market.

X = sprintf('Beta=%f, Alpha=%f, Rho=%f, Vol=%f',b,a,r,v);
disp(X)
P=sprintf('Mean Squared Error=%f',immse(Vol3,oldMV));
disp(P)
figure
plot(MK2, Vol3, 'b-',  oldMK, oldMV, 'kx-')
legend('SABR vol for 3 year maturity','Original Vol for 3 year maturity')
legend('boxoff')
title('BETA=.4')




disp('%%-----------------Part3--------------------------------%%')


figure
plot(MK2, Vol2, 'r-',MK2, Vol3, 'b-',  oldMK, oldMV, 'kx-')
legend('SABR vol with Beta=.7','SABR vol with Beta=.4','Original Vol for 3 year maturity')
legend('boxoff')
title('Combined Plot with BETA=.7 and BETA=.4')


disp('%%-----------------Part5--------------------------------%%')


b=.7;
start = [.3 ,.3 ,.2];
[param2, feval] = fminsearch(@(par)EstimateAllParameters(par, MK, MV, F, T, b), start, options);

a = param2(1);
r = param2(2);
v = param2(3);

oldMK    = xlsread('swap_data.xlsx', 'Sheet1', 'C19:U19');
oldMV    = xlsread('swap_data.xlsx', 'Sheet1', 'C20:U20');
% Input the Maturity (T), ATM Strike (F), ATM Vol
T      = xlsread('swap_data.xlsx', 'Sheet1', 'A20');
F      = xlsread('swap_data.xlsx', 'Sheet1', 'B23');
oldMV = oldMV(k,:);
T = T(k);
Index = find(~isnan(oldMV));
MV = oldMV(Index);
MK = oldMK(Index);
MK2 = oldMK; 

% Create the SABR curve based on these parameters.
for j=1:length(MK2);
	Vol2(j) = SABRvol(a, b, r, v, F, MK2(j), T);
end
% Plot the results of both SABR curves against the curve from the market.

X = sprintf('Beta=%f, Alpha=%f, Rho=%f, Vol=%f',b,a,r,v);
disp(X)
P=sprintf('Mean Squared Error=%f',immse(Vol2,oldMV));
disp(P)

figure
plot(MK2, Vol2, 'b-',  oldMK, oldMV, 'kx-')

legend('SABR vol for 12 year maturity','Original Vol for 12 year maturity')
legend('boxoff')
title('BETA=.4')
