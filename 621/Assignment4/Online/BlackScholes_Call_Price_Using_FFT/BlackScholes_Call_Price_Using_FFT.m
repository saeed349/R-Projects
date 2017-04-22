% Fast Fourier Transform for the Black Scholes Model

% --------------------------------------
% Inputs
%   N  = number of discretization points
%   uplimit = Upper limit of integration
%   S0 = spot price
%   r = risk free rate
%   tau = maturity
%   sigma = volatility
%   alpha = dampening factor
%   fast = fast versus slow algorithm.
%     fast = 1 fast version.  Uses vectorization.
%     fast = 0 slow version.  Uses loops
% --------------------------------------
% Outputs
%   CallFFT = Black Scholes call prices using FFT
%   CallBS  = Black Scholes call prices using closed form
%         K = Strike prices
%       eta = increment for integration range
%    lambda = increment for log-strike range
% --------------------------------------
% By Fabrice Douglas Rouah www.Volopta.com
 
clc; clear;

% Required inputs
S0 = 10;           % Spot price.
r = 0.0;           % Risk free rate.
q = 0.0;           % Dividend yield
tau = .5;          % Time to maturity.
sigma = 0.3;       % Volatility
alpha = 1.75;      % Dampening factor
N = 128;            % Grid size
uplimit = 200;      % Upper limit of integration
fast = 0;           % Choice of fast or slow algorithm

% Run the Fast Fourier Transform
[CallFFT CallBS K lambda eta] = BlackScholesFFT(N,uplimit,S0,r,q,tau,sigma,alpha,fast);

% Print the results near the ATM strikes
u1 = find(round(K*1000)/1000==S0);
du = 15;
[K(u1-du:u1+du) CallFFT(u1-du:u1+du) CallBS(u1-du:u1+du)]

% Print the increments for integration and for log strikes
IntegrationIncrement = eta
LogStrikeIncrement = lambda
NumberOfPoints = N
