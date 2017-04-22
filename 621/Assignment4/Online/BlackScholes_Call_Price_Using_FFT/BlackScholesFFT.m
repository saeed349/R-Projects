function [CallFFT CallBS K lambdainc eta] = ...
	      BlackScholesFFT(N,uplimit,S0,r,q,tau,sigma,alpha,fast);

% Fast Fourier Transform on Black Scholes
% --------------------------------------
% Inputs
%   N  = number of discretization points
%   uplimit = Upper limit of integration
%   S0 = spot price
%   r = risk free rate
%   q = dividend yield
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
% lambdainc = increment for log-strike range
% --------------------------------------
% By Fabrice Douglas Rouah www.Volopta.com

% Black-Scholes call price
BSC = inline('s*exp(-q*T)*normcdf((log(s/K) + (r-q+v^2/2)*T)/v/sqrt(T)) - K*exp(-r*T)*normcdf((log(s/K) + (r-q+v^2/2)*T)/v/sqrt(T) - v*sqrt(T))',...
	         's','K','r','q','v','T');
		 
% Black-Scholes characteristic function for ln(S(T))
BSCF = inline('exp(i*(log(S0) + (r-sigma^2/2)*T)*phi - 0.5*sigma^2*T*phi^2)',...
              'phi','S0','r','sigma','T');

% log spot price
s0 = log(S0);

% Specify the increments
eta = uplimit/N;
lambdainc = 2*pi/N/eta;

% Initialize and specify the weights
w = zeros(N,1);
w(1) = 1/2;
w(N) = 1/2;
w(2:N-1) = 1;

% Specify the b parameter
b = N*lambdainc/2;

% Create the grid for the integration
v = eta.*[0:N-1]';

% Create the grid for the log-strikes
k = -b + lambdainc.*[0:N-1]' + s0;

% Create the strikes and identify ATM 
K = exp(k);
u1  = find(round(K*1000)/1000==S0);

% Initialize the price vectors;
CallBS = zeros(N,1);
CallFFT = zeros(N,1);

if fast==1
	% Implement the FFT - fast algorithm
	U = [0:N-1];
	J = [0:N-1];
	psi = BlackScholesCF(v-(alpha+1).*i,S0,r,sigma,tau);
	psi = conj(psi);
	phi = exp(-r*tau).*psi./conj(alpha.^2 + alpha - v.^2 + i.*v.*(2*alpha+1));
	d = conj(alpha.^2 + alpha - v.^2 + i.*v.*(2*alpha+1));
	x  = conj(exp(i.*(b-s0).*v)).*phi.*w;
	e  = conj(exp(-i*2*pi/N.*(U'*J)))*x;
	CallFFT = eta.*exp(-alpha.*k)./pi .* real(e);
	for u=1:N
		CallBS(u) = BlackScholesCall(S0, K(u), r, q, sigma, tau);
	end
elseif fast==0
	% Implement the FFT - slow algorithm
	for u=1:N
		for j=1:N
			psi(j) = BSCF(v(j)-(alpha+1)*i,S0,r,sigma,tau);
			phi(j) = exp(-r*tau)*psi(j)/(alpha^2 + alpha - v(j)^2 + i*v(j)*(2*alpha+1));
%			d(j) = (alpha^2 + alpha - v(j)^2 + i*v(j)*(2*alpha+1));
			x(j) = exp(i*(b-s0)*v(j))*phi(j)*w(j);
			e(j) = exp(-i*2*pi/N*(j-1)*(u-1))*x(j);
		end
		CallFFT(u) = eta*exp(-alpha*k(u))/pi * real(sum(e));
		CallBS(u)  = BSC(S0, K(u), r, 0, sigma, tau);
	end
end
