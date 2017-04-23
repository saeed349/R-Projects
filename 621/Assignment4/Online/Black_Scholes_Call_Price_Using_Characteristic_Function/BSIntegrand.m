function y = BSIntegrand(phi,S,K,r,q,v,T,num)

% Returns the integrands for P1 and P2 for the Black Scholes model for the
% characteristic function approach for obtaining the call price

if num==2
	% Integrand for P2
	Integrand = exp(-i*phi*log(K))*BlackScholesCF(phi,S,r,q,v,T) / (i*phi);
elseif num==1
	% Integrand for P1
	Integrand = exp(-i*phi*log(K))*BlackScholesCF(phi-i,S,r,q,v,T) ...
		      / (i*phi*BlackScholesCF(-i,S,r,q,v,T));
end

y = real(Integrand);
