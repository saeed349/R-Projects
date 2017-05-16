import pandas as pd
import numpy as np
from scipy.stats import norm
import datetime
from scipy import interpolate

def find_vol(target_value, call_put, S, K, T, r):
    MAX_ITERATIONS = 100
    PRECISION = 1.0e-5

    sigma = 0.5
    for i in range(0, MAX_ITERATIONS):
        price = bs_price(call_put, S, K, T, r, sigma)
        vega = bs_vega(call_put, S, K, T, r, sigma)
        price = price
        diff = target_value - price  # our root
        if (abs(diff) < PRECISION):
            return sigma
        sigma = sigma + diff/vega # f(x) / f'(x)

    return sigma

n = norm.pdf
N = norm.cdf

def bs_price(cp_flag,S,K,T,r,v,q=0.0):
    d1 = (np.log(S/K)+(r+v*v/2.)*T)/(v*np.sqrt(T))
    d2 = d1-v*np.sqrt(T)
    if cp_flag == 'c':
        price = S*np.exp(-q*T)*N(d1)-K*np.exp(-r*T)*N(d2)
    else:
        price = K*np.exp(-r*T)*N(-d2)-S*np.exp(-q*T)*N(-d1)
    return price

def bs_vega(cp_flag,S,K,T,r,v,q=0.0):
    d1 = (np.log(S/K)+(r+v*v/2.)*T)/(v*np.sqrt(T))
    return S * np.sqrt(T)*n(d1)

def Local_Vol_Pricing(S0,option_df,r):
    iv=[]
    for index, row in option_df.iterrows():
        try:
            iv.append(find_vol(row.Price, 'c', S0, row.K, row.t, r))
        except ValueError:
            iv.append(0)        
    
    option_df['Imp_Vol'] = pd.Series(iv, index=option_df.index)
    option_df = option_df[np.isfinite(option_df['Imp_Vol'])]
    
    dupiare_iv=[]
    for index, row in option_df.iterrows():
        dupiare_iv.append(Dupiare_One(row.K,row.t,row.Imp_Vol))
    option_df['Dupiare1_IV']=dupiare_iv
    
    dupiare_price=[]
    for index,row in option_df.iterrows():
        dupiare_price.append(bs_price(cp_flag='c',K=row.K,r=r,S=S0,v=row.Dupiare1_IV,T=row.t))
    option_df['Dupiar_Price']=np.round(dupiare_price,2)
    
    final_option_df=option_df
    final_option_df.columns=['Expiry','Time to maturity', 'Strike price', 'Option market price', 'Implied Volatility', \
    'Local Volatility', 'Dupire-Price']
    
    final_option_df.to_csv("New_DATA_volatility.csv")
    return final_option_df


########## IMPLEMENTATION ##########
option_df=pd.read_csv('BloomBerg_Option_Data_Question_C.csv')

option_df.columns=['Expiry','t','K','Price']
option_df['t']=option_df['t']/365

data_frame=Local_Vol_Pricing(S0=156.1,r=1.182/100,option_df=option_df)
