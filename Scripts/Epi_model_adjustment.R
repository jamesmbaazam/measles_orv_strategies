###Vaccination event
#Q is number of susceptibles being injected in the arm
Q <- if (t < campaign_day | t > campaign_day + orv_dur){0} 
else if(S > team_performance){team_performance} 
else{S} 
#FIX
if (S==0){
<end campaign>
}
#model equations
S1[t+1] = S[t] - B*S[t]*I[t] - Q*vax_eff
S2[t+1] = Q*(1-vax_eff) #class of individuals who have failed immunisation but will not be revaccinated
during the course of the ORV
E1[t+1] = B*S[t]*I[t] 