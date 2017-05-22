% plot_polynomial_fig_2.m 2/7/2015 CJD
%
% for i=1:length(GDPZ)
%     GDPZ_P(i,:)=pchip(1:63,GDPZ(i,:),1:0.1:63);
%     LIFEZ_P(i,:)=pchip(1:63,LIFEZ(i,:),1:0.1:63);
%     DGDPZ(i,:)=fnval(fnder(pchip(1:63,GDPZ(i,:))),1:63);
%     DLIFEZ(i,:)=fnval(fnder(pchip(1:63,LIFEZ(i,:))),1:63);
% end

k=1
for m=-1:.1:1
for l=1:.1:5
    %for m=1:156
      %x1=GDPZ(m,l);
      %x2=LIFEZ(m,l);
      %first, evaluate the f vector polynomial for f(t) which is a der
      x1=l
      C=m;
      %derivative: 0=0.0114+(0.00703*x1)+(0.00102*x2)-(0.0144*x1^2);
      %integral with respect to x1
      %0=0.0114x1 + 1/2 0.00703x1x1 + 0.00102x1x2 -1/3 0.0144x1x1x1 + C
      %X2= (1/3 0.0144x1x1x1 - 1/2 0.00703x1x1 - 0.0114x1)/.00102
      X2(k)=((C+((1/3)*(0.0144*x1*x1*x1)) - ((1/2)*(0.00703*x1*x1)) - (0.0114*x1)))/.00102;
      X1(k)=x1;
      k=k+1;
      % then, calculate actual f after solving diff eq in matlab
      %[TOUT,YOUT] = ode45(ODEFUN,TSPAN,Y0)
end
figure(1);plot(X1,X2);hold on;
end
%end
