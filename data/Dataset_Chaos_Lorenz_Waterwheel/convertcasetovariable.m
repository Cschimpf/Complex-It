% convertcasetovariable.m  3/5/2017 cjd
%
% convert time sampled data to groups of variables at different
% sample times
%

% Create 5 variables for 5 time samples and place them as new variables
j=1;
for i=1:5:3775;
    NUM1(j,1)=NUM(i,1);
    NUM1(j,2)=NUM(i+1,1);
    NUM1(j,3)=NUM(i+2,1);
    NUM1(j,4)=NUM(i+3,1);
    NUM1(j,5)=NUM(i+4,1);
    %
    NUM1(j,6)=NUM(i,2);
    NUM1(j,7)=NUM(i+1,2);
    NUM1(j,8)=NUM(i+2,2);
    NUM1(j,9)=NUM(i+3,2);
    NUM1(j,10)=NUM(i+4,2);
    %
    NUM1(j,11)=NUM(i,3);
    NUM1(j,12)=NUM(i+1,3);
    NUM1(j,13)=NUM(i+2,3);
    NUM1(j,14)=NUM(i+3,3);
    NUM1(j,15)=NUM(i+4,3);
    %
    j=j+1;
end