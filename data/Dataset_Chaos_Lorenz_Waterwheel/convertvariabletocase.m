% convertvariabletocase.m  3/5/2017 cjd
%
% convert groups of variables at different
% sample times back into cases
%

% Restore 5 cases for each group of 5 time samples and place them as new
% cases

j=1;
for i=1:755;
    % remake the time index
    NUM1(j,1)=j;
    NUM1(j+1,1)=j+1;
    NUM1(j+2,1)=j+2;
    NUM1(j+3,1)=j+3;
    NUM1(j+4,1)=j+4;
    %
    NUM1(j,2)=NUM(i,2);
    NUM1(j+1,2)=NUM(i,3);
    NUM1(j+2,2)=NUM(i,4);
    NUM1(j+3,2)=NUM(i,5);
    NUM1(j+4,2)=NUM(i,6);
    %
    NUM1(j,3)=NUM(i,7);
    NUM1(j+1,3)=NUM(i,8);
    NUM1(j+2,3)=NUM(i,9);
    NUM1(j+3,3)=NUM(i,10);
    NUM1(j+4,3)=NUM(i,11);
    %
    NUM1(j,4)=NUM(i,12);
    NUM1(j+1,4)=NUM(i,13);
    NUM1(j+2,4)=NUM(i,14);
    NUM1(j+3,4)=NUM(i,15);
    NUM1(j+4,4)=NUM(i,16);
    %
    % cluster numbers
    NUM1(j,5)=NUM(i,17);
    NUM1(j+1,5)=NUM(i,17);
    NUM1(j+2,5)=NUM(i,17);
    NUM1(j+3,5)=NUM(i,17);
    NUM1(j+4,5)=NUM(i,17);
    %
    j=j+5;
end