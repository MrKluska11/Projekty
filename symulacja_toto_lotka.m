function [ilosc_trafien] = trafienie_6_w_toto_lotka
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here

T = 100000;  %gramy 100 tysiêcy razy w du¿ego lotka

%losowanie maszyny (wylosowanie 6 liczb z zakresu od 1 do 49)

for j = 1:6
    a(j) = ceil(unifrnd(0.5,49.49));
end

    if  a(1) == a(2) ||a(1) == a(3) || a(1) == a(4) || a(1) == a(5) || a(1) == a(6)
        a(1) = ceil(unifrnd(0,49));
    else a(1) = a(1);
    end
    if  a(2) == a(3) ||a(2) == a(4) || a(2) == a(5) || a(2) == a(6) || a(2) == a(1)
        a(2) = ceil(unifrnd(0,49));
    else a(2) = a(2);
    end
       if a(3) == a(1) ||a(3) == a(2) || a(3) == a(4) || a(3) == a(5) || a(3) == a(6)
        a(3) = ceil(unifrnd(0,49));
    else a(3) = a(3);
        end
        if  a(4) == a(1) ||a(4) == a(2) || a(4) == a(3) || a(4) == a(5) || a(4) == a(6)
        a(4) = ceil(unifrnd(0,49));
    else a(4) = a(4);
        end
        if  a(5) == a(1) ||a(5) == a(2) || a(5) == a(3) || a(5) == a(4) || a(5) == a(6)
        a(5) = ceil(unifrnd(0,49));
    else a(5) = a(5);
        end
        if  a(6) == a(1) ||a(6) == a(2) || a(6) == a(3) || a(6) == a(4) || a(6) == a(5)
        a(6) = ceil(unifrnd(0,49));
    else a(6) = a(6);
        end
        
        if  a(1) == a(2) ||a(1) == a(3) || a(1) == a(4) || a(1) == a(5) || a(1) == a(6)
        a(1) = ceil(unifrnd(0,49));
    else a(1) = a(1);
    end
    if  a(2) == a(3) ||a(2) == a(4) || a(2) == a(5) || a(2) == a(6) || a(2) == a(1)
        a(2) = ceil(unifrnd(0,49));
    else a(2) = a(2);
    end
       if a(3) == a(1) ||a(3) == a(2) || a(3) == a(4) || a(3) == a(5) || a(3) == a(6)
        a(3) = ceil(unifrnd(0,49));
    else a(3) = a(3);
        end
        if  a(4) == a(1) ||a(4) == a(2) || a(4) == a(3) || a(4) == a(5) || a(4) == a(6)
        a(4) = ceil(unifrnd(0,49));
    else a(4) = a(4);
        end
        if  a(5) == a(1) ||a(5) == a(2) || a(5) == a(3) || a(5) == a(4) || a(5) == a(6)
        a(5) = ceil(unifrnd(0,49));
    else a(5) = a(5);
        end
        if  a(6) == a(1) ||a(6) == a(2) || a(6) == a(3) || a(6) == a(4) || a(6) == a(5)
        a(6) = ceil(unifrnd(0,49));
    else a(6) = a(6);
        end
        
%nasze obstawione liczby (wybrano losowo 6 liczb z zakresu od 1 do 49)
for i = 1:T
for k = 1:6
    A(i,k) = ceil(unifrnd(0,49));
end

    if  A(i,1) == A(i,2) ||A(i,1) == A(i,3) || A(i,1) == A(i,4) || A(i,1) == A(i,5) || A(i,1) == A(i,6)
        A(i,1) = ceil(unifrnd(0,49));
    else A(i,1) = A(i,1);
    end
    if  A(i,2) == A(i,3) ||A(i,2) == A(i,4) || A(i,2) == A(i,5) || A(i,2) == A(i,6) || A(i,2) == A(i,1)
        A(i,2) = ceil(unifrnd(0,49));
    else A(i,2) = A(i,2);
    end
       if A(i,3) == A(i,1) ||A(i,3) == A(i,2) || A(i,3) == A(i,4) || A(i,3) == A(i,5) || A(i,3) == A(i,6)
        A(i,3) = ceil(unifrnd(0,49));
    else A(i,3) = A(i,3);
        end
        if  A(i,4) == A(i,1) ||A(i,4) == A(i,2) || A(i,4) == A(i,3) || A(i,4) == A(i,5) || A(i,4) == A(i,6)
        A(i,4) = ceil(unifrnd(0,49));
    else A(i,4) = A(i,4);
        end
        if  A(i,5) == A(i,1) ||A(i,5) == A(i,2) || A(i,5) == A(i,3) || A(i,5) == A(i,4) || A(i,5) == A(i,6)
        A(i,5) = ceil(unifrnd(0,49));
    else A(i,5) = A(i,5);
        end
        if  A(i,6) == A(i,1) ||A(i,6) == A(i,2) || A(i,6) == A(i,3) || A(i,6) == A(i,4) || A(i,6) == A(i,5)
        A(i,6) = ceil(unifrnd(0,49));
    else A(i,6) = A(i,6);
        end
        
        if  A(i,1) == A(i,2) ||A(i,1) == A(i,3) || A(i,1) == A(i,4) || A(i,1) == A(i,5) || A(i,1) == A(i,6)
        A(i,1) = ceil(unifrnd(0,49));
    else A(i,1) = A(i,1);
    end
    if  A(i,2) == A(i,3) ||A(i,2) == A(i,4) || A(i,2) == A(i,5) || A(i,2) == A(i,6) || A(i,2) == A(i,1)
        A(i,2) = ceil(unifrnd(0,49));
    else A(i,2) = A(i,2);
    end
       if A(i,3) == A(i,1) ||A(i,3) == A(i,2) || A(i,3) == A(i,4) || A(i,3) == A(i,5) || A(i,3) == A(i,6)
        A(i,3) = ceil(unifrnd(0,49));
    else A(i,3) = A(i,3);
        end
        if  A(i,4) == A(i,1) ||A(i,4) == A(i,2) || A(i,4) == A(i,3) || A(i,4) == A(i,5) || A(i,4) == A(i,6)
        A(i,4) = ceil(unifrnd(0,49));
    else A(i,4) = A(i,4);
        end
        if  A(i,5) == A(i,1) ||A(i,5) == A(i,2) || A(i,5) == A(i,3) || A(i,5) == A(i,4) || A(i,5) == A(i,6)
        A(i,5) = ceil(unifrnd(0,49));
    else A(i,5) = A(i,5);
        end
        if  A(i,6) == A(i,1) ||A(i,6) == A(i,2) || A(i,6) == A(i,3) || A(i,6) == A(i,4) || A(i,6) == A(i,5)
        A(i,6) = ceil(unifrnd(0,49));
    else A(i,6) = A(i,6);
        end


        ilosc_trafien = 0;
        
       if A(i,1) == a(1) || A(i,1) == a(2) || A(i,1) == a(3) || A(i,1) == a(4) || A(i,1) == a(5) || A(i,1) == a(6) &&  (A(i,2) == a(1) || A(i,2) == a(2) || A(i,2) == a(3) || A(i,2) == a(4) || A(i,2) == a(5) || A(i,2) == a(6)) &&  (A(i,3) == a(1) || A(i,3) == a(2) || A(i,3) == a(3) || A(i,3) == a(4) || A(i,3) == a(5) || A(i,3) == a(6)) &&  (A(i,4) == a(1) || A(i,4) == a(2) || A(i,4) == a(3) || A(i,4) == a(4) || A(i,4) == a(5) || A(i,4) == a(6)) &&  (A(i,5) == a(1) || A(i,5) == a(2) || A(i,5) == a(3) || A(i,5) == a(4) || A(i,5) == a(5) || A(i,5) == a(6)) &&  (A(i,6) == a(1) || A(i,6) == a(2) || A(i,6) == a(3) || A(i,6) == a(4) || A(i,6) == a(5) || A(i,6) == a(6))
           
           ilosc_trafien = ilosc_trafien + 1;
           
       else 
           ilosc_trafien = ilosc_trafien;
       end
end
end
           




