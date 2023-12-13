A=[];

k=0;
S=zeros(length(A),1);
L=zeros(length(A),1);
C=zeros(length(A),1);
Ne=zeros(length(A),1);
E=zeros(length(A),1);
e=0;
q=1;
W=zeros(size(A));
Z=zeros(size(A));
s=length(A);
U=[];

for h=1:aa
   
    
    for i=1:length(A)
        if A(:,i)==0
            A(:,i)=-1;
        end
    end
    
        A(1,:)=[];
        A(:,1)=[];
        U(1)=[];
        X=sum(A);
      
    T=zeros(length(X),1);
    for j=length(A):-1:1
        X=sum(A);
        T(j)=X(j)./U(j);  %para definir umbral en función del porcentaje de presas que le quedan
        if (T(j)>=0)&&(T(j)<=0.8)
          e=e+1;
          W(j,h)=j;
         A(:,j)=[];
         A(j,:)=[];
         U(j)=[];
        end 
%   
    end
   
    for t=length(A):-1:1
          if A(:,t)==0
              Z(t,h)=t;
              e=e+1;
              A(t,:)=[];
              A(:,t)=[];
              U(t)=[];
          end
    end
      
    p=0;
    [M,N]=size(A);
  for m=1:M
      for n=1:N
                  if A(m,n)>0
                  p=p+1;
                  end
      end
  end 
  for z=1:length(A)
        if A(:,z)==-1
            A(:,z)=0;
        end
  end

  
  W=A;

%  v=genvarname(['W',num2str(h)]);
%  save(['v_',num2str(h),'.mat']);

   B=length(A); 
   q=q+1;
   E(q-1)=e;
   k=k+1;
   S(k)=B;
   L(k)=p;
   C(k)=(2*L(k))/(S(k)*(S(k)-1));
   Ne(k)=s-B;
   
end
H=[Ne S L C E];
  %figure (1), plot(Ne,S,'-*r'), grid on
  %figure (2), plot(Ne(1:aa),C(1:aa),'-*g'), grid on
%  figure (3), plot(Ne(1:aa),L(1:aa),'-*b'), grid on  
