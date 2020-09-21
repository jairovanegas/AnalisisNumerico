print("Inicia el algoritmo de brent")
a=-1
b=2

print("con intervalos: A: ",a,"B: ",b)
fa=pow(a,3)-2*pow(a,2)+((4*a)/3) - 8/27
fb=pow(b,3)-2*pow(b,2)+((4*b)/3) - 8/27

if fa*fb>= 0:
  print("Sale de la funcion por que la raiz no esa en el intervalo")
  
if abs(fa)<abs(fb):
  temporal = a
  a=b
  b=temporal

c=a
tolerancia=0.001
fs=0
fc=pow(c,3)-2*pow(c,2)+((4*c)/3) - 8/27


if(fa != fc) and (fb!=fc):
  s=((a*fb*fc)/((fa-fb)*(fa-fc)))+((b*fa*fc)/((fb-fa)*(fb-fc)))+((c*fa*fb)/((fc-fa)*(fc-fb)))
  print("s",s)
else:
  s=b-fb*((b-a)/(fb-fa))
  
d=c
c=b

if(fa*fs<=0):
    b=s
else:
   a=s
    
if abs(fa)<abs(fb):
  temporal = a
  a=b
  b=temporal

i=1

while fa ==0 or fs==0 or i<=15:
  fa=pow(a,3)-2*pow(a,2)+((4*a)/3) - 8/27
  fb=pow(b,3)-2*pow(b,2)+((4*b)/3) - 8/27 
  fc=pow(c,3)-2*pow(c,2)+((4*c)/3) - 8/27

  if(fa != fc) and (fb!=fc):
    s=((a*fb*fc)/((fa-fb)*(fa-fc)))+((b*fa*fc)/((fb-fa)*(fb-fc)))+((c*fa*fb)/((fc-fa)*(fc-fb)))
    
  else:
    s=b-fb*((b-a)/(fb-fa))
    
    
  
  if  ((((3*a+b)/4) >=s and s>=b)
   or ((abs(s-b)>=((abs(b-c)/2)))
   or ((abs(s-b)>=((abs(c-d)/2))) 
   or ((abs(b-c) >=abs(tolerancia))) 
   or ((abs(c-d)>=abs(tolerancia)))))):
    s=(a+b)/2
   
   
  fs=pow(s,3)-2*pow(s,2)+((4*s)/3) - 8/27
  d=c
  c=b

  if(fa*fs<=0):
    b=s
  else:
    a=s
    
  if abs(fa)<abs(fb):
    temporal = a
    a=b
    b=temporal
  print("Raiz: ",s)
  i=i+1
  
 