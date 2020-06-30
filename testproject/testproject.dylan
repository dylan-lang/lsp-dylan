Module: testproject
Synopsis: 
Author: 
Copyright: 

define function zeor (q) 
format-out("Z:%=\n", q);
end;

define function another(k)
  zeor(k);
  zeor("and more");
end; // testproject:testproject:zeor

define function main
    (name :: <string>, arguments :: <vector>)
  zeor("Hello, world!\n");
  another(name);
  exit-application(0);
end function main;

main(application-name(), application-arguments());
