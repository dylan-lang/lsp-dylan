Module: testproject
Synopsis: 
Author: 
Copyright: 

define function zeor (q) 
format-out("Z:%=", q);
end;

define function main
    (name :: <string>, arguments :: <vector>)
  zeor("Hello, world!\n");
  exit-application(0);
end function main;

main(application-name(), application-arguments());
