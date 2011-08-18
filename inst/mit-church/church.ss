#!r6rs

(library (church)
         
         (export church
                 scored-value->value 
                 ;scored-value->score
                 register-primitive-procedure!
                 register-primitive-erp!
                 register-query!)
         
         (import (church church)))