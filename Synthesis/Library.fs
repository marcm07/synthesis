module Synthesis

let abelar x = 
    if x>12 && x<3097 && x % 12 = 0 then true else false

let area b h =
    if b<0.0 || h<0.0 then 
        failwith "Negative base or height"
    else 0.5*b*h
        

let zollo x =
    if (x<0) then -1*x
    else x*2


let min x y =
    if x<y then x
    else y

let max x y =
    if x<y then y
    else x

let ofTime h m s =
    (h*60*60)+(m*60)+s

let toTime s =
    if s<0 then 0,0,0
    else
        let hours = 
            s/(60*60)
        let mins = 
            (s-(hours*60*60))/60
        hours,mins,s%60

let digits x =
    let rec digitcount x acc =
        match x<10 && -10<x with
        |true -> (acc+1)
        |false -> digitcount (x/10) (acc+1) 
    digitcount x 0

let minmax tuple = 
    let a,b,c,d = tuple
    (min (min a b) (min c d), max (max a b) (max c d))

    

let isLeap x =
    if (x<1582) then failwith "input year less than 1582" else 
        if (x % 4 = 0 && x%100 <> 0) then 
            true
        else
            if (x%400 = 0) then true else false

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"