module Synthesis

let abelar x = 
    (x>12 && x<3097 && x % 12 = 0) 

let area b h =
    match b<0.0 || h<0.0 with 
    |true -> failwith "Negative base or height"
    |false -> 0.5*b*h
        

let zollo x =
    match (x<0) with 
    |true -> -1*x
    |false -> x*2


let min x y =
    match x<y with 
    |true -> x
    |false -> y

let max x y =
    match x<y with 
    |true -> y
    |false -> x

let ofTime h m s =
    (h*60*60)+(m*60)+s

let toTime s =
    let hours = s/(60*60)
    let mins = (s-(hours*60*60))/60
    match s<=0 with 
    |true -> 0,0,0
    |false -> hours,mins,s%60

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
    match (x<1582) with
    |true -> failwith "input year less than 1582" 
    |false-> match (x % 4 = 0 && (x%100 <> 0 || x%400=0)) with
                |true -> true
                |false -> false
                   

let month x =
       match x with
       |1 -> "January", 31
       |2 -> "February", 28
       |3 -> "March",31
       |4 -> "April",30
       |5 -> 
       |6 ->
       |7 ->
       |8 ->
       |9 ->
       |10 ->
       |11 ->
       |12 ->
       |_ -> 

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"