module Synthesis

open System.Diagnostics.Tracing
open System.Xml.Linq
open System.Xml.Linq

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
                   

let month  = function
       |1 -> "January", 31
       |2 -> "February", 28
       |3 -> "March",31
       |4 -> "April",30
       |5 -> "May", 31
       |6 -> "June", 30
       |7 -> "July", 31
       |8 -> "August", 31
       |9 -> "September", 30
       |10 -> "October", 31
       |11 -> "November", 30
       |12 -> "December", 31
       |_ -> failwith "Month out of bounds"

let toBinary x = 
    match x < 0 with
    |true -> failwith "Not implemented"
    |false -> 
        let rec binary x stringbuild =
            match x=0 || x=1 with
            |true -> string(x) + stringbuild
            |false -> binary (x/2) (string(x%2) + stringbuild)  
        binary x ""

let bizFuzz n =
    let rec loop n count (x,y,z) =
        match n>=count with
        |true -> (x,y,z)
        |false -> 
            match count%3=0 && count%5=0 with
            |true -> loop n (count+1) (x,y,z+1)
            |false -> 
                match count%3=0 with
                |true -> loop n (count+1) (x+1,y,z)
                |false -> 
                    match count%5 = 0 with 
                    |true -> loop n (count + 1) (x,y+1,z)
                    |false -> loop n (count+1) (x,y,z)
    loop n 1 (0,0,0)


let monthDay d y =
    match y < 1582 || d > 366 || d=0 || (d=366 && isLeap y = false) with
    |true -> failwith "year or days out of bounds or is not a leap year"
    |false ->  
        let rec findmonth d sum current_month =
            let a,p = month(current_month)
            match isLeap y = true && current_month = 2 with
            |true -> 
                let sum = sum + p + 1
                match d>sum with
                |true -> findmonth d sum  (current_month+1)
                |false -> a
            |false -> 
                let sum = sum + p 
                match d>sum with
                |true -> findmonth d sum  (current_month+1)
                |false -> a
        findmonth d 0 1
 
                    

let coord _ =
    failwith "Not implemented"