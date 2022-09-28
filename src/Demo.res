
let rec fib = n => {
    switch n {
    | 0 | 1 => 1
    | _ => fib(n-1)+fib(n-2)
    }
}

let memofib = {
    let cache = Hashtbl.create(100)
    (n) => {
        switch Hashtbl.find_opt(cache, n) {
        | Some(x) => x
        | None => {
            let x = fib(n)
            Hashtbl.replace(cache, n, x)
            x
            }
        }
    }
}

let myfib = (myfib,n)=>{
    switch n {
    | 0 | 1 => 1
    | _ => myfib(n-1)+myfib(n-2)
    }
}

let memo  = anyFunc => {
    let cache = Hashtbl.create(100)
    let rec fix = (n) => {
        switch Hashtbl.find_opt(cache, n) {
        | Some(x) => x
        | None => {
            let x = anyFunc(fix,n)
            Hashtbl.replace(cache, n, x)
            x
            }
        }
    }
    fix
}

let memofib = memo(myfib)

Js.Console.timeStart("fib")
Js.Console.log(memofib(4000))
Js.Console.timeEnd("fib")