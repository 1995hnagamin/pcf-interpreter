type 'a t = (string * 'a) list
exception Not_bound of string

let rec lookup var = function
  []          -> raise (Not_bound var)
| (k, v)::env -> if k = var then v else lookup var env

let initial_env = []

let extend k v env = (k, v)::env
