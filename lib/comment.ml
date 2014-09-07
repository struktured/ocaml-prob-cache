type t = Token.t list
let create (tokens:Token.t list) = tokens
let dummy_comment = create ["I";"like";"cats";]
