type t = {name:string;id:int}

let create ~name ~id = {name;id}

let dummy_discussion = create ~name:"test discussion" ~id:0
