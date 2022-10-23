module Core.NameFactory

/// Get a fresh variable name
let name_factory =
    let counter = ref -1

    fun () ->
        counter.Value <- counter.Value + 1
        "$" + counter.Value.ToString()

let fresh_var_alphabetic_generator () =   
    let fresh_var_alphabetic =
        let mutable counter = -1
        
        fun () ->
            counter <- counter + 1
            let rec loop n =
                if n < 26 then
                    string (char (n + 97))
                else
                    loop (n / 26) + string (char (n % 26 + 97))
            loop counter
    fresh_var_alphabetic
