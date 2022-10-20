module Core.NameFactory

/// Get a fresh variable name
let name_factory =
    let counter = ref -1

    fun () ->
        counter.Value <- counter.Value + 1
        "$" + counter.Value.ToString()
