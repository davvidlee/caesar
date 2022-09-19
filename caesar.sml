fun caesar (0, plainString) = plainString
    | caesar (n, plainString) = 
    let
        fun funPairs default [] _ = default
            | funPairs default ((u,v)::uvs) x =
                if u = x then
                    v
                else
                    funPairs default uvs x;

        fun sanitize [] = []
            | sanitize (x::xs) = if (Char.isAlpha x) orelse (Char.isSpace x) then
                    (Char.toUpper x)::(sanitize xs)
            else
                sanitize xs;

        fun myFilter f [] = []
            | myFilter f (x::xs) = if (f x) = true then
                x::(myFilter f xs)
            else
                myFilter f xs;

        fun cycle n [] = []
            | cycle 0 l = l
            | cycle n l = 
                let
                    fun aux n [] = []
                        | aux 0 l = l
                        | aux n (x::xs) = aux ((n mod length (x::xs)) - 1) (xs@[x])
                in
                    aux n l
                end;

        fun myZip [] [] = []
            | myZip [] (y::ys) = []
            | myZip (x::xs) [] = []
            | myZip (x::xs) (y::ys) = (x,y)::(myZip xs ys);        
        
        val alphabet = explode "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
        val cipherAlph = cycle n alphabet
        val plainList = sanitize(explode plainString)
        fun cipherList [] = []
            | cipherList (x::xs) = (funPairs #"?" (myZip alphabet cipherAlph) x)::(cipherList xs)
    in
        implode (cipherList plainList)
    end;