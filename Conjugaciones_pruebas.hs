import Data.List

ret r
    | r == 1 = main
    | r == 2 = putStrLn "Gracias por utilizar nuestro diccionario, cerrando programa..."
    | otherwise = putStrLn "Opcion invalida, cerrando programa..."

esVerboInfinitivo :: String -> Bool
esVerboInfinitivo palabra = any (`isSuffixOf` palabra) ["ar", "er", "ir"]

conjugacionAutomatica :: String -> IO ()
conjugacionAutomatica verbo = do
    let raiz = take (length verbo - 2) verbo
        sufijoPresenteP
            | "ar" `isSuffixOf` verbo = "o"
            | "er" `isSuffixOf` verbo = "o"
            | "ir" `isSuffixOf` verbo = "o"
            | otherwise = ""
        sufijoPresenteT
            | "ar" `isSuffixOf` verbo = "a"
            | "er" `isSuffixOf` verbo = "e"
            | "ir" `isSuffixOf` verbo = "e"
            | otherwise = ""
        sufijoPresenteS
            | "ar" `isSuffixOf` verbo = "a"
            | "er" `isSuffixOf` verbo = "e"
            | "ir" `isSuffixOf` verbo = "i"
            | otherwise = ""
        sufijoPasadoP
            | "ar" `isSuffixOf` verbo = "é"
            | "er" `isSuffixOf` verbo = "í"
            | "ir" `isSuffixOf` verbo = "í"
            | otherwise = ""
        sufijoPasadoS
            | "ar" `isSuffixOf` verbo = "a"
            | "er" `isSuffixOf` verbo = "i"
            | "ir" `isSuffixOf` verbo = "i"
            | otherwise = ""
        sufijoPasadoT
            | "ar" `isSuffixOf` verbo = "a"
            | "er" `isSuffixOf` verbo = "ie"
            | "ir" `isSuffixOf` verbo = "ie"
            | otherwise = ""
        sufijoFuturo
            | "ar" `isSuffixOf` verbo = "a"
            | "er" `isSuffixOf` verbo = "e"
            | "ir" `isSuffixOf` verbo = "i"
            | otherwise = ""
        conjugacionPresenteYo = raiz ++ sufijoPresenteP
        conjugacionPresenteTu = raiz ++ sufijoPresenteT ++ "s"
        conjugacionPresenteEl = raiz ++ sufijoPresenteT
        conjugacionPresenteElla = raiz ++ sufijoPresenteT
        conjugacionPresenteNosotros = raiz ++ sufijoPresenteS ++ "mos"
        conjugacionPresenteUstedes = raiz ++ sufijoPresenteT ++ "n"
        conjugacionPasadoYo = raiz ++ sufijoPasadoP
        conjugacionPasadoTu = raiz ++ sufijoPasadoS ++ "ste"
        conjugacionPasadoEl = raiz ++ sufijoPasadoT ++ "ó"
        conjugacionPasadoElla = raiz ++ sufijoPasadoT ++ "ó"
        conjugacionPasadoNosotros = raiz ++ sufijoPasadoS ++ "mos"
        conjugacionPasadoUstedes = raiz ++ sufijoPasadoT ++ "ron"
        conjugacionFuturoYo = raiz ++ sufijoFuturo ++ "ré"
        conjugacionFuturoTu = raiz ++ sufijoFuturo ++ "rás"
        conjugacionFuturoEl = raiz ++ sufijoFuturo ++ "rá"
        conjugacionFuturoElla = raiz ++ sufijoFuturo ++"rá"
        conjugacionFuturoNosotros = raiz ++ sufijoFuturo ++ "remos"
        conjugacionFuturoUstedes = raiz ++ sufijoFuturo ++ "rán"
        conjugacionCompleta = verbo ++ " " ++ conjugacionPresenteYo ++ " " ++ conjugacionPresenteTu ++ " " ++ conjugacionPresenteEl ++ " " ++ conjugacionPresenteElla ++  " " ++ conjugacionPresenteNosotros ++ " " ++ conjugacionPresenteUstedes ++ " " ++ conjugacionPasadoYo ++ " " ++ conjugacionPasadoTu ++ " " ++ conjugacionPasadoEl ++ " " ++ conjugacionPasadoElla ++ " " ++ conjugacionPasadoNosotros ++ " " ++ conjugacionPasadoUstedes ++ " " ++ conjugacionFuturoYo ++ " " ++ conjugacionFuturoTu ++ " " ++ conjugacionFuturoEl ++ " " ++ conjugacionFuturoElla ++ " " ++ conjugacionFuturoNosotros ++ " " ++ conjugacionFuturoUstedes
        conjugacionCompletaImp = verbo++ "PRESENTE:\n" ++ "Yo " ++ conjugacionPresenteYo ++ " Tu " ++ conjugacionPresenteTu ++ " El " ++ conjugacionPresenteEl ++ " Ella " ++ conjugacionPresenteElla ++  " Nosotros " ++ conjugacionPresenteNosotros ++ " Ustedes " ++ conjugacionPresenteUstedes ++"PASADO:\n"++ " Yo " ++ conjugacionPasadoYo ++ " Tu " ++ conjugacionPasadoTu ++ " El " ++ conjugacionPasadoEl ++ " Ella " ++ conjugacionPasadoElla ++ " Nosotros " ++ conjugacionPasadoNosotros ++ " Ustedes " ++ conjugacionPasadoUstedes ++"FUTURO:\n"++ " Yo " ++ conjugacionFuturoYo ++ " Tu " ++ conjugacionFuturoTu ++ " El " ++ conjugacionFuturoEl ++ " Ella " ++ conjugacionFuturoElla ++ " Nosotros " ++ conjugacionFuturoNosotros ++ " Ustedes " ++ conjugacionFuturoUstedes
    putStrLn $ "LA CONJUGACION ES: \n" ++conjugacionCompletaImp
    putStrLn "Hay algo que está mal en la conjugación?"
    putStrLn "1- Si \n 2- No"
    res <- readLn :: IO Int
    if res==1
        then agregarverbo res
        else appendFile "tabla_verbos.txt" (conjugacionCompleta ++ "\n")
    putStrLn $ "La conjugación automática para el verbo '" ++ verbo ++ "' se ha guardado con éxito en el diccionario."

afirmconjugacion :: (Eq a, Num a) => a -> p -> IO ()
afirmconjugacion res palabras
    | res == 1 = do
        putStrLn "Deseas realizar una corrección? "
        putStrLn "1- Si \n 2- No"
        res2 <- readLn :: IO Int
        agregarverbo res2 
    | res == 2 = do
        putStrLn "quieres conocer otras conjugaciones? "
        putStrLn "1- Si \n 2- No"
        res1 <- readLn :: IO Int
        ret res1
    | otherwise = putStrLn "Opcion invalida, cerrando programa..."

agregarverbo n 
    | n == 1 = do
        putStrLn "Ingresa la siguiente informacion: "
        putStrLn "Verbo en infinitivo (ej. comer, jugar, vivir): "
        el1 <- getLine
        putStrLn "Por favor ingrese la conjugacion del verbo: "
        putStrLn "Conjugacion en presente: "
        putStrLn "Yo: "
        el2 <- getLine 
        putStrLn "Tú: " 
        el3 <- getLine
        putStrLn "Él: " 
        el4 <- getLine
        putStrLn "Ella: " 
        el5 <- getLine
        putStrLn "Nosotros: "
        el6 <- getLine
        putStrLn "Ustedes: " 
        el7 <- getLine
        putStrLn "Conjugacion en pasado: "
        putStrLn "Yo: "
        el8 <- getLine
        putStrLn "Tú: " 
        el9 <- getLine
        putStrLn "Él: " 
        el10 <- getLine
        putStrLn "Ella: " 
        el11 <- getLine
        putStrLn "Nosotros: "
        el12 <- getLine
        putStrLn "Ustedes: " 
        el13 <- getLine
        putStrLn "Conjugacion en futuro: "
        putStrLn "Yo: "
        el14 <- getLine
        putStrLn "Tú: " 
        el15 <- getLine
        putStrLn "Él: " 
        el16 <- getLine
        putStrLn "Ella: " 
        el17 <- getLine
        putStrLn "Nosotros: "
        el18 <- getLine
        putStrLn "Ustedes: " 
        el19 <- getLine
        let verbonos = el1 ++ " " ++ el2 ++  " " ++ el3 ++  " " ++ el4 ++  " " ++ el5 ++  " " ++ el6 ++  " " ++ el7 ++  " " ++ el8 ++  " " ++ el9 ++  " " ++ el10 ++  " " ++ el11 ++  " " ++ el12 ++  " " ++ el13 ++ " " ++  el14 ++ " " ++  el15 ++ " " ++  el16 ++  " " ++ el17 ++ " " ++  el18 ++  " " ++ el19   
        appendFile "tabla_verbos.txt" verbonos
        putStrLn "se guardo el cambio con exito"
    | n == 2 = putStrLn "cerrando programa..."
    | otherwise = putStrLn "Opcion no encontrada, cerrando programa..."

conjugaciones n palabra palabras = do
    putStrLn $ "Conjugaciones del verbo: " ++ palabra
    putStrLn "Conjugacion en presente: "
    putStrLn $ "Yo: " ++ (palabras !! (n+1))
    putStrLn $ "Tú: " ++ (palabras !! (n+2))
    putStrLn $ "Él: " ++ (palabras !! (n+3))
    putStrLn $ "Ella: "++ (palabras !! (n+4))
    putStrLn $ "Nosotros: " ++(palabras !! (n+5))
    putStrLn $ "Ustedes: " ++  (palabras !! (n+6))
    putStrLn "Conjugacion en pasado: "
    putStrLn $ "Yo: " ++  (palabras !! (n+7))
    putStrLn $ "Tú: " ++  (palabras !! (n+8))
    putStrLn $ "Él: " ++  (palabras !! (n+9))
    putStrLn $ "Ella: " ++  (palabras !! (n+10))
    putStrLn $ "Nosotros: " ++  (palabras !! (n+11))
    putStrLn $ "Ustedes: " ++ (palabras !! (n+12))
    putStrLn "Conjugacion en futuro: "
    putStrLn $ "Yo: " ++  (palabras !! (n+13))
    putStrLn $ "Tú: " ++  (palabras !! (n+14))
    putStrLn $ "Él: " ++  (palabras !! (n+15))
    putStrLn $ "Ella: " ++  (palabras !! (n+16))
    putStrLn $ "Nosotros: " ++  (palabras !! (n+17))
    putStrLn $ "Ustedes: " ++  (palabras !! (n+18))
   

coincidencia n palabra palabras
    | n >= length palabras = do 
        putStrLn "Palabra no encontrada. \n vamos a conjugar y posteriormente usted validara esta palabra "
        conjugacionAutomatica palabra 
    | palabra == (palabras !! n) = do
        putStrLn "Conjugaciones: "
        conjugaciones n palabra palabras
    | otherwise = coincidencia (n+1) palabra palabras

main :: IO ()
main = do 
    archivo <- readFile "tabla_verbos.txt"
    let lineas = lines archivo
        palabras = concatMap words lineas
    putStrLn "Bienvenido a nuestro diccionario de conjugaciones."
    putStrLn "Ingresa una palabra para buscarla en nuestro diccionario: "
    putStr "Ingresa la palabra aquí -> "
    str <- getLine
    if esVerboInfinitivo str
        then coincidencia 0 str palabras
        else putStrLn "No es un verbo en infinitivo."
