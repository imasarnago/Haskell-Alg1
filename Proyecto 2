--EJERCICIO 1
data Carrera = Matematica | Fisica | Computación | Astronomia deriving (Eq, Show)

--b
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en " ++ "Matematica"
titulo Fisica = "Licenciatura en " ++ "Fisica"
titulo Computación = "Licenciatura en " ++ "Computación"
titulo Astronomia = "Licenciatura en " ++ "Astronomia"

--c
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Ord, Eq, Bounded, Show)

--d
cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

--EJERCICIO 3
--a
minimoElemento :: (Ord a) => [a] -> a
minimoElemento [a] = a
minimoElemento (x : xs) = min x (minimoElemento xs)

--Quizas aquí tengo que agregar la clase  Bounded y luego hacer el caso en que el argumento es la lista vacía, cuyo resultado sería maxBound

--b
minimoElemento' :: (Bounded a, Ord a) => [a] -> a
minimoElemento' [] = maxBound
minimoElemento' (x : xs) = min x (minimoElemento' xs)

--c
--Utilicé las funciones "minimoElemento" y "minimoElemento'" y me di cuenta que necesitaba agregarle "deriving show" a la definición del tipo de NotaBasica. Luego, el interprete permite evaluar sin problemas.

--minimoElemento' [Fa,La,Sol,Re,Fa]
--Re

-- EJERCICIO 4
type Ingreso = Int

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Show, Eq)

data Area = Administrativa | Enseñanza | Economica | Postgrado deriving (Show)

data Persona = Decane | Docente Cargo | NoDocente Area | Estudiante Carrera Ingreso deriving (Show)

--b
--Docente toma un elemento de tipo "cargo" y me devuelve algo del tipo "Persona", justamente porque Docente es un constructor.

--c
mismo :: Cargo -> Cargo -> Bool
mismo Titular Titular = True
mismo Asociado Asociado = True
mismo Adjunto Adjunto = True
mismo Asistente Asistente = True
mismo Auxiliar Auxiliar = True
mismo _ _ = False

cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc (Docente a : xs) c
  | mismo a c = 1 + cuantos_doc xs c
  | otherwise = cuantos_doc xs c
cuantos_doc (_ : xs) c = cuantos_doc xs c

--d
tienemismo :: Cargo -> Persona -> Bool
tienemismo Titular (Docente Titular) = True
tienemismo Asociado (Docente Asociado) = True
tienemismo Adjunto (Docente Adjunto) = True
tienemismo Asistente (Docente Asistente) = True
tienemismo Auxiliar (Docente Auxiliar) = True
tienemismo _ _ = False

cuantos_doc' :: [Persona] -> Cargo -> Int
cuantos_doc' xs c = length (filter (tienemismo c) xs)

-- EJERCICIO 5
data Alteracion = Bemol | Sostenido | Natural

data NotaMusical = Nota NotaBasica Alteracion

sonido :: NotaBasica -> Int
sonido Do = 1
sonido Re = 3
sonido Mi = 5
sonido Fa = 6
sonido Sol = 8
sonido La = 10
sonido Si = 12

--b
sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota Do Bemol) = 12
sonidoCromatico (Nota Si Sostenido) = 1
sonidoCromatico (Nota x Sostenido) = (sonido x) + 1
sonidoCromatico (Nota x Bemol) = (sonido x) -1
sonidoCromatico (Nota x Natural) = sonido (x)

-- EJERCICIO 6
--dividir :: Int -> Int -> Maybe Int
--dividir x 0 = Nothing
--dividir x y = Just (div x y)
primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x : xs) = Just x

-- EJERCICIO 7
data Cola = VaciaC | Encolada Persona Cola deriving (Show)

--a
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada p c) = Just c

encolar :: Persona -> Cola -> Cola
encolar p VaciaC = Encolada p VaciaC
encolar p (Encolada q cs) = Encolada q (encolar p cs)

--  ¡Funciona porque es una definición recursiva!

--encolar Decane (Encolada (Docente Titular) (VaciaC))     ejemplo de lo anterior

busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC d = Nothing
busca (Encolada (Docente c) cola) car
  | c == car = Just (Docente c)
  | otherwise = busca cola car
busca (Encolada p cola) car = busca cola car

-- ¡EL ANTERIOR ÚLTIMO CASO LO AGREGUÉ PORQUE SI LA LISTA NO TENIA NINGUNA PERSONA CON EL CARGO INGRESADO, HASKELL INDICABA QUE FALTABA UN CASO EN EL PATTERN MATCHING. MUY IMPORTANTE!

{-busca (Encolada (Docente Adjunto) ((Encolada Decane) VaciaC)) Adjunto
Just (Docente Adjunto)            ejemplo de lo anterior
-}

--b
--Cola se parece al tipo lista, en el sentido de que por ejemplo una lista puede ser vacia o contener a menos un elemento o muchos más. Al mismo tiempo, existe una forma de "pegar" elementos a una cola. En este caso, serían personas.

--colaej = Encolada Decane (Encolada (Docente Adjunto)(Encolada (Docente Titular) VaciaC))

{-
 busca colaej Titular
Just (Docente Titular)
 busca colaej Asistente
Nothing
 busca colaej Adjunto
Just (Docente Adjunto)
-}

-- EJERCICIO 8
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b)

--a
--type Guia = ListaAsoc String Int

--b

--1
la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo _ _ la) = 1 + la_long la

--2
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia ys = ys
la_concat xs Vacia = xs
la_concat (Nodo a b xs) ys = Nodo a b (la_concat xs ys)

--3
la_agregar :: ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar Vacia a b = (Nodo a b Vacia)
la_agregar (Nodo a b xs) c d = Nodo c d (Nodo a b xs)

--4
la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = []
la_pares (Nodo a b xs) = (a, b) : la_pares xs

--5
la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia d = Nothing
la_busca (Nodo a b xs) d
  | a == d = Just b
  | otherwise = Nothing

--6
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar d Vacia = Vacia
la_borrar d (Nodo a b xs)
  | d == a = xs
  | otherwise = Nodo a b (la_borrar d xs)

-- HASTA AQUI LLEGA EL PROYECTO 2.
