data Chocobo = Chocobo {
    fuerza::Int,
    peso::Int,
    velocidad::Int
} deriving(Show,Eq)

amarillo :: Chocobo
amarillo = Chocobo 10 5 20 
negro :: Chocobo
negro = Chocobo 15 10 15
blanco :: Chocobo
blanco = Chocobo 25 15 25
rojo :: Chocobo
rojo = Chocobo 18 12 30

data Jinete = Jinete {
    nombre::String,
    chocobo::Chocobo
} deriving(Show,Eq)

--tramo = [(distancia,correccionDeVelocidad)]
--type Tramo = [(Int,Int)]
type Pista = [Tramo]

data Tramo = Tramo {
    distancia::Int,
    correccionDeVelocidad::Chocobo -> Int
} deriving(Show,Eq)

--type Velocidad = Chocobo -> Int

mayorSegun :: Ord a => (t -> a) -> t -> t -> a
mayorSegun f valor1 valor2 = max (f valor1) (f valor2)

menorSegun :: Ord a => (t -> a) -> t -> t -> a
menorSegun f valor1 valor2 = min (f valor1) (f valor2)

tiempo::Tramo -> Chocobo -> Int
tiempo tramo chocobo = div (distancia tramo) (correccionDeVelocidad tramo chocobo)

tiempoTotal::Pista-> Chocobo ->Int
tiempoTotal pista chocobo = sum.map (tiempo chocobo) pista

--podio::Pista -> [Jinete] -> [Jinete]
--podio pista jinetes =  filter (tiempoTotal pista) chocobo jinetes

