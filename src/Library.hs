module Library where
import PdePreludat

data Tipo =  Agua | Fuego | Planta  deriving (Show, Eq)

data Pokemon = UnPokemon {
    nombre :: String,
    numPokedex :: Number,
    tipo :: Tipo
} deriving (Show, Eq)

--Pókemones iniciales de Kanto

bulbasaur :: Pokemon
bulbasaur = UnPokemon {nombre="Bulbasaur",numPokedex = 1 ,tipo = Planta}

charmander :: Pokemon
charmander = UnPokemon {nombre="Charmander",numPokedex = 4 ,tipo = Fuego}

squirtle :: Pokemon
squirtle = UnPokemon {nombre="Squirtle",numPokedex = 7 ,tipo = Agua}

-- Pokemones iniciales de Johto

chikorita :: Pokemon
chikorita = UnPokemon {nombre="Chikorita",numPokedex = 152 ,tipo = Planta}

cyndaquil :: Pokemon
cyndaquil = UnPokemon {nombre="Cyndaquil",numPokedex = 155 ,tipo = Fuego}

totodile :: Pokemon
totodile = UnPokemon {nombre="Totodile",numPokedex = 158 ,tipo = Agua}


{----------------------------------------------------PUNTO 1------------------------------------------------}
leGanaA :: Pokemon -> Pokemon -> Bool
leGanaA pokemon1 pokemon2 
    | tipo pokemon1 == Agua && tipo pokemon2 == Fuego = True
    | tipo pokemon1 == Fuego && tipo pokemon2 == Planta = True
    | tipo pokemon1 == Planta && tipo pokemon2 == Agua = True
    | otherwise = False

{----------------------------------------------------PUNTO 2------------------------------------------------}

aQuePokemonesLeGana :: Pokemon -> [Pokemon] -> [Pokemon]
aQuePokemonesLeGana pokemon pokemones = filter (leGanaA pokemon) pokemones

{----------------------------------------------------PUNTO 3------------------------------------------------}

aCuantosPokemonesLesGana :: Pokemon -> [Pokemon] -> Number
aCuantosPokemonesLesGana pokemon pokemones= length  (aQuePokemonesLeGana pokemon pokemones)

quienEsElMasPicante :: [Pokemon] -> Pokemon
quienEsElMasPicante pokemones = foldl1 (elMejorDeDos pokemones) pokemones

elMejorDeDos :: [Pokemon] -> Pokemon -> Pokemon -> Pokemon
elMejorDeDos pokemones poke1 poke2
    | aCuantosPokemonesLesGana poke1 pokemones >= aCuantosPokemonesLesGana poke2 pokemones= poke1
    | otherwise = poke2