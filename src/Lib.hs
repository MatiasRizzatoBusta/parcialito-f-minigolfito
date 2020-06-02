module Lib where
import Text.Show.Functions

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

---------------------------- Punto 1 ----------------------------
type Palos = Int->Jugador->Tiro
putter :: Palos--los hago asi pq para hacer la lista de funciones todos tienen que tener la misma forma
putter _ jugador = UnTiro{velocidad=10,precision=calculoPrecision (*2) (habilidad jugador),altura= 0}
madera :: Palos
madera _ jugador = UnTiro{velocidad=100,precision=calculoPrecision (`div` 2) (habilidad jugador),altura= 5}
hierros :: Palos
hierros n jugador = UnTiro{velocidad=calculoVelocidad n (habilidad jugador),precision=calculoPrecision (`div` n) (habilidad jugador),altura= max 0 (n-3)}

calculoVelocidad :: Int->Habilidad->Int
calculoVelocidad n  = (*n).fuerzaJugador

calculoPrecision :: (Int->Int)->Habilidad->Int
calculoPrecision funcion = funcion.precisionJugador

palos = [putter,madera,hierros]
---------------------------- Punto 2 ----------------------------
golpe :: Palos->Int->Jugador->Tiro
golpe palo n jugador = palo n jugador
---------------------------- Punto 3 ----------------------------
type Efecto = Tiro->Obstaculo->Tiro

data Obstaculo =UnObstaculo{
    nombreObstaculo::String,
    largo :: Int
}deriving (Show,Eq)

superaObstaculo :: Tiro->Obstaculo->Tiro
superaObstaculo tiro obstaculo |loSupera tiro (nombreObstaculo obstaculo) = efecto tiro obstaculo
                               |otherwise= tiro{velocidad=0,precision=0,altura=0}

loSupera :: Tiro->String->Bool
loSupera tiro "tunel con rampita" = (precision tiro)> 90 && (altura tiro)== 0
loSupera tiro "laguna" = (velocidad tiro)>80 && estaEntreValores (altura tiro) 5 1 
loSupera tiro "hoyo" = estaEntreValores (velocidad tiro) 20 5 && (altura tiro) == 0

estaEntreValores :: Int->Int->Int->Bool
estaEntreValores caracteristica max min = caracteristica>min && caracteristica<max

efecto :: Efecto
efecto tiro (UnObstaculo "tunel con rampita" _ ) = tiro{velocidad=(velocidad tiro)*2,precision= 100,altura=0}
efecto tiro (UnObstaculo "laguna" largo ) = tiro{altura= (altura tiro) `div` largo}
efecto tiro (UnObstaculo "hoyo" _ ) = tiro{velocidad=0,precision=0,altura=0}
