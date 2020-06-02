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
golpe :: Palos->Jugador->Tiro