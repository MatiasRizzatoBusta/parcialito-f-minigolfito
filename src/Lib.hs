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

---------------------------- Punto 1 ----------------------------
type Palos = Habilidad->Tiro
putter :: Palos--los hago asi pq para hacer la lista de funciones todos tienen que tener la misma forma
putter habilidad = UnTiro{velocidad=10,precision=calculoPrecision (*2) habilidad,altura= 0}
madera :: Palos
madera habilidad= UnTiro{velocidad=100,precision=calculoPrecision (`div` 2) habilidad ,altura= 5}
hierros :: Int->Palos --hago una funcion generica que modele a todos los hierros
hierros n habilidad = UnTiro{velocidad=calculoVelocidad n habilidad ,precision=calculoPrecision (`div` n) habilidad ,altura= max 0 (n-3)}

calculoVelocidad :: Int->Habilidad->Int
calculoVelocidad n  = (*n).fuerzaJugador

calculoPrecision :: (Int->Int)->Habilidad->Int
calculoPrecision funcion = funcion.precisionJugador

palos ::[Palos]
palos = [putter,madera] ++ map hierros [1..10]--esto me da una lista con todos los hierros y un valor

---------------------------- Punto 2 ----------------------------
golpe :: Jugador->Palos->Tiro
golpe jugador palo = palo (habilidad jugador)
---------------------------- Punto 3 ----------------------------
type Efecto = Tiro->Tiro
--no uso pattern matching pq puede habe mas obstaculos en el futuro y voy a tener que cambiar toda la funcion.
--planto los obstaculos como funciones.

tiroDetenido = UnTiro 0 0 0

data Obstaculo =UnObstaculo{
    puedeSuperar ::Tiro->Bool, --Evito hardcodearlas para cada tipo de obstaculo entonces ya me vienen las que usa cada uno en el data
    efecto :: Efecto
}deriving (Show)

superaObstaculo :: Obstaculo->Tiro->Tiro
superaObstaculo obstaculo tiro |(puedeSuperar obstaculo) tiro  = (efecto obstaculo) tiro --toma a la primera funcion del obstaculo
                               |otherwise = tiroDetenido

tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita efectoRampita

laguna :: Int->Obstaculo
laguna largo = UnObstaculo superaLaguna (efectoLaguna largo)

hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo

superaTunelConRampita :: Tiro->Bool
superaTunelConRampita tiro  = (precision tiro) > 90 && (altura tiro) == 0

superaLaguna ::Tiro->Bool
superaLaguna tiro = (velocidad tiro) > 80 && between 1 5 (altura tiro) 

superaHoyo :: Tiro->Bool
superaHoyo tiro = (between 5 20.velocidad) tiro && (altura tiro) == 0 && (precision tiro) > 95

efectoRampita :: Efecto
efectoRampita tiro = tiro{velocidad=(velocidad tiro)*2,precision= 100,altura=0}

efectoLaguna :: Int->Efecto
efectoLaguna largo tiro = tiro{altura= (altura tiro) `div` largo}

efectoHoyo :: Efecto
efectoHoyo tiro = tiroDetenido
---------------------------- Punto 4 ----------------------------
palosUtiles :: Jugador->Obstaculo->[Palos]
palosUtiles jugador obstaculo  = filter (esPaloUtil jugador obstaculo) palos 

esPaloUtil :: Jugador->Obstaculo->Palos->Bool
esPaloUtil jugador obstaculo = (puedeSuperar obstaculo).(golpe jugador)

tiroPrueba = UnTiro 10 95 0

cuantosSuperaConsecutivamente :: Tiro->[Obstaculo]->Int --sol de forma recursiva
cuantosSuperaConsecutivamente tiro (obstaculo:obstaculos)  |(puedeSuperar obstaculo) tiro = cuantosSuperaConsecutivamente ((efecto obstaculo) tiro) obstaculos

paloMasUtil :: Jugador->[Obstaculo]->[Palos]->Palos
paloMasUtil jugador listaObstaculos listap = foldl1 (elMejor jugador listaObstaculos) listap

elMejor :: Jugador->[Obstaculo]->Palos->Palos->Palos
elMejor jugador listaObstaculos  p1 p2  |(cuantosSuperaConsecutivamente (golpe jugador p1) listaObstaculos) >= (cuantosSuperaConsecutivamente (golpe jugador p2) listaObstaculos)= p1
                                        |otherwise = p2
---------------------------- Punto 5 ----------------------------
tomoJugador :: (Jugador,Puntos)->Jugador
tomoJugador = fst
tomoPuntos :: (Jugador,Puntos)->Puntos
tomoPuntos = snd

pierdenLaApuesta :: [(Jugador,Puntos)]->[String]
pierdenLaApuesta puntosTorneo = (map (padre.tomoJugador).filter (gano tomoPuntos)) puntosTorneo

gano :: [(Jugador,Puntos)]->(Jugador,Puntos)->Bool --tomo la lista sin el mismo jugador
gano puntosTorneo puntosJugador =  (not.(comparoPuntos puntosJugador).(map tomoPuntos).filter (/= puntosJugador)) puntosTorneo

comparoPuntos :: (Jugador,Puntos)->[Puntos]->Bool
comparoPuntos jugador ptosDemas = all ((snd jugador) >) ptosDemas