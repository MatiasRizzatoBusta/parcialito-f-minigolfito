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
type Palos = Int->Jugador->Tiro
putter :: Palos--los hago asi pq para hacer la lista de funciones todos tienen que tener la misma forma
putter _ jugador = UnTiro{velocidad=10,precision=calculoPrecision (*2) (habilidad jugador),altura= 0}
madera :: Palos
madera _ jugador = UnTiro{velocidad=100,precision=calculoPrecision (`div` 2) (habilidad jugador),altura= 5}
hierros :: Palos--revisar como seria lo del N
hierros n jugador = UnTiro{velocidad=calculoVelocidad n (habilidad jugador),precision=calculoPrecision (`div` n) (habilidad jugador),altura= max 0 (n-3)}

calculoVelocidad :: Int->Habilidad->Int
calculoVelocidad n  = (*n).fuerzaJugador

calculoPrecision :: (Int->Int)->Habilidad->Int
calculoPrecision funcion = funcion.precisionJugador

palos = [putter,madera,hierros]
---------------------------- Punto 2 ----------------------------
golpe :: Jugador->Int->Palos->Tiro
golpe jugador n palo = palo n jugador 
---------------------------- Punto 3 ----------------------------
type Efecto = Tiro->Obstaculo->Tiro

data Obstaculo =UnObstaculo{
    nombreObstaculo::String,
    largo :: Int
}deriving (Show,Eq)

hoyo = UnObstaculo "hoyo" 0
tunel = UnObstaculo "tunel con rampita" 0
laguna = UnObstaculo "laguna" 4


superaObstaculo :: Tiro->Obstaculo->Tiro
superaObstaculo tiro obstaculo  |loSupera (nombreObstaculo obstaculo) tiro  = efecto tiro obstaculo
                                |otherwise= tiro{velocidad=0,precision=0,altura=0}

loSupera :: String->Tiro->Bool
loSupera "tunel con rampita" tiro  = (precision tiro)> 90 && (altura tiro)== 0
loSupera "laguna" tiro  = (velocidad tiro)>80 && estaEntreValores (altura tiro) 5 1 
loSupera "hoyo" tiro  = estaEntreValores (velocidad tiro) 20 5 && (altura tiro) == 0

estaEntreValores :: Int->Int->Int->Bool
estaEntreValores caracteristica max min = caracteristica>min && caracteristica<max

efecto :: Efecto
efecto tiro (UnObstaculo "tunel con rampita" _ ) = tiro{velocidad=(velocidad tiro)*2,precision= 100,altura=0}
efecto tiro (UnObstaculo "laguna" largo ) = tiro{altura= (altura tiro) `div` largo}
efecto tiro (UnObstaculo "hoyo" _ ) = tiro{velocidad=0,precision=0,altura=0}
---------------------------- Punto 4 ----------------------------
palosUtiles :: Obstaculo->Jugador->[Palos]
palosUtiles obstaculo jugador = filter (esPaloUtil jugador obstaculo) palos --anda pero como son funciones no los muestra

esPaloUtil :: Jugador->Obstaculo->Palos->Bool
esPaloUtil jugador obstaculo = (loSupera (nombreObstaculo obstaculo)).(golpe jugador 5)--revisar que hacer con N

campo = [tunel,tunel,hoyo]
tiroPrueba = UnTiro 10 95 0

cuantosSuperaConsecutivamente :: Tiro->[Obstaculo]->Int
cuantosSuperaConsecutivamente tiro listaObstaculos  = (length.(armoNuevaLista tiro)) listaObstaculos

armoNuevaLista :: Tiro->[Obstaculo]->[Obstaculo]
armoNuevaLista tiro listaObstaculos = takeWhile ((==False).(criterioTiro tiro)) listaObstaculos

criterioTiro :: Tiro->Obstaculo->Bool
criterioTiro tiro = esTiroNulo.(superaObstaculo tiro)

esTiroNulo :: Tiro->Bool
esTiroNulo (UnTiro velocidad precision altura) = velocidad == 0 && precision == 0 && altura == 0

paloMasUtil :: Jugador->[Obstaculo]->[Palos]->Palos
paloMasUtil jugador listaObstaculos listap = foldl1 (elMejor jugador listaObstaculos) listap

elMejor :: Jugador->[Obstaculo]->Palos->Palos->Palos
elMejor jugador listaObstaculos  p1 p2  |(cuantosSuperaConsecutivamente (golpe jugador 0 p1) listaObstaculos) >= (cuantosSuperaConsecutivamente (golpe jugador 0 p2) listaObstaculos)= p1
                                        |otherwise = p2


