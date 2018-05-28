import Text.Show.Functions
import Data.List

type Habilidad = String
type Arma = Rehen -> Rehen
type Intimidacion = Ladron -> Rehen -> Rehen
data Ladron = Ladron { nombreLadron :: String, habilidades :: [Habilidad], armas :: [Arma] } deriving (Show)

type Plan = Ladron -> Ladron
data Rehen = Rehen { nombreRehen :: String, nivelDeComplot :: Int, nivelDeMiedo :: Int, plan :: [Plan] } deriving (Show)

pistola :: Int -> Arma
pistola calibre unRehen = (aumentarMiedo ((tresVecesLetrasNombreRehen.nombreRehen) unRehen).modificarNivelComplot (-) ((*) 5 calibre) ) unRehen

ametralladora :: Int -> Arma
ametralladora balas unRehen =(aumentarMiedo balas.modificarNivelComplot (-) ((flip div 2.nivelDeMiedo) unRehen) ) unRehen

tresVecesLetrasNombreRehen :: String -> Int
tresVecesLetrasNombreRehen = (3*).length

modificarNivelComplot :: (Int->Int->Int) -> Int -> Rehen -> Rehen
modificarNivelComplot funcion cantidad unRehen = unRehen { nivelDeComplot = (flip funcion cantidad.nivelDeComplot) unRehen }

aumentarMiedo :: Int -> Rehen -> Rehen
aumentarMiedo cantidad unRehen= unRehen { nivelDeMiedo = ((+) cantidad.nivelDeMiedo) unRehen }

disparos :: Intimicacion
disparos unLadron unRehen = (armaMiedosa unLadron unRehen) unRehen

armaMiedosa :: Ladron -> Rehen -> Arma
armaMiedosa unLadron unRehen = foldl1 (compararEfectoDosArmas unRehen) (armas unLadron)

compararEfectoDosArmas :: Rehen -> Arma -> Arma -> Arma
compararEfectoDosArmas unRehen arma1 arma2 
 | (nivelDeMiedo.arma1) unRehen > (nivelDeMiedo.arma2) unRehen = arma1
 | otherwise = arma2

hacerseElMalo :: Intimicacion
hacerseElMalo unLadron unRehen
 | ((=="Berlin").nombreLadron) unLadron = aumentarMiedo ((cantidadLetrasHabilidades.habilidades) unLadron) unRehen
 | ((=="Rio").nombreLadron) unLadron = modificarNivelComplot (+) 20 unRehen
 | otherwise = aumentarMiedo 10 unRehen

cantidadLetrasHabilidades :: [Habilidad] -> Int
cantidadLetrasHabilidades = length.concat 

atacarAlLadron :: Rehen -> Plan
atacarAlLadron companieroRehen = quitarArmas ((flip div 10.length.nombreRehen) companieroRehen )

esconderse :: Plan
esconderse unLadron = quitarArmas ((flip div 3.length.habilidades) unLadron) unLadron

quitarArmas :: Int -> Plan
quitarArmas cantidad unLadron = unLadron { armas = (drop cantidad.armas) unLadron }

-- EJERCICIO 1
tokio = Ladron "Tokio" ["trabajo psicologico","entrar en moto"] [(pistola 9),(pistola 9),(ametralladora 30)]
profesor = Ladron "Profesor" ["disfrazarse de linyera","disfrazarse de payaso","estar siempre un paso adelante"] []
pablo = Rehen "Pablo" 40 30 [esconderse]
arturito = Rehen "Arturito" 70 50 [esconderse, (atacarAlLadron pablo)] 

-- EJERCICIO 2
ladronInteligente :: Ladron -> Bool
ladronInteligente unLadron = ((>2).length.habilidades) unLadron || ((=="Profesor").nombreLadron) unLadron

-- EJERCICIO 3
conseguirArma :: Arma -> Plan
conseguirArma nuevoArma unLadron = unLadron { armas = ((:) nuevoArma.armas) unLadron }

-- EJERCICIO 4
intimidarRehen :: Intimicacion -> Ladron -> Rehen -> Rehen
intimidarRehen metodoIntimidacion unLadron unRehen = metodoIntimidacion unLadron unRehen

-- EJERCICIO 5
calmarLasAguas :: [Rehen] -> Ladron -> [Rehen]
calmarLasAguas listaRehenes unLadron = (map disparos.filter ((>60).nivelDeComplot)) listaRehenes

-- EJERCICIO 6
puedeEscaparseDeLaPolicia :: Ladron -> Bool
puedeEscaparseDeLaPolicia = elem "disfrazarse de".map (take 14).habilidades

-- EJERCICIO 7
laCosaPintaMal :: [Ladron] -> [Rehen] -> Bool
laCosaPintaMal listaLadrones listaRehenes = nivelPromedio nivelDeComplot listaRehenes > ((*) (nivelPromedio nivelDeMiedo listaRehenes) (cantidadArmasLadrones listaRehenes)) 

nivelPromedio :: (Rehen->Int) -> [Rehen] -> Float
nivelPromedio aComparar listaRehenes = div ((sum.map aComparar) listaRehenes) (length listaRehenes)

cantidadArmasLadrones :: [Ladron] -> Int
cantidadArmasLadrones = sum.map (length.armas)

-- EJERCICIO 8
seRebelan :: Ladron -> [Rehen] -> Ladron
seRebelan unLadron = ejecutarPlan unLadron.perderComplot

perderComplot :: [Rehen] -> [Rehen]
perderComplot = map (modificarNivelComplot (-) 10)

--ejecutarPlan :: no entendi la consigna
--ejecutarPlan unLadron listaRehenes =

-- EJERCICIO 9
ejecutarPlanValencia :: [Rehen] -> [Ladron] -> Int
ejecutarPlanValencia listaRehenes listaLadrones = conseguirDinero.ladronesSeArman.rehenesSeRebelan listaRehenes listaLadrones

todosSeRebelan :: [Rehen] -> [Ladron] -> [Ladron]
todosSeRebelan listaRehenes = map (\unLadron -> seRebelan unLadron listaRehenes)

ladronesSeArman :: [Ladron] -> [Ladron]
ladronesSeArman listaLadrones = map (conseguirArma (ametralladora 45)) listaLadrones

conseguirDinero :: [Ladron] -> Int
conseguirDinero = (*100000000).cantidadArmasLadrones 

-- EJERCICIO 10
--el plan valencia no se puede ejecutar si un ladron tiene infinitas armas ya que al querer hacer la multiplicacion 
--por la cantidad total de armas de los ladrones esto nunca se terminaria de ejecutar ya que haskell debe contar todas las armas y se terminaria colgando.

-- EJERCICIO 11
--el plan valencia si se puede ejecutar si un ladron tiene infinitas habilidades ya que 
--por lazy evaluation no se pone a evaluar las habilidades solo suma la cantidad de armas que es finita y si finalizaria de ejecutarse.