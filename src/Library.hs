module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Number,
  superficie :: Number,
  precio :: Number,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]

--PUNTO 1

--a

mayor :: (a -> Number) -> a -> a -> Bool
mayor funcion valor1 valor2 = funcion valor1 > funcion valor2

menor :: (a -> Number) -> a -> a -> Bool
menor funcion valor1 valor2 = funcion valor1 < funcion valor2

--b

ejemplo :: [String]
ejemplo = ordenarSegun (mayor length) ["Hola","Francisco","Pepito"]

--PUNTO 2

--a
ubicadoEn :: [Barrio] -> Requisito
ubicadoEn barrios depto = any (estaEnEsteBarrio depto) barrios

estaEnEsteBarrio :: Depto -> Barrio -> Bool
estaEnEsteBarrio depto barrioAAnalizar = barrio depto == barrioAAnalizar

--b

cumpleRango :: (Depto -> Number) -> Number -> Number -> Requisito
cumpleRango funcion num1 num2 depto = between num1 num2 (funcion depto)

--PUNTO 3

--a

cumpleBusqueda :: Busqueda -> Depto -> Bool
cumpleBusqueda busqueda depto = all (cumpleCaracteristicasDepto depto) busqueda

cumpleCaracteristicasDepto :: Depto -> Requisito -> Bool
cumpleCaracteristicasDepto depto requisito = requisito depto

--b
type CriterioBusqueda = Depto -> Depto -> Bool

buscar :: Busqueda -> CriterioBusqueda -> [Depto] -> [Depto]
buscar busquedas criterio = (ordenarSegun criterio).(filter (cumpleBusqueda busquedas))

--c

ejemplo2 :: [Depto]
ejemplo2 = buscar [ubicadoEn ["Recoleta","Palermo"] , cumpleRango ambientes 1 2 , cumpleRango precio 0 6000] (mayor superficie) deptosDeEjemplo

--PUNTO 4

mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas depto = (map obtenerMails).(filtrarPersonasInteresadas depto)

filtrarPersonasInteresadas :: Depto -> [Persona] -> [Persona]
filtrarPersonasInteresadas depto personas = filter (esPersonaInteresada depto) personas

esPersonaInteresada :: Depto -> Persona -> Bool
esPersonaInteresada depto persona = any (cumpleBusqueda' depto) (busquedas persona)

obtenerMails :: Persona -> Mail
obtenerMails persona = mail persona

cumpleBusqueda' :: Depto -> Busqueda -> Bool
cumpleBusqueda' depto busqueda = cumpleBusqueda busqueda depto