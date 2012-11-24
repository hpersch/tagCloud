{--
        Esqueleto de programa para geraÃ§Ã£o de bubble cloud em Haskell.
        Mais informaÃ§Ãµes em: http://www.inf.ufsm.br/~andrea/elc117-2012b
--}


module Main where

import Text.Printf -- Oba, Haskell tem printf! :-)
import Data.List
import System.Random
import System.IO.Unsafe

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Circle    = (Point,Float)

imageWidth :: Int
imageWidth = 360

imageHeight :: Int
imageHeight = 360


-- Funcao principal que faz leitura do dataset e gera arquivo SVG
main :: IO ()
main = do 
        strcontent <- readFile infile
        let pairs = map (span (/= ' ')) (lines strcontent)
            freqs = readInts (map snd pairs)
        writeFile outfile (svgCloudGen imageWidth imageHeight freqs)
        putStrLn "Ok!"
        where 
                infile = "dataset.txt"
                outfile = "tagcloud.svg"

--Frequencia e raios


-- Transforma lista de strings em lista de inteiros
readInts :: [String] -> [Int]
readInts ss = map read ss


-- Gera o documento SVG da tag cloud, concatenando cabecalho, conteudo e rodape
svgCloudGen :: Int -> Int -> [Int] -> String
svgCloudGen w h dataset = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
        (svgViewBox w h) ++
        (concat (svgBubbleGen w h dataset)) ++ "</svg>\n"

-- Esta funcao deve gerar a lista de circulos em formato SVG.
-- A implementacao atual eh apenas um teste que gera um circulo posicionado no meio da figura.
-- TODO: Alterar essa funcao para usar os dados do dataset.
svgBubbleGen:: Int -> Int -> [Int] -> [String]
svgBubbleGen w h dataset = [svgCircle ((fromIntegral w/2, fromIntegral h/2), (head (map (fromIntegral)(dataset))/50)) (cores (head dataset))]

--corolação
cores :: Int -> Color
cores raio = let
             r = unsafePerformIO (getStdRandom (randomR (0,255::Int)))
             g = unsafePerformIO (getStdRandom (randomR (0,255::Int)))
             b = unsafePerformIO (getStdRandom (randomR (0,255::Int)))
             in (r,g,b)

-- Gera string representando um circulo em SVG. A cor do circulo esta fixa. 
-- TODO: Alterar esta funcao para mostrar um circulo de uma cor fornecida como parametro.
svgCircle :: Circle -> Color -> String
svgCircle ((x,y),r) (r1,g,b) = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d, %d, %d)\" />\n" x y r r1 g b


-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
        printf  "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;\"/>\n" w h
        

--Gera lista de circulos
geraCirculos :: Int -> Int -> [Int] -> [Circle]
geraCirculos w h dataset = [((180,180), 50)]

--Gera a posicao dos Circulos
posicaoCirculos :: [Circle] -> Float -> Float -> Float -> Point -> [Circle]
posicaoCirculos circulo a t raio centro = if (testaposicao == True) then circulo
                                          else (posicaoCirculos circulo a (t + 45)) raio centro

testaposicao :: Circle -> Float -> Float -> Point
testaposicao circ a t = let
                        x = a * t * (cos t)
                        y = a * t * (sin (t))
                        in (x, y)
                        
--Distancia dos circulos
distanciaCirc :: Point -> Point -> Float
distanciaCirc (a1, b1) (a2, b2) = let
                                x = (a2 - a1) ^ 2
                                y = (b2 - b1) ^ 2
                                d = x + y
                                in sqrt d 
--Analisa a distancia
analisaDistancia :: Circle -> Circle -> Bool
analisaDistancia c1 c2 = if (distanciaCirc (fst c1) (fst c2)) > 0.1 then True else False

--Verifica ponto a ponto
validaPonto :: [Circle] -> Circle -> [Bool]
validaPonto [] _ = []
validaPonto listc circ = analisaDistancia (head listc) circ : (validaPonto (tail listc) circ)


--Extrai Raios 
extraiRaio :: [Int] -> [Float]
extraiRaio [] = []
extraiRaio dataset = let 
                a = (head dataset)
                b = ((fromIntegral a)/100) + 3
                in b : extraiRaio (tail dataset)
                                            
