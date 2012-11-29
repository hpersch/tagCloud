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
svgBubbleGen w h dataset = let
                           raios = reverse (sort (extraiRaio dataset))
                           in [geracao (fromIntegral w/2) (fromIntegral h/2) raios]

--Gera as cores de forma randomica!
cores ::  IO Int
cores = randomRIO (0,255::Int)
           
-- Gera string representando um circulo em SVG. A cor do circulo esta fixa. 
-- TODO: Alterar esta funcao para mostrar um circulo de uma cor fornecida como parametro.
-- Recebe o circulo para impressao e as cores como parametro!
svgCircle :: Circle -> Color -> String
svgCircle ((x,y),raio) (r,g,b) = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d, %d, %d)\" />\n" x y raio r g b 


-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
        printf  "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;\"/>\n" w h
        

--Gera a posicao dos Circulos, analisando a distancia entre os circulos.
posicaoCirculos :: [Circle] -> Float -> Point -> [Circle] -> Int -> [Circle]
posicaoCirculos circulos t ponto [] c = []
posicaoCirculos circulos t ponto circulo2 c = if analisaDistancia circulos (head circulo2) ponto == True 
                                             then (ponto, (snd(head circulo2))) : posicaoCirculos novoCirc 0 (180, 180) (tail circulo2) (c+1)
                                             else posicaoCirculos circulos (t + 0.1) (testaPosicao ponto t) circulo2 c
                                             where novoCirc = novosCirculos c circulos ponto (snd (head circulo2))

--Cria uma nova posição a partir de um ponto e um t repassado.
testaPosicao :: Point -> Float -> Point
testaPosicao p t = let
                        x1 = (fst p) + (0.01 * t * (sin (t + 0.01)))
                        y1 = (snd p) + (0.01 * t * (cos (t + 0.01)))
                        in (x1, y1)

--Cria novos circulos com novas posições e insere novamente.
novosCirculos :: Int -> [Circle] -> Point -> Float -> [Circle]
novosCirculos c circulos ponto raio = [a | a <- (take c circulos) ++ [((ponto), raio)]]
                        
-- Caculo das Distancia dos circulos utilizado para a validação do ponto.
distanciaCirc :: Point -> Point -> Float
distanciaCirc (a1, b1) (a2, b2) = let
                                x = (a2 - a1)^2
                                y = (b2 - b1)^2
                                d = x + y
                                in sqrt d 
 
--Analisa a distancia, caso seja válida retorna TRUE senão retorna False.
analisaDistancia :: [Circle] -> Circle -> Point -> Bool
analisaDistancia circulos (p, r2) ponto = if (validaPonto ponto circulos r2) == 0 then True else False

--Verifica ponto a ponto.. Chama as funções de distancia para analisar se o ponto é válido ou não.
validaPonto :: Point -> [Circle] -> Float -> Int
validaPonto p [] raio = 0
validaPonto p listc raio = if distF >= 0.1 then 0 + (validaPonto p (tail listc) raio) else 1
                           where
                           dist = distanciaCirc p (fst (head listc))
                           distF = dist - ((snd (head listc)) + raio)
--Caso seja válido, retorna 0, caso não seja válido retorna 1.

--Extrai Raios 
extraiRaio :: [Int] -> [Float]
extraiRaio [] = []
extraiRaio dataset = let 
                a = (head dataset)
                b = (sqrt(fromIntegral a))+ 0.5 --tira a raiz e acrescenta 0.5, para os circulos bem pequenos apararecerem.
                in b : extraiRaio (tail dataset)
                                            
--Criando os circulos, gerando a string com cada um dos circulos..
imprimeCriaCirc :: [Circle] -> String
imprimeCriaCirc [] = []
imprimeCriaCirc listaCirc = svgCircle (head listaCirc) (unsafePerformIO cores, unsafePerformIO cores, unsafePerformIO cores) ++ imprimeCriaCirc (tail listaCirc)

--Cria uma lista com todos os circulos desenhados já, no centro da figura.
criaCirc :: Circle -> [Float] -> [Circle]
criaCirc circ [] = []
criaCirc circ raiosDataset = circ : criaCirc ((180, 180), (head (tail raiosDataset))) (tail raiosDataset)

--Cria todos os circulos e depois ordena a posição de cada um deles.
geracao :: Float -> Float -> [Float] -> String
geracao x y raiosDataset = let
                           cria = criaCirc ((x, y), (head raiosDataset)) raiosDataset
                           insere = head cria :  posicaoCirculos [((x,y),(head raiosDataset))] 0 (x,y) (tail cria) 1
                           in imprimeCriaCirc insere