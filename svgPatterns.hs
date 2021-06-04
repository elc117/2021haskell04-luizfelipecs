import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)
type Triangle  = (Point,Point,Point)


-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paleta (R, G, B) só com tons de verde "hard-coded" 
-- (pode ser melhorado substituindo os valores literais por parâmetros)
-- Além disso, o que acontecerá se n for muito grande ou negativo?
-- Vai sair do intervalo do cores de rgb 0 - 255 se valor > 255 assumirá o valor 255 se valor < 0  assumirá o valor 0

greenPalette :: Int -> [(Int,Int,Int)]
greenPalette n = [(0, 80+i*10, 0) | i <- [0..n] ]

-- Paleta com n valores retirados de uma lista com sequências de R, G e B 
-- O '$' é uma facilidade sintática que substitui parênteses
-- O cycle é uma função bacana -- procure saber mais sobre ela :-)

rgbPaletteRect :: Int -> [(Int,Int,Int)]
rgbPaletteRect n = take n $ cycle [(50,0,0),(0,255,0),(0,0,255)]

rgbPaletteCircle :: Int -> [(Int,Int,Int)]
rgbPaletteCircle n = take n $ cycle [(155,0,0)]

rgbPaletteTriangle :: Int -> [(Int,Int,Int)]
rgbPaletteTriangle n = take n $ cycle [(100,0,0),(255,255,255),(0,255,255)]

-------------------------------------------------------------------------------
-- Geração de retângulos
-------------------------------------------------------------------------------
genRectsInLine :: Int -> [Rect]
genRectsInLine n  = [((m*(w+gap), 0.0), w, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (200,300)
        gap = 10


-------------------------------------------------------------------------------
-- Geracao de circulos
-------------------------------------------------------------------------------
genCircle :: Int -> [Circle]
genCircle n  = [((gapx,(gapy-r*2) - m*(r*2+gap*2)), r) | m <- [0..fromIntegral (n-1)]]
  where r = 8
        gap = 20
        gapx = 100
        gapy = 300


-------------------------------------------------------------------------------
-- Geracao de triangulos
-------------------------------------------------------------------------------
genTriangle :: Int -> [Triangle]
genTriangle n = [((m*(gap/(m+1)),0.0),(gap*2-m*(gap/(m+1)),0.0),(gap,(gap+gap/2)-m*(gap/(m+1)))) | m <- [0..fromIntegral (n-1)]]
  where gap = 100

genTriangleLeft :: Int -> [Triangle]
genTriangleLeft n = [((gap*7,gap),(gap*7,gap*3),(gap*10,gap*2)) | m <- [0..fromIntegral (n-1)]]
  where gap = 10

genTriangleRight :: Int -> [Triangle]
genTriangleRight n = [((gap*13,gap),(gap*13,gap*3),(gap*10,gap*2)) | m <- [0..fromIntegral (n-1)]]
  where gap = 10


-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style

-- Gera string representando circulo SVG
svgCircle :: Circle -> String -> String 
svgCircle ((x,y),r) style = 
  printf "<circle cx='%.3f' cy='%.3f' r='%.3f' fill='%s' stroke='black' stroke-width='3'/>\n" x y r style

-- Gera string representando triângulo SVG
svgTriangle :: Triangle -> String -> String
svgTriangle ((x1,y1),(x2,y2),(x3,y3)) style =
  printf "<polygon points = '%.3f,%.3f %.3f,%.3f %.3f,%.3f' style='%s' />\n" x1 y1 x2 y2 x3 y3 style


-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
svgStyleRect :: (Int,Int,Int) -> String
svgStyleRect (r,g,b) = printf "fill:rgb(%d,%d,%d);" r g b

svgStyleCircle :: (Int,Int,Int) -> String
svgStyleCircle (r,g,b) = printf "rgb(%d,%d,%d)" r g b

svgStyleTriangle :: (Int,Int,Int) -> String
svgStyleTriangle (r,g,b) = printf "fill:rgb(%d,%d,%d); stroke-width:2; stroke:rgb(0,0,0);" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos/triangulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
  writeFile "figs.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgfigs2 ++ svgfigs3 ++ svgfigs4 ++ svgfigs5 ++ svgEnd
        
        svgfigs = svgElements svgRect rects (map svgStyleRect paletteRect) 
        rects = genRectsInLine nrects
        paletteRect = rgbPaletteRect nrects
        nrects = 1

        svgfigs2 = svgElements svgCircle circle (map svgStyleCircle paletteCircle)
        circle = genCircle ncircle
        paletteCircle = rgbPaletteCircle ncircle
        ncircle = 3


        svgfigs3 = svgElements svgTriangle triangle (map svgStyleTriangle paletteTriangle)
        triangle = genTriangle ntriangle
        paletteTriangle = rgbPaletteTriangle ntriangle
        ntriangle = 2

        svgfigs4 = svgElements svgTriangle triangle2 (map svgStyleTriangle paletteTriangle2)
        triangle2 = genTriangleLeft ntriangle2
        paletteTriangle2 = rgbPaletteTriangle ntriangle2
        ntriangle2 = 1

        svgfigs5 = svgElements svgTriangle triangle3 (map svgStyleTriangle paletteTriangle3)
        triangle3 = genTriangleRight ntriangle2
        paletteTriangle3 = rgbPaletteTriangle ntriangle3
        ntriangle3 = 1
        
        (w,h) = (200,300) -- width,height da imagem SVG



