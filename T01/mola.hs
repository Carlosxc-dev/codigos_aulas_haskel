-- compressão da mola, dados força e constante
compressao :: Float -> Float -> Float
compressao f k = f/k

-- Energia potencial da mola, dados força aplicada e constante
energia :: Float -> Float -> Float
energia f k = 0.5*k*(compressao f k)^2
