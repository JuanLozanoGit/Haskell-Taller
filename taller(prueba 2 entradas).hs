import Text.Read (readMaybe)

type Inventory = [(String, Double, Int)]

addProduct :: Inventory -> String -> Double -> Int -> Inventory
addProduct inventory name price quantity = (name, price, quantity) : inventory

updateQuantity :: Inventory -> String -> Int -> Inventory
updateQuantity inventory name newQuantity =
  map (\(n, p, q) -> if n == name then (n, p, newQuantity) else (n, p, q)) inventory

removeProduct :: Inventory -> String -> Inventory
removeProduct inventory name = filter (\(n, _, _) -> n /= name) inventory

findProduct :: Inventory -> String -> Maybe (Double, Int)
findProduct [] _ = Nothing
findProduct ((n, p, q):xs) name
    | n == name = Just (p, q)
    | otherwise = findProduct xs name

applyDiscount :: Inventory -> Double -> Inventory
applyDiscount inventory discount = [(n, p * (1 - discount / 100), q) | (n, p, q) <- inventory]

inventorySummary :: Inventory -> (Int, Double)
inventorySummary inventory = (totalQuantity, totalValue)
  where
    totalQuantity = sum [q | (_, _, q) <- inventory]
    totalValue = sum [p * fromIntegral q | (_, p, q) <- inventory]

readInput :: (Read a, Num a, Ord a) => String -> IO a
readInput label = do
    input <- getLine
    case readMaybe input of
        Just value | value >= 0 -> return value
        _ -> do
            putStrLn $ "Entrada inválida para " ++ label ++ ". Intente de nuevo."
            readInput label

main :: IO ()
main = do
    let initialInventory = [] :: Inventory
    putStrLn "Bienvenido al sistema de gestión de inventario"
    loop initialInventory

loop :: Inventory -> IO ()
loop inventory = do
    putStrLn "\nOpciones:"
    putStrLn "1. Agregar producto"
    putStrLn "2. Actualizar cantidad de producto"
    putStrLn "3. Eliminar producto"
    putStrLn "4. Consultar inventario"
    putStrLn "5. Buscar producto"
    putStrLn "6. Aplicar descuento"
    putStrLn "7. Salir"
    putStr "Seleccione una opción, recuerda primero ingresa producto(nombre), precio, cantidad "
    option <- getLine
    case option of
        "1" -> addProductAction inventory >>= loop
        "2" -> updateQuantityAction inventory >>= loop
        "3" -> removeProductAction inventory >>= loop
        "4" -> consultInventoryAction inventory >> loop inventory
        "5" -> searchProductAction inventory >> loop inventory
        "6" -> applyDiscountAction inventory >>= loop
        "7" -> putStrLn "Saliendo..."
        _   -> putStrLn "Opción no válida, intente nuevamente." >> loop inventory

addProductAction :: Inventory -> IO Inventory
addProductAction inventory = do
    putStrLn "\n--- Agregar Producto ---"
    putStr "Ingrese el nombre del producto: "
    name <- getLine
    putStr "Ingrese el precio del producto (en números): "
    price <- readInput "Precio"
    putStr "Ingrese la cantidad del producto: "
    quantity <- readInput "Cantidad"
    let newInventory = addProduct inventory name price quantity
    putStrLn $ "Producto '" ++ name ++ "' agregado exitosamente."
    return newInventory
    
updateQuantityAction :: Inventory -> IO Inventory
updateQuantityAction inventory = do
    putStrLn "\n--- Actualizar Cantidad de Producto ---"
    putStr "Ingrese el nombre del producto a actualizar: "
    name <- getLine
    case findProduct inventory name of
        Just _ -> do
            putStr "Ingrese la nueva cantidad: "
            quantity <- readInput "Cantidad"
            let newInventory = updateQuantity inventory name quantity
            putStrLn $ "Cantidad del producto '" ++ name ++ "' actualizada a " ++ show quantity
            return newInventory
        Nothing -> do
            putStrLn "Producto no encontrado en el inventario."
            return inventory

removeProductAction :: Inventory -> IO Inventory
removeProductAction inventory = do
    putStrLn "\n--- Eliminar Producto ---"
    putStr "Ingrese el nombre del producto a eliminar: "
    name <- getLine
    let newInventory = removeProduct inventory name
    putStrLn $ "Producto '" ++ name ++ "' eliminado exitosamente."
    return newInventory

consultInventoryAction :: Inventory -> IO ()
consultInventoryAction inventory = do
    putStrLn "\n--- Resumen del Inventario ---"
    let (totalQty, totalValue) = inventorySummary inventory
    putStrLn $ "Total de productos en inventario: " ++ show totalQty
    putStrLn $ "Valor total del inventario: " ++ show totalValue

searchProductAction :: Inventory -> IO ()
searchProductAction inventory = do
    putStrLn "\n--- Buscar Producto ---"
    putStr "Ingrese el nombre del producto a buscar: "
    name <- getLine
    case findProduct inventory name of
        Just (price, quantity) -> putStrLn $ "Producto: '" ++ name ++ "' encontrado. Precio: " ++ show price ++ ", Cantidad: " ++ show quantity
        Nothing -> putStrLn "Producto no encontrado."

applyDiscountAction :: Inventory -> IO Inventory
applyDiscountAction inventory = do
    putStrLn "\n--- Aplicar Descuento ---"
    putStr "Ingrese el porcentaje de descuento (ejemplo: 10 para 10%): "
    discount <- readInput "Descuento"
    let newInventory = applyDiscount inventory discount
    putStrLn $ "Descuento de " ++ show discount ++ "% aplicado a todos los productos."
    return newInventory
