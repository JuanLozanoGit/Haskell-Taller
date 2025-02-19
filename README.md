import Text.Read (readMaybe)
-- Importa la función readMaybe para convertir cadenas en números de manera segura.


type Inventory = [(String, Double, Int)]
-- Define un tipo llamado Inventory como una lista de tuplas.
-- Cada tupla contiene:
-- * Un String (nombre del producto)
-- * Un Double (precio del producto)
-- * Un Int (cantidad disponible del producto)


addProduct :: Inventory -> String -> Double -> Int -> Inventory
-- Función para agregar un producto al inventario.
-- Recibe el inventario actual, el nombre del producto, el precio y la cantidad.
-- Retorna un nuevo inventario con el producto agregado.
addProduct inventory name price quantity = (name, price, quantity) : inventory


updateQuantity :: Inventory -> String -> Int -> Inventory
-- Función para actualizar la cantidad de un producto en el inventario.
-- Recorre la lista y si encuentra el producto, actualiza su cantidad.
updateQuantity inventory name newQuantity =
  map (\(n, p, q) -> if n == name then (n, p, newQuantity) else (n, p, q)) inventory


removeProduct :: Inventory -> String -> Inventory
-- Función para eliminar un producto del inventario.
-- Filtra la lista eliminando el producto que coincide con el nombre dado.
removeProduct inventory name = filter (\(n, _, _) -> n /= name) inventory


findProduct :: Inventory -> String -> Maybe (Double, Int)
-- Función para buscar un producto en el inventario.
-- Si lo encuentra, devuelve Just (precio, cantidad), si no, devuelve Nothing.
findProduct [] _ = Nothing
findProduct ((n, p, q):xs) name
    | n == name = Just (p, q)
    | otherwise = findProduct xs name


applyDiscount :: Inventory -> Double -> Inventory
-- Función para aplicar un descuento a todos los productos.
-- Recibe el inventario y el porcentaje de descuento.
-- Retorna un nuevo inventario con los precios reducidos según el porcentaje.
applyDiscount inventory discount = [(n, p * (1 - discount / 100), q) | (n, p, q) <- inventory]


inventorySummary :: Inventory -> IO ()
-- Función para mostrar el resumen del inventario.
-- Imprime todos los productos con su nombre, precio y cantidad.
inventorySummary inventory = do
    putStrLn "\n--- Inventario Actual ---"
    if null inventory then putStrLn "El inventario está vacío."
    else mapM_ printProduct inventory
  where
    printProduct (n, p, q) = putStrLn $ "Producto: " ++ n ++ ", Precio: " ++ show p ++ ", Cantidad: " ++ show q


readInput :: (Read a, Num a, Ord a) => String -> IO a
-- Función para leer un valor de entrada y validar que sea un número válido.
-- Si la entrada no es válida, vuelve a pedir el dato hasta que sea correcto.
readInput label = do
    putStr (label ++ ": ")
    input <- getLine
    case readMaybe input of
        Just value | value >= 0 -> return value
        _ -> do
            putStrLn $ "Entrada inválida para " ++ label ++ ". Intente de nuevo."
            readInput label


main :: IO ()
-- Función principal que inicia el programa y el bucle de opciones.
main = do
    let initialInventory = [] :: Inventory
    putStrLn "Bienvenido al sistema de gestión de inventario"
    loop initialInventory


loop :: Inventory -> IO ()
-- Función que maneja el menú de opciones y permite la interacción con el usuario.
loop inventory = do
    putStrLn "\nOpciones:"
    putStrLn "1. Agregar producto"
    putStrLn "2. Actualizar cantidad de producto"
    putStrLn "3. Eliminar producto"
    putStrLn "4. Consultar inventario"
    putStrLn "5. Buscar producto"
    putStrLn "6. Aplicar descuento"
    putStrLn "7. Salir"
    putStr "Seleccione una opción: "
    option <- getLine
    case option of
        "1" -> addProductAction inventory >>= loop
        "2" -> updateQuantityAction inventory >>= loop
        "3" -> removeProductAction inventory >>= loop
        "4" -> inventorySummary inventory >> loop inventory
        "5" -> searchProductAction inventory >> loop inventory
        "6" -> applyDiscountAction inventory >>= loop
        "7" -> putStrLn "Saliendo..."
        _   -> putStrLn "Opción no válida, intente nuevamente." >> loop inventory


addProductAction :: Inventory -> IO Inventory
-- Función para agregar un producto solicitando los datos al usuario.
addProductAction inventory = do
    putStrLn "\n--- Agregar Producto ---"
    putStr "Ingrese el nombre del producto: "
    name <- getLine
    price <- readInput "Ingrese el precio"
    quantity <- readInput "Ingrese la cantidad"
    let newInventory = addProduct inventory name price quantity
    putStrLn $ "\nProducto '" ++ name ++ "' agregado exitosamente."
    return newInventory
    

updateQuantityAction :: Inventory -> IO Inventory
-- Función para actualizar la cantidad de un producto existente.
updateQuantityAction inventory = do
    putStrLn "\n--- Actualizar Cantidad de Producto ---"
    putStr "Ingrese el nombre del producto: "
    name <- getLine
    case findProduct inventory name of
        Just _ -> do
            quantity <- readInput "Ingrese la nueva cantidad"
            let newInventory = updateQuantity inventory name quantity
            putStrLn $ "Cantidad de '" ++ name ++ "' actualizada."
            return newInventory
        Nothing -> putStrLn "Producto no encontrado." >> return inventory


removeProductAction :: Inventory -> IO Inventory
-- Función para eliminar un producto del inventario.
removeProductAction inventory = do
    putStrLn "\n--- Eliminar Producto ---"
    putStr "Ingrese el nombre del producto: "
    name <- getLine
    let newInventory = removeProduct inventory name
    putStrLn $ "Producto '" ++ name ++ "' eliminado."
    return newInventory


searchProductAction :: Inventory -> IO ()
-- Función para buscar un producto en el inventario y mostrar su información.
searchProductAction inventory = do
    putStrLn "\n--- Buscar Producto ---"
    putStr "Ingrese el nombre del producto: "
    name <- getLine
    case findProduct inventory name of
        Just (price, quantity) -> putStrLn $ "Producto: '" ++ name ++ "' - Precio: " ++ show price ++ ", Cantidad: " ++ show quantity
        Nothing -> putStrLn "Producto no encontrado."


applyDiscountAction :: Inventory -> IO Inventory
-- Función para aplicar un descuento a todos los productos y mostrar los precios antes y después.
applyDiscountAction inventory = do
    putStrLn "\n--- Aplicar Descuento ---"
    discount <- readInput "Ingrese el porcentaje de descuento"
    let newInventory = applyDiscount inventory discount
    putStrLn "\n--- Resumen de Descuentos ---"
    mapM_ (printDiscount discount) (zip inventory newInventory)
    return newInventory
  where
    printDiscount d ((n, oldP, q), (_, newP, _)) =
        putStrLn $ "Producto: " ++ n ++ ", Precio antes: " ++ show oldP ++ ", Precio después de " ++ show d ++ "%: " ++ show newP

