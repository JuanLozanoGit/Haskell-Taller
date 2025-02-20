# Sistema de Gestión de Inventario en Haskell

## Descripción
Este es un programa en Haskell que permite gestionar un inventario de productos. Permite agregar productos, actualizar cantidades, eliminar productos, consultar el inventario, buscar productos y aplicar descuentos. Se ejecuta en la terminal y usa una estructura de datos basada en listas para almacenar los productos.

## Funcionalidades
1. **Agregar Producto**: Permite añadir un nuevo producto con nombre, precio y cantidad.
2. **Actualizar Cantidad**: Modifica la cantidad de un producto existente en el inventario.
3. **Eliminar Producto**: Remueve un producto del inventario.
4. **Consultar Inventario**: Muestra todos los productos en el inventario.
5. **Buscar Producto**: Permite consultar un producto específico por su nombre.
6. **Aplicar Descuento**: Reduce el precio de todos los productos en un porcentaje especificado.
7. **Salir**: Termina la ejecución del programa.

## Requisitos
- GHC (Glasgow Haskell Compiler) instalado en el sistema.
- Conocimientos básicos de Haskell para modificar y personalizar el código.

## Ejecución
1. Guarda el código en un archivo con extensión `.hs`, por ejemplo: `inventario.hs`.
2. Compila el archivo usando GHC:
   ```bash
   ghc -o inventario inventario.hs
   ```
3. Ejecuta el programa:
   ```bash
   ./inventario
   ```

## Uso
Al ejecutar el programa, se presentará un menú con opciones numeradas. Ingresa el número de la opción deseada y sigue las instrucciones en pantalla.

Ejemplo:
```
Bienvenido al sistema de gestión de inventario

Opciones:
1. Agregar producto
2. Actualizar cantidad de producto
3. Eliminar producto
4. Consultar inventario
5. Buscar producto
6. Aplicar descuento
7. Salir
Seleccione una opción:
```

## Estructura del Código
- **`Inventory`**: Representa el inventario como una lista de tuplas `(nombre, precio, cantidad)`.
- **`addProduct`**: Agrega un producto al inventario.
- **`updateQuantity`**: Modifica la cantidad de un producto.
- **`removeProduct`**: Elimina un producto del inventario.
- **`findProduct`**: Busca un producto por nombre y devuelve su precio y cantidad.
- **`applyDiscount`**: Aplica un descuento a todos los productos del inventario.
- **`inventorySummary`**: Muestra todos los productos en el inventario.
- **Funciones de interacción (`addProductAction`, `updateQuantityAction`, etc.)**: Gestionan la entrada del usuario y actualizan el inventario.
- **`main`**: Punto de entrada del programa, maneja el bucle de ejecución.


## Integrantes del grupo

-**Juan Lozano**
-**Andres**
-**Julio Guarnizo**
-**Maria Parra**

