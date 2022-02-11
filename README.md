# Automáta Le Pila
##### Diseño e implementación de un DSL para autómatas de pila no deterministas
###### TP Final - Análisis de Lenguajes de Programación 

---

## Instalación y uso
### Requisitos
Para poder usar el programa es necesario tener instalada la **plataforma de Haskell** junto a **Stack**. 

Para poder graficar autómatas es necesario tener instalado el 
paquete **graphviz**, en caso contrario este comando nunca se ejecutará.

### Instalación
1) Clonar el repositorio con el comando:
`git clone https://github.com/jfarizano/AutomataLePila.git`

2) Moverse a la carpeta del programa creada por el comando anterior y compilar con el comando:
`stack build`

Con esto ya tenemos el programa instalado y está listo para su uso.

### Manual de uso
Para ejecutar el programa utilizamos los comandos:
`stack run`
`stack run -- [FLAGS] [FILE]`
Donde el primero ejecutará el programa sin cargar ningún autómata ni 
recibirá flags, en este caso se podrá usar la consola pero solo se permitirá
cargar archivos o usar la ayuda hasta que se use el comando de cargar archivos y
se lea un autómata válido. A partir de este punto se puede utilizar los comandos restantes.

En la segunda opción se le puede dar un archivo de entrada directamente al programa
para que este lo lea directamente sin tener que darlo en la consola interactiva, 
además de poder darle flags para cambiar parámetros de su ejecución y del comando de graficar.

Las acciones y opciones de estas flags se pueden ver con el comando
`stack run -- -h`

Dónde se pueden ver las siguientes opciones:

- **--verbose** activa el modo verbose, haciendo que en consola se imprima más
  información relevante a la ejecución, por ejemplo en la evaluación de un autómata
  se imprimen todas las transiciones y configuraciones posibles por las que se puede
  seguir en cada paso.
- **--hs** o **--hsep** recibe un número como argumento, de esta forma se puede cambiar
  la distancia horizontal entre nodos en el gráfico resultante del comando de graficar.
  Se recomienda dar una distancia de 2 o mayor cuando el autómata contiene muchos nodos
  o uno donde tiene muchas transiciones que loopean en el mismo estado.
  **--vs** o **--vsep** análoga a la flag anterior pero de distancia vertical entre todos.
- **--dpi** recibe un número como argumento y esta será la densidad de pixeles
  de la resolución, no la resolución en sí.
- **-t**, **--tr** **o --transparentbg** si se activa esta flag cuando se grafique un
  autómata el gráfico resultante saldrá con fondo transparente.
- **--h**, **-?** o **--help** muestra la lista de opciones dadas aquí también.

Una vez dentro del loop de la consola interactiva y con un autómata válido cargado 
podemos realizar diferentes comandos, entre ellos imprimir una pantalla de ayuda,
activar o desactivar el modo verbose, cargar o recargar un archivo,
imprimir en consola el autómata cargado o graficar y exportar el mismo. La ayuda
de estos comandos se puede ver escribiendo el comando **:h** en consola.

Los comandos disponibles son los siguientes:

- **:verbose** o **:v** activa el modo verbose si estaba desactivado y viceversa.
- **:print** o **:p** imprime en consola el autómata cargado actualmente.
- **:graphic** o **:g** recibe un archivo como argumento y exporta un gráfico del autómata
  cargado a ese archivo.
- **:load** o **:l** recibe un archivo con un autómata y lo carga en memoria.
- **:reload** o **:r** recarga el último archivo cargado.
- **:help**, **:h** o **:?** muestra la lista de comandos.
- **:quit** o **:q** sale del programa.


Si no hay un autómata cargado los únicos comandos que se podran usar serán los de cargar un archivo, recargar el último archivo dado (en caso de que este haya sido inválido), mostrar la pantalla de ayuda, activar y desactivar verbose, o salir del programa.

---

## Notación
Para definir un autómata, se lo debe escribir en un archivo con extensión **.pda** donde dentro se lo define siguiendo la gramática dada anteriormente, como se puede ver en el siguiente ejemplo:

```
InputAlph = {x, y};
StackAlph = {x, y, #};
States = {s0, s1, s2, s3};
AccStates = {s0, s3};
Transitions = {(s0, λ, λ, #, s1), (s1, x, λ, x, s1), (s1, y, x, λ, s2),
(s2, y, x, λ, s2), (s2, λ, #, λ, s3)}
```


En este autómata podemos ver que en cada línea se define:
- El alfabeto de entrada compuesto por x e y.
- El alfabeto de pila compuesto por x, y el símbolo \#.
- Los estados s0, s1, s2 y s3.
- Los estados de aceptación s0 y s3.
- Las transiciones.

Para el estado inicial se utiliza el primer estado definido.

Las transiciones son tuplas compuestas por estado actual, símbolo a leer de la entrada, símbolo que se extrae de la pila, símbolo que se inserta a la pila y el estado siguiente. \\


**Nota:** El símbolo λ no es necesario incluirlo en los lenguajes de entrada y pila, ya que está incluido por defecto.
