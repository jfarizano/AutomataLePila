# Automáta Le Pila
##### Diseño e implementación de un DSL para autómatas de pila no deterministas
###### TP Final - Análisis de Lenguajes de Programación 

\section{Instalación y uso}
\subsection{Requisitos}
Para poder usar el programa es necesario tener instalada la plataforma de Haskell
junto a Stack. 

Para poder graficar autómatas es necesario tener instalado el 
paquete \emph{graphviz}, en caso contrario este comando nunca se ejecutará.

\subsection{Instalación}
\begin{enumerate}
  \item Clonar el repositorio con el comando:
    \begin{verbatim}
      git clone https://github.com/jfarizano/AutomataLePila.git
    \end{verbatim}
  \item Moverse a la carpeta del programa creada por el comando anterior y compilar con el comando:
    \begin{verbatim}
      stack build
    \end{verbatim}
\end{enumerate}
Con esto ya tenemos el programa instalado y está listo para su uso.

\subsection{Manual de uso}
Para ejecutar el programa utilizamos los comandos:
\begin{verbatim}
  stack run
  stack run -- [FLAGS] [FILE]
\end{verbatim}
Donde el primero ejecutará el programa sin cargar ningún autómata ni 
recibirá flags, en este caso se podrá usar la consola pero solo se permitirá
cargar archivos o usar la ayuda hasta que se use el comando de cargar archivos y
se lea un autómata válido. A partir de este punto se puede utilizar los comandos restantes.

En la segunda opción se le puede dar un archivo de entrada directamente al programa
para que este lo lea directamente sin tener que darlo en la consola interactiva, 
además de poder darle flags para cambiar parámetros de su ejecución y del comando de graficar.

Las acciones y opciones de estas flags se pueden ver con el comando
\begin{verbatim}
  stack run -- -h
\end{verbatim}

Dónde se pueden ver las siguientes opciones:

\begin{itemize}
  \item \textbf{--verbose} activa el modo verbose, haciendo que en consola se imprima más
  información relevante a la ejecución, por ejemplo en la evaluación de un autómata
  se imprimen todas las transiciones y configuraciones posibles por las que se puede
  seguir en cada paso.
  \item \textbf{--hs} o \textbf{--hsep} recibe un número como argumento, de esta forma se puede cambiar
  la distancia horizontal entre nodos en el gráfico resultante del comando de graficar.
  Se recomienda dar una distancia de 2 o mayor cuando el autómata contiene muchos nodos
  o uno donde tiene muchas transiciones que loopean en el mismo estado.
  \item \textbf{--vs} o \textbf{--vsep} análoga a la flag anterior pero de distancia vertical entre todos.
  \item \textbf{--dpi} recibe un número como argumento y esta será la densidad de pixeles
  de la resolución, no la resolución en sí.
  \item \textbf{-t}, \textbf{--tr} o \textbf{--transparentbg} si se activa esta flag cuando se grafique un
  autómata el gráfico resultante saldrá con fondo transparente.
  \item \textbf{--h}, \textbf{-?} o \textbf{--help} muestra la lista de opciones dadas aquí también.
\end{itemize}

Una vez dentro del loop de la consola interactiva y con un autómata válido cargado 
podemos realizar diferentes comandos, entre ellos imprimir una pantalla de ayuda,
activar o desactivar el modo verbose, cargar o recargar un archivo,
imprimir en consola el autómata cargado o graficar y exportar el mismo. La ayuda
de estos comandos se puede ver escribiendo el comando '\textbf{:h}' en consola.

Los comandos disponibles son los siguientes:

\begin{itemize}
  \item \textbf{:verbose} o \textbf{:v} activa el modo verbose si estaba desactivado y viceversa.
  \item \textbf{:print} o \textbf{:p} imprime en consola el autómata cargado actualmente.
  \item \textbf{:graphic} o \textbf{:g} recibe un archivo como argumento y exporta un gráfico del autómata
  cargado a ese archivo.
  \item \textbf{:load} o \textbf{:l} recibe un archivo con un autómata y lo carga en memoria.
  \item \textbf{:reload} o \textbf{:r} recarga el último archivo cargado.
  \item \textbf{:help}, \textbf{:h} o \textbf{:?} muestra la lista de comandos.
  \item \textbf{:quit} o \textbf{:q} sale del programa.
\end{itemize}

Si no hay un autómata cargado los únicos comandos que se podran usar serán los de
cargar un archivo, recargar el último archivo dado (en caso de que este haya sido inválido),
mostrar la pantalla de ayuda, activar y desactivar verbose, o salir del programa.

\subsection{Notación}
Para definir un autómata, se lo debe escribir en un archivo con extensión \textbf{.pda}
donde dentro se lo define siguiendo la gramática dada anteriormente, como
se puede ver en el siguiente ejemplo:

\begin{lstlisting}
InputAlph = {x, y};
StackAlph = {x, y, #};
States = {s0, s1, s2, s3};
AccStates = {s0, s3};
Transitions = {(s0, $\lambda$, $\lambda$, #, s1), (s1, x, $\lambda$, x, s1), (s1, y, x, $\lambda$, s2),
(s2, y, x, $\lambda$, s2), (s2, $\lambda$, #, $\lambda$, s3)}
\end{lstlisting}


En este autómata podemos ver que en cada línea se define:
\begin{itemize}
  \item El alfabeto de entrada compuesto por x e y.
  \item El alfabeto de pila compuesto por x, y el símbolo \#.
  \item Los estados s0, s1, s2 y s3.
  \item Los estados de aceptación s0 y s3.
  \item Las transiciones.
\end{itemize}

Para el estado inicial se utiliza el primer estado definido.

Las transiciones son tuplas compuestas por estado actual, símbolo a leer de la entrada,
símbolo que se extrae de la pila, símbolo que se inserta a la pila y el estado siguiente. \\


\textbf{Nota:} El símbolo $\lambda$ no es necesario incluirlo en los lenguajes de entrada y pila,
ya que está incluido por defecto.
