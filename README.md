---
title: Laboratorio de Funcional
author: Ignacio Ramírez, Manu Joan Saharrea, Luca Oliva
---
### Consigna del laboratorio
https://tinyurl.com/funcional-2024-famaf

---
# Experiencia

Esta es la primera vez que hacemos un proyecto de este tamaño con un lenguaje funcional, por lo que al principio hubo un poco de confusión a la hora de encarar el proyecto. Por suerte fue de mucha ayuda hacerlo en grupos, ya que pudimos ir aportando cada uno una parte del código a la vez que nos explicábamos la funcionalidad de cada cosa y el cómo encarábamos para resolver ciertos problemas.

Mezclar el uso de un TAD hecho por nosotros con funciones de una librería gráfica (Gloss) nos mostró ciertos desafíos, pero nos introdujo de manera más simple a como utilizar interfaces gráficas. Además, poder ver nuestros resultados en una imagen en vez de una simple línea de texto resulto ser más gratificante.

---
# Preguntas

### 1. ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.

Las funcionalidades están separadas en módulos para facilitar la lectura del programa. Tener todo escrito en un mismo archivo dificultaría trabajar sobre el programa. Además, separarlas por funcionalidad ayuda a interpretar que hace cada función sin tener que leerla en detalle.

##### Dibujos.hs
En este módulo está el tipo de dato de Dibujo y sus funciones asociadas. El resto de nuestro código funciona en torno a este tipo de dato y utiliza las funciones implementadas acá para interactuar con este.

##### Pred.hs
En este módulo están las funciones de predicado aplicadas sobre el tipo Dibujo. Estas funciones nos permiten obtener valores bool a partir de un tipo Dibujo utilizando predicados/condiciones, lo cual puede ser útil a la hora de hacer dibujos.

##### Interp.hs
En este módulo están las funciones que toman los valores dentro de un tipo Dibujo y los "traduce" para que Gloss pueda interpretar lo que queremos que dibuje.

### 2. ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?

Usar un parámetro de tipo permite cierta flexibilidad para agregar las figuras que uno necesita en cada dibujo. Esto pone más responsabilidad en la persona que hace los dibujos, pero le da la libertad de poder crear sus propias figuras básicas y adaptarlas según necesite. En caso de que se repita el uso de ciertas figuras en distintos proyectos, el usuario podría crear una "librería" que importaría cuando la necesite (además de poder compartirla con otras personas).

### 3. ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?

Si hiciéramos pattern-matching en el `fold` tendríamos que tomar en cuenta demasiados casos, ya que un dibujo puede recibir una cantidad ilimitada de transformaciones. Usar recursividad ayuda a mantener la funcionalidad de `fold` a su vez que tener una definición corta. 

### 4. ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?

Los predicados definidos en Pred.hs están pensados para ser usados a la hora de hacer figuras, no tienen otra funcionalidad aparte de esta. Los que utilizamos en los tests ya venían definidos en HUnit y ya sabemos que su funcionalidad está bien implementada. Gracias a esto, los podemos usar para comprobar que nuestro condigo este bien implementado, utilizándolos para comparar los resultados de nuestras funciones con los resultados que esperamos.