# MOVICOVID
Es un proyecto que caracterizará el sector de la población que lamentablemente, por diferentes causas, no pudo cumplir los lineamientos de #SusanaDistancia y #QuedateEnCasa sugeridos por la SSaludMX desde el 23 de Marzo.

## Preguntas de investigación
1. ¿Qué características tiene la población que tuvieron movimientos constantes durante la jornada de #SusanaDistancia?
   - 10 de Mayo hubo pico, sábado 16 y 23.
2. ¿Qué características tiene la población que tuvo movimiento durante picos de conteo de casos
3. ¿Qué características tienen los movimientos en el tiempo? ¿Qué rutas siguen sin camio? ¿Qué rutas sufrieron cambio? ¿Se debe hacer un baseline para comparar?
4. ¿Qué características geográficas y demográficas tienen los movimientos en cuanto a su destino y origen? ¿Cuántos viajes hubo por recreación? ¿Por actividad económica? ¿De qué nivel? ¿Por transporte público? ¿Uber?
5. ¿Cuál es el 80-20 de los movimientos? ¿Y el 90-10? Los movimientos que suceden con poca frecuencia, ¿qué se puede inferir de ellos?
6. ¿Es verdad que los que tuvieron estos movimientos pertenecen al la clase trabajadora, a como lo define INEGI, y de acuerdo a los puntos de orígen y destino?
7. ¿Qué recomendaciones de política de movilidad se pueden sugerir dadas las respuestas a las preguntas anteriores? (Simulación)

## Datos disponibles sugeridos
1. Datos de movilidad de FB obtenidos por la convocatoria de "Data 4 Good"
2. DENUE para la caracterización de actividad económica de destinos y orígenes
3. ENIGH para la caracterización de ingreso y gasto de destinos y origenes.
4. Uber movement para validar que este conjunto es subconjunto de los datos de movilidad de FB
5. SEMOVI para complementar viajes hacia y desde estaciones de metro o metrobús (Tláhuac, Iztapalapa pueden ser hotspots)
6. EODH para validar si el subconjunto de MovFB pertenece al conjunto total descrito por la EODH

## Entregables y productos finales
1. Sitio web dinámico con visualizaciones y productos editoriales atractivos:
   - Identificar rutas que no tuvieron cambio
   - Población que transitó esas rutas que no cambiaron
2. Insights semanales acompañado por visualizaciones de alto impacto para que sirva de INSUMO para una nota periodística.
3. Reporte/presentación con insumos para diagnóstico ejecutivo en materia de movilidad, salud y bienestar.

## Fases y Calendario tentativo
1. Kickoff & Inducción - 8 de Junio
2. EDA MovFB + DENUE, MovFB + ENIGH, MovFB + UberMov, MovFB + SEMOVI - 8 de Junio - 26 de Junio para beta con visualizaciones para end user.
4. Modelo no supervisado para agrupación y asociación - 27 de Junio - 10 de Julio
5. Sanity check con expertos sobre la conformación de los grupos - 10 Julio - 17 Julio
5. Desarrollo de tablero para exploración de grupos - 17 Julio - 31 de Julio
5. Desarrollo de producto de datos / editorial - 1 Agosto - 14 de Agosto

## Organización
1. Frida será la líder de Proyecto
2. Brenda Jiménez será la Científica de Datos
3. Eddie Lozada será el Ingeniero de Datos
4. Las responsabilidades de Frida será establecer y validar preguntas de investigación, validación de producto y desarrollo de análisis.
5. Las responsabilidades de Brenda será las de desarrollo de análisis, validación de hipótesis, desarrollo de modelos geoestadísticos, cruce de datos, y desarrollo de modelo no supervisado para agrupación y asociación de demográficos, sociográficos, económicos y de movilidad.
4. Las responsabilidades de Eddie serán las de levantamiento de infraestructura, desarrollo de pipelines de adquisición y transformación de datos, pipelines de análisis y desarrollo de plataforma de consulta.

