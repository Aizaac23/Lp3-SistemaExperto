% glosario.pl
% Definición de términos y glosario

descripcion(muestreo_aleatorio_simple,
    "Muestreo aleatorio simple: todos los elementos tienen igual probabilidad de ser seleccionados.").
descripcion(muestreo_estratificado,
    "Muestreo estratificado: la población se divide en grupos homogéneos (estratos) y se toma muestra de cada uno.").
descripcion(muestreo_por_conglomerados,
    "Muestreo por conglomerados: se seleccionan grupos enteros al azar, útil cuando no se puede acceder a la población completa.").
descripcion(muestreo_sistematico,
    "Muestreo sistemático: se elige cada k-ésimo elemento de una lista ordenada, comenzando en un punto aleatorio.").

% Mostrar glosario
display_glosario :-
    writeln("\n📘 Glosario de técnicas de muestreo:"),
    forall(descripcion(T, D), format("- ~w: ~w~n", [T, D])).
