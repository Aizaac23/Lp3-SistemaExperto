% base_conocimiento.pl
% Hechos dinámicos y reglas de recomendación

:- dynamic tiene_marco_muestral/0.
:- dynamic poblacion_homogenea/0.
:- dynamic poblacion_heterogenea/0.
:- dynamic poblacion_amplia/0.
:- dynamic lista_ordenada/0.
:- dynamic busca_rapidez/0.

% Reglas para sugerir la técnica de muestreo
recomendar_muestreo(muestreo_aleatorio_simple) :-
    tiene_marco_muestral, poblacion_homogenea.
recomendar_muestreo(muestreo_estratificado) :-
    tiene_marco_muestral, poblacion_heterogenea.
recomendar_muestreo(muestreo_por_conglomerados) :-
    \+ tiene_marco_muestral, poblacion_amplia.
recomendar_muestreo(muestreo_sistematico) :-
    tiene_marco_muestral, lista_ordenada, busca_rapidez.

% Explicaciones asociadas
explicacion(muestreo_aleatorio_simple,
    "Se recomienda muestreo aleatorio simple porque se cuenta con marco muestral y la población es homogénea.").
explicacion(muestreo_estratificado,
    "Se recomienda muestreo estratificado porque hay marco muestral y la población es heterogénea.").
explicacion(muestreo_por_conglomerados,
    "Se recomienda muestreo por conglomerados porque no se cuenta con marco muestral y la población es amplia.").
explicacion(muestreo_sistematico,
    "Se recomienda muestreo sistemático porque hay una lista ordenada y se busca rapidez.").

% Reset y verificación de inconsistencias
enreset_datos :-
    retractall(tiene_marco_muestral),
    retractall(poblacion_homogenea),
    retractall(poblacion_heterogenea),
    retractall(poblacion_amplia),
    retractall(lista_ordenada),
    retractall(busca_rapidez).

everificar_inconsistencias :-
    (poblacion_homogenea, \+ tiene_marco_muestral ->
        writeln("⚠️ Población homogénea pero sin marco muestral. Considera revisar acceso o usar muestreo por conveniencia.")
    ; true),
    (\+ lista_ordenada, busca_rapidez ->
        writeln("⚠️ Buscas rapidez pero no tienes lista ordenada. Esto limita técnicas sistemáticas.")
    ; true).
