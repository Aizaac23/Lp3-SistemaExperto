% recomendaciones.pl
% Recomendaciones generales para cada técnica

recomendacion(muestreo_aleatorio_simple,
    "Es útil cuando tienes un buen marco muestral y recursos suficientes.").
recomendacion(muestreo_estratificado,
    "Adecuado para poblaciones heterogéneas, permite mejorar la precisión.").
recomendacion(muestreo_por_conglomerados,
    "Práctico cuando no hay acceso completo a una lista de población, pero se tiene acceso a grupos.").
recomendacion(muestreo_sistematico,
    "Rápido y eficiente cuando los datos están organizados y se necesita una selección ágil.").

% ... tus hechos recomendacion/2 ...

% Mostrar todas las recomendaciones
mostrar_recomendaciones :-
    writeln("\n📄 Recomendaciones generales:"),
    forall(recomendacion(Tipo, Texto),
        format("- ~w: ~w~n", [Tipo, Texto])
    ).
