% calculo_muestra.pl
% Cálculo de tamaño de muestra para poblaciones finitas e infinitas

% Muestra infinita: Z = crítico, P = proporción, E = margen
tamano_muestra_infinita(Z, P, E, N) :-
    Raw is (Z*Z * P * (1-P)) / (E*E),
    N is round(Raw).

% Muestra finita: Pobl = tamaño población
tamano_muestra_finita(Z, P, E, Pobl, N) :-
    tamano_muestra_infinita(Z, P, E, N0),
    Adj is N0 / (1 + ((N0 - 1) / Pobl)),
    N is round(Adj).

% Interfaz de cálculo interactivo
calcular_muestra :-
    writeln("\n📊 Cálculo de tamaño de muestra"),
    write("Valor crítico Z (ej: 1.96): "), flush_output, read(Z),
    write("Proporción esperada P (ej: 0.5): "), flush_output, read(P),
    write("Margen de error E (ej: 0.05): "), flush_output, read(E),
    write("Tamaño de población (0 para infinita): "), flush_output, read(Pobl),
    ( Pobl =:= 0 ->
        tamano_muestra_infinita(Z, P, E, N),
        format("→ Tamaño de muestra (infinita): ~w~n", [N])
    ;
        tamano_muestra_finita(Z, P, E, Pobl, N),
        format("→ Tamaño de muestra (finita): ~w~n", [N])
    ).