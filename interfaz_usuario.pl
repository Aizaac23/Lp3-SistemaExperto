% interfaz_usuario.pl
% Menú y gestión de interacción con el usuario

:- set_prolog_flag(encoding, utf8).
:- set_prolog_flag(file_encoding, utf8).

% Punto de arranque
inicio :-
    writeln("🎯 Bienvenido al sistema experto de técnicas de muestreo"),
    loop_menu.

% Bucle del menú principal
loop_menu :-
    repeat,
    nl,
    writeln("📋 Menú principal"),
    writeln("1. Ingresar características de la población"),
    writeln("2. Mostrar datos ingresados"),
    writeln("3. Ver técnica de muestreo recomendada"),
    writeln("4. Calcular tamaño de muestra"),
    writeln("5. Mostrar glosario"),
    writeln("6. Ver recomendaciones generales"),
    writeln("7. Limpiar datos ingresados"),
    writeln("8. Salir"),
    write("Seleccione una opción (1-8): "), flush_output,
    read(Op),
    opcion_menu(Op),
    Op == 8, !.

% Opciones del menú
opcion_menu(1) :- ingresar_datos.
opcion_menu(2) :- mostrar_datos.
opcion_menu(3) :-
    (   catch(recomendar_tecnica, _, fail)
    ->  true
    ;   writeln('⚠️  No se pudo determinar una técnica adecuada. Revisa los datos ingresados.')
    ).
opcion_menu(4) :- calcular_muestra.
opcion_menu(5) :- mostrar_glosario.
opcion_menu(6) :- mostrar_recomendaciones.
opcion_menu(7) :- reset_datos, writeln("✅ Datos limpiados.").
opcion_menu(8) :- writeln("👋 Saliendo. ¡Hasta luego!").
opcion_menu(_) :- writeln("❌ Opción inválida. Intenta de nuevo.").

% Ingreso de datos
ingresar_datos :-
    writeln("Por favor, responda solo 'si' o 'no' a las siguientes preguntas:"),
    reset_datos,
    preguntar("¿Cuenta con marco muestral? (si/no):", tiene_marco_muestral),
    preguntar("¿La población es homogénea? (si/no):", poblacion_homogenea),
    preguntar("¿La población es heterogénea? (si/no):", poblacion_heterogenea),
    preguntar("¿La población es amplia? (si/no):", poblacion_amplia),
    preguntar("¿Tiene lista ordenada? (si/no):", lista_ordenada),
    preguntar("¿Se busca rapidez? (si/no):", busca_rapidez),
    writeln("✅ Características registradas."),
    verificar_inconsistencias.

% Mostrar datos ingresados
mostrar_datos :-
    writeln("\n🔍 Datos ingresados:"),
    (tiene_marco_muestral -> writeln("- Cuenta con marco muestral.") ; true),
    (poblacion_homogenea -> writeln("- Población homogénea.") ; true),
    (poblacion_heterogenea -> writeln("- Población heterogénea.") ; true),
    (poblacion_amplia -> writeln("- Población amplia.") ; true),
    (lista_ordenada -> writeln("- Tiene lista ordenada.") ; true),
    (busca_rapidez -> writeln("- Busca rapidez.") ; true),
    nl.

% Lectura validada de respuestas 'si'/'no'
preguntar(Preg, Hecho) :-
    write(Preg), flush_output,
    leer_respuesta(Resp),
    (Resp == "si" -> assertz(Hecho) ; true).

leer_respuesta(Resp) :-
    read_line_to_string(user_input, Raw),
    string_lower(Raw, L),
    ( member(L, ["si","no"]) -> Resp = L
    ; writeln("Por favor, responde 'si' o 'no'."), leer_respuesta(Resp)
    ).

% Limpiar hechos ingresados
reset_datos :-
    retractall(tiene_marco_muestral),
    retractall(poblacion_homogenea),
    retractall(poblacion_heterogenea),
    retractall(poblacion_amplia),
    retractall(lista_ordenada),
    retractall(busca_rapidez).

verificar_inconsistencias :-
    poblacion_homogenea,
    poblacion_heterogenea,
    writeln("⚠️ Advertencia: La población no puede ser homogénea y heterogénea a la vez."),
    !.

verificar_inconsistencias.
