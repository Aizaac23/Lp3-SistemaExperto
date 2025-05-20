% gui.pl
% Interfaz web para LP3-SistemaExperto
% Requiere SWI-Prolog con soporte HTTP

% Carga de los módulos del sistema experto
:- consult(base_conocimiento).
:- consult(calculo_muestra).
:- consult(glosario).
:- consult(recomendaciones).
:- consult(interfaz_usuario).

% Bibliotecas HTTP y HTML
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_error)).

% Puerto por defecto
:- dynamic puerto_servidor/1.
puerto_servidor(8080).

% Declaración de rutas HTTP (handlers)
:- http_handler(root(.), pagina_principal, []).
:- http_handler(root(ingresar), ingresar_handler, []).
:- http_handler(root(mostrar), mostrar_handler, []).
:- http_handler(root(recomendar), recomendar_handler, []).
:- http_handler(root(calcular), calcular_handler, []).
:- http_handler(root(glosario), glosario_handler, []).
:- http_handler(root(recgen), recgen_handler, []).
:- http_handler(root(reset), reset_handler, []).
:- http_handler(root(salir), salir_handler, []).

% Predicado para iniciar el servidor
start_server :-
    puerto_servidor(Puerto),
    format('Iniciando servidor web en http://localhost:~w/~n', [Puerto]),
    http_server(http_dispatch, [port(Puerto)]).

% Detener el servidor
stop_server :-
    http_stop_server(8080, []).

% Cambiar el puerto (opcional)
set_port(Puerto) :-
    retractall(puerto_servidor(_)),
    assertz(puerto_servidor(Puerto)).

%% HANDLERS HTTP %%

% Handler para página principal - Menú con enlaces a todas las opciones
pagina_principal(_Request) :-
    reply_html_page(
        [title('LP3-SistemaExperto')],
        [
            h1('🎯 Sistema Experto en Técnicas de Muestreo'),
            h2('📋 Menú Principal'),
            ul([
                li(a(href('/ingresar'), '1. Ingresar características de la población')),
                li(a(href('/mostrar'), '2. Mostrar datos ingresados')),
                li(a(href('/recomendar'), '3. Ver técnica de muestreo recomendada')),
                li(a(href('/calcular'), '4. Calcular tamaño de muestra')),
                li(a(href('/glosario'), '5. Mostrar glosario')),
                li(a(href('/recgen'), '6. Ver recomendaciones generales')),
                li(a(href('/reset'), '7. Limpiar datos ingresados')),
                li(a(href('/salir'), '8. Salir'))
            ]),
            footer([
                hr([]),
                p('LP3-SistemaExperto - Interfaz Web')
            ])
        ]
    ).

% Handler para ingresar datos - Formulario con checkboxes
ingresar_handler(Request) :-
    member(method(Method), Request),
    ingresar_handler(Method, Request).

% GET - Muestra el formulario
ingresar_handler(get, _Request) :-
    reply_html_page(
        [title('Ingresar Datos')],
        [
            h1('Ingresar características de la población'),
            form([action='/ingresar', method='post'], [
                p([
                    \checkbox('tiene_marco_muestral', 'yes', [], []), 
                    ' ¿Cuenta con marco muestral?'
                ]),
                p([
                    \checkbox('poblacion_homogenea', 'yes', [], []), 
                    ' ¿La población es homogénea?'
                ]),
                p([
                    \checkbox('poblacion_heterogenea', 'yes', [], []), 
                    ' ¿La población es heterogénea?'
                ]),
                p([
                    \checkbox('poblacion_amplia', 'yes', [], []), 
                    ' ¿La población es amplia?'
                ]),
                p([
                    \checkbox('lista_ordenada', 'yes', [], []), 
                    ' ¿Tiene lista ordenada?'
                ]),
                p([
                    \checkbox('busca_rapidez', 'yes', [], []), 
                    ' ¿Se busca rapidez?'
                ]),
                p(input([type=submit, value='Guardar Datos']))
            ]),
            p(a(href('/'), 'Volver al menú principal'))
        ]
    ).

% POST - Procesa el formulario
ingresar_handler(post, Request) :-
    % Limpiamos datos previos
    reset_datos,
    
    % Procesamos los parámetros del formulario
    http_parameters(Request, [
        tiene_marco_muestral(TMM, [default('')]),
        poblacion_homogenea(PH, [default('')]),
        poblacion_heterogenea(PHet, [default('')]),
        poblacion_amplia(PA, [default('')]),
        lista_ordenada(LO, [default('')]),
        busca_rapidez(BR, [default('')])
    ]),
    
    % Assertamos los hechos según los checkboxes marcados
    (TMM == 'yes' -> assertz(tiene_marco_muestral) ; true),
    (PH == 'yes' -> assertz(poblacion_homogenea) ; true),
    (PHet == 'yes' -> assertz(poblacion_heterogenea) ; true),
    (PA == 'yes' -> assertz(poblacion_amplia) ; true),
    (LO == 'yes' -> assertz(lista_ordenada) ; true),
    (BR == 'yes' -> assertz(busca_rapidez) ; true),
    
    % Capturamos posibles advertencias de verificación
    with_output_to(
        string(Advertencias),
        verificar_inconsistencias
    ),

    % Respondemos confirmando la operación
    reply_html_page(
        [title('Datos Guardados')],
        [
            h1('Datos Registrados'),
            p('✅ Características registradas correctamente.'),
            \mostrar_advertencias(Advertencias),
            p(a(href('/mostrar'), 'Ver datos ingresados')),
            p(a(href('/'), 'Volver al menú principal'))
        ]
    ).

% Helper para mostrar advertencias si existen
mostrar_advertencias("") --> [].
mostrar_advertencias(Advertencias) --> 
    [div(class('advertencia'), [
        h3('⚠️ Advertencias:'),
        pre(Advertencias)
    ])].

% Handler para mostrar datos ingresados
mostrar_handler(_Request) :-
    % Capturamos todos los datos actuales
    findall(Hecho-Descripcion, hecho_descripcion(Hecho, Descripcion), Hechos),
    
    % Generamos la respuesta HTML
    reply_html_page(
        [title('Datos Ingresados')],
        [
            h1('🔍 Datos Ingresados'),
            \mostrar_hechos(Hechos),
            p(a(href('/'), 'Volver al menú principal'))
        ]
    ).

% Helper para mostrar los hechos actuales
mostrar_hechos([]) --> 
    [p('No hay datos ingresados. ', a(href('/ingresar'), 'Ingresar datos ahora.'))].
mostrar_hechos(Hechos) --> 
    [ul(\mostrar_hechos_lista(Hechos))].

mostrar_hechos_lista([]) --> [].
mostrar_hechos_lista([Hecho-Desc|Resto]) --> 
    [li([strong(Desc)])],
    mostrar_hechos_lista(Resto).

% Mapeo de hechos a descripciones amigables
hecho_descripcion(tiene_marco_muestral, 'Cuenta con marco muestral').
hecho_descripcion(poblacion_homogenea, 'Población homogénea').
hecho_descripcion(poblacion_heterogenea, 'Población heterogénea').
hecho_descripcion(poblacion_amplia, 'Población amplia').
hecho_descripcion(lista_ordenada, 'Tiene lista ordenada').
hecho_descripcion(busca_rapidez, 'Busca rapidez').

% Handler para recomendar técnica de muestreo
recomendar_handler(_Request) :-
    % Intentamos determinar una técnica recomendada
    (recomendar_muestreo(Tecnica) ->
        % Si se encuentra una técnica
        explicacion(Tecnica, Explicacion),
        recomendacion(Tecnica, RecomendacionExtra),
        
        reply_html_page(
            [title('Técnica Recomendada')],
            [
                h1('Técnica de Muestreo Recomendada'),
                div(class('recomendacion'), [
                    h2(['✅ ', \tecnica_nombre(Tecnica)]),
                    h3('Explicación:'),
                    p(Explicacion),
                    h3('Recomendación adicional:'),
                    p(RecomendacionExtra)
                ]),
                p(a(href('/'), 'Volver al menú principal'))
            ]
        )
    ;
        % Si no se puede determinar una técnica
        reply_html_page(
            [title('No se puede recomendar')],
            [
                h1('No se puede determinar una técnica'),
                p('⚠️ No se pudo determinar una técnica de muestreo adecuada con los datos proporcionados.'),
                p('Por favor, asegúrese de ingresar suficiente información sobre su población.'),
                p(a(href('/ingresar'), 'Ingresar más datos')),
                p(a(href('/'), 'Volver al menú principal'))
            ]
        )
    ).

% Helper para formatear nombre de técnica
tecnica_nombre(muestreo_aleatorio_simple) --> ['Muestreo Aleatorio Simple'].
tecnica_nombre(muestreo_estratificado) --> ['Muestreo Estratificado'].
tecnica_nombre(muestreo_por_conglomerados) --> ['Muestreo por Conglomerados'].
tecnica_nombre(muestreo_sistematico) --> ['Muestreo Sistemático'].

% Handler para cálculo de muestra
calcular_handler(Request) :-
    member(method(Method), Request),
    calcular_handler(Method, Request).

% GET - Formulario para ingresar datos del cálculo
calcular_handler(get, _Request) :-
    reply_html_page(
        [title('Calcular Muestra')],
        [
            h1('📊 Cálculo de Tamaño de Muestra'),
            form([action='/calcular', method='post'], [
                div([
                    label([for='z'], 'Valor crítico Z (ej: 1.96):'),
                    input([type=number, step='0.01', name='z', value='1.96', required])
                ]),
                div([
                    label([for='p'], 'Proporción esperada P (ej: 0.5):'),
                    input([type=number, step='0.01', min='0', max='1', name='p', value='0.5', required])
                ]),
                div([
                    label([for='e'], 'Margen de error E (ej: 0.05):'),
                    input([type=number, step='0.01', min='0', max='1', name='e', value='0.05', required])
                ]),
                div([
                    label([for='n'], 'Tamaño de población (0 para infinita):'),
                    input([type=number, step='1', min='0', name='n', value='0'])
                ]),
                p(input([type=submit, value='Calcular']))
            ]),
            p(a(href('/'), 'Volver al menú principal'))
        ]
    ).

% POST - Procesa el formulario y calcula el tamaño de muestra
calcular_handler(post, Request) :-
    % Obtenemos los parámetros del formulario
    http_parameters(Request, [
        z(Z, [float]),
        p(P, [float]),
        e(E, [float]),
        n(Pobl, [integer])
    ]),
    
    % Calculamos el tamaño de muestra
    (Pobl =:= 0 ->
        % Para población infinita
        tamano_muestra_infinita(Z, P, E, N),
        TipoMuestra = 'infinita'
    ;
        % Para población finita
        tamano_muestra_finita(Z, P, E, Pobl, N),
        TipoMuestra = 'finita'
    ),
    
    % Mostramos el resultado
    reply_html_page(
        [title('Resultado del Cálculo')],
        [
            h1('Resultado del Cálculo de Muestra'),
            div(class('resultado'), [
                h2('Parámetros utilizados:'),
                ul([
                    li(['Z = ', \valor_decimal(Z)]),
                    li(['P = ', \valor_decimal(P)]),
                    li(['E = ', \valor_decimal(E)]),
                    li(['Población = ', \poblacion_valor(Pobl)])
                ]),
                h2('Resultado:'),
                p([
                    'Tamaño de muestra (', TipoMuestra, '): ',
                    strong(N)
                ])
            ]),
            p(a(href='/calcular'), 'Calcular otra muestra'),
            p(a(href('/'), 'Volver al menú principal'))
        ]
    ).

% Helper para formatear valores decimales
valor_decimal(V) --> {format(string(S), '~2f', [V])}, [S].

% Helper para mostrar población
poblacion_valor(0) --> ['Infinita'].
poblacion_valor(N) --> {number_string(N, S)}, [S].

% Handler para el glosario
glosario_handler(_Request) :-
    % Obtenemos todos los términos del glosario
    findall(Termino-Definicion, descripcion(Termino, Definicion), Glosario),
    
    % Generamos la respuesta HTML
    reply_html_page(
        [title('Glosario')],
        [
            h1('📘 Glosario de Técnicas de Muestreo'),
            div(class('glosario'), [
                dl(\mostrar_terminos(Glosario))
            ]),
            p(a(href('/'), 'Volver al menú principal'))
        ]
    ).

% Helper para mostrar términos del glosario
mostrar_terminos([]) --> [].
mostrar_terminos([Termino-Definicion|Resto]) --> 
    [
        dt(\tecnica_nombre(Termino)),
        dd(Definicion)
    ],
    mostrar_terminos(Resto).

% Handler para mostrar recomendaciones generales
recgen_handler(_Request) :-
    % Obtenemos todas las recomendaciones
    findall(Tecnica-Texto, recomendacion(Tecnica, Texto), Recomendaciones),
    
    % Generamos la respuesta HTML
    reply_html_page(
        [title('Recomendaciones Generales')],
        [
            h1('📄 Recomendaciones Generales'),
            div(class('recomendaciones'), [
                ul(\mostrar_recomendaciones(Recomendaciones))
            ]),
            p(a(href('/'), 'Volver al menú principal'))
        ]
    ).

% Helper para mostrar recomendaciones
mostrar_recomendaciones([]) --> [].
mostrar_recomendaciones([Tecnica-Texto|Resto]) --> 
    [
        li([
            strong(\tecnica_nombre(Tecnica)),
            ': ',
            Texto
        ])
    ],
    mostrar_recomendaciones(Resto).

% Handler para reset de datos
reset_handler(_Request) :-
    % Limpiamos todos los hechos dinámicos
    reset_datos,
    
    % Generamos la respuesta HTML
    reply_html_page(
        [title('Datos Limpiados')],
        [
            h1('Limpiar Datos'),
            p('✅ Datos limpiados correctamente.'),
            p(a(href('/'), 'Volver al menú principal'))
        ]
    ).

% Handler para salir
salir_handler(_Request) :-
    % Generamos la respuesta HTML de despedida
    reply_html_page(
        [title('Hasta Luego')],
        [
            h1('👋 ¡Hasta luego!'),
            p('Gracias por utilizar el Sistema Experto en Técnicas de Muestreo.'),
            p(a(href('/'), 'Volver al inicio'))
        ]
    ).

%% HELPERS DCG para HTML %%

% Checkbox - Crea un input checkbox
checkbox(Name, Value, Checked, Attributes) -->
    { (member(checked, Checked) -> CheckedAttr = [checked]; CheckedAttr = []) },
    html(input([type=checkbox, name=Name, value=Value|CheckedAttr])).

:- multifile http:location/3.
:- dynamic   http:location/3.

% Definición de la ubicación raíz
http:location(root, /, []).

% Añadimos un poco de estilo CSS inline para mejorar la apariencia
:- multifile 
    user:head//2.

user:head(_, Head) -->
    html(Head),
    html([
        style([
'
body {
    font-family: Arial, sans-serif;
    line-height: 1.6;
    margin: 0;
    padding: 20px;
    color: #333;
    max-width: 800px;
    margin: 0 auto;
}
h1, h2, h3 {
    color: #2c3e50;
}
ul {
    padding-left: 20px;
}
a {
    color: #3498db;
    text-decoration: none;
}
a:hover {
    text-decoration: underline;
}
form div {
    margin-bottom: 15px;
}
input[type="text"], input[type="number"] {
    padding: 8px;
    width: 250px;
    margin-left: 10px;
}
input[type="submit"] {
    padding: 8px 20px;
    background-color: #3498db;
    color: white;
    border: none;
    cursor: pointer;
}
.advertencia {
    background-color: #fff3cd;
    border-left: 5px solid #ffc107;
    padding: 10px;
    margin: 15px 0;
}
.recomendacion, .resultado {
    background-color: #e8f4f8;
    border-left: 5px solid #3498db;
    padding: 15px;
    margin: 15px 0;
}
dl dt {
    font-weight: bold;
    margin-top: 15px;
}
dl dd {
    margin-left: 20px;
}
footer {
    margin-top: 40px;
    color: #7f8c8d;
    font-size: 0.9em;
}
'
        ])
    ]).
