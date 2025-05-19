% main.pl
% Carga de módulos y punto de entrada

:- set_prolog_flag(encoding, utf8).
:- set_prolog_flag(file_encoding, utf8).

:- consult(base_conocimiento).
:- consult(glosario).
:- consult(calculo_muestra).
:- consult(recomendaciones).
:- consult(interfaz_usuario).

% Para iniciar el sistema, cargar y ejecutar:
% ?- [main].
% ?- inicio.