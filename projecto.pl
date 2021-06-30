% Joao Santos Jorge 88079

:- [codigo_comum].



%----------------------------------------------------------------------------
%combinacoes_soma(N, Els, Soma, Combs): 'Combs' e' a lista que resulta da
%           das combinacoes 'N' a 'N' de 'Els' com soma = 'Soma'.
%           findall: predicado que permite obter todas as combinacoes
%           de 'Els' de 'N' a 'N'.
%
%escolhe_els(Soma, Combs_Total, Combs): Reduz a lista 'Combs_Total' para
%            lista 'Combs' em que a soma dos elementos de cada sub-lista e'
%            igual a 'Soma'.
%
%soma_lista(Lst, Soma_2): predicado que calcula a soma dos elementos de 'Lst'
%           em 'Soma_2'.
%----------------------------------------------------------------------------
combinacoes_soma(N, Els, Soma, Combs):-
    findall(Comb, combinacao(N,Els,Comb), Combs_Total),
    escolhe_els(Soma, Combs_Total, Combs),!.


escolhe_els(_,[],[]).

escolhe_els(S, [H|T], [H|Res]):-
    soma_lista(H, S_aux),
    S =:= S_aux,
    escolhe_els(S,T,Res).

escolhe_els(S, [H|T], Res):-
    soma_lista(H, S_aux),
    S =\= S_aux,
    escolhe_els(S,T,Res).


soma_lista([],0).

soma_lista([H|T], Soma):-
    soma_lista(T,Soma_Aux),
    Soma is H + Soma_Aux.



%----------------------------------------------------------------------------
%permutacoes_soma(N, Els, Soma, Perms): 'Perms' e' a lista de permutacoes com
%   soma = a 'Soma'.
%
%perm_elm(Combs, Perms): 'Perms' e' a lista de todas as permutacoes da combinacao
%   'Combs'.
%----------------------------------------------------------------------------
permutacoes_soma(N, Els, Soma, Perms):-
    combinacoes_soma(N,Els, Soma, Combs),
    perm_elm(Combs, Perms_1),
    append(Perms_1,Perms_2),
    sort(Perms_2, Perms),!.
    


perm_elm([],[]).
perm_elm([H|T], [Lst|R]):-
    findall(Lst_Aux, permutation(H, Lst_Aux), Lst),
    perm_elm(T,R).



%----------------------------------------------------------------------------
%espaco_fila(Fila, Esp, H_V): 'Esp' e' um espaco da fila 'Fila'. 'H_V'
%   indica a orientacao do espaco, horizontal ou vertical. 
%
%retira_soma_h(Fila, Fila_Aux, Soma): Obtem soma da Fila para guardar no 
%   espaco. retira_soma_v tem comportamento semelhante, mas trata de filas
%   com orientacao vertical.
%
%espaco_fila_aux(Fila_Aux, Lst): Obtem a lista de um espaco atraves da Fila.
%----------------------------------------------------------------------------


espaco_fila_aux([H|_], []):-
    nonvar(H).

espaco_fila_aux([H|T], [H|R]):-
    var(H),
    espaco_fila_aux(T, R).

espaco_fila_aux([],[]).

%-------------------------------
retira_elm_2(Lst, Elm):- nth0(1,Lst,Elm).


retira_elm_1(Lst, Elm):- nth0(0,Lst,Elm).

%-------------------------------
compara_lista([H|T], [H|T]).

%-------------------------------


retira_soma_h([H|T], T, Soma):-
    is_list(H),
    retira_elm_2(H, Soma).

retira_soma_v([H|T], T, Soma):-
    is_list(H),
    retira_elm_1(H, Soma).

%-------------------------------

espaco_fila(Fila, espaco(Soma, Lst), 'h'):-
    retira_soma_h(Fila, Fila_Aux, Soma),
    espaco_fila_aux(Fila_Aux, Lst),
    Lst \== [].

espaco_fila(Fila, espaco(Soma, Lst), 'v'):-
    retira_soma_v(Fila, Fila_Aux, Soma),
    espaco_fila_aux(Fila_Aux, Lst),
    Lst \== [].


espaco_fila([_|T], Esp, H_V):-
    espaco_fila(T, Esp, H_V).



%----------------------------------------------------------------------------
%espacos_fila(H_V, Fila, Espacos): Espacos e' a lista de todos os espacos da
%   fila com orientacao horizontal ou vertical.
%----------------------------------------------------------------------------

espacos_fila(H_V, Fila, Esp):-
    bagof(espaco(Soma, Lst), espaco_fila(Fila, espaco(Soma, Lst), H_V), Esp).

espacos_fila(_,_,[]).
%----------------------------------------------------------------------------
%espacos_puzzle(Puzzle, Espacos): Obtem todos os espacos de um puzzle,
%   primeiro os espacos obtidos na horizontal e depois na vertical.
%
%percorre_puzzle(H_V, Puzzle, Espacos): Percorre cada linha do puzzle na
%   horizontal ou na vertical para obter todos os Espacos da orientacao 'H_V'.
%
%junta(E1,E2, Espacos): junta numa lista os Espacos 'h' e 'v' e assim obtem-se
%   os Espacos completos.
%----------------------------------------------------------------------------

junta([], L, L).
junta([P | R], L1, [P | L2]) :- junta(R, L1, L2),!.


percorre_puzzle(_,[],[]).

percorre_puzzle(H_V, [H|T], [Espaco|R]):-
    espacos_fila(H_V, H, Espaco),
    percorre_puzzle(H_V, T, R),!.

espacos_puzzle(Puzzle, Espacos):-
    percorre_puzzle('h', Puzzle, Espacos_h),
    append(Espacos_h, Espacos_h_f),
    mat_transposta(Puzzle, Transp),
    percorre_puzzle('v', Transp, Espacos_v),
    append(Espacos_v, Espacos_v_f),
    junta(Espacos_h_f, Espacos_v_f, Espacos).




%percorre_puzzle('h',[[[0, 0], [0, 0], [0, 0], [17, 0], [10, 0]], [[0, 0], [24, 0], [11, 3], _, _], [[0,16], _, _, _, _], [[0,26], _, _, _, _], [[0,17], _, _, [0,0], [0,0]]], Espacos).
%percorre_puzzle('h',[[[0, 0], [0, 0], [0, 0], [17, 0], [10, 0]], [[0, 0], [24, 0], [11, 3], P24, P25], [[0,16], P32, P33, P34, P35], [[0,26], P42, P43, P44, P45], [[0,17], P52, P53, [0,0], [0,0]]], Espacos).

%espacos_puzzle([[[0, 0], [0, 0], [0, 0], [17, 0], [10, 0]], [[0, 0], [24, 0], [11, 3], P24, P25], [[0,16], P32, P33, P34, P35], [[0,26], P42, P43, P44, P45], [[0,17], P52, P53, [0,0], [0,0]]], Espacos).



%----------------------------------------------------------------------------
%espacos_com_posicoes_comuns(Espacos, Esp, Esps_com): 'Esps_com' e' a lista de
%   espacos que partilham variaveis com 'Esp'.
%
%veridica_espacos(Esp1, Esp2): verifica se espaco1 tem variaveis em comum com
%   espaco 2.
%
%var_igual(Var, Lst_Vars): verifica se Var esta na lista de vars.
%----------------------------------------------------------------------------

var_igual(Var, [H|_]):-
    Var == H,!.

var_igual(Var,[_|T]):-  var_igual(Var, T).
%var_igual(P2,[P3,P4,P2])


verifica_espacos(espaco(_,[]), _):-false.

verifica_espacos(espaco(_,[H|_]), espaco(_,Lst)):-
    var_igual(H, Lst),!.

verifica_espacos(espaco(_,[_|T]), espaco(_,Lst)):-
    verifica_espacos(espaco(_,T), espaco(_,Lst)).
    
%    verifica_espacos(espaco(3, [P24,P25]), espaco(16, [P32,P33,P34,P35])).


espacos_com_posicoes_comuns(Espacos, Esp, Esps_com):-
    include(verifica_espacos(Esp), Espacos, Esps_aux),
    subtract(Esps_aux, [Esp], Esps_com).




%espacos_com_posicoes_comuns( [ espaco(3, [P24,P25]), espaco(16, [P32,P33,P34,P35]), espaco(26, [P42,P43,P44,P45]), espaco(17, [P52,P53])], espaco(3, [P24,P25]), E).




%----------------------------------------------------------------------------
%permutacoes_soma_espacos(Espacos, Perms_Soma):- Perms_Soma e' uma lista de 
%   dois elementos. 1o elemento e' espaco e 2o a lista de permutacoes possiveis
%   para o espaco
%
%permuta_espaco(Esp, Espaco_Perm): obtem permutacoes_soma para espaco.
%----------------------------------------------------------------------------



permuta_espaco(espaco(Soma, Lst), [espaco(Soma, Lst), Perms]):-
    length(Lst, Length),
    permutacoes_soma(Length,[1,2,3,4,5,6,7,8,9], Soma, Perms).
%permuta_espaco(espaco(3, [P24, P25]), E).

permutacoes_soma_espacos([],[]).

permutacoes_soma_espacos([H|T], [Espaco_Perm|R]):-
    permuta_espaco(H, Espaco_Perm),
    permutacoes_soma_espacos(T,R).  





%----------------------------------------------------------------------------
%permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma): Perm e' uma Perm
%   possivel para 'Esp'.
%
%tira_espaco_comp(Esp, Perms_soma, Esp_com): 'Esp_com' e' espaco com permutacoes
%   obtido por percorrer Perms_soma.
%
%unifica_perm(Perm, Vars, Espacos, Esp, Perms_soma): Unifica Perm com Vars,
%   caso falhe significa que Perm nao e' possivel.
%
%unifica_poss(Perm): verifica se ha alguma permutacao possivel para espaco depois
%   da unificacao que houve no predicado anterior
%
%tira_possibilidades(Esps_com, Perms_soma, Perms_com): Perms_com sao as permutacoes
%   tiradas de Perms_soma iguais a Esp_com
%----------------------------------------------------------------------------


tira_espaco_comp(espaco(S,Vars), [[espaco(S,Lst_Var), Lista_Perm]|_], [espaco(S,Lst_Var), Lista_Perm]):- Vars == Lst_Var,!.

tira_espaco_comp(Esp, [_|T], Lista_Perm):-
    tira_espaco_comp(Esp, T, Lista_Perm).
%Perms_soma = [[espaco(3, [P24, P25]), [[1, 2], [2, 1]]],[espaco(10, [P25, P35, P45]), [[1, 2, 7], [1, 3, 6], [1, 4, 5]]],[espaco(17, [P24, P34, P44]), [[1, 7, 9], [1, 9, 7], [2, 6, 9]]],
%[espaco(11, [P33, P43, P53]), [[1, 2, 8], [1, 3, 7], [1, 4, 6]]]],
%tira_espaco_comp(espaco(3, [P24, P25]),Perms_soma, L).

tira_espacos_com_poss(H, [[Esp, Lista_Perm]|_], [Esp, Lista_Perm]):-
    H == Esp,!.

tira_espacos_com_poss(H, [_|T_P], Esp_completo):-
    tira_espacos_com_poss(H, T_P, Esp_completo).

%Esps_com = espacos(5,[P,Q]),
%Perms_soma = [ [espacos(4,[M,N]), [[1,2],[4,3]]], [espacos(5,[P,Q]), [[1,2],[4,3]]], [espacos(6,[Q,W]), [[8,9],[6,7]]] ],
%tira_espacos_com_poss(Esps_com, Perms_soma, Perms_com).

tira_possibilidades([],_,[]):- !.

tira_possibilidades([H|T], Perms_soma, [Esp_completo|R]):-
    tira_espacos_com_poss(H, Perms_soma, Esp_completo),
    tira_possibilidades(T, Perms_soma, R).

%Esps_com = [espacos(5,[P,Q]), espacos(6,[Q,W])],
%Perms_soma = [ [espacos(4,[M,N]), [[1,2],[4,3]]], [espacos(5,[P,Q]), [[1,2],[4,3]]], [espacos(6,[Q,W]), [[8,9],[6,7]]] ],
%tira_possibilidades(Esps_com, Perms_soma, Perms_com).

unifica_poss([espaco(_, Lst_var), Perm_poss]):-
    member(El ,Perm_poss),
    forall(member(M, [El]), M = Lst_var),!.


unifica_perm(Perm, Vars, Espacos, espaco(S, Vars), Perms_soma):-
    Perm = Vars,
    espacos_com_posicoes_comuns(Espacos, espaco(S, Vars), Esps_com),
    tira_possibilidades(Esps_com, Perms_soma, Perms_com),
    forall(member(P, Perms_com), unifica_poss(P)).
    


permutacao_possivel_espaco(Perm, espaco(S, Vars), Espacos, Perms_soma):-
    tira_espaco_comp(espaco(S, Vars), Perms_soma, [espaco(_, _), Lst_poss]),
    member(Perm, Lst_poss),
    forall(member(X, [Perm]), unifica_perm(X, Vars, Espacos, espaco(S, Vars), Perms_soma)).
    


%----------------------------------------------------------------------------
%permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss): Perms_poss
%   e' uma lista de 2 elementos em que o primeiro e' a lista de variaveis e o 
%   segundo a lista de todas as permutacoes possiveis para o espaco Esp.
%----------------------------------------------------------------------------

permutacoes_possiveis_espaco(Espacos, Perms_soma, espaco(S, Lst_vars), [Lst_vars, Lst_perms]):-
    bagof(Perm, permutacao_possivel_espaco(Perm, espaco(S, Lst_vars), Espacos, Perms_soma), Lst_perms).



%----------------------------------------------------------------------------
%permutacoes_possiveis_espacos(Espacos, Perms_poss_esps): Perms_poss_esps e' a
%   lista de permutacoes possiveis para todos os espacos.
%   
%permutacoes_possiveis_espacos_aux/4 : Percorre todos os espacos e resolve o 
%   predicado permutacoes_posssiveis_espaco para cada espaco possibilitando assim
%   que haja Perms_poss para todos os espacos
%----------------------------------------------------------------------------

permutacoes_possiveis_espacos_aux([], _, _, []).

permutacoes_possiveis_espacos_aux([Esp|T], Espacos, Perms_soma, [Perms_poss|R]):-
    permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss),
    permutacoes_possiveis_espacos_aux(T, Espacos, Perms_soma, R).



permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    permutacoes_possiveis_espacos_aux(Espacos, Espacos, Perms_soma, Perms_poss_esps).



%----------------------------------------------------------------------------
%numeros_comuns(Lst_Perms, Numeros_comuns): Numeros_comuns e' uma lista que
%   guarda os elementos comuns de cada lista de permutacoes.
%
%compara_perm(Lst_Perms, Pos, Length, N): para cada permutacao compara se valor
%   do elemento da lista e' igual ao das outras listas.
%
%compara_elm(Elm, Pos, Lst_Perms): verifica valor de Lst_Perms na posicao 'Pos'
%   e' igual a Elm
%----------------------------------------------------------------------------
compara_elm(_,_,[]):-!.

compara_elm(Elm, Pos, [H|T]):-
    nth1(Pos, H, Elm),
    compara_elm(Elm, Pos, T).
%compara_elm(3,4,[[7,1,5,3],[7,5,1,3],[7,4,2,3]]).


compara_perm(_, Pos, Length, []):- Pos == Length,!.


compara_perm([H|T], Pos, Length, [(Pos, Elm)|R]):-
    nth1(Pos, H, Elm),
    compara_elm(Elm, Pos, [H|T]),!,
    Pos_1 is Pos + 1,
    compara_perm([H|T], Pos_1, Length, R).


compara_perm(Lst_Perms, Pos, Length, N):-
    Pos_1 is Pos + 1,
    compara_perm(Lst_Perms, Pos_1, Length, N).

numeros_comuns([H|T], Numeros_comuns):-
    length(H, Length),
    L is Length + 1,
    compara_perm([H|T], 1, L, Numeros_comuns).

%numeros_comuns([[7,1,5,3],[7,5,1,3],[7,4,2,3]], Numeros_comuns).


%----------------------------------------------------------------------------
%atribui_comuns(Perms_Possiveis): actualiza a lista atribuindo a cada espaco
%    numeros comuns.
%
%atribui_comuns_aux(Perms_Possiveis, Lst_numeros_comuns): Para cada elemento 
%   de Perms_Possiveis calcula numeros_comuns e mete-os em Lst_numeros_comuns
%
%unifica_comuns_aux(Lst_vars, Lst_numeros_comuns): unifica cada elemento de
%   Lst_numeros_comuns com variavel de Lst_vars correspondente
%----------------------------------------------------------------------------

atribui_comuns_aux([], []).

atribui_comuns_aux([[_, Lst_Poss]|T], [Numeros_comuns|R]):-
    numeros_comuns(Lst_Poss, Numeros_comuns),
    atribui_comuns_aux(T, R).




unifica_comuns_aux(_, []):-!.

unifica_comuns_aux(Lst_vars, [(Pos, El)|T]):-
    nth1(Pos, Lst_vars, Var),
    Var = El,
    unifica_comuns_aux(Lst_vars, T).





unifica_comuns([], []):-!.

unifica_comuns([[Lst_vars, _]|T], [Numeros_comuns|R]):-
    unifica_comuns_aux(Lst_vars, Numeros_comuns),
    unifica_comuns(T,R).


atribui_comuns(Perms_Possiveis):-
    atribui_comuns_aux(Perms_Possiveis, Lst_numeros_comuns),
    unifica_comuns(Perms_Possiveis, Lst_numeros_comuns).


%----------------------------------------------------------------------------
%retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis): Para cada elemento
%   de Perms_Possiveis verifica quais Permutacoes nao sao possiveis de acordo 
%   com as variaveis ja unificadas
%
%verifica_linha(Perm_Possivel, Nova_Perm_Possivel): apaga permutacoes que nao 
%   unificam com espaco de Perm_Possivel
%----------------------------------------------------------------------------

verifica_linha([_, []], []):-!.

verifica_linha([Vars, [H|T]], [H|R]):-
    forall(member(X, [H]), X = Vars),
    verifica_linha([Vars, T], R).

verifica_linha([Vars, [_|T]], R):-
    verifica_linha([Vars, T], R).

%verifica_linha([[_234,_282,7],[[1,2,7],[1,3,6],[1,4,5],[1,5,4],[1,6,3],[1,7,2],[2,1,7],[2,3,5],[2,5,3]]], List_Final).

retira_impossiveis([],[]):-!.

retira_impossiveis([[Vars, Perms_Possiveis]|T], [[Vars, Nova_Perms_Possiveis]|R]):-
    verifica_linha([Vars, Perms_Possiveis], Nova_Perms_Possiveis),!,
    retira_impossiveis(T, R).



%----------------------------------------------------------------------------
%simplifica(Perms_Possiveis, Nova_Perms_Possiveis): atribui_comuns e retira_impossiveis
%   a Perms_Possiveis ate ja nao haver alteracoes. Ou seja, faz ete processo
%   L vezes. L sendo o numero de elementos que a lista Perms_Possiveis tem.
%----------------------------------------------------------------------------

simplifica_Aux(_, [], 0):-!.

simplifica_Aux(Perms_Possiveis, [Nova_Perms_Possiveis|R], L):-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Nova_Perms_Possiveis),
    L_1 is L - 1,
    simplifica_Aux(Nova_Perms_Possiveis, R, L_1).




simplifica(Perms_Possiveis, Nova_Perms_Possiveis):-
    length(Perms_Possiveis, L),
    simplifica_Aux(Perms_Possiveis, Lst_Nova_Perms_Possiveis, L),
    last(Lst_Nova_Perms_Possiveis,Nova_Perms_Possiveis).


%----------------------------------------------------------------------------
%inicializa(Puzzle, Perms_Possiveis): Perms_possiveis e' a lista de permutacoes
%   possiveis para o puzzle. Vai buscar os espacos a predicado 'espacos_puzzle'
%   e depois obtem as permutacoes_possiveis atraves do predicado permutacoes_possiveis_espacos.
%   Por fim, simplifica Perms_Possiveis com o predicado 'simplifica'.
%----------------------------------------------------------------------------
%verificar.
inicializa(Puzzle, Perms_Possiveis):-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_Possiveis_Espacos),
    simplifica(Perms_Possiveis_Espacos, Perms_Possiveis).



%----------------------------------------------------------------------------
%escolhe_menos_alternativas(Perms_Possiveis, Escolha): Escolha e' o elemento de 
%   Permutacoes que tem menos alternativas mas sempre mais que uma. Caso so'
%   haja uma alternativa, entao a sua permutacao ja unificou com as variaveis.
%
%conta_possibilidades(Perms_Possiveis, Num_Poss): Num_Poss e' lista com o numero
%   de possibilidades  de Perms_Possiveis
%
%linha_menor_possibilidades(Num_Poss, 1, Pos): Ve qual a lista com menor numero de
%   possibilidades. E guarda a sua posicao em Pos.
%----------------------------------------------------------------------------

linha_menor_possibilidades([], _, _):-!.

linha_menor_possibilidades([H|T], Menos_poss, Pos):-
    H > 1,
    H > Menos_poss,
    Pos_1 is Pos + 1,
    linha_menor_possibilidades(T, H, Pos_1).

linha_menor_possibilidades([_|T], Menos_poss, Pos):-
    Pos_1 is Pos + 1,
    linha_menor_possibilidades(T, Menos_poss, Pos_1).


conta_possibilidades([],[]):-!.

conta_possibilidades([[_, Perms_Possiveis]|T], [Length|R]):-
    length(Perms_Possiveis, Length),
    conta_possibilidades(T, R).


escolhe_menos_alternativas(Perms_Possiveis, Escolha):-
    conta_possibilidades(Perms_Possiveis, Num_Poss),!,
    Pos = 1,
    linha_menor_possibilidades(Num_Poss, 1, Pos),!,
    nth1(Pos, Num_Poss, Verifica),
    Verifica \= 1,
    nth1(Pos, Perms_Possiveis, Escolha).



%----------------------------------------------------------------------------
%experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis): apaga permutacoes
%   de Perms_Possiveis de acordo com a unificacao de Escolha.
%
%apaga_lista_Perms(Esp, Perm, Perms_Possiveis, Novas_Perms_Possiveis): para cada
%   elemento de Perms_Possiveis, retira elementos impossiveis.
%----------------------------------------------------------------------------

apaga_lista_Perms(_,_,[],[]).

apaga_lista_Perms(Esp, Perm, [[Vars, _]|T], [[Vars, [Perm]]|R]):-
    Esp == Vars,
    apaga_lista_Perms(Esp,Perm, T, R).

apaga_lista_Perms(Esp, Perm, [[Vars, Lst_Perms]|T], [[Vars, Lst_Perms]|R]):-
    apaga_lista_Perms(Esp,Perm, T, R).



experimenta_perm([Esp, Lst_Perms], Perms_Possiveis, Novas_Perms_Possiveis):-
    member(Perm, Lst_Perms),
    Esp = Perm,
    apaga_lista_Perms(Esp, Perm, Perms_Possiveis, Novas_Perms_Possiveis),!.

