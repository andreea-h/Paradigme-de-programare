:- ensure_loaded('chat.pl').

% Returneaza true dacă regula dată ca argument se potriveste cu
% replica data de utilizator. Replica utilizatorului este
% reprezentata ca o lista de tokens. Are nevoie de
% memoria replicilor utilizatorului pentru a deduce emoția/tag-ul
% conversației.
%
% primeste toti 3 parametrii si intoarce true sau false
%
match_rule(Tokens, _UserMemory, rule(Expresie, _, _, _, _)) :-
  Tokens = Expresie.

% Primeste replica utilizatorului (ca lista de tokens) si o lista de
% reguli, iar folosind match_rule le filtrează doar pe cele care se
% potrivesc cu replica dată de utilizator.
%
% primele 3 argumente se dau
% matchingRules e de iesire
% rules este o lista de rule (adica o lista de structuri rule)
% din toate rule ia doar rule-urile care au match-iut cu tokens
find_matching_rules(Tokens, Rules, UserMemory, MatchingRules) :-
    get_emotion(UserMemory, fericit),
    findall(Rule, (member(Rule, Rules), Rule = rule(_,_,_,[fericit],_), match_rule(Tokens, UserMemory, Rule)), MatchingRules).

find_matching_rules(Tokens, Rules, UserMemory, MatchingRules) :-
    get_emotion(UserMemory, trist),
    findall(Rule, (member(Rule, Rules), Rule = rule(_,_,_,[trist],_), match_rule(Tokens, UserMemory, Rule)), MatchingRules).

find_matching_rules(Tokens, Rules, UserMemory, MatchingRules) :-
    findall(Rule, (member(Rule, Rules), match_rule(Tokens, UserMemory, Rule)), MatchingRules).

% Intoarce in Answer replica lui Gigel. Selecteaza un set de reguli
% (folosind predicatul rules) pentru care cuvintele cheie se afla in
% replica utilizatorului, in ordine; pe setul de reguli foloseste
% find_matching_rules pentru a obtine un set de raspunsuri posibile.
% Dintre acestea selecteaza pe cea mai putin folosita in conversatie.
%
% Replica utilizatorului este primita in Tokens ca lista de tokens.
% Replica lui Gigel va fi intoarsa tot ca lista de tokens.
%
% UserMemory este memoria cu replicile utilizatorului, folosita pentru
% detectarea emotiei / tag-ului.
% BotMemory este memoria cu replicile lui Gigel și va si folosită pentru
% numararea numarului de utilizari ale unei replici.
%
% In Actions se vor intoarce actiunile de realizat de catre Gigel in
% urma replicii (e.g. exit).
%
% Hint: min_score, ord_subset, find_matching_rules
%
% answer este o lista de tokens
% intoarce un raspuns si o lista de actiuni (o lista [exit])

head([H|_], H).

get_reply(rule(_, [H|_], _, _, _), _, H).

tail([_|Tail], Tail).

% check_rule reuseste daca replica memorata in regula primita la intrare NU este [nu, inteleg]
check_rule(rule(_, Reply, _, _ , _)) :- head(Reply, H), \+ (H == [nu, inteleg]), Reply \= [].

snd_get_rules(Tokens, RulesList) :- rules(Tokens, RulesList).

get_rules(Tokens, RulesList) :-
  (tail(Tokens, Tail), get_rules(Tail, RulesList));
  (head(Tokens, H), rules([H], RulesList));
  snd_get_rules(Tokens, RulesList).

% primeste o lista de reguli si le separa doar pe cele care nu contin replica [nu, inteleg]
select_rules(InpRulesList, Result) :-
    findall(H, (member(H, InpRulesList), check_rule(H)), Result).

get_action(rule(_,_,Action,_,_), Action).

add_new_answer(Answer, BotMemory, NewMemory):- NewMemory = BotMemory.put(Answer, 1).

update_memory(NewMemory, NewMemory).

% pentru situatia in care memoria botului este goala
select_answer(Tokens, UserMemory, BotMemory, Answer, Action) :-
    get_rules(Tokens, RulesList),
    select_rules(RulesList, Rules), Rules \== [],
    find_matching_rules(Tokens, Rules, UserMemory, MatchingRules), head(MatchingRules, H),
    get_reply(H, BotMemory, Answer), get_action(H, Action),
    BotMemory == memory{}.


% atunci cand memoria botului nu este goala, se alege replica cu ce mai mica utilizare de pena acum
select_answer(Tokens, UserMemory, BotMemory, Answer, Action) :-
    get_rules(Tokens, RulesList),
    select_rules(RulesList, Rules), Rules \== [],
    find_matching_rules(Tokens, Rules, UserMemory, MatchingRules), head(MatchingRules, H),
    get_action(H, Action),
    get_all_replies(H, Replies), %extrage lista de replici pentru prima regula care a facut match
    get_replies_list(Replies, BotMemory, [], RulesAsList),
    reverse(RulesAsList, Rez),
    min_element(Rez, Answer).

% urmatoarele 2 predicate select_answer trateaza situatiile in care robotul raspunde doar cu [nu, inteleg]
% (caz in care ultimele 2 interogari pentru select_answer de mai sus esueaza)
% pentru aceaste situatii poate fi construit direct raspunsul dat([nu, inteleg])

% pentru situatia in care memoria botului este goala
select_answer(_, _, BotMemory, Answer, _) :-
    Answer = [nu, inteleg],
    BotMemory == memory{}.
% atunci cand memoria botului nu este goala, se alege replica cu ce mai mica utilizare de pena acum
select_answer(_, _, _, Answer, _) :-
    Answer = [nu, inteleg].

get_all_replies(rule(_, Replies, _,_,_), Replies).

get_replies_pair(Reply, BotMemory, ReplyPair) :- get_answer(Reply, BotMemory, Val), ReplyPair = (Reply, Val).

get_replies_list([], _, Acc, Acc).
get_replies_list([Reply|Replies], BotMemory, Acc, Result) :-
  get_replies_pair(Reply, BotMemory, Pair), get_replies_list(Replies, BotMemory,[Pair|Acc], Result).

% get_best_reply(rule(_, [H|Rules], _, _, _), BotMemory, Answer) :-

% Esuează doar daca valoarea exit se afla in lista Actions.
% Altfel, returnează true.
%
% primeste actiunile si da true / false daca trebuie sau nu sa faca exit
handle_actions([]) :- true.
handle_actions([H|RestActions]) :- \+ (H = exit), handle_actions(RestActions).


% Caută frecvența (numărul de apariți) al fiecarui cuvânt din fiecare
% cheie a memoriei.
% e.g
% ?- find_occurrences(memory{'joc tenis': 3, 'ma uit la box': 2, 'ma uit la un film': 4}, Result).
% Result = count{box:2, film:4, joc:3, la:6, ma:6, tenis:3, uit:6, un:4}.
% Observați ca de exemplu cuvântul tenis are 3 apariți deoarce replica
% din care face parte a fost spusă de 3 ori (are valoarea 3 în memorie).
% Recomandăm pentru usurința să folosiți înca un dicționar în care să tineți
% frecvențele cuvintelor, dar puteți modifica oricum structura, această funcție
% nu este testată direct.
%
% primeste userMemory si intoarce result (un scor)
% scorul este numarul de aparitii ale cuvintelor

% primeste un dictionar si intoarce o lista alcatuita din toate cuvintele catre
% se gasesc in alcatuirea propozitiilor din cheile din dictionar (inclus elemente duplicate)
get_Tokens_from_Dict(Dict, RepList) :-
  dict_keys(Dict, AuxList), get_words_from_list(Dict, AuxList, [], RepList).

% apendeaza la List de 'Counter' ori lista Elem
repeated_append(Max, Counter, Elem, List, Acc, ResultList) :-
  Counter =< Max,
  append(Elem, Acc, Result),
  Counter1 is Counter + 1,
  repeated_append(Max, Counter1, Elem, List, Result, ResultList).

% regula de mai sus esueaza pentru Counter > Max
repeated_append(Max, Max1, _, _, Acc, Acc) :- Max1 =:= Max + 1.

% primeste o lista de replici si intoarce o lista de tokeni
get_words_from_list(_, [], Acc, Acc).
get_words_from_list(Dict, [H|RepList], Acc, TokensList) :-
  words(H, List),
  Val = Dict.get(H),
  Counter = 1,
  repeated_append(Val, Counter, List, List, Acc, RezP),
  get_words_from_list(Dict, RepList, RezP, TokensList).

count(_, [], 0).
count(X, [X | T], N) :-
  !, count(X, T, N1),
  N is N1 + 1.
count(X, [_ | T], N) :-count(X, T, N).

% primeste o lista si un dictionar si intoarce numarul de aparitii al
% acelui cuvant in dictionar
find_occurrences(Dict, Word, Result) :-
  get_Tokens_from_Dict(Dict, TokensList),
  count(Word, TokensList, Result).


% Atribuie un scor pentru fericire (de cate ori au fost folosit cuvinte din predicatul happy(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie fericit.
%
% frecventa pentru cuvintele de fericire
get_happy_score(UserMemory, Score) :-
  happy(Word),
  find_occurrences(UserMemory, Word, Score).

% Atribuie un scor pentru tristețe (de cate ori au fost folosit cuvinte din predicatul sad(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie trist.
get_sad_score(UserMemory, Score) :-
  sad(Word),
  find_occurrences(UserMemory, Word, Score).


% Pe baza celor doua scoruri alege emoția utilizatorul: `fericit`/`trist`,
% sau `neutru` daca scorurile sunt egale.
% e.g:
% ?- get_emotion(memory{'sunt trist': 1}, Emotion).
% Emotion = trist.
%
% intoarce emotia cu scorul mai mare
% daca sunt egale, da neutru
get_emotion(UserMemory, fericit) :-
  get_happy_score(UserMemory, HappyScore),
  get_sad_score(UserMemory, SadScore),
  HappyScore > 0,
  SadScore > 0,
  HappyScore > SadScore.

get_emotion(UserMemory, trist) :-
  get_happy_score(UserMemory, HappyScore),
  get_sad_score(UserMemory, SadScore),
  HappyScore > 0,
  SadScore > 0,
  HappyScore < SadScore.

get_emotion(UserMemory, neutru) :-
  get_happy_score(UserMemory, HappyScore),
  get_sad_score(UserMemory, SadScore),
  HappyScore > 0,
  SadScore > 0,
  HappyScore == SadScore.

% Atribuie un scor pentru un Tag (de cate ori au fost folosit cuvinte din lista tag(Tag, Lista))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să vorbească despre acel subiect.
get_tag_score(_Tag, _UserMemory, _Score) :- fail.

% Pentru fiecare tag calculeaza scorul și îl alege pe cel cu scorul maxim.
% Dacă toate scorurile sunt 0 tag-ul va fi none.
% e.g:
% ?- get_emotion(memory{'joc fotbal': 2, 'joc box': 3}, Tag).
% Tag = sport.
%
%tag-urile sunt generalizari ale emotiilor
%un tag este un cuvant(sport/film)
% primeste user memory, cauta toate tagurile si intoarce tag-ul cu
% scorul de frecventa cea mai mare
% mai multe taguri cu acelasi scor: se ia primul tag

get_tag(_UserMemory, _Tag) :- fail.
