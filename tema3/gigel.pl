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
  Tokens = Expresie, ord_subset(Token, Expresie).

% Primeste replica utilizatorului (ca lista de tokens) si o lista de
% reguli, iar folosind match_rule le filtrează doar pe cele care se
% potrivesc cu replica dată de utilizator.
%
% primele 3 argumente se dau
% matchingRules e de iesire
% rules este o lista de rule (adica o lista de structuri rule)
% din toate rule ia doar rule-urile care au match iut cu tokens
find_matching_rules(Tokens, [HRule|RestRules], UserMemory, MatchingRules) :-
    findall(HRule, match_rule(Tokens, UserMemory, HRule), MatchingRules).

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

% intoarce acea replica care a fost utilizata de cele mai putine ori de
% gigel
% face o lista de perechi valoarea (replica) + key (de cate ori a fost
% zisa) pentru replicile care au facut match
get_used_rules_as_list([Rule|RestRule], BotMemory, ReplyList) :-
  [(Rule, BotMemory.get(Rule))|ReplyList], get_used_rules_as_list(RestRules, BotMemory, ReplyList).

get_reply(rule(_, [H|Rules], _, _, _), BotMemory, H).

tail([_|Tail], Tail).

% check_rule reuseste daca replica memorata in regula primita la intrare NU este [nu, inteleg]
check_rule(rule(_, Reply, _, _ , _)) :- head(Reply, H), \+ (H == [nu, inteleg]).

%delete_non_answer(RulesList, Rules_Result) :-

% ord_subset intoarce true daca
snd_get_rules(Tokens, RulesList) :- rules(Tokens, RulesList).
get_rules(Tokens, RulesList) :-
 snd_get_rules(Tokens, RulesList);
 (head(Tokens, H), rules([H], RulesList)); (tail(Tokens, Tail), get_rules(Tail, RulesList)).

% primeste o lista de regula si le separa doar pe cele care nu contin replica [nu, inteleg]
select_rules([H|Rest], Result) :-
    findall(H, check_rule(H), Result).

get_action(rule(_,_,Action,_,_), Action).

add_new_answer(Answer, BotMemory, NewMemory):- NewMemory = BotMemory.put(Answer, 1).

update_memory(NewMemory, NewMemory).

% pentru situatia in care memoria botului este goala
select_answer(Tokens, UserMemory, BotMemory, Answer, Action) :-
    get_rules(Tokens, RulesList),
    select_rules(RulesList, Rules),
    find_matching_rules(Tokens, Rules, UserMemory, MatchingRules), head(MatchingRules, H),
    get_reply(H, BotMemory, Answer), get_action(H, Action),
    BotMemory == memory{}.

% atunci cand memoria botului nu este goala, se alege replica cu ce mai mica utilizare de pena acum
select_answer(Tokens, UserMemory, BotMemory, Answer, Action) :-
    get_rules(Tokens, RulesList),
    select_rules(RulesList, Rules),
    find_matching_rules(Tokens, Rules, UserMemory, MatchingRules), head(MatchingRules, H),
    get_action(H, Action),
    get_all_replies(H, Replies), %extrage lista de replici pentru prima regula care a facut match
    get_replies_list(Replies, BotMemory, [], RulesAsList),
    reverse(RulesAsList, Rez),
    min_element(Rez, Answer).


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

count_words(Token, Word_Dict) :-  Word_Dict.put(Token, Word_Dict.get(Token) + 1).

find_occurrences(Memory, _Result) :- fail.

% Atribuie un scor pentru fericire (de cate ori au fost folosit cuvinte din predicatul happy(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie fericit.
%
% frecventa pentru cuvintele de fericirex
get_happy_score(_UserMemory, _Score) :- fail.

% Atribuie un scor pentru tristețe (de cate ori au fost folosit cuvinte din predicatul sad(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie trist.
get_sad_score(_UserMemory, _Score) :- fail.

% Pe baza celor doua scoruri alege emoția utilizatorul: `fericit`/`trist`,
% sau `neutru` daca scorurile sunt egale.
% e.g:
% ?- get_emotion(memory{'sunt trist': 1}, Emotion).
% Emotion = trist.
%
% intoarce emotia cu scorul mai mare
% daca sunt egale, da neutru
get_emotion(_UserMemory, _Emotion) :- fail.

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
