%% Popovici Stefan 323CA

:- ensure_loaded('chat.pl').

% Returneaza true dacă regula dată ca argument se potriveste cu
% replica data de utilizator. Replica utilizatorului este
% reprezentata ca o lista de tokens. Are nevoie de
% memoria replicilor utilizatorului pentru a deduce emoția/tag-ul
% conversației.
% match_rule/3
% match_rule(_Tokens, _UserMemory, rule(_, _, _, _, _)) :- fail.
match_rule(Tokens, UserMemory, rule(Expr, _, _, Emotions, Tags)) :- Tokens = Expr,
                                                                    get_emotion(UserMemory, Emotion),
                                                                    get_tag(UserMemory, Tag),
                                                                    member(Emotion, Emotions),
                                                                    member(Tag, Tags).
                                                                 
match_rule2(Tokens, UserMemory, rule(Expr, _, _, Emotions, _)) :- Tokens = Expr,
                                                                  get_emotion(UserMemory, Emotion),
                                                                  member(Emotion, Emotions).
                                                                     
match_rule3(Tokens, UserMemory, rule(Expr, _, _, _, Tags)) :- Tokens = Expr,
                                                              get_tag(UserMemory, Tag),
                                                              member(Tag, Tags).
                                                                 
match_rule4(Tokens, _, rule(Expr, _, _, Emotions, _)) :- Tokens = Expr,
                                                         Emotions = [].

% Primeste replica utilizatorului (ca lista de tokens) si o lista de
% reguli, iar folosind match_rule le filtrează doar pe cele care se
% potrivesc cu replica dată de utilizator.
% find_matching_rules/4
% find_matching_rules(+Tokens, +Rules, +UserMemory, -MatchingRules)
%find_matching_rules(Tokens, Rules, UserMemory, MatchingRules) :- findall(Rule, (member(Rule, Rules), match_rule(Tokens, UserMemory, Rule)), MatchingRules).

find_matching_rules(Tokens, Rules, UserMemory, MatchingRules) :- findall(Rule, (member(Rule, Rules), match_rule(Tokens, UserMemory, Rule)), MatchingRules1),
                                                                 findall(Rule, (member(Rule, Rules), match_rule2(Tokens, UserMemory, Rule)), MatchingRules2),
                                                                 findall(Rule, (member(Rule, Rules), match_rule3(Tokens, UserMemory, Rule)), MatchingRules3),
                                                                 findall(Rule, (member(Rule, Rules), match_rule4(Tokens, UserMemory, Rule)), MatchingRules4),
                                                                 append(MatchingRules1, MatchingRules2, M1),
                                                                 append(M1, MatchingRules3, M2),
                                                                 append(M2, MatchingRules4, MatchingRules).

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

% select_answer/5
% select_answer(+Tokens, +UserMemory, +BotMemory, -Answer, -Actions)
select_answer(Tokens, UserMemory, BotMemory, Answer, Actions) :- rules(Keywords, Rules),
                                                                 members_in_order(Keywords, Tokens),
                                                                 find_matching_rules(Tokens, Rules, UserMemory, MatchingRules),
                                                                 member(Rule1, MatchingRules), Rule1 = rule(_, Replies1, Actions1, _, _),
                                                                 member(Reply1, Replies1), get_answer(Reply1, BotMemory, Freq1),
                                                                 \+ (member(Rule2, MatchingRules), Rule2 = rule(_, Replies2, _, _, _),
                                                                     member(Reply2, Replies2), get_answer(Reply2, BotMemory, Freq2), Freq2 < Freq1),
                                                                 Answer = Reply1,
                                                                 Actions = Actions1,!.
                                                                 
% Verifica ca cuvintele cheie se afla in ordine in replica utilizatorului.            
% members_in_order/2
% members_in_order(+Keywords, +Message)                     
members_in_order([], _).                                
members_in_order([K|Rk], [W|Rw]) :- (K = W, members_in_order(Rk, Rw)),!; members_in_order([K|Rk], Rw).

% Esuează doar daca valoarea exit se afla in lista Actions.
% Altfel, returnează true.
% handle_actions/1
% handle_actions(+Actions)
handle_actions(Actions) :- not(member(exit, Actions)),!; fail.


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

% find_occurrences/2
% find_occurrences(+UserMemory, -Result)
find_occurrences(UserMemory, Count) :- find_string_occurrences(UserMemory, StrOcc),
                                       find_words_occurrences(StrOcc, [], WordsOcc),
                                       add_to_dict(WordsOcc, count{}, Count).
                                       
% Primeste o lista de perechi (cuvant, recventa) si formeaza un dictionar.
% add_to_dict/3
% add_to_dict(+WordsOcc, +Dict, -NewDict)      
add_to_dict([], Dict, Dict).
add_to_dict([(Word, Freq) | R], Dict, Res) :- NewDict = Dict.put(Word, Freq),
                                              add_to_dict(R, NewDict, Res).
                                       
% Returneaza o lista de perechi (string, frecventa).
% find_string_occurrences/2
% find_string_occurrences(+UserMemory, -StrOcc)
find_string_occurrences(UserMemory, Res) :- findall((Key, Val), Val = UserMemory.Key, Res).

% Returneaza o lista de perechi (cuvant, frecventa).
% Cuvintele vor avea apariții unice (fiind calculate frecventele totale).
% find_words_occurences/3
% find_words_occurences(+StrOcc, +Acc, -WordOcc)
find_words_occurrences([], Acc, Acc).
find_words_occurrences([(Str, Freq) | R], Acc, Res) :- update_words_occurrences(Str, Freq, Acc, NewAcc),
                                                       find_words_occurrences(R, NewAcc, Res).
                
% Primeste ca parametru un string si frecventa acestuia si actualizeaza lista de aparitii ale cuvintelor.               
% update_words_occurrences/4
% update_words_occurrences(+Str, +Freq, +Acc, +WordOcc)                     
update_words_occurrences(Str, Freq, Acc, Res) :- words(Str, Words),
                                                 addWords(Words, Freq, Acc, Res).
                
% Primeste o lista de cuvinte si frecventa acestora si actualizeaza lista de aparitii.              
% addWords/4
% addWords(+Words, +Freq, +Acc, -Result)
addWords([], _, Acc, Acc).
addWords([W|R], Freq, Acc, Res) :- addWord(W, Freq, Acc, NewAcc),
                                   addWords(R, Freq, NewAcc, Res).
            
% Primeste un cuvant si frecventa acestuia si actualizeaza lista de aparitii.           
% addWord/4
% addWord(+Word, +Freq, +Acc, -Result)
addWord(Word, Freq, Acc, Res) :- (member((Word, OldFreq), Acc), delete(Acc, (Word, OldFreq), Acc1), NewFreq is OldFreq + Freq, append(Acc1, [(Word, NewFreq)], Res)),!;
                                 append(Acc, [(Word, Freq)], Res).

% Atribuie un scor pentru fericire (de cate ori au fost folosit cuvinte din predicatul happy(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie fericit.
% get_happy_score/2
% get_happy_score(+UserMemory, -Score)
get_happy_score(UserMemory, Score) :- find_occurrences(UserMemory, Count),
                                      findall((Word, Freq), Freq = Count.Word, WordsOcc),
                                      get_happy_score_from_list(WordsOcc, 0, Score).
                                      
% get_happy_score_from_list/3
% get_happy_score_from_list(+WordsOcc, +Acc, -Score)
get_happy_score_from_list([], Acc, Acc).
get_happy_score_from_list([(Word, Freq) | R], Acc, Res) :- (happy(Word), NewAcc is Acc + Freq, get_happy_score_from_list(R, NewAcc, Res)),!;
                                                           get_happy_score_from_list(R, Acc, Res).

% Atribuie un scor pentru tristețe (de cate ori au fost folosit cuvinte din predicatul sad(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie trist.
% get_sad_score/2
% get_sad_score(+UserMemory, -Score)
get_sad_score(UserMemory, Score) :- find_occurrences(UserMemory, Count),
                                    findall((Word, Freq), Freq = Count.Word, WordsOcc),
                                    get_sad_score_from_list(WordsOcc, 0, Score).
                                      
% get_sad_score_from_list/3
% get_sad_score_from_list(+WordsOcc, +Acc, -Score)
get_sad_score_from_list([], Acc, Acc).
get_sad_score_from_list([(Word, Freq) | R], Acc, Res) :- (sad(Word), NewAcc is Acc + Freq, get_sad_score_from_list(R, NewAcc, Res)),!;
                                                         get_sad_score_from_list(R, Acc, Res).

% Pe baza celor doua scoruri alege emoția utilizatorul: `fericit`/`trist`,
% sau `neutru` daca scorurile sunt egale.
% e.g:
% ?- get_emotion(memory{'sunt trist': 1}, Emotion).
% Emotion = trist.
% get_emotion/2
% get_emotion(+UserMemory, -Emotion)
get_emotion(UserMemory, Emotion) :- get_happy_score(UserMemory, HappyScore),
                                    get_sad_score(UserMemory, SadScore),
                                    ((HappyScore > SadScore, Emotion = fericit),!;
                                    (HappyScore < SadScore, Emotion = trist),!;
                                    (HappyScore =:= SadScore, Emotion = neutru)).

% Atribuie un scor pentru un Tag (de cate ori au fost folosite cuvinte din lista tag(Tag, Lista))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să vorbească despre acel subiect.
% get_tag_score/3
% get_tag_score(+Tag, +UserMemory, -Score)
get_tag_score(Tag, UserMemory, Score) :- find_occurrences(UserMemory, Count),
                                         findall((Word, Freq), Freq = Count.Word, WordsOcc),
                                         get_tag_score_from_list(Tag, WordsOcc, 0, Score).
                                         
% get_tag_score_from_list/4
% get_tag_score_from_list(+Tag, +WordsOcc, +Acc, -Score)
get_tag_score_from_list(_, [], Acc, Acc).
get_tag_score_from_list(Tag, [(Word, Freq) | R], Acc, Res) :- tag(Tag, Keywords),
                                                              ((member(Word, Keywords), NewAcc is Acc + Freq, get_tag_score_from_list(Tag, R, NewAcc, Res)),!;
                                                              get_tag_score_from_list(Tag, R, Acc, Res)).

% Pentru fiecare tag calculeaza scorul și îl alege pe cel cu scorul maxim.
% Dacă toate scorurile sunt 0 tag-ul va fi none.
% e.g:
% ?- get_tag(memory{'joc fotbal': 2, 'joc box': 3}, Tag).
% Tag = sport.
% get_tag/2
% get_tag(+UserMemory, -Tag)
get_tag(UserMemory, ResTag) :- tag(Tag, _),
                               get_tag_score(Tag, UserMemory, Score),
                               \+ (tag(OtherTag, _), get_tag_score(OtherTag, UserMemory, OtherScore), OtherScore > Score),
                               (Score > 0, ResTag = Tag,!; ResTag = none),!.
                            