# Opis
Projekt w języku R mający na celu analizę danych z serwisu Stackoverflow.

**Prezentacja:** https://bit.ly/2UXmqn9

* Apple 4 minuty
* Szachy 5 minut
* Język programowania DS 1 min

# Zadania do zrobienia:
### 1. Opis tabel w każdym forum Stackoverflow
**Marcin:**
  * Badges
  
Osiagniecia moga byc całkiem ciekawe poniewaz swiadcza o działalnosci na forum, np osiagniecie “Famous
Question” oznacza, ze uzytkownik zadał pytanie, które miało co najmniej 10000 wyswietlen.

materiały dodtyczace Badges: 

https://stackoverflow.com/help/badges

https://stackoverflow.com/help/badges?tab=tags
  
  | Nazwa kolumny | Opis |
  |-|-|
  |UserId|Id uzytkownika|
  |Name |Nazwa osiagniecia|
  |Date |Data i czas zdobycia osiagniecia|
  |Class|3- osiagniecie brazowe; 2- srebrne; 1- złote|
  |TagBased|TagBased=“True” gdy zdobedzie sie sumarycznie Score >=“liczba” w pytaniach dotyczacych konkretnych tagów w non-community wiki. =“False” wpp|
  
  * Comments
  
  | Nazwa kolumny | Opis |
  |-|-|
  |Id|Po prostu Id konkretnego komentarza (wnioskuje z tego, ze jest unikalne dla kazdej pozycji)|
  |Post Id |Jest to Id konkretnego watku (wiele komentarzy moze byc w jednym watku)|
  |UserId |Id uzytkownika wstawiajacego komentarz (Brak jezeli uzytkownik został usuniety)|
  |Score|upvote-downvote dla konkretnego komentarza (Tak wyczytałem w internecie, chociaz jest to dosyc dziwne, nie znalazłem zadnego Score<0)|
  |UserDisplayName|UserDisplayName=NA oznacza, ze uzytkownik nie zmienił swojej domyslnej nazwy uzytkownika (czyli przypisanej sobie liczby)|
  |CreationDate|Data i czas utworzenia komentarza|
  
  * PostHistory
  * PostLinks

**Tomek**
* Posts

Tabela zawierająca najważniejszą cześć forum - treści postów.

| Nazwa kolumny | Opis |
|------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------|
| Id | Id posta |
| PostTypeId | Id typu posta. Rozróżniamy następujące: <ul><li>1 = Question</li><li>2 = Answer</li><li>3 = Wiki</li><li>4 = TagWikiExcerpt</li><li>5 = TagWiki</li><li>6 = ModeratorNomination</li><li>7 = WikiPlaceholder</li><li>8 PrivilegeWiki</li></ul> |
| AcceptedAnswerId | Id zaakceptowanej odpowiedzi (z zielonym ptaszkiem). Pole obecne tylko jeżeli PostTypeId = 1. |
| ParentId | Id pytania do którego została napisana odpowiedź. Pole obecne tylko jeżeli PostTypeId = 2. |
| CreationDate | Data napisania posta. |
| Score | Wynik posta, liczba którą widzimy pomiędzy strzałkami w górę i w dół. Zaczyna się od 0. Potem pozostali użytkownicy decydują jak ocenić post. |
| ViewCount | Ilość wyświetleń posta. Pole obecne tylko jeżeli PostTypeId = 1. |
| Body | Treść posta |
| OwnerUserId | Id użytkownika który napisał post. Pole obecne tylko jeżeli użytkownik nie został usunięty. |
| LastEditorUserId | Id użytkownika, który ostatni odpowiadał na post. |
| LastEditDate | Data ostatniej edycji posta. |
| LastActivityDate | Data ostatniej aktywności w poście |
| Title | Tytuł posta. Pole obecne tylko jeżeli PostTypeId = 1. |
| Tags | Tagi posta. Pole obecne tylko jeżeli PostTypeId = 1. |
| AnswerCount | Ilość odpowiedzi do posta/odpowiedzi, czyli takich z PostTypeId = 2. Nie chodzi tutaj o komentarze. Pole obecne tylko dla PostTypeId = 1 |
| CommentCount | Ilość komentarzy do posta. |
| FavoriteCount | Ile użytkowników dodało post do ulubionych. Do ulubionych można dodawać tylko posty, które są PostTypeId = 1 |

* Tags

Tabela zawierająca informacje o tagach, które potem pomagają wyszukiwać posty przez innych użytkowników.

| Nazwa kolumny | Opis |
|---------------|---------------------------------------------------------------------------------------------|
| Id | Id tagu. Klucz w tej tabeli |
| TagName | Nazwa tagu. Po prostu jednowyrazowe słowo wykorzystywane jako tag. Np: "intel", "macos",... |
| Count | Ilość użyć danego taga we wszystkich postach |
| ExcerptPostId | Nie za bardzo rozumiem co to jest, ale raczej do niczego się nie przyda. |
| WikiPostId | Nie za bardzo rozumiem co to jest, ale raczej do niczego się nie przyda. |

* Users

Tabela zawierająca bazę wszystkich użytkowników forum.

| Nazwa kolumny  | Opis
|----------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Reputation     | Kiedy inni użytkownicy głosują (UpVote - dodatni lub DwonVote - ujemny) na twoje posty (pytania i odpowiedzi), generujesz reputację.  Ilość punktów zależy od wielu czynników, ale mniej więcej po przeskalowaniu działa to w sposób:<br>sum(UpVote) + sum(downVote) = Reputation                 |
| CreationDate   | Data stworzenia użytkownika                                                                                                                                                                                                                                                                    |
| DisplayName    | Nick użytkownika                                                                                                                                                                                                                                                                               |
| LastAccessDate | Kiedy ostatnio był zalogowany                                                                                                                                                                                                                                                                  |
| WebsiteUrl     | Strona internetowa użytkownika                                                                                                                                                                                                                                                                 |
| Location       | Miejsce zamieszkania użytkownika. Zazwyczaj w formie Miasto, Państwo                                                                                                                                                                                                                           |
| AboutMe        | To co użytkownik napisał sam o sobie. Zazwyczaj kilka zdań.                                                                                                                                                                                                                                    |
| Views          | Ilość wyświetleń posta użytkownika (jedno IP może naliczyć się tylko raz w ciągu godziny)                                                                                                                                                                                                      |
| UpVotes        | UpVote powoduje przesuwanie treści "w górę", aby była widoczna dla większej liczby osób.<br>Zasady:   <ul><li>Awanse na pytanie dają popisowi reputację +5.</li><li>Awanse na odpowiedź dają replikatorowi reputację +10</li><li>Możesz głosować 30 razy dziennie w UTC plus 10 razy więcej tylko na pytania.</li></ul> |
| DownVotes      | DownVote przesuwa treść "w dół" strony, więc będzie ją widzieć mniej osób.<br>Zasady:<ul><li>Downvotes usuwa 2 reputacje od właściciela postu.</li><li>Downvotes na odpowiedzi usuwają 1 reputację od ciebie</li><li>Downvotes na pytania są bezpłatne</li><li>?Możesz głosować 30 razy w ciągu UTC.</li></ul>|

* Votes

Tabela zawierająca informacje o wszystkich głosach i interakcjach jakie użytkownicy zrobili z postami.

| Nazwa kolumny | Opis |
|---------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Id | Id głosu na dany post. |
| PostId | Id posta na który ktoś zagłosował. |
| VoteTypeId | Typ głosu. Rozróżniamy następujące: <ul><li>1 = AcceptedByOriginator</li><li>2 = UpVote</li>3 = DownVote</li><li>4 = Offensive</li><li>5 = Favorite (UserId will also be populated)</li><li>6 = Close</li><li>7 = Reopen</li><li>8 = BountyStart (UserId and BountyAmount will also be populated)</li><li>9 = BountyClose (BountyAmount will also be populated)</li><li>10 = Deletion</li><li>11 = Undeletion</li><li>12 = Spam</li><li>15 = ModeratorReview</li><li>16 = ApproveEditSuggestion</li></ul>|
| UserId | Kolumna obecna tylko jeżeli VoteTypeId = 5 albo 8. Przyjmuje wartość -1, jeżeli użytkownik został usunięty. |
| CreationDate | Data głosu. Podana tylko data - godzina została usunięta aby chronić tożsamość uzytkownika. |
| BountyAmount | Kolumna obecna tylko jeżeli VoteTypeId = 8 albo 9. Nie za bardzo rozumiem o co chodzi z tą kolumną, ale również nie wydaje mi się, żeby się do czegokolwiek przydała. |

### 2. Sprawdzenie pomysłów na analizę
**Marcin**
  * reputacja w zaleznosci od miejsca zamieszkania (regresja)
  * ego uzytkownika na podstawie opisu
  * w zaleznosci od stazu na forum ile user dostawal upvote
  * badges - jak user szybko zdobywal awans

**Tomek**
  * reputacja w zaleznosci od miejsca zamieszkania (wykres)
   * **TODO:** podwójny barplot aby na jednym wykresie pokazać ilość userów i ich ranking
  * po jakim czasie pada odpowiedz ktora jest oznaczona ptaszkiem
  * jakie są najbardziej popularne posty na wybranych forach
  
### 3. Ulepszenie funkcji wczytujących dane
Prototypowe wersje funkcji znajdują się *firstLookAtData.R*
