# Recenzja projektu Functional-programming-project

###### Autor recenzji: Mieszko Makuch

###### Autorki recenzowanego projektu: Joanna Palewicz, Justyna Maciąg


## Spostrzeżenia

1. Projekt został podzielony na **moduły** co tworzy logiczny podział i pomaga w zrozumieniu kodu.
2. W kodzie występują liczne komentarze, na plus zaliczyć trzeba też zbudowanie projektu ze **stackiem** i wygenerowanie dokumentacji **haddock**.
	
    Przykład funkcji z odpowiednim komentarzem:
    ```haskell
    -- |Function which is using 'useFunction' in order to check if is it a upper letter
    upper :: Parser Char
    upper = useFunction (\x -> 'A' <= x && x <= 'Z')
    ```
3. Kod został napisany w sposób estetyczny, wcięcia i odpowiednie odstępy od deklarowanych funkcji poprawiają czytelność kodu.
4. Nazwy funkcji i zmiennych dobrze określają swoje przeznaczenie.
3. W projekcie nie wykryłem żadnych poważnych błędów logicznych, wszystkie przetestowane przeze mnie funkcje działały prawidłowo.
4. Zaletą jest także przeprowadzenie testów jednostkowych **HUnit**.	
    Test HUnit:
    ```haskell
    test 1 = testCase  $ assertEqual "should return 3 " 3 (countLetters "ala")
    ```

## Uwagi
Przy dalszej rozbudowie projektu myślę, że warto byłoby zwrócić uwagę na następujące rzeczy:

1. Program mógłby obsługiwać większą ilość argumentów.
2. Moduł ParserMonads mógłby zostać rozdzielony na mniejsze pod moduły.

## Podsumowanie
Przegląd projektu innego zespołu pomógł mi zrozumieć jak ważne jest pisanie czytelnego kodu. Programując powinniśmy myśleć osobach, które w przyszłości będą odpowiedzialne za refaktoryzację i utrzymanie naszego kodu.

Myślę, że autorki recenzowanego projektu wzięły to pod uwagę, jest to w mojej ocenie kod czytelny i dobrej jakości.