% Определяем модуль с именем mobius
-module(mobius).

% Экспортируем функции для внешнего использования
-export([print_result/0, is_prime/1, prime_factors/1, sieve_of_eratosthenes/1, 
         precomputed_prime_factors/1,
         is_square_multiple/1, find_square_multiples/2]).

% Цель: Определяет, является ли заданное число простым.
% Входные данные: Целое число N.
% Выходные данные: true, если число простое; false в противном случае.
% Проверка числа N на простоту, начиная проверку с числа M
is_prime(N, M) when M*M > N -> true;
% Если M^2 больше N, то число простое
is_prime(N, M) -> 
  % Вычисляем остаток от деления N на M
  Remainder = N rem M,
  if
    % Если остаток равен 0, число не простое
    Remainder == 0 -> false;
    % Иначе проверяем следующий делитель
    true -> is_prime(N, M+1)
  end.

% 1 не является простым числом
is_prime(1) -> false;

% Общая функция для проверки простоты числа, начинаем с делителя 2
is_prime(N) when N > 1 -> is_prime(N, 2).

% Цель: Возвращает список простых сомножителей числа.
% Входные данные: Целое число N.
% Выходные данные: Список простых сомножителей N.
% Получение списка простых сомножителей числа N
prime_factors(N, Rest, M) when M*M > N -> [N|Rest];
% Если M^2 больше N, добавляем N к списку сомножителей

prime_factors(N, Rest, M) -> 
  ChkPrime = is_prime(M),
  % Проверяем, является ли M простым числом
  if
    ChkPrime == true, (N rem M) == 0 -> prime_factors(N div M, [M|Rest], M);
    % Если M простое и делит N, добавляем M к списку сомножителей и продолжаем с N/M
    M == 2 -> prime_factors(N, Rest, 3);
    % После проверки делителя 2 переходим к 3
    true -> prime_factors(N, Rest, M+2)
    % Для всех других случаев увеличиваем делитель на 2
  end.

prime_factors(N) -> lists:reverse(prime_factors(N, [], 2)).
% Возвращает список простых сомножителей в обратном порядке

% Цель: Генерирует список простых чисел до заданного N с использованием решета Эратосфена.
% Входные данные: Целое число N.
% Выходные данные: Список простых чисел до N.
% Создание списка простых чисел с использованием решета Эратосфена
sieve_of_eratosthenes(Prime, Max, Primes, Integers) when Prime > Max -> lists:reverse([Prime|Primes]) ++ Integers;
% Если текущее простое число больше максимального, возвращаем список простых чисел

sieve_of_eratosthenes(Prime, Max, Primes, Integers) ->
  [NewPrime|NewIntegers] = [ X || X <- Integers, X rem Prime =/= 0 ],
  % Фильтруем список, удаляя числа, делящиеся на Prime
  sieve_of_eratosthenes(NewPrime, Max, [Prime|Primes], NewIntegers).
  % Продолжаем с новым списком простых чисел

sieve_of_eratosthenes(1) -> [];
% Возвращает пустой список для N = 1

sieve_of_eratosthenes(N) -> sieve_of_eratosthenes(2, round(math:sqrt(N)), [], lists:seq(3,N,2)).
% Инициализирует решето для чисел от 2 до N

% Получение списка простых сомножителей с использованием предварительно вычисленного списка простых чисел
precomputed_prime_factors(_N, Rest, []) -> Rest;
% Если список простых чисел пуст, возвращаем накопленный список сомножителей

% Цель: Возвращает список простых сомножителей числа, используя предварительно вычисленный список простых чисел.
% Входные данные: Целое число N.
% Выходные данные: Список простых сомножителей N.
precomputed_prime_factors(N, Rest, [Prime|Primes]) ->
  if
    N rem Prime == 0 -> precomputed_prime_factors(N div Prime, [Prime|Rest], [Prime|Primes]);
    % Если N делится на Prime, добавляем Prime к списку сомножителей
    true -> precomputed_prime_factors(N, Rest, Primes)
    % Иначе продолжаем с оставшимися простыми числами
  end.

precomputed_prime_factors(N) -> lists:reverse(precomputed_prime_factors(N, [], sieve_of_eratosthenes(N))).
% Инициализирует функцию с пустым аккумулятором и списком простых чисел

% Цель: Определяет, делится ли число на квадрат простого числа.
% Входные данные: Целое число N.
% Выходные данные: true, если N делится на квадрат простого числа; иначе false.
% Проверка, делится ли число N на квадрат простого числа
is_square_multiple(N) -> 
  List = prime_factors(N),
  % Получаем список простых сомножителей N
  ListSize = erlang:length(List), 
  SetSize = sets:size(sets:from_list(List)),
  % Сравниваем размеры списка и множества сомножителей
  ListSize /= SetSize.
  % Если размеры не равны, значит есть повторяющиеся сомножители

% Цель: Находит первое число в последовательности заданной длины, где каждое число делится на квадрат простого числа.
% Входные данные: Длина последовательности Count и максимальное значение для поиска MaxN.
% Выходные данные: Первое число в последовательности или fail, если таковая не найдена.
% Поиск чисел, делящихся на квадрат простого числа
find_square_multiples(_N, _MaxN, List, Len, Count) when Len == Count -> 
  NewList = lists:reverse(List), 
  lists:nth(1, NewList);
  % Если найдена нужная последовательность, возвращаем первое число

find_square_multiples(N, MaxN, _List, _Len, _Count) when N > MaxN -> fail;
% Если достигнут MaxN и не найдена последовательность, возвращаем fail

find_square_multiples(N, MaxN, List, Len, Count) -> 
  ChkMultiple = is_square_multiple(N),
  % Проверяем, делится ли N на квадрат простого числа
  if
    ChkMultiple == true -> find_square_multiples(N+1, MaxN, [N|List], Len+1, Count);
    % Если да, добавляем N к списку и увеличиваем счетчик
    true -> find_square_multiples(N+1, MaxN, [], 0, Count)
    % Если нет, начинаем поиск заново с следующего числа
  end.

find_square_multiples(Count, MaxN) -> find_square_multiples(2, MaxN, [], 0, Count).
% Инициализирует поиск с начальными параметрами

% Цель: Выводит результаты поиска последовательностей чисел, делящихся на квадрат простого числа, заданной длины.
% Вывод результатов поиска последовательностей и замер времени выполнения
print_result() -> 
  Start = os:timestamp(),
  % Записываем начальное время
  io:format("Sequence length 4: ~w~n", [find_square_multiples(4, 30000)]),
  io:format("Sequence length 5: ~w~n", [find_square_multiples(5, 30000)]),
  io:format("Sequence length 6: ~w~n", [find_square_multiples(6, 30000)]),
  % Выводим результаты поиска для последовательностей длиной 4, 5, 6
  io:format("Total time: ~f seconds~n", [timer:now_diff(os:timestamp(), Start) / 1000000]).
  % Выводим общее время выполнения
