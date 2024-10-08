# Система оценивания допуска к экзамену

Получениe допуска необходимо для сдачи экзамена.<br>
Оценка за допуск ограничивает максимальную оценку, которую будет возможно получить на экзамене.

В решениях *задачи на E* проверяются **только тесты**.<br>
Решения крайне желательно сдать до какой-то пока неопределенной даты в декабре.<br>
**Промежуточных дедлайнов нет**.


## Дедлайны

Для всех остальных задач предлагается следующая система оценивания:

Каждая выданная задача разделена на **5 подзадач**.<br>
Незачет подзадачи снижает максимальную оценку на указанное количество баллов.

Каждая подзадача имеет свой дедлайн, состоящий из двух отметок времени:
- Дата, к которой необходимо выполнить подзадачу;
- Дата, к которой необходимо исправить замечания, указанные в ревью.

Таким образом если подзадача выдана на `(1 неделя на выполнение; 1 на исправление)`, то
- Необходимо создать PR c выполненной задачей и пройти CI в течение недели, после чего ожидать ревью;
- После получения ревью исправить замечания в течение недели.

**Пропуск любого из дедлайнов** ведет к **незачету** подзадачи, а соответственно снижению максимальной оценки.<br>
Если студент пытается исправить решение в срок, но никак не получается :(, то зачет/незачет ставится на усмотрение проверяющего (для зачета должна быть хотя бы одна попытка сдачи исправленного решения).


*Дедлайны предлагается устанавливать сразу после прочтения лекции по соответствующей теме.
При этом требуемый срок выполнения может различаться от темы к теме в зависимости от трудоемкости конкретных подзадач.*

## Разделение на подзадачи
Требуемый **минимальный процент покрытия** тестами для всех задач кроме первой - **80%**.

- `AST` (-0.5 балла)<br>
  Необходимо спроектировать AST выбранного ЯП.<br>Должно быть возможно представить реализацию функции факториала.<br>
  Пример представления факториала должен быть в проекте и успешно компилироваться.

- `Parsing 1` (-0.5 балла)<br>
  Необходимо реализовать часть парсера выбранного ЯП.<br>
  Парсер должен корректно обрабатывать реализацию функции факториала.<br>

- `Parsing 2` (-1 балл)<br>
  Необходимо доработать парсер выбранного ЯП.<br>
  Парсер должен корректно обрабатывать заранее предоставленный проверяющими модуль.<br>
  Парсер должен быть автоматически протестирован QuickCheck-подобным способом.

- `Type check / type inference`  (-1 балл)<br>
  Необходимо разработать type checker / inferencer для выбранного ЯП.<br>
  Type checker должен корректно обрабатывать заранее предоставленный проверяющими модуль.

- `Eval` (-1 балл)<br>
  Необходимо разработать непосредственно интерпретатор выбранного ЯП.<br>
  Интерпретатор должен корректно обрабатывать заранее предоставленный проверяющими модуль.
