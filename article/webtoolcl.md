# Как написать веб-утилиту небольшого размера на Common Lisp

Когда-то написал небольшую программку для сообщества игры [Prime World](http://ru.playpw.com).
Она парсит [таблицу рейтингов](http://ru.playpw.com/ratings.html) и составляет по ней линейные графики, плюс вычислят средний рейтинг (в рейтинге указываются первые 25 мест) по каждому персонажу.
Персонажей 62, соответственно, линий на графике тоже 62. :)

Здесь напишу, как и из чего сделал и как оно работает.

[Полный исходный код выложил к себе на GitHub](https://github.com/hijarian/Prime-World-Dno).

## Как работает

План был такой: приложение представляет собой одну HTML страницу, с одним файлом CSS и одним Javascript.

При загрузке страницы яваскрипт сразу делает AJAX-запрос на бэкэнд за данными.

Бэкэнд грабит исходную HTML-страницу, при помощи XPath выбирает значения рейтингов, имена игроков, места и имена персонажей.
Затем кодирует это всё в JSON и отправляет скрипту обратно.

Скрипт отрисовывает полученные от бэкэнда рейтинги в виде графика при помощи библиотеки [Highcharts](http://www.highcharts.com/).
Дополнительно бэкэнд возвращает вычисленные средние значения рейтинга; они отображаются в виде таблицы, которая обрабатывается javascript библиотекой [DataTables](http://www.datatables.net/), чтобы добавить сортировку.

Между таблицей и графиком переключаемся кнопками.

[Вот само приложение](https://pw-dno.herokuapp.com/).

Как я уже сказал, приложение достаточно маленькое.

В качестве недостатка можно упомянуть то, что каждый раз, когда приложение открывается, делается запрос на исходный сайт Нивала.
Делать так, конечно, дурной тон, и стоит прикрутить какое-нибудь кэширование на стороне сервера. 
Хотя у них всё равно топ-25 не так часто обновляется, да и посещаемость никакая, так что для начала сойдёт.

## Как собрано

Серверная часть основана на [Hunchentoot](http://weitz.de/hunchentoot/), то есть, мы сами себе веб-сервер.

Вот все необходимые URL:

1. `/`, по которому возвращается HTML страница приложения.
2. `/data`, который AJAX endpoint, возвращающий JSON объект с данными
3. `/assets/scripts.js`, клиентский скрипт, который раскладывает данные из JSON в график и таблицу
4. `/assets/styles.css`, бумажка со стилями (с украшениями я не парился, в стилях только сокрытие некоторых элементов для инициализации интерфейса).
5. `/assets/loading.gif`, показываем эту картинку пока `/data` грузится.
6. `/favicon.ico`, иконка до кучи, честно стырил из пакета [Html5 Boilerplate](http://html5boilerplate.com/).

Хостимся на [Heroku](https://www.heroku.com/).

### Извлечение исходных данных

Сам парсинг исходной веб-страницы полностью заключён в одном файле под названием `dno.lisp`, и единственная функция, которая оттуда нужна публично, это `send-data-from-origin-as-json`, которая генерирует строку в JSON формате, содержащую данные, нужные в `scripts.js` для того, чтобы построить графики и таблицу.
[Сам файл `dno.lisp` можно посмотреть в репозитарии на гитхабе](https://github.com/hijarian/Prime-World-Dno/blob/master/src/dno.lisp), я здесь его приводить не буду, слишком большой.

В итоге мы можем делать `(load "dno.lisp")` и получать с этого функцию, которая даст нам JSON, который мы можем возвращать как результат AJAX запроса, то есть, то, что нужно для пункта 2.

### Отдача статических файлов

В идеологии hunchentoot'а (если не углубляться в детали), инициализация приложения выглядит так:

1.  Настраиваем все пути в приложении, вызывая `hunchentoot:create-folder-dispatcher-and-handler`, `hunchentoot-create-static-file-dispatcher-and-handler`, `hunchentoot:define-easy-handler` и т. п.
    Это как раз было упрощено при помощи хелперов в предыдущем пункте.

2.  Создаём экземпляр приложения заклинанием:
    
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))

Для начала делаем три хелпера в отдельном файле, для того, чтобы добавление путей в Hunchentoot не было пыткой:

**helpers.lisp** 

    (defun publish-directory (uri dirname)
      "Makes files in given <dirname> accessible under the URL prefix <uri>. <dirname> should be in relative directory path format, e. g. \"foo/bar/baz\""
      (push (hunchentoot:create-folder-dispatcher-and-handler uri dirname)
            hunchentoot:*dispatch-table*))

    (defun publish-file (uri filename)
      (push (hunchentoot:create-static-file-dispatcher-and-handler uri filename)
            hunchentoot:*dispatch-table*))

    (defmacro publish-ajax-endpoint (uri name params &body body)
      "<name> is a name of handler function, <uri> is a relative URI for endpoint, <params> is a list of symbol names of expected params in request>"
      `(hunchentoot:define-easy-handler (,name :uri ,uri) ,params 
         (setf (hunchentoot:content-type*) "application/json")
         ,@body))

Имея эти три хелпера, файл инициализации, фактически, будет иметь следующий вид:

**init.lisp**

    ;; Webroot
    (publish-file "/" "webroot/index.html")

    ;; Favicon, just because
    (publish-file "/favicon.ico" "webroot/favicon.ico")

    ;; Images, CSS and JS files referenced from index.html
    (publish-directory "/assets/" "assets/")

    ;; AJAX endpoint to grab data to display in chart
    (publish-ajax-endpoint "/data" data () (send-data-from-origin-as-json))

    (defun run ()
      "Launch Hunchentoot web server instance on default port"
      (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))

Само содержимое файлов [`index.html`](https://github.com/hijarian/Prime-World-Dno/blob/master/webroot/index.html), [`scripts.js`](https://github.com/hijarian/Prime-World-Dno/blob/master/assets/scripts.js) и [`styles.css`](https://github.com/hijarian/Prime-World-Dno/blob/master/assets/styles.css) можно посмотреть на гитхабе, здесь я описываю только то, как приложение собрано, а не как оно работает.

Мы не можем сделать `(publish-directory "/" "webroot/")`, потому что мы хотим дефолтный хэндлер для пути `/`.

Я завернул старт приложения в отдельную функцию (и не вызываю её сразу же), потому что так будет удобнее в дальнейшём.

Теперь мы можем делать так:

    (load "dno.lisp")
    (load "helpers.lisp")
    (load "init.lisp")
    (run)

После чего открываем браузер по адресу http://localhost:4242/ и наблюдаем готовое приложение, при условии, что в том же каталоге, что и эти два скрипта, находятся папки `webroot` и `assets` с соответствующими файлами в них.

### Структурирование

Теперь запакуем всё в ASD.
Вот моё определение приложения:

**dno.asd**

    (asdf:defsystem #:dno
        :serial t
        :description "Presents ratings of players of Prime World as charts."
        :author "Mark Safronov <hijarian@gmail.com>"
        :license "Public Domain"
        :depends-on (#:drakma
                     #:cl-ppcre
                     #:cl-libxml2
                     #:iterate
                     #:cl-json
                     #:hunchentoot)
        :components ((:file "package")
                     (:module :src
                              :serial t
                              :components ((:file "helpers")
                                           (:file "dno")
                                           (:file "init")))))

**package.lisp**

    (defpackage #:dno
      (:export :run)
      (:use #:cl
            #:cl-user 
            #:hunchentoot))

Как видно, все три исходника на Common Lisp, описанные выше, были переложены в отдельный подкаталог `src`. 
Понятное дело, во всех файлах теперь первой строчкой идёт вызов `(in-package :dno)`.

Заметьте, что в ASD прописаны библиотеки, от которых зависит приложение (`cl-ppcre`, `cl-libxml2`, `iterate` и `cl-json` используются только в `dno.lisp`, на самом деле), однако сам пакет (`package.lisp`) не подключает эти библиотеки.
Это просто дело вкуса: я предпочитаю использовать символы из чужих пакетов по их полному имени, не сокращая.
Так сразу понятно, в какую документацию лезть.

Теперь у нас такая структура файлов:

    assets/
        scripts.js
        styles.css
    webroot/
        index.html
        favicon.ico
    src/
        dno.lisp
        init.lisp
        helpers.lisp
    dno.asd
    package.lisp

### Запуск на локальной машине

Так как у нас теперь определён ASD пакет, мы можем воспользоваться механизмами [Quicklisp](http://www.quicklisp.org) для того, чтобы максимально просто загружать приложение на локальной машине.

Если в рабочем каталоге Quicklisp в подкаталоге local-projects сделать символическую ссылку на каталог приложения, то можно будет подключать пакет приложения простым вызовом `(ql:quickload :dno)`.

Таким образом, кладём в корневой каталог приложения следующий скрипт запуска:

**runner.lisp**

    (in-package :cl-user)
    (ql:quickload :dno)
    (dno:run)

Теперь видно, зачем нужна отдельная функция `run`, определённая в `init.lisp`: для того, чтобы разделить загрузку самого приложения в рантайм и запуск приложения как веб-сервера.

Всё, готовое веб-приложение можно запустить из консоли, например, так:

    $ cd path/to/dno/app
    $ sbcl
    * (load "runner.lisp")

Понятное дело, вместо SBCL может быть любой Лисп на ваш выбор.

Консоль становится неюзабельной после этого.
Когда понадобится загасить приложение — нажатие Ctrl+D убивает приложение вместе с рантаймом лиспа.

### Деплой на Heroku

Благодаря [специальному buildpack'у для CL](http://github.com/jsmpereira/heroku-buildpack-cl.git) появилась возможность хостить SBCL + Hunchentoot приложения на Heroku.

Для этого нужно подготовить приложение следующим образом:

1. [Настраиваем у себя подключение к Heroku](https://devcenter.heroku.com/articles/quickstart).
2. `heroku create -s cedar --buildpack http://github.com/jsmpereira/heroku-buildpack-cl.git`. Запоминаем название проги, которое Heroku нам сгенерировало.
3. `heroku labs:enable user-env-compile -a myapp`, вместо myapp пишем название проги из п.2.
4. `heroku config:add CL_IMPL=sbcl`
5. `heroku config:add CL_WEBSERVER=hunchentoot`
6. `heroku config:add LANG=en_US.UTF-8`

Теперь в корневом каталоге приложения нужно добавить следующий скрипт, который ожидает buildpack:

**heroku-setup.lisp**

    (in-package :cl-user)
    (load (merge-pathnames *build-dir* "dno.asd"))
    (ql:quickload :dno)

Крайне важно то, что этот скрипт *не* выполняет `(dno:run)`, как это делает `runner.lisp`, потому что buildpack сам запустит Hunchentoot, так что от `init.lisp` требуется только настроить пути.

После того, как этот скрипт добавлен в репозитарий (имя `heroku-setup.lisp` *имеет* значение), можно пушить: `git push heroku master`, при условии, что п. 1 выполнен полностью.
    
## Поздравляю

Всё, теперь прога готова, есть как возможность запустить на локальной машине, так и деплой сразу в Сеть (доменное имя тоже вполне приличное получается).

По тому же принципу можно строить любые другие простые веб-приложения, добавляя в `init.lisp` определения для путей в приложении.

Конечно же, если прога сложная, то придётся делать какой-то кустарный роутер, а также скорее всего, не отдавать статичные HTML файлы, а генерировать страницы при помощи какого-нибудь шаблонизатора типа [CL-WHO](http://weitz.de/cl-who/), [CL-EMB](http://common-lisp.net/project/cl-emb/) или чего-то подобного. 
Яваскрипт и CSS тоже можно генерировать прямо из Common Lisp, для этого есть проекты [Parenscript](http://common-lisp.net/project/parenscript/) и [css-lite](https://github.com/paddymul/css-lite) соответственно. 

Мне всё это не понадобилось, приложение достаточно простое чтобы сразу отдавать статичные ассеты и HTML.


