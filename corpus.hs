{-# LANGUAGE OverloadedStrings #-}
module Duckling.Time.RU.Corpus
(corpus
)where

import Data.String
import Predule

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month, refTime)
import Duckling.TimeGrain.Types hiding (add)

context :: Context
context = testContext {locale = makeLocale RU Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
    [examples(datetime (2019,12,15,0,0,0) Day)
           [ "сегодня"
           ]
	, examples (datetime(2019,12,15,3,55,0) Second)
	       [ "сейчас"
           ]
    , examples (datetime (2019,12,16,0,0,0) Day)
           [ "завтра"
           ]
    , examples (datetime (2019, 12, 14, 0, 0, 0) Day)
           [ "вчера"
           ]
    , examples (datetime (2019, 12, 23, 0, 0, 0) Day)
           [ "понеделник"
           ]
    , examples (datetime (2019, 12, 23, 0, 0, 0) Day)
           [ "18 декабря"
           ]
    , examples (datetime (2019, 12, 24, 0, 0, 0) Day)
           [ "вторник"
           ]
    , examples (datetime (2019, 12, 25, 0, 0, 0) Day)
           [ "вторник"
           ]
    , examples (datetime (2019, 12, 26, 0, 0, 0) Day)
             [ "четверг"
             ]
    , examples (datetime (2019, 12, 27, 0, 0, 0) Day)
             [ "пятница"
             ]
    , examples (datetime (2019, 12, 28, 0, 0, 0) Day)
             [ "суббота"
             ]
    , examples (datetime (2019, 12, 29, 0, 0, 0) Day)
             [ "воскресенье"
             ]
    , examples (datetime (2019, 12,  1, 0, 0, 0) Day)
             [ "1 декабря"
               "первого декабря"
             ]
    , examples (datetime (2019,  10,  10, 0, 0, 0) Day)
             [ "10 декабря 2019 г."
               "десятого декабря 2019"
               "10 декабря 2019 года"
             ]
    , examples (datetime (2019, 10, 11, 0, 0, 0) Day)
             [ "11 декабря"
             , "11.10"
             , "одиннацадьтого декабря"
             ]
    , examples (datetime (2019, 10,  0, 0, 0, 0) Month)
             [ "в декабре 2019"
             ]
    , examples (datetime (2019, 11,  0, 0, 0, 0) Month)
             [ "в ноябре 2019"
             ]
    , examples (datetime (2019, 10, 31, 0, 0, 0) Day)
             [ "31.10.2019"
             , "31.10.19"
             ]
    , examples (datetime (2019, 12, 13, 0, 0, 0) Day)
             [ "позавчера"
             ]
    , examples (datetime (2019,12,17,0,0,0) Day)
			 [ "послезавтра"
			 ]
    , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14 апреля 2015"
             ]
    , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "в воскресенье 10 февраля"
             ]
    , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "в среду 13 фев."
            ]
    , examples (datetime (2019, 12, 9, 0, 0, 0) Week)
             [ "эта неделя"
			   "на этой неделе"
             ]
    , examples (datetime (2019, 12, 2, 0, 0, 0) Week)
             [ "прошлая неделя"
			   "на прошлой неделе"
             ]
    , examples (datetime (2019, 12, 16, 0, 0, 0) Week)
             [ "следующая неделя"
			   "на следующей неделе"
             ]
    , examples (datetime (2020, 1, 0, 0, 0, 0) Month)
             [ "следующий месяц"
			   "в следующем месяце"
             ]
    , examples (datetime (2019, 11, 0, 0, 0, 0) Month)
             [ "прошлый месяц"
			   "в прошлом месяце"
             ]
    , examples (datetime (2018, 0, 0, 0, 0, 0) Year)
             [ "прошлый год"
			   "в прошлом году"
             ]
    , examples (datetime (2019, 0, 0, 0, 0, 0) Year)
             [ "этот год"
			   "в этом году"
             ]
    , examples (datetime (2020, 0, 0, 0, 0, 0) Year)
             [ "следующий год"
			   "в следующем году"
             ]
    , examples (datetime (2019, 12, 8, 0, 0, 0) Day)
			 [ "прошлое воскресенье"
			   "в прошлое воскресенье"
			   "воскресенье на прошлой неделе"
             ]
    , examples (datetime (2019, 12, 3, 0, 0, 0) Day)
             [ "в прошлый вторник"
             ]
    , examples (datetime (2019, 12, 17, 0, 0, 0) Day)
             [ "в следующий вторник"
			 ]
	]
	
	