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
           [ "cреда"
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
      ,examples (datetime (2019, 9,  3, 0, 0, 0) Day)
             [ "3 марты"
               "третьего марты"
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
    , examples (datetime (2019, 12,  0, 0, 0, 0) Month)
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
    , examples (datetime (2019, 4, 14, 0, 0, 0) Day)
             [ "14 апреля 2019"
             ]
    ,examples (datetime(2019,12,19,4,25,0) Minute))
            [ 
             "4:25"
             "4 часов 25"
            ]
    ,examples (datetime(2019,12,19,4,0,0) Hour)
            [
              " в 4 часов утра"
            ]
    ,examples (datetime(2019,12,19,16,0,0) Hour)
            [
              " в 4 часов вечера"
            ]
    , examples (datetime(2019,12,15,5,55,0) Minute)
            [ "через 2 часа"
            ]
    , examples (datetime(2019,12,15,3,58,0) Second)
            [ "через 3 минуты"
            ]
    , examples (datetime(2019,12,15,1,55,0) Minute)
            [ "2 часа назад"
            ]
    , examples (datetime(2019,12,15,3,50,0) Second)
            [ "5 минут назад "
            ]
    ,examples (datetime(2019,12,15,14,0,0) Hour))
            [
              "до 2 часов дня"
              "до 14 часов дня"
            ]
    ,examples (datetime(2019,12,15,5,0,0) Minute)
            [
              "после 5 часов"
            ]                  
    
     ]
	
	
