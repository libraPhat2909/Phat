module Duckling.Time.RU.Rules
  ( rules
  ) where
  
import Prelude
import Data.Text (Text)
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (isGrain)
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Ordinal.Types (OrdinalData(..))
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData(..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

--data
ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Понедельник","понедельни(к|ка|ку)|пн")
  , ( "вторник","вторни(к|ка|ку)|вт")
  , ( "среда","сред(а|у|е)|ср")
  , ( "четверг","четвер(г|га|гу)|чт")
  , ( "пятница","пятниц(а|ы|у)|пт")
  , ( "суббота","суббот(а|ы|у)|cб")
  , ( "воскресенье","воскресень(е|ю)|вс")
  ]

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ( "сейчас", TG.Second,  0, "сейчас"                 )
  , ( "сегодня", TG.Day,  0, "сегодня"              )
  , ( "завтра", TG.Day,  1, "завтра"                )
  , ( "вчера", TG.Day, -1, "вчера"                 )
  , ( "послезавтра", TG.Day,  2, "послезавтра"           )
  , ( "позавчера", TG.Day, -2, "позавчера"             )
  , ( "конец месяца", TG.Month,  1, "(конець|конця) месяця" )
  , ( "конец года", TG.Year,  1, "(конець|конця) года"   )
  ]  
  
 ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "Январь", "Янв(арь|аря|аре)|янв\\.?"   )
  , ( "февраль", "февр(аль|аля|але)|фев\\.?"   )
  , ( "март", "мар(т|та|те)|мар\\.?" )
  , ( "апрель", "апр(ель|еля|еле)|апр\\.?"  )
  , ( "май", "(май|мая|мае)|май\\.?"  )
  , ( "июнь", "и(юнь|юня|юне)|июн\\.?"  )
  , ( "июль", "и(юль|юля|юле)|июл\\.?"   )
  , ( "август", "авг(уст|уста|усте)|авг\\.?"  )
  , ( "сентябрь", "сент(ябрь|ября|ябре)|сент\\.?" )
  , ( "октябрь", "окт(ябрь|ября|ябре)|окт\\.?"  )
  , ( "ноябрь", "ноя(брь|бря|бре)?|ноя\\.?"    )
  , ( "декабрь", "дек(абрь|абря|абре)|дек\\.?"  )
  ]
--Пример : 10 января 
ruleDayofmonthNonOrdinalNamedmonth :: Rule
ruleDayofmonthNonOrdinalNamedmonth = Rule
  { name = "<day-of-month> (non ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  } 
 
--Пример : января 10 
 ruleNamedmonthDayofmonthNonOrdinal :: Rule
ruleNamedmonthDayofmonthNonOrdinal = Rule
  { name = "<named-month> <day-of-month> (non ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }
  
--Пример: шестого марты
 ruleDayofmonthordinalNamedmonth :: Rule
ruleDayofmonthordinalNamedmonth = Rule
  { name = "<day-of-month>(ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }
--Пример:  марты шестого
 ruleNamedmonthDayofmonthOrdinal :: Rule
ruleNamedmonthDayofmonthOrdinal = Rule
  { name = "<named-month> <day-of-month> (ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }
--пятнацатого
 ruleDayofmonthOrdinal :: Rule
ruleDayofmonthOrdinal = Rule
  { name = "<day-of-month> (ordinal)"
  , pattern =
    [ Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = v}:_) ->
        tt $ dayOfMonth v
      _ -> Nothing
  }
 --Пример седьмого января 2019
ruleDayofmonthordinalNamedmonthYear :: Rule
ruleDayofmonthordinalNamedmonthYear = Rule
  { name = "<day-of-month>(ordinal) <named-month> year"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    , regex "(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (token:
       Token Time td:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
        n <- parseInt match
        dom <- intersectDOM td token
        Token Time <$> intersect dom (year n)
      _ -> Nothing
  }
ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isNotLatent
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }  
ruleAbsorbInMonthYear :: Rule
ruleAbsorbInMonthYear = Rule
  { name = "in <named-month>|year"
  , pattern =
    [ regex "в"
    , Predicate $ or . sequence [isAMonth, isGrainOfTime TG.Year]
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }
ruleYear :: Rule
ruleYear = Rule
  { name = "year"
  , pattern =
    [ Predicate $ isIntegerBetween 1000 2100
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        y <- getIntValue token
        tt $ year y
      _ -> Nothing
  }
  
--время

--Абсолютное время: в 9:00 
ruleAtTimeofday :: Rule
ruleAtTimeofday = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "в"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }
--Абсолютное время: 9:30 ( hour:minutes
ruleHhmm :: Rule
ruleHhmm = Rule
  { name = "hh:mm"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.ч]([0-5]\\d)(?:годин(и|і|а)?|ч)?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        h <- parseInt m1
        m <- parseInt m2
        tt $ hourMinute False h m
      _ -> Nothing
  }
--Абсолютное время: 0-23 
ruleTimeofdayLatent :: Rule
ruleTimeofdayLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour (n < 12) n
      _ -> Nothing
  }
--Абсолютное время: 10 часов
ruleTimeofdayOclock :: Rule
ruleTimeofdayOclock = Rule
  { name = "<time-of-day>  o'clock"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "час(а|сов)?|ч(?:[\\s'\"-_{}\\[\\]()]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }
 
--Абсолютное время: 10 часов ночи
ruleTimePartofday :: Rule
ruleTimePartofday = Rule
  { name = "<time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }
  
 
 ruleLastCycleOfTime2 :: Rule
ruleLastCycleOfTime2 = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "последний"
    , dimension TimeGrain
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
 }
 
--относительное время : через <duration> 2 часа
 ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in <duration>"
  , pattern =
    [ regex "через"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }
 
--относительное время : 10 минут назад
 ruleDurationAgo :: Rule
ruleDurationAgo = Rule
  { name = "<duration> ago"
  , pattern =
    [ dimension Duration
    , regex "назад"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:_) ->
        tt $ durationAgo dd
      _ -> Nothing
  }

--относительное время : в течении
 ruleWithinDuration :: Rule
ruleWithinDuration = Rule
  { name = "within <duration>"
  , pattern =
    [ regex "в течении"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> Token Time <$>
        interval TTime.Open now (inDuration dd)
      _ -> Nothing
  }
--относительное время : до 10 часов
ruleUntilTimeofday :: Rule
ruleUntilTimeofday = Rule
  { name = "until <time-of-day>"
  , pattern =
    [ regex "до"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.Before td
      _ -> Nothing
  }

ruleAfterTimeofday :: Rule
ruleAfterTimeofday = Rule
  { name = "after <time-of-day>"
  , pattern =
    [ regex "после"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.After td
      _ -> Nothing
  }

 
  rules :: [Rule]
   rules =
    [ruleDayofmonthordinalNamedmonth
    , ruleDayofmonthOrdinal
    , ruleAfterTimeofday
    , ruleTimeofdayLatent
    , ruleDayofmonthNonOrdinalNamedmonth 
    , ruleNamedmonthDayofmonthNonOrdinal 
    , ruleNamedmonthDayofmonthOrdinal
    , ruleLastTime
    , ruleIntersect
    , ruleYear
    , ruleHhmm
    , ruleTimeofdayOclock
    , ruleTimePartofday
    , ruleDayofmonthordinalNamedmonthYear
    , ruleAtTimeofday
    , ruleLastCycleOfTime2
    , ruleInDuration
    , ruleDurationAgo
    , ruleWithinDuration
    , ruleUntilTimeofday
    ]
    ++ ruleInstants
    ++ ruleDaysOfWeek
    ++ ruleMonths