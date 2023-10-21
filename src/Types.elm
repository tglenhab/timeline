module Types exposing (..)


type Model
    = Setup SetupModel
    | Play PlayModel
    | MoreInfo Event Model
    | Wrong Event (Maybe Int) (Maybe Int) Model
    | Ended Int (List Event)


type alias SetupModel =
    { units : List Unit
    , size : Int
    , hardMode : Bool
    }


type alias PlayModel =
    { deck : List Event
    , active : Event
    , played : List Event
    , score : Int
    , hardMode : Bool
    }


type alias Event =
    { name : String
    , date : Int
    , desc : String
    }


type Msg
    = Guess (Maybe Int) (Maybe Int) -- Guess the *active* to be *between* event 1 & 2
    | Select Unit
    | LearnMore Event
    | Back
    | Start
    | ChangeNum String
    | HaveDeck (List Event)
    | ChangeHardMode


type Unit
    = I
    | II
    | III
    | VI
    | VII



-- Not great that this is needed


unitList : List Unit
unitList =
    [ I, II, III, VI, VII]


unitToStr : Unit -> String
unitToStr u =
    case u of
        I -> "I"
        II -> "II"
        III -> "III"
        VI -> "VI"
        VII -> "VII"

-- gives a (string) of the start and end dates of the unit
unitPeriod : Unit -> String
unitPeriod u =
    " (" ++ (case u of
                I  -> "17th Century"
                II -> "Period of Salutary Neglect"
                III -> "Federalist Period"
                VI -> "Reconstruction"
                VII -> "Gilded Age"
           )
        ++ ") "
                
