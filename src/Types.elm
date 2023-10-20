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
    | VI



-- Not great that this is needed


unitList : List Unit
unitList =
    [ I, II, VI]


unitToStr : Unit -> String
unitToStr u =
    case u of
        I -> "I"
        II -> "II"
        VI -> "VI"

-- gives a (string) of the start and end dates of the unit
unitDates : Unit -> String
unitDates u =
    "(" ++ (case u of
                I  -> "Colonial"
                II -> "1650-1780s"
                VI -> "Reconstruction"
                
