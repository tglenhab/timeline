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
    = VI
    | VII
    | VIII
    | IX
    | X



-- Not great that this is needed


unitList : List Unit
unitList =
    [ VI, VII, VIII, IX, X ]


unitToStr : Unit -> String
unitToStr u =
    case u of
        VI ->
            "VI"

        VII ->
            "VII"

        VIII ->
            "VIII"

        IX ->
            "IX"

        X ->
            "X"
