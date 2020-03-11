module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, p, button, input)
import Html.Attributes exposing (src, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import String exposing (fromInt)
-- https://package.elm-lang.org/packages/elm/http/latest/Http


---- MODEL ----

type PostHumanError = PostHumanError String
type HttpResponse = HttpResponse String

type alias Model =
    { url : String
    , response : Maybe HttpResponse
    , dataFields : List (String, String)
    , error : Maybe PostHumanError
    }


init : ( Model, Cmd Msg )
init =
    ( { url = ""
      , error = Nothing
      , response = Nothing
      , dataFields = []
      }
      , Cmd.none )



---- UPDATE ----


type Msg
    = UrlUpdated String
    | HttpResponseReturned (Result Http.Error String)
    | SendHttpRequestButtonClicked
    | KeyUpdated Int String
    | ValueUpdated Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequestButtonClicked ->
           ( model, getUrl model.url model.dataFields)

        HttpResponseReturned (Err error) ->
           ( { model | error = Just (toHttpError error), response = Nothing }, Cmd.none)


        HttpResponseReturned (Ok response) ->
            ( { model | response = Just <| HttpResponse response, error = Nothing }, Cmd.none)

        UrlUpdated url ->
            ( { model | url = url }, Cmd.none)

        KeyUpdated idx newKey ->
          if idx == List.length model.dataFields then
            ( { model | dataFields = model.dataFields ++ List.singleton (newKey, "") }, Cmd.none )
          else
            ( { model | dataFields = List.indexedMap (\i (k, v) -> if i == idx then (newKey, v) else (k, v) ) model.dataFields }, Cmd.none )

        ValueUpdated idx newValue ->
          if idx == List.length model.dataFields then
            ( { model | dataFields = model.dataFields ++ List.singleton ("", newValue) }, Cmd.none )
          else
            ( { model | dataFields = List.indexedMap (\i (k, v) -> if i == idx then (k, newValue) else (k, v) ) model.dataFields }, Cmd.none )

getUrl : String -> List (String, String) -> Cmd Msg
getUrl url dataFields =
  Http.get
    { url = toFullUrl url dataFields
    , expect = Http.expectString HttpResponseReturned
    }


toFullUrl : String -> List (String, String) -> String
toFullUrl url dataFields =
    url ++ "?" ++ (String.join "&" (List.map presentDataField dataFields))

presentDataField : (String, String) -> String
presentDataField (k, v) =
  k ++ "=" ++ v

toHttpError : Http.Error -> PostHumanError
toHttpError error =
  PostHumanError <|
    case error of
      BadUrl url ->
          url

      Timeout ->
          "timed out"

      NetworkError ->
          "network error"

      BadStatus status ->
          fromInt status

      BadBody body ->
          body

---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text <| toFullUrl model.url model.dataFields ]
        , input [ placeholder "URL", value model.url, onInput UrlUpdated ] []
        , viewDataFields model.dataFields
        , p [] [ text (displayError model.error) ]
        , div [] [ text (displayHttpResponse model.response)]
        , button [ onClick SendHttpRequestButtonClicked ] [ text "do stuff"]
        ]

viewDataFields : List (String, String) -> Html Msg
viewDataFields dataFields =
  div [] <| (List.indexedMap viewDataField dataFields) ++ [ viewDataField (List.length dataFields) ("","") ]

viewDataField : Int -> (String, String) -> Html Msg
viewDataField idx (k, v) =
  div []
    [ input [ placeholder "Key", value k, onInput (\s -> KeyUpdated idx s) ] []
    , input [ placeholder "Value", value v, onInput (\s -> ValueUpdated idx s) ] []
    ]

displayHttpResponse : Maybe HttpResponse -> String
displayHttpResponse httpResponse =
  case httpResponse of
    Nothing ->
      ""
    Just (HttpResponse raw) ->
      raw

displayError : Maybe PostHumanError -> String
displayError postHumanError =
  case postHumanError of
    Nothing ->
      ""
    Just (PostHumanError raw) ->
      raw

-- displayItem : Maybe a -> (String -> a) -> String
-- displayItem toItem extractor =
--   case toItem of
--     Nothing ->
--       ""
--     Just (extractor raw)->
--       raw

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
