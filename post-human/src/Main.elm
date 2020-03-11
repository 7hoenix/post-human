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
    , error : Maybe PostHumanError
    }


init : ( Model, Cmd Msg )
init =
    ( { url = ""
      , error = Nothing
      , response = Nothing
      }
      , Cmd.none )



---- UPDATE ----


type Msg
    = UrlUpdated String
    | HttpResponseReturned (Result Http.Error String)
    | NoOp
    | SendHttpRequestButtonClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
             ( model, Cmd.none )

        SendHttpRequestButtonClicked ->
           ( model, getUrl model.url )

        HttpResponseReturned (Err error) ->
           ( { model | error = Just (toHttpError error) }, Cmd.none)


        HttpResponseReturned (Ok response) ->
            ( { model | response = Just <| HttpResponse response }, Cmd.none)

        UrlUpdated url ->
            ( { model | url = url }, Cmd.none)

getUrl : String -> Cmd Msg
getUrl url =
  Http.get
    { url = url
    , expect = Http.expectString HttpResponseReturned
    }

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
        [ p [] [ text model.url ]
        , input [ placeholder "URL", value model.url, onInput UrlUpdated ] []
        , p [] [ text (displayError model.error) ]
        , div [] [ text (displayHttpResponse model.response)]
        , button [ onClick SendHttpRequestButtonClicked ] [ text "do stuff"]
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
