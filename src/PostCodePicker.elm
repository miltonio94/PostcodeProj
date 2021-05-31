module PostCodePicker exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html exposing (Html, button, div, h1, img, input, p, text)
import Html.Attributes exposing (class, default, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Icons exposing (pinIcon, randomIcon)
import Json.Decode as Decode exposing (Decoder, Error(..), int, list, string)
import Json.Decode.Pipeline as Pipeline
import Svg.Attributes exposing (d)
import Url


apiUrl =
    "http://postcodes.io/postcodes/"


emptyPostcode : PostcodeData
emptyPostcode =
    PostcodeData "" "" "" "" ""



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangeUrl
        , onUrlRequest = LinkClick
        }



-- MODEL


type alias PostcodeData =
    { country : String
    , region : String
    , outcode : String
    , incode : String
    , postcode : String
    }


type alias Response =
    { status : Int, result : List PostcodeData }


type alias Model =
    { postcode : String
    , data : RestCall
    , key : Navigation.Key
    }


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model "" Inactive key, Cmd.none )



-- UPDATE


type Msg
    = PostcodeUpdate String
    | ChangeUrl Url.Url
    | LinkClick Browser.UrlRequest
    | Submit
    | ClearData
    | PostcodeSuccess (Result Http.Error Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PostcodeUpdate newContent ->
            ( { model | postcode = newContent }, Cmd.none )

        Submit ->
            ( { model | data = Inactive }, getPostcode model.postcode )

        ClearData ->
            ( { model | data = Inactive }, Navigation.pushUrl model.key model.postcode )

        PostcodeSuccess result ->
            case result of
                Ok information ->
                    ( { model | data = Success information }, Navigation.pushUrl model.key model.postcode )

                Err _ ->
                    ( model, Cmd.none )

        LinkClick clickRequest ->
            ( model, Cmd.none )

        ChangeUrl urlRequest ->
            ( model, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


submitOrClear : Model -> Msg
submitOrClear model =
    if String.isEmpty model.postcode then
        ClearData

    else
        Submit


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ div [ class "postcodePicker" ]
            [ div
                [ class "main" ]
                [ img [ src "img/map.png", class "picMap" ] []
                , div
                    [ class "inputField" ]
                    [ input [ placeholder "ENTER POSTCODE", value model.postcode, onInput PostcodeUpdate ] []
                    , button [ onClick (submitOrClear model) ] [ pinIcon ]
                    ]
                , div [ class "information" ] [ renderResult model.data ]
                ]
            ]
        ]
    }



-- responseHeadFromMaybe : Maybe


maybePostcodeToPostcode : Maybe PostcodeData -> PostcodeData
maybePostcodeToPostcode maybePostcode =
    case maybePostcode of
        Just postcode ->
            postcode

        Nothing ->
            emptyPostcode


maybeListPostcodeToListPostcode : Maybe (List PostcodeData) -> List PostcodeData
maybeListPostcodeToListPostcode maybePostcodes =
    case maybePostcodes of
        Just postcodes ->
            postcodes

        Nothing ->
            [ emptyPostcode ]


renderResult : RestCall -> Html Msg
renderResult restCall =
    case restCall of
        Success data ->
            let
                sellectedPostcode =
                    maybePostcodeToPostcode (List.head data.result)

                nearestPostcodes =
                    maybeListPostcodeToListPostcode (List.tail data.result)
            in
            div
                [ class "displayPostcodeData" ]
                [ div [ class "selectedPostcode" ] [ renderPostcodeData sellectedPostcode ]
                , div [ class "nearestPostcodes" ] [ renderNearestPostcodes nearestPostcodes ]
                ]

        Inactive ->
            div [] []

        Failure ->
            div [] []


renderPostcodeData : PostcodeData -> Html Msg
renderPostcodeData data =
    div
        [ class "postcodeData" ]
        [ h1 [] [ text data.postcode ]
        , p [] [ text ("Country: " ++ data.country) ]
        , p [] [ text ("Region: " ++ data.region) ]
        ]


renderNearestPostcodes : List PostcodeData -> Html Msg
renderNearestPostcodes postcodes =
    div [] (List.map renderPostcodeData postcodes)



-- HTTP


type RestCall
    = Failure
    | Success Response
    | Inactive


getPostcode : String -> Cmd Msg
getPostcode postcode =
    Http.get
        { url = apiUrl ++ postcode ++ "/nearest"
        , expect = Http.expectJson PostcodeSuccess resultDecoder
        }



--informationDecoder : Decoder String


resultDecoder : Decoder Response
resultDecoder =
    Decode.succeed Response
        |> Pipeline.required "status" int
        |> Pipeline.optional "result" (Decode.list postcodeDecoder) [ emptyPostcode ]


postcodeDecoder : Decoder PostcodeData
postcodeDecoder =
    Decode.succeed PostcodeData
        |> Pipeline.required "country" string
        |> Pipeline.required "region" string
        |> Pipeline.required "outcode" string
        |> Pipeline.required "incode" string
        |> Pipeline.required "postcode" string
