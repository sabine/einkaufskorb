port module Cart exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src, class, href, value, type_, placeholder, style)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Models exposing (..)


cartCloseButton = 
  button [ class "cart__close", onClick ToggleCart ] [ text "X" ]

  
view model = case model.cart_open of
  True -> case model.checkout of
    NotCheckingOut -> cartView model  
    CheckoutInProgress address -> checkoutView model
  False -> text ""

checkoutView : Model -> Html Msg
checkoutView model =
    div [ class "cart" ] [
      div [] (List.map checkoutItemView model.cart)
      , cartCloseButton
      , cartTotalView model
      
      , div [] [
        div [] [ input [ onInput UpdateAddress1, placeholder model.cart_localization.address_form_placeholder1 ] [] ]
        , div [] [input [ onInput UpdateAddress2, placeholder model.cart_localization.address_form_placeholder2 ] []]
        , div [] [input [ onInput UpdateZipCodeCity, placeholder model.cart_localization.address_form_placeholder_zip_code_city ] []]
        , div [] [input [ onInput UpdateCountry, placeholder model.cart_localization.address_form_placeholder_country ] []]
      ]
      
      , button [ onClick Checkout ] [ text model.cart_localization.order_now_by_email ]
      , button [ onClick AbortCheckout ] [ text model.cart_localization.abort_checkout ]
    ]
  
checkoutItemView : CartItem -> Html Msg
checkoutItemView productInCart =
    div [ class "checkout-item" ]
        [ span [ class "cart-item__name" ] [ text productInCart.product.name ]
        , text ": "
        , span [ class "cart-item__amount" ] [ text (String.fromInt productInCart.amount)]
        , text " x "
        , span [ class "cart-item__price" ] [ text (productPrice productInCart.product.price) ]
        , text " = "
        , span [ class "cart-item__total" ] [ text (productPrice (productInCart.product.price * productInCart.amount)) ]
        ]
        
        
cartView : Model -> Html Msg
cartView model =
  let 
    products = model.cart
  in
    div [ class "cart" ]
        [ div [ class "panel panel-default" ]
            [ cartCloseButton
            , div [ class "panel-body" ]
                [ if List.isEmpty products then
                    div [ class "alert alert-info" ]
                        [ text "Cart is empty" ]
                  else
                    div [ class "cart__body" ] (List.map cartItemView products)
                , cartTotalView model
                ]
            ],
          if (List.length products) > 0 then 
            button [ onClick BeginCheckout ] [ text model.cart_localization.begin_checkout ]
          else text ""
        ]

        

productThumbnailView : Maybe Image -> Html msg
productThumbnailView mi = 
  case mi of
    Nothing -> text "TODO: Bild fehlt!!"
    Just i -> img [ src i.small_thumbnail, class "product-thumbnail" ] []

cartItemView : CartItem -> Html Msg
cartItemView productInCart =
    div [ class "cart-item" ]
        [  button [ class "btn btn-danger btn-xs", onClick (RemoveFromCart productInCart.product.id) ] [ text "X" ]
        , productThumbnailView (List.head productInCart.product.images)
        , div [ class "cart-item__amount" ] ([
            span [ class "cart-item__name" ] [ text productInCart.product.name ]
            ]
            ++ addRemoveButtons productInCart
            ++ [ text " x "
            , span [ class "cart-item__price" ] [ text (productPrice productInCart.product.price) ]
            , text " = "
            , span [ class "cart-item__total" ] [ text (productPrice (productInCart.product.price * productInCart.amount)) ]])
        ]

cartTotalView : Model -> Html Msg
cartTotalView model =
    let
        total =
            List.foldr (+) 0 (List.map (\{ product, amount } -> product.price * amount) model.cart)
    in
        div [ class "cart__total" ] [ text (model.cart_localization.total ++ productPrice total) ]

init : CartLocalization -> ( Model, Cmd Msg )
init cart_localization =
    ( initialModel (Just cart_localization) Nothing, Cmd.none )

main : Program CartLocalization Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
