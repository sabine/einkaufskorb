port module Products exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src, class, href, placeholder)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Models exposing (..)


     

---- VIEW ----

view : Model -> Html Msg
view model =
  let
    products = List.filter (\p -> String.contains (String.toLower model.name_filter) (String.toLower p.name)) model.products
  
    addToCartButtonView product = case List.head (List.filter (\pinc -> pinc.product.id == product.id) model.cart) of
      Nothing ->
        productButton model.products_localization product
      Just productInCart ->
        productInCartButton model.products_localization productInCart
  in
    div [] [
      input [ onInput FilterName, placeholder "Produkt suchen..." ] []
      , (case model.view of 
          SmallProductList -> smallProductListView addToCartButtonView products
          LargeProductList -> largeProductListView addToCartButtonView products
        )
      ]

smallProductListView : (Product -> Html Msg) -> List Product -> Html Msg
smallProductListView addToCartButtonView products =
    div [ class "small-product-list" ]
        (List.map (\product -> smallProductThumbnailView addToCartButtonView product) products)
    
smallProductThumbnailView : (Product -> Html Msg) -> Product -> Html Msg
smallProductThumbnailView addToCartButtonView product =
  div [ class "product-detail" ]
      [ div [ class "product thumbnail" ]
          [ (case (List.head product.images) of
            Nothing -> text "TODO: Bild fehlt!!"
            Just i -> img [ src i.small_thumbnail, class "small-product-thumbnail" ] []
          )
          , div [ class "caption" ]
              [ h3 [] [ text product.name ]
              , div [ class "product__price" ]
                  [ text (productPrice product.price) ]
              , addToCartButtonView product
              ]
          ]
      ]
      
      
largeProductListView : (Product -> Html Msg) -> List Product -> Html Msg
largeProductListView addToCartButtonView products =
    div [ class "large-product-list" ]
        (List.map (\product -> largeProductThumbnailView addToCartButtonView product) products)
    
largeProductThumbnailView : (Product -> Html Msg) -> Product -> Html Msg
largeProductThumbnailView addToCartButtonView product =
  div [ class "product-detail" ]
      [ div [ class "product thumbnail" ]
          [ (case (List.head product.images) of
            Nothing -> text "TODO: Bild fehlt!!"
            Just i -> img [ src i.large_thumbnail, class "large-product-thumbnail" ] []
          )
          , div [ class "caption" ]
              [ h3 [] [ text product.name ]
              , div [ class "product__price" ]
                  [ text (productPrice product.price) ]
              , addToCartButtonView product
              ]
          ]
      ]
      
      
      
productInCartButton products_localization productInCart =
  div [ class "product__button-wrap" ]
      ([ button
          [ class "button danger"
          , onClick (RemoveFromCart productInCart.product.id)]
          [ text products_localization.remove ]
      ]
      ++ addRemoveButtons productInCart)
      
productButton localization product =
  div [ class "product__button-wrap" ]
    [ button
        [ class "button primary"
        , onClick (AddToCart product)]
        [ text localization.add_to_cart ]
    ]


init : ProductsLocalization -> ( Model, Cmd Msg )
init products_localization = (initialModel Nothing (Just products_localization), Cmd.none)

main : Program ProductsLocalization Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
