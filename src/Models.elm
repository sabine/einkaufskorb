port module Models exposing (..)

import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (src, class, href, value, type_)
import Html.Events exposing (onClick, onInput)

type alias CartLocalization =
  { begin_checkout: String
  , abort_checkout: String
  
  , total: String
  , address_form_placeholder1: String
  , address_form_placeholder2: String
  , address_form_placeholder_zip_code_city: String
  , address_form_placeholder_country: String
  
  , order_now_by_email: String
  }

type alias ProductsLocalization =
  { add_to_cart: String
  , remove: String
  }
  
type alias Image =
  { src: String
  , small_thumbnail: String
  , large_thumbnail: String
  }

type alias ProductId = String

type alias Product =
  { id: ProductId
  , name: String
  , price: Int
  , images: List Image
  }
  
type alias CartItem =
  { product: Product 
  , amount: Int }
  
type alias Cart = List CartItem

type ProductView = SmallProductList | LargeProductList
  
type alias Model =
    { cart: Cart
    , cart_open: Bool
    , checkout: Checkout
    , cart_localization: CartLocalization
    
    , products: List Product
    , view: ProductView
    , name_filter: String
    , products_localization: ProductsLocalization
    
    , error_message: String}
    
type alias Address =
  { address1: String
  , address2: String
  , zipCodeCity: String
  , country: String }

    
type Checkout = NotCheckingOut
  | CheckoutInProgress Address
  
type alias CheckoutData =
  { address: Address
  , cart: Cart }
        
initialModel : Maybe CartLocalization -> Maybe ProductsLocalization -> Model
initialModel cart_localization products_localization =
  { cart = []
  , cart_open = False
  , checkout = NotCheckingOut
  , cart_localization = Maybe.withDefault (CartLocalization "" "" "" "" "" "" "" "") cart_localization
  
  , products = []
  , view = SmallProductList
  , name_filter = ""
  , products_localization = Maybe.withDefault (ProductsLocalization "" "") products_localization
  
  , error_message = ""}
    
    
---- JSON ----
decodeImage : Decode.Decoder Image
decodeImage = 
    Decode.map3 Image
        (Decode.field "src" Decode.string)
        (Decode.field "small_thumbnail" Decode.string)
        (Decode.field "large_thumbnail" Decode.string)
        
        
decodeProducts : Decode.Decoder (List Product)
decodeProducts =
    Decode.list decodeProduct


decodeProduct : Decode.Decoder Product
decodeProduct =
    Decode.map4 Product
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "price" Decode.int)
        (Decode.field "images" (Decode.list decodeImage))
        
decodeCartItems : Decode.Decoder (List CartItem)
decodeCartItems = 
    Decode.list decodeCartItem
    
decodeCartItem =
  Decode.map2 CartItem
      (Decode.field "product" decodeProduct)
      (Decode.field "amount" Decode.int)
        
decodeCart : Decode.Decoder Cart
decodeCart =
    decodeCartItems
    
    
    
---- UPDATE ----

type Msg
    =  SetProducts Decode.Value
    | SetCart Decode.Value
    
    | ToggleCart
    | AddToCart Product
    | UpdateCartAmount ProductId String
    | RemoveFromCart ProductId
    
    | BeginCheckout
    | AbortCheckout
    | Checkout
    | UpdateAddress1 String
    | UpdateAddress2 String
    | UpdateZipCodeCity String
    | UpdateCountry String
    
    | FilterName String

    
addToCart : Product -> List CartItem -> List CartItem
addToCart product cart = 
  case cart of
    [] -> [CartItem product 1]
    x::xs -> 
      if x.product.id == product.id then {x | amount = x.amount + 1}::xs else x::addToCart product xs
        
updateAmount productId amount cart =
  if amount < 0 then cart
  else case cart of
    [] -> []
    x :: xs ->
      let
        updated_x = if x.product.id == productId then { x | amount = amount } else x
      in
        updated_x :: updateAmount productId amount xs

updateIfCheckingOut : (Address -> String -> Address) -> Model -> String -> ( Model, Cmd Msg )
updateIfCheckingOut upd model value =
  case model.checkout of
    NotCheckingOut -> (model, Cmd.none)
    CheckoutInProgress addr ->
      ({ model | checkout = CheckoutInProgress (upd addr value) }, Cmd.none)
        
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetProducts v ->
            let
              p = Decode.decodeValue decodeProducts v
            in
              case p of
                Ok (m) ->
                  ( { model | products = m }, Cmd.none )
                Err (e) -> 
                  ( {model | error_message = ("Error: " ++ Decode.errorToString e)} , Cmd.none )
        SetCart v ->
          let
              c = Decode.decodeValue decodeCart v
            in
              case c of
                Ok (m) ->
                  ( { model | cart = m }, Cmd.none )
                Err (e) -> 
                    ( {model | error_message = ("Error: " ++ Decode.errorToString e)} , Cmd.none )
        ToggleCart ->
          ( {model | cart_open = not model.cart_open }, Cmd.none )
                    
        AddToCart product ->
            let
                newcart = addToCart product model.cart
            in
                ( { model | cart = newcart }, saveCart newcart )
                
        UpdateCartAmount productId amount_str ->
            let
              amount = Maybe.withDefault 0 (String.toInt amount_str)
              newcart = updateAmount productId amount model.cart
            in
                ( { model | cart = newcart }, saveCart newcart )

        RemoveFromCart productId ->
            let
                newcart =
                    List.filter (\{ product } -> product.id /= productId) model.cart
            in
                ( { model | cart = newcart }, saveCart newcart )
                
                
        BeginCheckout ->
          ({model | checkout = CheckoutInProgress (Address "" "" "" "")}, Cmd.none)
          
        AbortCheckout ->
          ({model | checkout = NotCheckingOut}, Cmd.none)      
          
        Checkout ->
          case model.checkout of
            NotCheckingOut -> ({ model | error_message = "trying to check out while not in state CheckoutInProgress"}, Cmd.none)
            CheckoutInProgress address ->
              (model, checkout (CheckoutData address model.cart))
              
        UpdateAddress1 value ->
          updateIfCheckingOut (\c -> \a -> {c | address1 = a}) model value
         
        UpdateAddress2 value ->
          updateIfCheckingOut (\c -> \a -> {c | address2 = a}) model value
          
        UpdateZipCodeCity value ->
          updateIfCheckingOut (\c -> \a -> {c | zipCodeCity = a}) model value
          
        UpdateCountry value ->
          updateIfCheckingOut (\c -> \a -> {c | country = a}) model value
              
          
        FilterName name ->
          ({model | name_filter = name}, Cmd.none)
                
                
productPrice : Int -> String
productPrice price =
    String.fromFloat (toFloat price / 100) ++ " EUR"


isInCart : Product -> List Product -> Bool
isInCart product cart =
    let
        cartOfOneItem =
            List.filter (\{ id } -> id == product.id) cart
    in
        not (List.isEmpty cartOfOneItem)
        
        
addRemoveButtons productInCart =
  [ button [ class "btn btn-danger btn-xs", onClick (UpdateCartAmount productInCart.product.id (String.fromInt (productInCart.amount - 1))) ] [ text "-" ]
  , input [ class "cart-item__amount_input", type_ "number", value (String.fromInt productInCart.amount), onInput (UpdateCartAmount productInCart.product.id) ] []
  , button [ class "btn btn-danger btn-xs", onClick (UpdateCartAmount productInCart.product.id (String.fromInt (productInCart.amount + 1))) ] [ text "+" ]]
                
----- PORTS -----
port saveCart : Cart -> Cmd msg
port setCart : (Decode.Value -> msg) -> Sub msg

port setProducts : (Decode.Value -> msg) -> Sub msg

port toggleCart : (() -> msg) -> Sub msg
port addProductToCart : (Product -> msg) -> Sub msg
port removeProductFromCart: (ProductId -> msg) -> Sub msg

port checkout : CheckoutData -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [
  setProducts SetProducts
  , setCart SetCart
  , toggleCart (always ToggleCart) 
  , addProductToCart AddToCart
  , removeProductFromCart RemoveFromCart
  ]