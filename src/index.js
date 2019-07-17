import * as Products from './Products.elm';
import * as Cart from './Cart.elm';
import registerServiceWorker from './registerServiceWorker';


registerServiceWorker();

window.Products = Products;
window.Cart = Cart;



  // Stripe Checkout 2019
  /*
  var stripe = Stripe('pk_test_Jhl6ixu7Pv9vndeS3Fam7GXE');

  function startCheckout (model) {
    
    stripe.redirectToCheckout({
      items: [
        
        {sku: 'sku_123', quantity: 2},
        {sku: 'sku_124', quantity: 1}
      ],
      billingAddressCollection: 'required',
      successUrl: 'http://127.0.0.1:3000/#success',
      cancelUrl: 'http://127.0.0.1:3000/#cancel',
    }).then(function (result) {
      console.log(["stripe", result]);
      // If `redirectToCheckout` fails due to a browser or network
      // error, display the localized error message to your customer
      // using `result.error.message`.
    });
  }
  */