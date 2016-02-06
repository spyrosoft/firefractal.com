Stripe.setPublishableKey( 'pk_test_6pRNASCoBOKtIshFeQd4XMUh' );

$( '.buy-poster-form' ).submit(
	function ( submitEvent ) {
		$( this ).find( 'button' ).prop( 'disabled', true );
		Stripe.card.createToken( $( this ), stripeResoponseCallback );
	}
);

function stripeResoponseCallback( status, response ) {
	if ( response.error ) {
		$( '.payment-errors' ).text( response.error.message );
		$( '.buy-poster-form' ).find( 'button' ).prop( 'disabled', false );
	} else {
		var token = response.id;
		paymentTokenAJAX( token );
	}
}

function paymentTokenAJAX( token ) {
	$.post(
		
	);
}