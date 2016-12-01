//TODO: Link to explore for preview button

var url_hash = window.location.hash;

if ( url_hash.length > 1 ) {
	window.location = '/explore/' + url_hash;
}

var buy_print_steps = [
	'size-and-orientation',
	'payment',
	'shipping',
	'success'
];

var print_prices = {
	'11x8.5' : 200
	, '24x18' : 400
	, '32x16' : 400
	, '48x16' : 600
	, '60x30' : 1200
};

$( '.buy-print-step.size-and-orientation select[name="print-size"]' )
	.change( display_buy_print_total );
	
$.each(
	buy_print_steps,
	function(index, step)
	{
		$('.buy-print button.' + step + '-step')
			.click( function() { show_buy_print_step(step); } );
	}
);

$( 'button.create-payment-token' )
	.click( create_payment_token );

$( '.create-payment-token-form' )
	.submit( create_payment_token_form_submit );

$( '.create-payment-token-form' ).find( 'input' )
	.keydown( create_payment_token_on_enter );

$( '.ship-print-form' )
	.submit( ship_print_form_submit );

$( 'button.buy-another-print' )
	.click( hide_buy_print );

var dev_or_live = 'live';
if ( window.location.hostname === 'localhost' ) {
	dev_or_live = 'dev';
	initialize_test_cc_info();
}

$('img').on('click', print_image_clicked);

function print_image_clicked(click_event) {
	buy_print();
	$('img.buy-print-display').attr('src', this.src).attr('alt', this.alt);
	$('select[name="print-size"]').first().focus();
}

function buy_print() {
	show_buy_print_step('size-and-orientation');
}

function show_buy_print_step(buy_print_step)
{
	$('.buy-print').show();
	$('.buy-print-step').hide();
	$('.buy-print-step.' + buy_print_step).show();
	display_buy_print_total();
	clear_buy_print_error();
	$('.buy-print-step.' + buy_print_step + ' input[type="text"]').first().select();
}

function display_buy_print_error( error_message )
{
	$( '.buy-print-error' ).html( '<br>' + error_message );
}

function clear_buy_print_error()
{
	$( '.buy-print-error' ).html( '' );
}

function display_buy_print_total()
{
	$( '.buy-print .total' ).html(
		format_human_readable_dollars(buy_print_total())
	);
}

function buy_print_total()
{
	var print_size = document.getElementsByName( 'print-size' )[ 0 ].value;
	return print_prices[ print_size ];
}

function create_payment_token_form_submit( submit_event )
{
	submit_event.preventDefault();
}

function create_payment_token_on_enter( key_event )
{
	if ( check_for_enter_key_event( key_event ) )
	{
		create_payment_token();
	}
}

function create_payment_token()
{
	$( '.create-payment-token-form' )
		.find( 'button' )
		.prop( 'disabled', true );
	if ( dev_or_live === 'dev' ) {
		Stripe.setPublishableKey( 'pk_test_a3d3NSbI6QUBR4knlav1Cs0K' );
	} else {
		Stripe.setPublishableKey( 'pk_live_5GYzGkxM6bSXlvnIZWNC2n48' );
	}
	Stripe.card.createToken(
		$( '.create-payment-token-form' ),
		buy_print_payment_callback
	);
}

function buy_print_payment_callback( status, response )
{
	$( '.create-payment-token-form' )
		.find( 'button' )
		.prop( 'disabled', false );
	
	if ( status === 200 )
	{
		document.getElementById( 'buy-print-token' ).value = response[ 'id' ];
		show_buy_print_step( 'shipping' );
	}
	else
	{
		if ( response[ 'error' ] === undefined || response[ 'error' ][ 'message' ] === undefined )
		{
			display_buy_print_error( 'Something went wrong connecting with the server. Please try again after a while.' );
		}
		else
		{
			display_buy_print_error( response[ 'error' ][ 'message' ] );
		}
	}
}

function ship_print_form_submit( submit_event )
{
	submit_event.preventDefault();
	ship_print();
}

function ship_print()
{
	if ( document.getElementById( 'buy-print-token' ).value === '' )
	{
		show_buy_print_step( 'payment' );
		display_buy_print_error( 'Somehow your payment did not go through. Please fill out your payment details again.' );
		return;
	}
	
	var ship_print_data = populate_ship_print_text_fields();
	if ( ship_print_data === null ) { return; }
	
	ship_print_data = populate_ship_print_additional_fields( ship_print_data );
	
	$.post( '/buy-print', ship_print_data )
		.done( buy_print_success )
		.fail( ship_print_ajax_fail );
}

function populate_ship_print_text_fields()
{
	//TODO: Consider populating this via the text input contents
	var text_input_ids = [
		'buy-print-token',
		'shipping-name',
		'shipping-address',
		'shipping-city',
		'shipping-state',
		'shipping-zip',
		'shipping-email'
	];
	var ship_print_data = {};
	
	for ( var i in text_input_ids )
	{
		var text_input_id = text_input_ids[ i ];
		if ( document.getElementById( text_input_id ).value === '' )
		{
			display_buy_print_error( 'All fields are required.' );
			document.getElementById( text_input_id ).select();
			return null;
		}
		ship_print_data[ text_input_id ]
			= document.getElementById( text_input_id ).value;
	}
	
	return ship_print_data;
}

function populate_ship_print_additional_fields( ship_print_data )
{
	ship_print_data[ 'destination-link' ]
		= $('.buy-print-display').attr('alt');
	
	ship_print_data[ 'print-size' ]
		= document.getElementsByName( 'print-size' )[ 0 ].value;
	
	ship_print_data[ 'print-orientation' ]
		= document.getElementsByName( 'print-orientation' )[ 0 ].value;
	
	ship_print_data[ 'order-total' ]
		= format_human_readable_dollars( buy_print_total() );
	
	return ship_print_data;
}

function buy_print_success( response )
{
	try {
		response = JSON.parse( response );
	} catch ( error ) {
		ship_print_ajax_fail();
		return;
	}
	if ( response[ 'Success' ] === undefined )
	{
		ship_print_ajax_fail();
	}
	else if ( response[ 'Success' ] === true )
	{
		show_buy_print_step( 'success' );
		clear_credit_card_fields();
	}
	else if ( response[ 'Message' ] !== undefined )
	{
		display_buy_print_error( 'The transaction was rejected. The payment gateway\'s response: ' + response[ 'Message' ] );
	}
	else
	{
		ship_print_ajax_fail();
	}
}

function ship_print_ajax_fail()
{
	display_buy_print_error( 'Something went wrong while communicating with the server. It is possible that the payment gateway is down. Please try again after a while. If the problem persists, please contact us and let us know.' );
}

function clear_credit_card_fields()
{
	$( '.create-payment-token-form' )[ 0 ].reset();
	$( '#buy-print-token' ).val( '' );
}

function hide_buy_print() {
	$('.buy-print').hide();
}

function format_human_readable_dollars(number) {
	if ( parseFloat( number ) != number ) { throw 'The argument provided was not a number: ' + number; }
	return '$' + number.toFixed( 2 );
}

function initialize_test_cc_info() {
	$('.create-payment-token-form input[data-stripe="number"]').val(4111111111111111);
	$('.create-payment-token-form input[data-stripe="cvc"]').val(111);
	$('.create-payment-token-form input[data-stripe="exp-month"]').val('01');
	$('.create-payment-token-form input[data-stripe="exp-year"]').val('2020');
	$('#shipping-name').val('Fire Fractal');
	$('#shipping-address').val('111 Mandelbrot Way');
	$('#shipping-city').val('Town City');
	$('#shipping-state').val('CA');
	$('#shipping-zip').val('99999');
}