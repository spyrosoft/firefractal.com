$('#payment-amount').keyup(stripe_checkout_on_enter);
$('.next-step.donate').click(stripe_checkout);

function stripe_checkout_on_enter(key_event) {
	if (Utilities.isEnter(key_event)) {
		stripe_checkout();
	}
}

function payment_amount_step() {
	$('#check-correct-amount').html('');
	show_step('payment-amount');
	$('#payment-amount').select();
}

function show_step(which_step) {
	$('.step').addClass('display-none');
	$('.step.' + which_step).removeClass('display-none');
}

function display_error(message) {
	$('.payment-amount-error').html(message);
}

function clear_error() {
	display_error('');
}



/* Stripe */

var payment_amount_in_cents = 0;

var stripe_key = 'pk_live_5GYzGkxM6bSXlvnIZWNC2n48';
if (window.location.hostname === 'localhost') {
    stripe_key = 'pk_test_a3d3NSbI6QUBR4knlav1Cs0K';
}

var stripe_handle = StripeCheckout.configure({
	key: stripe_key,
	locale: 'auto',
	bitcoin: true,
	token: stripe_submit_order
});

function stripe_checkout() {
	var payment_amount = $('#payment-amount').val().replace(/[^0-9.]/g, '');
	if (payment_amount === '' || !isFinite(payment_amount)) {
		display_error('Please enter a number.');
		$('#payment-amount').focus();
		return;
	}
	clear_error();
	payment_amount_in_cents = parseFloat(payment_amount) * 100;
	stripe_handle.open({
		'name' : 'Send Payment',
		'amount' : payment_amount_in_cents
	});
}

$(window).on('popstate', function() {stripe_handle.close();});

/* Our Server */

function stripe_submit_order(stripe_token) {
	var stripe_data = {
		'stripe-token' : stripe_token.id,
		'payment-amount-in-cents' : payment_amount_in_cents
	};
	$.post('/donate', stripe_data)
		.done( stripe_transaction_success )
		.fail( stripe_transaction_fail );
}

function stripe_transaction_success(server_response) {
	try {
		server_response = JSON.parse(server_response);
	} catch (error) {
		stripe_communication_failure();
		return;
	}
	if ( typeof server_response['Success'] === 'undefined' ) {
		stripe_communication_failure();
	} else if (server_response['Success'] === true) {
		window.location = '/donation-received.html';
	} else if (typeof server_response['message'] !== 'undefined') {
		display_error('The transaction was rejected. The payment gateway\'s server responded with the following message: ' + server_response['message']);
	} else {
		stripe_communication_failure();
	}
}

function stripe_transaction_fail(server_response) {
	stripe_communication_failure();
}

function stripe_communication_failure() {
	display_error('Something went wrong while communicating with the server. It is possible that the payment gateway is down. Please try again after a while. If the problem persists, please contact us and let us know.');
}