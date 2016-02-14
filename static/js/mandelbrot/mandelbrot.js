/* --------------------Color Presets-------------------- */

var gradient_colors_presets = {
	'Custom' : '',
	'Fire' : '000000ff0000ff9000000000',
	'Water' : '0007072389950080be000010',
	'Earth' : '0000002114000013008a5400000000',
	'Wind' : 'ffffff0000c68d90ff0000670000ffffffff',
	'Smoke' : 'ff0000151515000000',
	'Embers' : '0000001c1c1c0c0c0c1c1c1c0000001c1c1c0c0c0c1818180000001818180c0c0c1818180000001818180c0c0c1818180000001c1c1c0c0c0c1c1c1c0000001c1c1c0c0c0c1818180000001818180c0c0cff0000181818000000',
	'Ice' : 'ffffff00ffff006fffffffff',
	'Black and White' : 'ffffff000000',
	'Red' : '100000cc0000330000ff0000100000',
	'Orange' : '000000ff3e00ff9000ff3e00000000',
	'Yellow' : 'ffff00ff9000ffff00000000',
	'Green' : '00000000ff00008c0000ff00000000',
	'Blue' : '0000060000230000c60000670000ff000000',
	'Purple' : '100010cc00cc330033ff00ff100010',
	'Teal' : '00000000ffff000000',
	'Pink' : 'ff007f5b002eff007f000000',
	'Vampire' : '0000002c0000100000710000ff0000000000',
	'Rainbow' : 'ff0000ffff0000ff000000ffff00ff000000',
	'Neon' : 'ff00000003ff00ff00ffff00000000',
	'Medusa' : '00ff00002e0000ff0000000000ff00002e0000ff0000000000ff00002e0000ff0000000000ff00002e0000ff0000000000ff00002e0000ff0000000000ff00002e0000ff00000000',
	'Electric Eel' : '00000000ffff004f4f00ffff004f4f00ffff004f4f00ffff004f4f00ffff004f4f00ffff004f4f00ffff000000',
	'Pastel' : '00245cff0000ff900004a03f',
	'Snow Cone' : '00000000ff00002e0000ff00000000ff00002e0000ff00000000ff00002e0000ff00000000ff00002e0000ff00000000ff00002e0000ff00000000ff00002e0000ff000000000000'
};

/* --------------------Color Presets-------------------- */



/* --------------------Global Variables-------------------- */

var canvas_element;
var canvas_overlay_element;
var canvas_context;
var section_image;
var section_image_data_index;
var painting_in_progress = false;
var USER_INDICATION_TTL = 100;
var WORKER_ROW_HEIGHT = 90;
var ZOOM_RATE_MAX = 16;
var ZOOM_RATE_MIN = 1.2;

var MAX_COLOR_VALUE = 255;

var settings = {};
var default_settings = {
	'zoom-level' : 60,
	'canvas-width' : 180,
	'canvas-height' : 120,
	'x' : -0.75,
	'y' : 0,
	'zoom-rate' : 4,
	'max-iterations' : 200,
	'gradient-colors' : gradient_colors_presets[ 'Fire' ],
	'fractal' : 'leaves'
};
var load_setting_function_table = {
	'zoom-level' : load_setting_int,
	'canvas-width' : load_setting_int,
	'canvas-height' : load_setting_int,
	'x' : load_setting_float,
	'y' : load_setting_float,
	'zoom-rate' : load_setting_float,
	'max-iterations' : load_setting_int,
	'gradient-colors' : load_setting_gradient_colors,
	'fractal' : load_setting_fractal
};
var valid_settings_keys_and_types = {
	'zoom-level' : 'integer',
	'x' : 'float',
	'y' : 'float',
	'canvas-width' : 'integer',
	'canvas-height' : 'integer',
	'max-iterations' : 'integer',
	'invert-enabled' : 'boolean',
	'gradient-colors' : 'six-hex-string',
	'fractal' : 'string'
};

var gradient_colors;

if ( window.Worker )
{
	var mandelbrot_worker = new Worker( '/js/mandelbrot/mandelbrot-worker.js' );
}

var SETTINGS_TIMEOUT_DELAY = 500;
var hide_settings_timeout;
var hide_zoom_rate_meter_timeout;

var wait_cursor_selectors = [
	'#canvas-overlay',
	'button.more-detail, button.less-detail',
	'button.full-screen, button.fit-to-screen',
	'button.reset-zoom',
	'button.explore'
];

var keyboard_shortcut_function_table = {
	'27' : hide_settings, //Escape
	'187' : higher_zoom_rate, //+ ... Shift key needs to be down or it's =
	'189' : lower_zoom_rate, //-
	'77' : more_iterations_detail, //m
	'76' : less_iterations_detail, //l
	'83' : show_settings_menu, //s
	'67' : function() { //c
		show_settings_menu();
		show_setting( 'colors' );
	},
	'84' : function() { //t
		show_settings_menu();
		show_setting( 'controls' );
	},
	'82' : reverse_gradient_colors, //r
	'73' : invert_gradient_colors //i
};

/* --------------------Global Variables-------------------- */



/* --------------------Initialize-------------------- */

canvas_element = document.getElementById( 'canvas' );
canvas_overlay_element = document.getElementById( 'canvas-overlay' );

initialize_settings();

initialize_gradient_color_presets();

initialize_event_listeners();

//TODO: Delete this
if ( window.location.href.match( /debug/ ) ) {
	show_settings();
	$( '.buy-poster-size-and-orientation-setting' ).removeClass( 'display-none' );
	show_setting( 'buy-poster-size-and-orientation' );
	document.getElementById( 'buy-poster-token' ).value = 'a';
}

/* ---------------Initialize Settings--------------- */

function initialize_settings()
{
	var new_settings = get_settings_from_url_hash();
	
	new_settings = populate_missing_settings_with_default( new_settings );
	
	new_settings = scale_settings_to_screen( new_settings );
	
	load_settings( new_settings );
}

function populate_missing_settings_with_default( new_settings )
{
	$.each(
		default_settings,
		function( default_setting_key, default_setting_value )
		{
			if ( ! new_settings[ default_setting_key ] )
			{
				new_settings[ default_setting_key ] = default_setting_value;
			}
		}
	);
	
	return new_settings;
}

function scale_settings_to_screen( new_settings )
{
	var screen_width = $( 'html' ).width();
	var screen_height = $( 'html' ).height();
	return scale_settings_width_and_height( new_settings, screen_width, screen_height );
}

function scale_settings_width_and_height( new_settings, new_width, new_height )
{
	var scale_ratio = get_settings_scale_ratio( new_settings, new_width, new_height );
	
	new_settings[ 'canvas-width' ] = new_width;
	new_settings[ 'canvas-height' ] = new_height;
	new_settings[ 'zoom-level' ] = new_settings[ 'zoom-level' ] * scale_ratio;
	
	return new_settings;
}

function get_settings_scale_ratio( new_settings, new_width, new_height )
{
	var width_ratio = new_width / new_settings[ 'canvas-width' ];
	var height_ratio = new_height / new_settings[ 'canvas-height' ];
	
	if ( width_ratio < height_ratio )
	{
		return width_ratio;
	}
	else
	{
		return height_ratio;
	}
}

function initialize_gradient_color_presets()
{
	load_gradient_colors_presets( gradient_colors_presets );
}

function load_gradient_colors_presets( new_presets )
{
	remove_all_but_one_gradient_colors_preset();
	
	clone_gradient_color_presets( new_presets );
	
	var new_presets_index = 0;
	$.each(
		new_presets,
		function( name, value )
		{
			$( $( 'select.gradient-colors-presets option' )[ new_presets_index ] )
				.attr( 'name', name )
				.html( name );
			new_presets_index++;
		}
	);
}

function remove_all_but_one_gradient_colors_preset()
{
	while ( $( 'select.gradient-colors-presets option' ).length > 1 )
	{
		$( 'select.gradient-colors-presets option' ).eq( 1 ).remove();
	}
}

function clone_gradient_color_presets( new_presets )
{
	$.each(
		new_presets,
		function ( name, value )
		{
			$( 'select.gradient-colors-presets option' ).first().clone().insertAfter( $( 'select.gradient-colors-presets option' ).first() );
		}
	);
	$( 'select.gradient-colors-presets option' ).first().remove();
}

/* ---------------Initialize Settings--------------- */


function initialize_event_listeners()
{
	$( 'html' )
		.keydown( check_for_keyboard_shortcut );
	
	/* ----Settings---- */
	
	$( '.settings-icon' )
		.mouseenter( show_settings_menu )
		.click( settings_icon_click );
	
	$( '.settings' )
		.mouseenter( cancel_hide_settings );
	
	$( '.settings button.back' )
		.click( show_settings_menu );
	
	var settings_buttons = [
		'colors',
		'controls',
		'share'
	];
	
	$.each(
		settings_buttons,
		function( index, setting )
		{
			$( 'button.' + setting + '-setting' )
				.click( show_setting_button_click );
		}
	);
	
	$( 'button.zoom-faster, button.zoom-slower' )
		.click( set_zoom_rate_meter );
	
	$( 'select.gradient-colors-presets' )
		.change( load_gradient_colors_preset );
	
	$( '.gradient-colors' )
		.sortable({
			stop: gradient_colors_changed
		});
	
	$( '.gradient-colors input[type="color"]' )
		.change( gradient_colors_changed );
	
	$( 'button.more-detail' )
		.click( more_iterations_detail );
	
	$( 'button.less-detail' )
		.click( less_iterations_detail );
	
	$( 'button.zoom-faster' )
		.click( higher_zoom_rate );
	
	$( 'button.zoom-slower' )
		.click( lower_zoom_rate );
	
	$( 'button.full-screen' )
		.click( function () { full_screen(); fit_to_screen(); } );
	
	$( 'button.fit-to-screen' )
		.click( fit_to_screen );
	
	$( 'button.reset-zoom' )
		.click( reset_zoom );
	
	$( 'select.fractal' )
		.change( load_fractal_type );
	
	$( '.setting.controls input[type="text"]' )
		.keydown( load_settings_on_enter )
		.keydown( text_input_escape_input );
	
	$( window ).on( 'hashchange', hash_changed );
	window.addEventListener( 'popstate', hash_changed );
	
	$( 'button.reverse' )
		.click( reverse_gradient_colors );
	
	$( 'button.invert' )
		.click( invert_gradient_colors );
	
	$( '#share-url' ).hover( select_share_url );
	
	$( 'button.share-setting' )
		.click( generate_png );
	
	$( 'button.buy-poster-preview' ).click( buy_poster_preview );
	
	$( 'button.buy-poster-size-and-orientation-setting' )
		.click( function() { show_buy_poster_setting( 'buy-poster-size-and-orientation' ); } );
	
	$( 'button.buy-poster-payment-setting' )
		.click( function() { show_buy_poster_setting( 'buy-poster-payment' ); } );
	
	$( 'button.buy-poster-shipping-setting' )
		.click( buy_poster );
	
	$( '.buy-poster-form' )
		.submit( buy_poster_form_submit );
	
	$( '.buy-poster-form' ).find( 'input' )
		.keydown( buy_poster_on_enter );
	
	$( '.ship-poster-form' )
		.submit( ship_poster_form_submit );
	
	/* ----Settings---- */
	
	
	/* ----Canvas---- */
	
	$( '#canvas-overlay' )
		.mousedown( canvas_mousedown )
		.mouseup( canvas_mouseup )
		.bind( 'contextmenu', canvas_context_menu );
	
	var canvas_hammer_manager = new Hammer.Manager( canvas_overlay_element );
	
	canvas_hammer_manager
		.add( new Hammer.Tap( { event: 'twofingertap', pointers: 2 } ) );
	
	canvas_hammer_manager.on( 'twofingertap', zoom_out_canvas );
	
	$( '#canvas-overlay' )
		.mouseenter( delay_hide_settings );
	
	/* ----Canvas---- */
}

/* --------------------Initialize-------------------- */



/* --------------------Settings-------------------- */

function get_settings_from_url_hash()
{
	var new_settings = new Object;
	var url_hash = window.location.hash.substring( 1, window.location.hash.length );
	if ( url_hash.length === 0 )
	{
		return new_settings;
	}
	
	new_settings = parse_url_hash_for_settings( url_hash );
	
	return new_settings;
}

function parse_url_hash_for_settings( url_hash )
{
	var url_settings = new Object;
	var url_hash_get_parameter_settings = get_settings_from_get_parameters( url_hash );
	if ( url_hash_get_parameter_settings )
	{
		url_settings = url_hash_get_parameter_settings;
	}
	else
	{
		var url_hash_json_settings = get_json_object_from_string( url_hash );
		if ( url_hash_json_settings )
		{
			url_settings = url_hash_json_settings;
		}
	}
	return url_settings;
}

//This is a horrible, monstrous block of code. Not really sure how to break it up, though. Any ideas?
function get_settings_from_get_parameters( get_parameters_string )
{
	var new_settings = new Object;
	var split_get_parameters = get_parameters_string.split( '&' );
	var malformatted_parameters = new Array;
	
	split_get_parameters.forEach(
		function( get_parameter_string )
		{
			var get_parameter_split = get_parameter_string.split( '=' );
			if ( get_parameter_split.length != 2 )
			{
				malformatted_parameters.push( get_parameter_string );
				return;
			}
			var get_parameter_key = get_parameter_split[ 0 ];
			var get_parameter_value = get_parameter_split[ 1 ];
			if ( valid_settings_keys_and_types[ get_parameter_key ] )
			{
				var scrub_input_by_type_results
					= scrub_input_by_type(
						get_parameter_value,
						valid_settings_keys_and_types[ get_parameter_key ]
					);
				if ( typeof scrub_input_by_type_results !== 'boolean'
					|| valid_settings_keys_and_types[ get_parameter_value ] === 'boolean' )
				{
					new_settings[ get_parameter_key ] = scrub_input_by_type_results;
				}
				else
				{
					malformatted_parameters.push( get_parameter_key + ' ' + get_parameter_value );
				}
			}
		}
	);
	notify_user_of_malformed_settings_parameters( malformatted_parameters );
	
	if ( Object.getOwnPropertyNames( new_settings ).length === 0 )
	{
		return false;
	}
	
	return new_settings;
}

function notify_user_of_malformed_settings_parameters( malformatted_settings_parameters )
{
	if ( malformatted_settings_parameters.length > 0 )
	{
		alert( 'The settings are malformatted: ' + malformatted_settings_parameters.join( ', ' ) + '.' );
	}
}

function load_settings( new_settings )
{
	if ( object_equal( new_settings, settings ) )
	{
		return;
	}
	var settings_before_load = $.extend( true, {}, settings ); //New settings object
	$.each( new_settings, load_setting );
	
	update_settings_dependent_environment( settings_before_load );
}

function load_setting( setting_name, setting_value )
{
	if ( ! has_setting_changed( setting_name, setting_value ) )
	{
		return;
	}
	var errors = new Array;
	if ( load_setting_function_table[ setting_name ] )
	{
		load_setting_function_table[ setting_name ]( setting_name, setting_value );
		settings[ setting_name ] = setting_value;
	}
	else
	{
		errors.push( 'The setting "' + setting_name + '" cannot be loaded - skipping.' );
	}
	if ( errors.length > 0 )
	{
		alert( errors.join( '\n' ) );
	}
}

function has_setting_changed( setting_name, setting_value )
{
	if ( settings[ setting_name ]
		 && settings[ setting_name ] === setting_value )
	{
		return false;
	}
	return true;
}

function load_setting_int( element_id, new_value )
{
	document.getElementById( element_id ).value = parseInt( new_value );
}

function load_setting_float( element_id, new_value )
{
	document.getElementById( element_id ).value = parseFloat( new_value );
}

function load_setting_gradient_colors( ignored_element_id, new_value )
{
	var gradient_colors = new Array();
	for ( var i = 0; i < new_value.length; i += 6 )
	{
		gradient_colors.push( new_value.substring( i, ( i + 6 ) ) );
	}
	
	remove_all_but_one_gradient_color();
	
	clone_gradient_colors( gradient_colors.length );
	
	var gradient_colors_index = 0;
	$( '.setting.colors input[type="color"]' ).each(
		function()
		{
			this.value = '#' + gradient_colors[ gradient_colors_index ];
			gradient_colors_index++;
		}
	);
	
	generate_gradient_colors();
	
	settings[ 'gradient-colors' ] = new_value;
	
	reset_gradient_colors_event_listeners();
}

function remove_all_but_one_gradient_color()
{
	while ( $( '.setting.colors .gradient-colors .color' ).length > 1 )
	{
		$( '.setting.colors .gradient-colors .color' ).eq( 1 ).remove();
	}
}

function clone_gradient_colors( total_gradient_colors )
{
	while ( $( '.setting.colors .color' ).length < total_gradient_colors )
	{
		$( '.setting.colors .color' ).first().clone().insertBefore( $( '.setting.colors .color' ).first() );
	}
}

function load_setting_fractal( ignored_element_id, new_value )
{
	if ( fractal_function_table[ new_value ] )
	{
		settings[ 'fractal' ] = new_value;
		$( 'select.fractal' ).val( new_value );
	}
}

//TODO: Refactor this - it's ugly - lookup table?
function update_settings_dependent_environment( old_settings )
{
	update_url_hash();
	
	var need_to_generate_iteration_pixels = false;
	var need_to_repaint = false;
	
	if ( settings[ 'canvas-width' ]
		!== old_settings[ 'canvas-width' ]
		|| settings[ 'canvas-height' ]
		!== old_settings[ 'canvas-height' ] )
	{
		resize_canvas( settings );
		need_to_generate_iteration_pixels = true;
	}
	
	if ( settings[ 'max-iterations' ]
		!== old_settings[ 'max-iterations' ]
		|| settings[ 'zoom-level' ]
		!== old_settings[ 'zoom-level' ]
		|| settings[ 'x' ]
		!== old_settings[ 'x' ]
		|| settings[ 'y' ]
		!== old_settings[ 'y' ]
		|| settings[ 'fractal' ]
		!== old_settings[ 'fractal' ] )
	{
		need_to_generate_iteration_pixels = true;
	}
	
	if (
		settings[ 'gradient-colors' ] !== old_settings[ 'gradient-colors' ]
		|| settings[ 'fractal' ] !== old_settings[ 'fractal' ]
	) {
		need_to_repaint = true;
	}
	
	if ( need_to_generate_iteration_pixels )
	{
		generate_iteration_pixels();
	}
	else if ( need_to_repaint )
	{
		var section_to_paint = $.extend( true, {}, settings );
		section_to_paint[ 'initial-row-index' ] = 0;
		section_to_paint[ 'final-row-index' ] = canvas_element.height;
		paint_section( section_to_paint );
	}
}

function update_url_hash()
{
	var get_parameters = convert_settings_to_get_parameters();
	document.location = '#' + get_parameters;
	set_share_url( get_parameters );
}

function set_share_url( get_parameters )
{
	$( '#share-url' ).val(
		window.location.protocol
			+ '//'
			+ window.location.host
			+ window.location.pathname
			+ '#'
			+ get_parameters
	);
}

function convert_settings_to_get_parameters()
{
	var get_parameters = '';
	
	$.each(
		settings,
		function ( setting_name, setting_value )
		{
			if ( setting_name === 'zoom-rate' )
			{
				return;
			}
			get_parameters += setting_name + '=' + setting_value + '&';
		}
	);
	
	get_parameters = get_parameters.substr( 0, get_parameters.length - 1 );
	
	return get_parameters;
}


function get_current_settings()
{
	return {
		'max-iterations' : parseInt( $( '#max-iterations' ).val() ),
		'canvas-width' : parseFloat( $( '#canvas-width' ).val() ),
		'canvas-height' : parseFloat( $( '#canvas-height' ).val() ),
		'zoom-level' : parseInt( $( '#zoom-level' ).val() ),
		'x' : parseFloat( $( '#x' ).val() ),
		'y' : parseFloat( $( '#y' ).val() ),
		'zoom-rate' : parseFloat( $( '#zoom-rate' ).val() ),
		'gradient-colors' : get_gradient_colors_setting(),
		'fractal' : $( 'select.fractal' ).first().val()
	};
}


function show_settings()
{
	show_settings_menu();
	$( '.settings' ).removeClass( 'settings-hidden' );
}

function hide_settings()
{
	$( '.settings' )
		.addClass( 'settings-hidden' );
	
	load_settings( get_current_settings() );
}

function delay_hide_settings()
{
	hide_settings_timeout = setTimeout( hide_settings, SETTINGS_TIMEOUT_DELAY );
}

function cancel_hide_settings()
{
	clearTimeout( hide_settings_timeout );
}

function show_settings_menu()
{
	$( '.setting' ).hide();
	$( '.settings' ).removeClass( 'settings-hidden' );
	$( '.settings-navigation' ).show();
	$( '.settings button.back' ).addClass( 'display-none' );
}

function settings_icon_click( click_event )
{
	click_event.preventDefault();
	//TODO: Test out the mobile friendly check if the settings are open when the canvas is clicked
	toggle_settings_hidden();
}

function toggle_settings_hidden()
{
	if ( $( '.settings' ).first().hasClass( 'settings-hidden' ) )
	{
		$( '.settings' ).removeClass( 'settings-hidden' );
	}
	else
	{
		$( '.settings' ).addClass( 'settings-hidden' );
	}
}

function show_setting_button_click()
{
	var setting = $( this ).attr( 'class' ).replace( '-setting', '' );
	show_setting( setting );
}

function show_setting( setting )
{
	$( '.setting.' + setting ).show();
	$( '.settings-navigation' ).hide();
	$( '.settings button.back' ).removeClass( 'display-none' );
}

function remove_gradient_color()
{
	$( this.parentNode.parentNode ).remove();
	show_or_hide_gradient_color_plus_minus_buttons();
	gradient_colors_changed();
}

function add_gradient_color()
{
	var this_gradient_color_element = $( this.parentNode.parentNode );
	$( this_gradient_color_element )
		.after( this_gradient_color_element.clone() );
	show_or_hide_gradient_color_plus_minus_buttons();
	gradient_colors_changed();
	reset_gradient_colors_event_listeners();
}

function show_or_hide_gradient_color_plus_minus_buttons()
{
	if ( $( '.color-controls' ).length <= 2 )
	{
		$( '.color-controls button.minus' ).addClass( 'display-none' );
	}
	else
	{
		$( '.color-controls button.minus' ).removeClass( 'display-none' );
	}
}

function reset_gradient_colors_event_listeners()
{
	$( '.color-controls' )
		.off( 'click', 'button.plus', add_gradient_color )
		.off( 'click', 'button.minus', remove_gradient_color )
		.on( 'click', 'button.plus', add_gradient_color )
		.on( 'click', 'button.minus', remove_gradient_color );
	
	$( '.gradient-colors' )
		.off( 'change', 'input[type="color"]', gradient_colors_changed )
		.on( 'change', 'input[type="color"]', gradient_colors_changed );
}

function full_screen()
{
	request_browser_full_screen();
}

function fit_to_screen()
{
	var new_settings = scale_settings_to_screen( get_current_settings() );
	load_settings( new_settings );
}

function reset_zoom()
{
	if ( painting_in_progress ) { return; }
	
	var new_settings = get_current_settings();
	new_settings[ 'zoom-level' ] = default_settings[ 'zoom-level' ];
	new_settings[ 'x' ] = default_settings[ 'x' ];
	new_settings[ 'y' ] = default_settings[ 'y' ];
	new_settings[ 'canvas-width' ] = default_settings[ 'canvas-width' ];
	new_settings[ 'canvas-height' ] = default_settings[ 'canvas-height' ];
	load_settings( scale_settings_to_screen( new_settings ) );
}

function load_fractal_type()
{
	if (
		this.value === settings[ 'fractal' ]
		|| ! fractal_function_table[ this.value ]
	) {
		return;
	}
	var old_settings = get_current_settings();
	old_settings[ 'fractal' ] = settings[ 'fractal' ];
	load_setting( 'fractal', this.value );
	update_settings_dependent_environment( old_settings );
}

function more_iterations_detail()
{
	if ( painting_in_progress ) { return; }
	
	var new_settings = get_current_settings();
	new_settings[ 'max-iterations' ]
		= Math.ceil( Math.pow( new_settings[ 'max-iterations' ], 1.03 ) ); //Seems to be an appropriate amount
	load_settings( new_settings );
}

function less_iterations_detail()
{
	if ( painting_in_progress ) { return; }
	
	var new_settings = get_current_settings();
	var new_max_iterations
		= Math.floor( Math.pow( new_settings[ 'max-iterations' ], 0.97 ) ); //Corollary to more_iterations_detail
	if ( new_max_iterations < 1 )
	{
		new_max_iterations = 1;
	}
	new_settings[ 'max-iterations' ] = new_max_iterations;
	load_settings( new_settings );
}

function higher_zoom_rate()
{
	var new_settings = get_current_settings();
	if ( new_settings[ 'zoom-rate' ] === ZOOM_RATE_MAX ) { return; }
	
	new_settings[ 'zoom-rate' ] *= 2;
	if ( new_settings[ 'zoom-rate' ] > ZOOM_RATE_MAX )
	{
		new_settings[ 'zoom-rate' ] = ZOOM_RATE_MAX;
	}
	if ( ! is_integer( new_settings[ 'zoom-rate' ] ) )
	{
		new_settings[ 'zoom-rate' ] = Math.floor( new_settings[ 'zoom-rate' ] );
	}
	
	load_settings( new_settings );
	set_zoom_rate_meter();
}

function lower_zoom_rate()
{
	var new_settings = get_current_settings();
	if ( new_settings[ 'zoom-rate' ] === ZOOM_RATE_MIN ) { return; }
	
	new_settings[ 'zoom-rate' ] /= 2;
	if ( new_settings[ 'zoom-rate' ] < ZOOM_RATE_MIN )
	{
		new_settings[ 'zoom-rate' ] = ZOOM_RATE_MIN;
	}
	
	load_settings( new_settings );
	set_zoom_rate_meter();
}

function set_zoom_rate_meter()
{
	$( '.zoom-rate-meter-outer' ).show();
	var zoom_rate_proportion = ( ( settings[ 'zoom-rate' ] - ZOOM_RATE_MIN ) * 10 / ( ZOOM_RATE_MAX - ZOOM_RATE_MIN ) ) * 10;
	if ( zoom_rate_proportion > 100 )
	{
		zoom_rate_proportion = 100;
	}
	$( '.zoom-rate-meter-inner' )
		.css( 'width', zoom_rate_proportion + '%' );

	clearTimeout( hide_zoom_rate_meter_timeout );
	hide_zoom_rate_meter_timeout
		= setTimeout(
			function() {
				$( '.zoom-rate-meter-outer' ).hide();
			},
			1000
		);
}

function hash_changed()
{
	var new_settings = get_settings_from_url_hash();
	load_settings( new_settings );
}

function reverse_gradient_colors()
{
	var new_settings = get_current_settings();
	var new_gradient_colors_setting = '';
	for (
		var color_index = new_settings[ 'gradient-colors' ].length - 6;
		color_index >= 0;
		color_index -= 6
	) {
		new_gradient_colors_setting
			+= new_settings[ 'gradient-colors' ].substr( color_index, 6 );
	}
	new_settings[ 'gradient-colors' ] = new_gradient_colors_setting;
	load_settings( new_settings );
	select_first_gradients_preset();
}

function invert_gradient_colors()
{
	var new_settings = get_current_settings();
	var new_gradient_colors_setting = '';
	for (
		var color_index = 0;
		color_index < new_settings[ 'gradient-colors' ].length;
		color_index += 6
	) {
		var current_gradient_color = new_settings[ 'gradient-colors' ].substr( color_index, 6 );
		new_gradient_colors_setting
			+= invert_gradient_color( current_gradient_color );
	}
	new_settings[ 'gradient-colors' ] = new_gradient_colors_setting;
	load_settings( new_settings );
	select_first_gradients_preset();
}

function invert_gradient_color( gradient_color )
{
	var gradient_color_rgb = six_hex_to_rgb( gradient_color );
	var new_gradient_color_rgb = new Array;
	new_gradient_color_rgb[ 0 ] = MAX_COLOR_VALUE - gradient_color_rgb[ 0 ];
	new_gradient_color_rgb[ 1 ] = MAX_COLOR_VALUE - gradient_color_rgb[ 1 ];
	new_gradient_color_rgb[ 2 ] = MAX_COLOR_VALUE - gradient_color_rgb[ 2 ];
	var new_gradient_color = rgb_to_six_hex( new_gradient_color_rgb );
	return new_gradient_color;
}

function load_gradient_colors_preset()
{
	if ( gradient_colors_presets[ this.value ] === get_gradient_colors_setting()
		|| this.value === 'Custom' )
	{
		return;
	}
	var old_settings = get_current_settings();
	load_setting( 'gradient-colors', gradient_colors_presets[ this.value ] );
	update_settings_dependent_environment( old_settings );
	show_or_hide_gradient_color_plus_minus_buttons();
}

function gradient_colors_changed()
{
	load_settings( get_current_settings() );
	select_first_gradients_preset();
}

function select_first_gradients_preset()
{
	$( 'select.gradient-colors-presets' )
		.val( $( 'select.gradient-colors-presets option' ).first().val() );
}

function select_share_url()
{
	$( '#share-url' ).select();
}

function generate_png()
{
	var png_source = canvas_element.toDataURL( 'image/png' );
	$( 'img.generated-png' )
		.attr( 'src', png_source );
	
	$( 'span.canvas-dimensions' ).html(
		'(' + canvas_element.width + 'x' + canvas_element.height + ')'
	);
}

/* --------------------Settings-------------------- */



/* --------------------Resize Canvas-------------------- */

function resize_canvas( new_settings )
{
	canvas_element.width = new_settings[ 'canvas-width' ];
	canvas_element.height = new_settings[ 'canvas-height' ];
	
	document.getElementById( 'canvas-overlay' ).width = new_settings[ 'canvas-width' ];
	document.getElementById( 'canvas-overlay' ).height = new_settings[ 'canvas-height' ];
	
	$( '.canvas-content' ).css( 'width', new_settings[ 'canvas-width' ] );
	$( '.canvas-content' ).css( 'height', new_settings[ 'canvas-height' ] );
	
	canvas_element_dimensions_have_changed();
	
	create_iteration_pixels_skeleton( new_settings[ 'canvas-height' ] );
	
	vertically_align_canvas();
	
	remove_canvas_html_spacing();
}

function canvas_element_dimensions_have_changed()
{
	canvas_context = canvas_element.getContext( '2d' );
}

function vertically_align_canvas()
{
	var canvas_height = canvas_element.height;
	var screen_height = $( 'html' ).height();
	
	$( '.canvas-content' ).css( 'top', 0 );
	
	if ( screen_height > canvas_height )
	{
		var canvas_vertical_spacing = parseInt( ( screen_height / 2 ) - ( canvas_height / 2 ) );
		$( '.canvas-content' ).css( 'top', canvas_vertical_spacing );
	}
}

function remove_canvas_html_spacing()
{
	var canvas_height = $( '#canvas' ).height();
	var screen_height = $( 'html' ).height();
	var canvas_width = $( '#canvas' ).width();
	var screen_width = $( 'html' ).width();
	if ( canvas_height <= screen_height && canvas_width <= screen_width )
	{
		$( 'body' ).addClass( 'overflow-hidden' );
	}
	else
	{
		$( 'body' ).removeClass( 'overflow-hidden' );
	}
}

/* --------------------Resize Canvas-------------------- */



/* --------------------Iteration Pixels-------------------- */

function generate_iteration_pixels()
{
	start_painting_in_progress();
	
	if ( window.Worker )
	{
		max_iterations = settings[ 'max-iterations' ] - 1;
		max_iteration_pixel = 0;
		min_iteration_pixel = max_iterations;
		generate_iteration_pixels_spawn_workers();
	}
	else
	{
		//TODO: Actually test this - a local snapshot of the website disables web workers, try it that way
		var new_settings = get_current_settings();
		new_settings[ 'initial-row-index' ] = 0;
		new_settings[ 'final-row-index' ] = settings[ 'canvas-height' ] - 1;
		generate_iteration_pixels_section( new_settings );
		stop_painting_in_progress();
	}
}

function start_painting_in_progress()
{
	painting_in_progress = true;
	$( wait_cursor_selectors.join( ', ' ) )
		.addClass( 'wait-cursor' );
}

function stop_painting_in_progress()
{
	painting_in_progress = false;
	$( wait_cursor_selectors.join( ', ' ) )
		.removeClass( 'wait-cursor' );
}

function generate_iteration_pixels_spawn_workers()
{
	var section_settings = get_current_settings();
	var total_rows = section_settings[ 'canvas-height' ] / WORKER_ROW_HEIGHT;
	for ( var i = 0; i < total_rows; i++ ) {
		section_settings[ 'initial-row-index' ] = i * WORKER_ROW_HEIGHT;
		section_settings[ 'final-row-index' ] = section_settings[ 'initial-row-index' ] + WORKER_ROW_HEIGHT;
		if ( section_settings[ 'final-row-index' ] > canvas_element.height )
		{
			section_settings[ 'final-row-index' ] = canvas_element.height;
		}
		mandelbrot_worker.postMessage( section_settings );
	}
}

mandelbrot_worker.onmessage = function( message_event )
{
	var worker_response = message_event.data;
	set_new_max_and_min_iteration_pixel( worker_response );
	append_new_iteration_pixels( worker_response );
	if ( worker_response[ 'max-iterations' ] !== max_iterations
		|| worker_response[ 'min-iterations' ] !== min_iterations )
	{
		worker_response[ 'initial-row-index' ] = 0;
	}
	paint_section( worker_response );
	if ( worker_response[ 'final-row-index' ] >= settings[ 'canvas-height' ] )
	{
		stop_painting_in_progress();
	}
};

function set_new_max_and_min_iteration_pixel( worker_response )
{
	if ( worker_response[ 'max-iteration-pixel' ] > max_iteration_pixel )
	{
		max_iteration_pixel = worker_response[ 'max-iteration-pixel' ];
	}
	if ( worker_response[ 'min-iteration-pixel' ] < min_iteration_pixel )
	{
		min_iteration_pixel = worker_response[ 'min-iteration-pixel' ];
	}
}

function append_new_iteration_pixels( worker_response )
{
	for ( var i = 0; i < worker_response[ 'iteration-pixels' ].length; i++ )
	{
		var row_index = worker_response[ 'initial-row-index' ] + i;
		iteration_pixels[ row_index ] = worker_response[ 'iteration-pixels' ][ i ];
	}
}

/* --------------------Iteration Pixels-------------------- */



/* --------------------Paint Canvas-------------------- */

function paint_section( section_settings )
{
	var initial = section_settings[ 'initial-row-index' ];
	var final = section_settings[ 'final-row-index' ];
	reset_section_image( initial, final );
	paint_row( initial, final );
	canvas_context.putImageData( section_image, 0, initial );
}

function reset_section_image( initial, final )
{
	var height_difference = final - initial;
	section_image = canvas_context.createImageData(
		canvas_element.width,
		height_difference
	);
	section_image_data_index = 0;
}

function paint_row( initial_row_index, final_row_index )
{
	for (
		var y_pixel = 0;
		y_pixel < final_row_index - initial_row_index;
		y_pixel++
	) {
		paint_column( y_pixel, initial_row_index );
	}
}

function paint_column( y_pixel, initial_row_index )
{
	for (
		var x_pixel = 0;
		x_pixel < canvas_element.width;
		x_pixel++
	) {
		paint_iteration_pixel( x_pixel, y_pixel, initial_row_index );
	}
}

function paint_iteration_pixel( x_pixel, y_pixel, initial_row_index )
{
	var pixel_iteration_colors = get_iteration_gradient_color( x_pixel, y_pixel + initial_row_index );
	section_image.data[ section_image_data_index ] = pixel_iteration_colors[ 0 ];
	section_image.data[ section_image_data_index + 1 ] = pixel_iteration_colors[ 1 ];
	section_image.data[ section_image_data_index + 2 ] = pixel_iteration_colors[ 2 ];
	section_image.data[ section_image_data_index + 3 ] = MAX_COLOR_VALUE;
	section_image_data_index += 4;
}

function get_iteration_gradient_color( x_pixel, y_pixel )
{
	//This condition occurs when a later worker thread returns first and the canvas width has become greater - all previous rows are printed, but the missing section is too short - avoiding an index out of bounds error
	if ( iteration_pixels[ y_pixel ].length < canvas_element.width )
	{
		return 0;
	}
	
	var optimized_iteration = iteration_pixels[ y_pixel ][ x_pixel ];
	
	optimized_iteration = get_contrast_optimized_iteration( optimized_iteration );
	
	optimized_iteration = get_gradient_color_index_from_iteration( optimized_iteration );
	
	return gradient_colors[ optimized_iteration ];
}

function get_contrast_optimized_iteration( optimized_iteration )
{
	//If min and max are the same, the code below tries to divide max by max - min, which is zero. Min needs to be decremented - not max. This way, the maximum color value is painted when this condition arises.
	if ( max_iteration_pixel === min_iteration_pixel )
	{
		min_iteration_pixel--;
	}
	
	optimized_iteration = parseInt(
		( optimized_iteration - min_iteration_pixel )
			* (
				max_iterations / ( max_iteration_pixel - min_iteration_pixel )
			)
	);
	
	return optimized_iteration;
}

function get_gradient_color_index_from_iteration( optimized_iteration )
{
	return parseInt(
		(
			( gradient_colors.length - 1 )
				/ max_iterations
		) * optimized_iteration
	);
}

/* --------------------Paint Canvas-------------------- */



/* --------------------Color Gradients Map-------------------- */

function have_gradient_colors_changed()
{
	return ( settings[ 'gradient-colors' ]
		=== get_gradient_colors_setting() );
}

function get_gradient_colors_setting()
{
	var gradient_colors = '';
	$( '.setting.colors .color input[type="color"]' ).each(
		function()
		{
			gradient_colors += this.value.replace( /#/, '' );
		}
	);
	return gradient_colors;
}

function generate_gradient_colors()
{
	gradient_colors = new Array();
	var gradient_colors_setting = get_split_gradient_colors_setting();
	for ( var i = 0; i < gradient_colors_setting.length - 1; i++ )
	{
		gradient_colors = gradient_colors.concat(
			generate_gradient(
				six_hex_to_rgb( gradient_colors_setting[ i ] ),
				six_hex_to_rgb( gradient_colors_setting[ i + 1 ] )
			)
		);
	}
}

function get_split_gradient_colors_setting()
{
	var gradient_colors = new Array();
	$( '.setting.colors .color input[type="color"]' ).each(
		function()
		{
			gradient_colors.push( this.value.replace( /#/, '' ) );
		}
	);
	return gradient_colors;
}

function generate_gradient( start_color, end_color )
{
	var gradient = new Array();
	var max_rgb_difference = get_max_rgb_difference( start_color, end_color );
	for (
		var gradient_color_index = 0;
		gradient_color_index < max_rgb_difference;
		gradient_color_index++
	) {
		var gradient_color = generate_gradient_color(
			gradient_color_index,
			start_color,
			end_color,
			max_rgb_difference
		);
		gradient.push( gradient_color );
	}
	return gradient;
}

function get_max_rgb_difference( start_color, end_color )
{
	var rgb_differences = new Array();
	rgb_differences.push( Math.abs( start_color[ 0 ] - end_color[ 0 ] ) );
	rgb_differences.push( Math.abs( start_color[ 1 ] - end_color[ 1 ] ) );
	rgb_differences.push( Math.abs( start_color[ 2 ] - end_color[ 2 ] ) );
	var max_rgb_difference = rgb_differences[ 0 ];
	if ( rgb_differences[ 1 ] > max_rgb_difference )
	{
		max_rgb_difference = rgb_differences[ 1 ];
	}
	if ( rgb_differences[ 2 ] > max_rgb_difference )
	{
		max_rgb_difference = rgb_differences[ 2 ];
	}
	return max_rgb_difference + 1;
}

function generate_gradient_color(
	gradient_color_index,
	start_color,
	end_color,
	max_rgb_difference
) {
	var gradient_color = new Array();
	for ( var rgb_index = 0; rgb_index < 3; rgb_index++ )
	{
		gradient_color[ rgb_index ] = calculate_gradient_color(
			gradient_color_index,
			rgb_index,
			start_color,
			end_color,
			max_rgb_difference
		);
	}
	return gradient_color;
}

function calculate_gradient_color(
	gradient_color_index,
	rgb_index,
	start_color,
	end_color,
	max_rgb_difference
) {
	return Math.ceil(
		gradient_color_index
			* (
				end_color[ rgb_index ]
					- start_color[ rgb_index ]
			)
			/ max_rgb_difference
	) + start_color[ rgb_index ];
}

/* --------------------Gradients Map-------------------- */



/* --------------------User Events-------------------- */

function canvas_mousedown( mouse_event )
{
	if ( painting_in_progress ) { return; }
	if ( ! $( '.settings' ).hasClass( 'settings-hidden' ) ) { return; }
	mouse_event.preventDefault();
	if ( mouse_event.which == 1 )
	{
		indicate_zoom_in_to_user( mouse_event );
	}
	else if ( mouse_event.which == 3 )
	{
		indicate_zoom_out_to_user( mouse_event );
	}
}

function canvas_mouseup( mouse_event )
{
	mouse_event.preventDefault();
	if ( mouse_event.which == 1 )
	{
		canvas_clicked( mouse_event );
	}
	else if ( mouse_event.which == 3 )
	{
		canvas_right_clicked( mouse_event );
	}
}

function canvas_context_menu( mouse_event )
{
	mouse_event.preventDefault();
}

function canvas_clicked( mouse_event )
{
	if ( ! $( '.settings' ).hasClass( 'settings-hidden' ) )
	{
		$( '.settings' ).addClass( 'settings-hidden' );
		return;
	}
	if ( painting_in_progress ) { return; }
	
	offset_canvas_to_click_x_y( mouse_event );
	zoom_in_canvas( mouse_event );
	clear_canvas_overlay();
}

function zoom_in_canvas( mouse_event )
{
	var new_settings = get_current_settings();
	new_settings[ 'zoom-level' ]
		= Math.ceil( settings[ 'zoom-level' ] * settings[ 'zoom-rate' ] );
	load_settings( new_settings );
}

function indicate_zoom_in_to_user( mouse_event )
{
	var canvas_overlay_x_y = get_mouse_x_y_within_canvas_overlay( mouse_event );
	var zoom_in_width = parseInt( settings[ 'canvas-width' ] / settings[ 'zoom-rate' ] );
	var zoom_in_height = parseInt( settings[ 'canvas-height' ] / settings[ 'zoom-rate' ] );
	
	var canvas_boundary = {
		'left' : (
			parseInt(
				canvas_overlay_x_y[ 'x' ] - ( zoom_in_width / 2 )
			) + 0.5
		),
		'top' : (
			parseInt(
				canvas_overlay_x_y[ 'y' ] - ( zoom_in_height / 2 )
			) + 0.5
		),
		'right' : zoom_in_width - 1,
		'bottom' : zoom_in_height - 1
	};
	
	paint_indication_rectangle( canvas_boundary );
}

function canvas_right_clicked( mouse_event )
{
	if ( painting_in_progress ) { return; }
	offset_canvas_to_click_x_y( mouse_event );
	zoom_out_canvas();
}

function zoom_out_canvas()
{
	var new_settings = get_current_settings();
	new_settings[ 'zoom-level' ]
		= Math.ceil( settings[ 'zoom-level' ] / settings[ 'zoom-rate' ] );
	load_settings( new_settings );
	clear_canvas_overlay();
}

function offset_canvas_to_click_x_y( mouse_event )
{
	var canvas_overlay_x_y = get_mouse_x_y_within_canvas_overlay( mouse_event );
	offset_x_y_settings(
		canvas_overlay_x_y[ 'x' ],
		canvas_overlay_x_y[ 'y' ]
	);
}

function get_mouse_x_y_within_canvas_overlay( mouse_event )
{
	return get_mouse_x_y_within_element(
		canvas_overlay_element,
		mouse_event
	);
}

function offset_x_y_settings( x, y )
{
	load_setting( 'x', map_x_pixel_to_cartesian( x, settings ) );
	load_setting( 'y', map_y_pixel_to_cartesian( y, settings ) );
}

function indicate_zoom_out_to_user()
{
	var canvas_boundary = {
		'left' : 0.5,
		'top' : 0.5,
		'right' : settings[ 'canvas-width' ] - 1,
		'bottom' : settings[ 'canvas-height' ] - 1
	};
	paint_indication_rectangle( canvas_boundary );
}

function clear_canvas_overlay()
{
	var canvas_overlay_context = canvas_overlay_element.getContext( '2d' );
	clear_canvas_context( canvas_overlay_context );
}

function paint_indication_rectangle( boundary )
{
	var canvas_overlay_context = canvas_overlay_element.getContext( '2d' );
	canvas_overlay_context.strokeStyle = '#ffffff';
	canvas_overlay_context.strokeRect(
		boundary.left,
		boundary.top,
		boundary.right,
		boundary.bottom
	);
	canvas_overlay_context.strokeStyle = '#000000';
	canvas_overlay_context.strokeRect(
		boundary.left + 1,
		boundary.top + 1,
		boundary.right - 2,
		boundary.bottom - 2
	);
}

function clear_canvas_context( canvas_context )
{
	canvas_context.clearRect(
		0,
		0,
		canvas_overlay_element.width,
		canvas_overlay_element.height
	);
}

function check_for_keyboard_shortcut( key_event )
{
	if ( key_event.ctrlKey ) { return; }
	var event_target = $( key_event.target );
	if ( is_key_event_inside_text_field( event_target ) ) { return; }
	if ( ! keyboard_shortcut_function_table[ key_event.keyCode ] ) { return; }
	keyboard_shortcut_function_table[ key_event.keyCode ]();
}

function is_key_event_inside_text_field( event_target )
{
	if ( ! event_target.prop( 'tagName' ) || ! event_target.prop( 'type' ) )
	{
		return false;
	}
	var event_tag_name = event_target.prop( 'tagName' ).toLowerCase();
	var event_tag_type = event_target.prop( 'type' ).toLowerCase();
	if ( event_tag_name === 'input'
		&& ( event_tag_type === 'text' || event_tag_type === 'color' ) || event_tag_type === 'email'
		|| event_tag_name === 'textarea'
	) {
		return true;
	}
	return false;
}

function load_settings_on_enter( key_event )
{
	if ( key_event.keyCode === 13 )
	{
		load_settings( get_current_settings() );
	}
}

function text_input_escape_input( key_event )
{
	if ( key_event.keyCode === 27 )
	{
		keyboard_shortcut_function_table[ key_event.keyCode ]();
	}
}

/* --------------------User Events-------------------- */



/* --------------------Buy Poster-------------------- */

function show_buy_poster_setting( buy_poster_setting )
{
	show_settings_menu();
	show_setting( buy_poster_setting );
	var total = buy_poster_total();
	$( '.buy-poster-form .total' ).html( format_human_readable_dollars( total ) );
	$( '.' + buy_poster_setting + ' input[type="text"]' ).first().select();
	$( '.buy-poster-error' ).html( '' );
	$( '.ship-poster-error' ).html( '' );
}

function buy_poster_preview()
{
	var poster_dimensions = {
		'small' : [ 8.5, 11 ],
		'medium' : [ 16, 20 ],
		'large' : [ 20, 30 ]
	};
	
	var poster_size = document.getElementsByName( 'poster-size' )[ 0 ].value;
	var poster_orientation = document.getElementsByName( 'poster-orientation' )[ 0 ].value;
	
	var new_settings = get_current_settings();
	var screen_width = $( 'html' ).width();
	var screen_height = $( 'html' ).height();
	var poster_width, poster_height;
	if ( poster_orientation === 'portrait' ) {
		poster_width = poster_dimensions[ poster_size ][ 0 ];
		poster_height = poster_dimensions[ poster_size ][ 1 ];
	} else {
		poster_width = poster_dimensions[ poster_size ][ 1 ];
		poster_height = poster_dimensions[ poster_size ][ 0 ];
	}
	var screen_ratio = screen_width / screen_height;
	var poster_ratio = poster_width / poster_height;
	var new_width, new_height;
	
	if ( screen_ratio > poster_ratio ) {
		new_width = screen_height * poster_ratio;
		new_height = screen_height;
	} else {
		new_width = screen_width;
		new_height = screen_width / poster_ratio;
	}
	
	new_settings[ 'canvas-width' ] = new_width;
	new_settings[ 'canvas-height' ] = new_height;
	
	load_settings( new_settings );
}

function buy_poster_total()
{
	var poster_prices = {
		'small' : '15',
		'medium' : '40',
		'large' : '55'
	};
	var poster_size = document.getElementsByName( 'poster-size' )[ 0 ].value;
	return poster_prices[ poster_size ];
}

function buy_poster_form_submit( submit_event )
{
	submit_event.preventDefault();
}

function buy_poster_on_enter( key_event )
{
	if ( check_for_enter_key_event( key_event ) )
	{
		buy_poster();
	}
}

function buy_poster()
{
	Stripe.setPublishableKey( 'pk_live_5GYzGkxM6bSXlvnIZWNC2n48' );
	$( '.buy-poster-form' ).find( 'button' ).prop( 'disabled', true );
	Stripe.card.createToken( $( '.buy-poster-form' ), buy_poster_payment_callback );
}

function buy_poster_payment_callback( status, response )
{
	$( '.buy-poster-form' ).find( 'button' ).prop( 'disabled', false );
	if ( status === 200 ) {
		document.getElementById( 'buy-poster-token' ).value = response[ 'id' ];
		show_buy_poster_setting( 'buy-poster-shipping' );
	} else {
		if ( typeof response[ 'error' ] === 'undefined' || typeof response[ 'error' ][ 'message' ] === 'undefined' ) {
			$( '.buy-poster-error' ).html( 'Something went wrong connecting with the server. Please try again after a while.' );
		} else {
			$( '.buy-poster-error' ).html( response[ 'error' ][ 'message' ] );
		}
	}
}

function ship_poster_form_submit( submit_event )
{
	submit_event.preventDefault();
	ship_poster();
}

function ship_poster()
{
	if ( document.getElementById( 'buy-poster-token' ).value === '' ) {
		show_buy_poster_setting( 'buy-poster-payment' );
		$( '.buy-poster-error' ).html( 'Somehow your payment did not go through. Please fill out your payment details again.' );
		return;
	}
	
	var text_input_ids = [
		'buy-poster-token',
		'shipping-first-name',
		'shipping-last-name',
		'shipping-address',
		'shipping-city',
		'shipping-state',
		'shipping-zip',
		'shipping-email'
	];
	var ship_poster_data = {};
	
	for ( var i in text_input_ids )
	{
		var text_input_id = text_input_ids[ i ];
		if ( document.getElementById( text_input_id ).value === '' )
		{
			$( '.ship-poster-error' ).html( 'All fields are required.' );
			document.getElementById( text_input_id ).select();
			return;
		}
		ship_poster_data[ text_input_id ]
			= document.getElementById( text_input_id ).value;
	}
	
	ship_poster_data[ 'destination-link' ] = window.location.origin + '/#' + convert_settings_to_get_parameters();
	ship_poster_data[ 'poster-size' ] = document.getElementsByName( 'poster-size' )[ 0 ].value;
	ship_poster_data[ 'poster-orientation' ] = document.getElementsByName( 'poster-orientation' )[ 0 ].value;
	ship_poster_data[ 'order-total' ] = format_human_readable_dollars( buy_poster_total() );
	
	$.post( '/buy-poster/', ship_poster_data )
		.done( buy_poster_success )
		.fail( ship_poster_ajax_fail );
}

function buy_poster_success( response )
{
	response = JSON.parse( response );
	if ( typeof response[ 'success' ] === 'undefined' ) {
		$( '.ship-poster-error' ).html( 'Something went wrong while communicating with the server. It is possible that the payment gateway is down. Please try again after a while. If the problem persists, please contact us and let us know.' );
	} else if ( response[ 'success' ] === 'true' ) {
		show_buy_poster_setting( 'buy-poster-success' );
	} else if ( response[ 'success' ] === 'false' ) {
		$( '.ship-poster-error' ).html( 'The payment was rejected. Please try again, or use a different payment method.' );
	}
}

function ship_poster_ajax_fail()
{
	$( '.ship-poster-error' ).html( 'Your browser is having some trouble connecting to our server. You will not be charged until the full transaction is successful. Please check your internet connection, or try again after a while.' );
}

/* --------------------Buy Poster-------------------- */



/* --------------------Utilities-------------------- */

/* ---------------Scrub Input--------------- */

function scrub_input_by_type( input, type )
{
	if ( type === 'integer' )
	{
		return parseInt( input );
	}
	else if ( type === 'float' )
	{
		return parseFloat( input );
	}
	else if ( type === 'boolean' )
	{
		return input === 'true';
	}
	else if ( type === 'six-hex-string' )
	{
		return scrub_six_hex_string( input );
	}
	else if ( type === 'string' )
	{
		return input.toString();
	}
	return false;
}

function scrub_six_hex_string( input )
{
	if ( ! input.length || input.length % 6 !== 0 )
	{
		return false;
	}
	
	if ( input.toLowerCase().match( /^[a-f0-9]+$/ ) )
	{
		return input.toLowerCase();
	}
	
	return false;
}

function check_array_each_second_dimension_is_array( first_dimension )
{
	var each_second_dimension_is_array = true;
	first_dimension.forEach(
		function( second_dimension )
		{
			if ( Object.prototype.toString.call( second_dimension ) !== '[object Array]' )
			{
				each_second_dimension_is_array = false;
			}
		}
	);
	return each_second_dimension_is_array;
}

function get_json_object_from_string( json_string )
{
	if ( json_string === '' )
	{
		return false;
	}
	
	try
	{
		var url_settings = JSON.parse( json_string );
		return url_settings;
	}
	catch ( error )
	{
		return false;
	}
}

/* ---------------Scrub Input--------------- */

function check_arrays_are_the_same( array1, array2 )
{
	return array1.join( '~~' ) === array2.join( '~~' );
}

function six_hex_to_rgb( six_hex )
{
	var rgb = new Array();
	rgb[ 0 ] = hex_to_decimal( six_hex.substr( 0, 2 ) );
	rgb[ 1 ] = hex_to_decimal( six_hex.substr( 2, 2 ) );
	rgb[ 2 ] = hex_to_decimal( six_hex.substr( 4, 2 ) );
	return rgb;
}

function rgb_to_six_hex( rgb )
{
	var six_hex = '';
	six_hex += decimal_to_hex( rgb[ 0 ] );
	six_hex += decimal_to_hex( rgb[ 1 ] );
	six_hex += decimal_to_hex( rgb[ 2 ] );
	return six_hex;
}

function decimal_to_hex( decimal, pad_length )
{
	var hex = decimal.toString( 16 );
	
	if ( ! pad_length )
	{
		pad_length = 2;
	}
	while ( hex.length < pad_length )
	{
		hex = '0' + hex;
	}
	
	return hex;
}

function hex_to_decimal( hex, pad_length )
{
	return parseInt( hex, 16 );
}

function is_integer( integer_to_test )
{
	return integer_to_test === parseInt( integer_to_test );
}

function object_equal( object1, object2 )
{
	if ( typeof object1 !== 'object' || typeof object2 !== 'object' )
	{
		return false;
	}
	
	var objects_are_equal = true;
	
	$.each(
		object1,
		function( key, value )
		{
			if ( object2[ key ] === undefined )
			{
				objects_are_equal = false;
				return;
			}
			if ( typeof object2[ key ] === 'object' )
			{
				objects_are_equal = object_equal( object1[ key ], object2[ key ] );
				if ( ! objects_are_equal )
				{
					return;
				}
			}
			if ( object1[ key ] !== object2[ key ] )
			{
				objects_are_equal = false;
				return;
			}
		}
	);
	
	return objects_are_equal;
}

function get_mouse_x_y_within_element( dom_element, event )
{
	var x, y;
	
	var dom_element_offset = $( dom_element ).offset();
	
	x = event.pageX - dom_element_offset.left;
	y = event.pageY - dom_element_offset.top;
	
	return { 'x': x, 'y': y };
}

function request_browser_full_screen()
{
	var body = document.body;
	if( body.requestFullScreen )
	{
		body.requestFullScreen();
	}	
	else if( body.webkitRequestFullScreen )
	{
		body.webkitRequestFullScreen();
	}	
	else if( body.mozRequestFullScreen )
	{
		body.mozRequestFullScreen();
	}	
}

function legacy_settings_to_new_settings( unparsed_json )
{
	var parsed_json = JSON.parse( unparsed_json );
	parsed_json[ 'gradient-colors' ]
		= legacy_gradient_colors_to_six_hex( parsed_json[ 'gradient-colors' ] );
	return parsed_json;
}

function legacy_gradient_colors_to_six_hex( uri_component )
{
	var decoded_uri = decodeURIComponent( uri_component );
	var parsed_json;
	try {
		parsed_json = JSON.parse( decoded_uri );
	} catch ( error )
	{
		parsed_json = uri_component;
	}
	var hex_output = '';
	for ( var i = 0; i < parsed_json.length; i++ )
	{
		if ( typeof parsed_json[ i ] === 'array' )
		{
			hex_output += rgb_to_six_hex( parsed_json[ i ] );
		}
		else if ( typeof parsed_json[ i ] === 'object' )
		{
			var rgb = new Array;
			rgb.push( parsed_json[ i ][ 'r' ] );
			rgb.push( parsed_json[ i ][ 'g' ] );
			rgb.push( parsed_json[ i ][ 'b' ] );
			hex_output += rgb_to_six_hex( rgb );
		}
	}
	return hex_output;
}

function check_for_enter_key_event( key_event )
{
	if ( typeof key_event.keyCode === 'undefined' ) { return false; }
	if ( key_event.keyCode === 13 ) { return true; }
	return false;
}

function format_human_readable_dollars( number )
{
	if ( isNaN( number ) ) { throw "Input to format_human_readable_dollars must be a number. The following was provided: " + number; }
	var number_string = number.toString();
	if ( ! number_string.match( /\./ ) ) {
		number_string += '.00';
	}
	var dollars_and_cents = number_string.split( '.' );
	if ( dollars_and_cents[ 1 ].length > 2 ) {
		// It's not very obvious what's happening here.
		// We know that we have more digits than we need, so we want to round up:
		// We take the first three digits
		// Convert them to an integer, divide by 10, and take the ceiling
		dollars_and_cents[ 1 ] = Math.ceil( parseInt( dollars_and_cents[ 1 ].substring( 0, 3 ) ) / 10 ).toString();
	}
	while ( dollars_and_cents[ 1 ].length < 2 ) {
		dollars_and_cents[ 1 ] += '0';
	}
	if ( dollars_and_cents[ 0 ].length > 3 ) {
		var dollars = dollars_and_cents[ 0 ];
		var new_dollars_and_cents = '';
		for ( var i = 0; i < dollars.length; i++ ) {
			if ( i != 0 && i % 3 === 0 ) {
				new_dollars_and_cents = ',' + new_dollars_and_cents;
			}
			new_dollars_and_cents = dollars[ dollars.length - 1 - i ] + new_dollars_and_cents;
		}
		dollars_and_cents[ 0 ] = new_dollars_and_cents;
	}
	return '$' + dollars_and_cents[ 0 ] + '.' + dollars_and_cents[ 1 ];
}

/* --------------------Utilities-------------------- */