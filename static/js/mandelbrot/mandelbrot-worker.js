//Note the duality of syntax - in the worker context, these variables refer to an iteration_pixels_section - in the main context, they refer to the entire iteration_pixels
var iteration_pixels;
var max_iterations;
var max_iteration_pixel;
var min_iteration_pixel;

var fractal_function_table = {
	'leaves' : fractal_leaves,
	'tendrils' : fractal_tendrils,
	'ocean-floor' : fractal_ocean_floor,
	'hyperspace' : fractal_hyperspace,
	'space' : fractal_space,
	'quantum' : fractal_quantum,
	'generalized-celtic' : fractal_generalized_celtic,
	'burning-ship' : fractal_burning_ship,
	'halloween' : fractal_halloween,
	'test' : fractal_test
};

/* --------------------Worker Messaging-------------------- */

if ( typeof window === 'undefined' )
{
	onmessage = function( message_event )
	{
		var message = message_event.data;
		generate_iteration_pixels_section( message );
		postMessage(
			{
				'iteration-pixels' : iteration_pixels,
				'initial-row-index' : message[ 'initial-row-index' ],
				'final-row-index' : message[ 'final-row-index' ],
				'max-iteration-pixel' : max_iteration_pixel,
				'min-iteration-pixel' : min_iteration_pixel
			}
		);
	};
}

function generate_iteration_pixels_section( new_settings )
{
	max_iterations = new_settings[ 'max-iterations' ];
	max_iteration_pixel = 0;
	min_iteration_pixel = max_iterations;
	create_iteration_pixels_skeleton(
		new_settings[ 'final-row-index' ]
			- new_settings[ 'initial-row-index' ]
	);
	generate_x_iteration_pixels( new_settings );
}

/* --------------------Worker Messaging-------------------- */



/* --------------------Generate Mandelbrot-------------------- */

function create_iteration_pixels_skeleton( y_limit )
{
	iteration_pixels = new Array(); //Column
	
	for ( var y = 0; y < y_limit; y++ )
	{
		iteration_pixels[ y ] = new Array(); //Rows
	}
}

function generate_x_iteration_pixels( new_settings )
{
	var canvas_width = new_settings[ 'canvas-width' ];
	for ( var x_pixel = 0; x_pixel < canvas_width; x_pixel++ )
	{
		var x0 = map_x_pixel_to_cartesian( x_pixel, new_settings );
		
		generate_y_iteration_pixels( new_settings, x0, x_pixel );
	}
}

function map_x_pixel_to_cartesian( x_pixel, new_settings )
{
	return (
		( x_pixel
			- ( new_settings[ 'canvas-width' ] / 2 )
		) / new_settings[ 'zoom-level' ]
	) + new_settings[ 'x' ];
}

function generate_y_iteration_pixels( new_settings, x0, x_pixel )
{
	for (
		var y_pixel = 0;
		y_pixel < new_settings[ 'final-row-index' ] - new_settings[ 'initial-row-index' ];
		y_pixel++
	) {
		var y0 = map_y_pixel_to_cartesian( y_pixel + new_settings[ 'initial-row-index' ], new_settings );
		
		var iteration = fractal_function_table[ new_settings[ 'fractal' ] ]( x0, y0 );
		
		iteration_pixels[ y_pixel ][ x_pixel ] = iteration;
		
		if ( iteration > max_iteration_pixel )
		{
			max_iteration_pixel = iteration;
		}
		if ( iteration < min_iteration_pixel )
		{
			min_iteration_pixel = iteration;
		}
	}
}

function map_y_pixel_to_cartesian( y_pixel, new_settings )
{
	return (
		-1 * (
			( y_pixel
				- ( new_settings[ 'canvas-height' ] / 2 )
			) / new_settings[ 'zoom-level' ] )
	) + new_settings[ 'y' ];
}

function fractal_leaves( x0, y0 )
{
	var x = 0;
	var y = 0;
	
	var iteration = 0;
	
	while ( x*x - y*y < 4 && iteration < max_iterations )
	{
		var x_temporary = x*x - y*y + x0;
		
		y = 2*x*y + y0;
		
		x = x_temporary;
		
		iteration++;
	}
	
	return iteration;
}

function fractal_tendrils( x0, y0 )
{
	var x = 0;
	var y = 0;
	
	var iteration = 0;
	
	while ( x*x - y*y < 10000 && iteration < max_iterations )
	{
		var x_temporary = x*x - y*y + x0;
		
		y = 2*x*y + y0;
		
		x = x_temporary;
		
		iteration++;
	}
	
	return iteration;
}

function fractal_ocean_floor( x0, y0 )
{
	var x = 0;
	var y = 0;
	
	var iteration = 0;
	
	while ( x*x + y*y < 4 && iteration < max_iterations )
	{
		var x_temporary = x*x - y*y + x0;
		
		y = 2*x*y + y0;
		
		x = x_temporary;
		
		iteration++;
	}
	
	return iteration;
}

function fractal_hyperspace( x0, y0 )
{
	var x = 0;
	var y = 0;
	
	var iteration = 0;
	
	while ( x*x + y*y < 1000 && iteration < max_iterations )
	{
		var x_temporary = x*x - Math.tan( y*y ) + x0;
		
		y = 2*x*y + y0;
		
		x = x_temporary;
		
		iteration++;
	}
	
	return iteration;
}

function fractal_space( x0, y0 )
{
	var x = 0;
	var y = 0;
	
	var iteration = 0;
	
	while ( x*x - y*y < 2 && iteration < max_iterations )
	{
		var x_temporary = Math.sin( x*x ) - Math.sin( y*y ) + x0;
		
		y = 2*x*y + y0;
		
		x = x_temporary;
		
		iteration++;
	}
	
	return iteration;
}

function fractal_quantum( x0, y0 )
{
	var x = 0;
	var y = 0;
	
	var iteration = 0;
	
	while ( x*x - y*y < 0.2 && iteration < max_iterations )
	{
		var x_temporary = x*x - Math.sin( y*y ) + x0;
		
		y = 2*x*y + y0;
		
		x = x_temporary;
		
		iteration++;
	}
	
	return iteration;
}

function fractal_generalized_celtic( x0, y0 )
{
	var x = 0;
	var y = 0;
	
	var iteration = 0;
	
	while ( x*x + y*y < 4 && iteration < max_iterations )
	{
		var x_temporary = Math.abs( x*x - y*y ) + x0;
		
		y = 2*x*y + y0;
		
		x = x_temporary;
		
		iteration++;
	}
	
	return iteration;
}

function fractal_burning_ship( x0, y0 )
{
	var x = 0;
	var y = 0;
	
	var iteration = 0;
	
	while ( x*x + y*y < 4 && iteration < max_iterations )
	{
		var x_temporary = x*x - y*y + x0;
		
		y = Math.abs( 2*x*y ) + y0;
		
		x = x_temporary;
		
		iteration++;
	}
	
	return iteration;
}

function fractal_halloween( x0, y0 )
{
	var x = 0;
	var y = 0;
	
	var iteration = 0;
	
	while ( x*x + y*y < 10000 && iteration < max_iterations )
	{
		var x_temporary = Math.sin( x*x ) - y*y + x0;
		
		y = 2*x*y + y0;
		
		x = x_temporary;
		
		iteration++;
	}
	
	return iteration;
}

function fractal_angles( x0, y0 )
{
	var x = 0;
	var y = 0;
	
	var iteration = 0;
	
	while ( x*x + y*y < 100 && iteration < max_iterations )
	{
		var x_temporary = Math.sqrt( x*x ) - Math.sqrt( y*y ) + x0;
		
		y = 2*x*y + y0;
		
		x = x_temporary;
		
		iteration++;
	}
	
	return iteration;
}

function fractal_test( x0, y0 )
{
	var x = 0;
	var y = 0;
	
	var iteration = 0;
	
	while ( x*x + y*y < 100 && iteration < max_iterations )
	{
		var x_temporary = ( x*x ) - Math.atan( y*y ) + x0;
		
		y = 2*x*y + y0;
		
		x = x_temporary;
		
		iteration++;
	}
	
	return iteration;
}

/* --------------------Generate Mandelbrot-------------------- */