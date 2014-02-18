#!/usr/bin/env php
<?php
date_default_timezone_set( 'Europe/Amsterdam' );

/**
 * This script converts a CSV from OhlohAnalytics project to R script input.
 * I'm not fond of using globals, however, they come in handy to keep state in a non-OO script.
 */

const STATUS_OK = 0;
const STATUS_ERROR_GENERAL = 1;

const NOT_AVAILABLE_VALUE = 'NA';
const CSV_OUT_DELIMITER = ';';
const CSV_OUT_ENCLOSURE = '"';

global $CSV_MAP;
$CSV_MAP = array(
	'ProjectId' => 'project_name_fact',
	'CommitId' => '',
	'Date' => array( 'createDate' => array( 'Year', 'Month', 'Day' ) ),
	'DeveloperId' => '',
	'Active Developers' => 'contributors_fact',
	'Commit LOC Added' => 'loc_added_fact',
	'Commit LOC Churn' => array( 'sum' => array( 'LOC Added', 'Commit LOC Modified', 'Commit LOC Removed' ) ),
	'Commit LOC Modified' => '',
	'Commit LOC Removed' => 'loc_deleted_fact',
	'Cumulative Developers' => array( 'cumulative' => array( 'Active Developers' ) ),
	'Cumulative LOC Added' => array( 'cumulative' => array( 'LOC Added' ) ),
	'Cumulative LOC Churn' => array( 'cumulative' => array( 'LOC Added', 'Commit LOC Modified', 'Commit LOC Removed' ) ),
	'Cumulative LOC Modified' => array( 'cumulative' => array( 'Commit LOC Modified' ) ),
	'Cumulative LOC Removed' => array( 'cumulative' => array( 'Commit LOC Removed' ) ),
	'LOC' => 'loc_fact',
	'Relative Date Progress' => '',
	'Relative LOC Churn Progress' => '',
	'Relative Team Size' => array( 'relativeTeamSize' => array( 'Active Developers' ) ),
	'Files' => '',
	'Year' => 'year_fact',
	'Month' => 'month_fact',
	'Day' => array( 'value' => array( '1' ) )
);

global $CSV_OUT_HEADER;
$CSV_OUT_HEADER = array_keys( $CSV_MAP );

global $warned; // keeps track of what columns have been warned to not have a mapping.
$warned = array();

// Keeps track of current column
global $currentColumn;

global $previousRow; // keeps the previous output row
$previousRow = array();

global $currentProject;
global $projectCounter;

/**
 * Prints help.
 */
function showHelp() {
	$text = <<<HELP
Usage: %script% options

Options:
	-h	Shows this help.
	-i	Specify an input file [required].
	-m	Specify a maximum number of projects [optional].
	-o	Specify an output file [optional].
	
Example usage:
	$ %script% -i input.csv
	The above will output the converted input file to the console.
	
	$ %script% -i input.csv -o output.csv
	The above will send the output to the supplied output file.

HELP;
	
	out( str_replace( '%script%', basename( __FILE__ ), $text ) );
	stop();
}

/**
 * Writes a message to standard output.
 * 
 * @param string $message
 */
function out( $message ) {
	fwrite( STDOUT, $message . PHP_EOL );
}

/**
 * Writes an error to standard error.
 * 
 * @param string $message
 */
function error( $message ) {
	fwrite( STDERR, "ERROR: {$message}" . PHP_EOL );
}

/**
 * Writes a warning to standard error.
 * 
 * @param string $message
 */
function warning( $message ) {
	fwrite( STDERR, "WARNING: {$message}" . PHP_EOL );
}

/**
 * Writes an info message to standard error.
 * 
 * @param string $message
 */
function info( $message ) {
	fwrite( STDERR, "INFO: {$message}" . PHP_EOL );
}

/**
 * Exits the script with given status code.
 * 
 * @param integer $status
 */
function stop( $status = STATUS_OK ) {
	exit( $status );
}

/**
 * Warns about missing mapping for given column.
 * 
 * @param string $column
 */
function warnNoColumnMapping( $column ) {
	global $warned;
	
	if ( in_array( $column, $warned ) ) {
		return;
	}
	else {
		$warned[] = $column;
	}
	
	warning( "No mapping specified for column '{$column}'." );
}

/**
 * Converts the current input row to a new output row.
 * 
 * @param array $currentInputRow
 * 
 * @return array
 */
function mapRow( array $currentInputRow ) {
	global $CSV_MAP,
		$currentColumn,
		$currentProject,
		$projectCounter,
		$previousRow;
	
	$result = array_fill_keys( array_keys( $CSV_MAP ), NOT_AVAILABLE_VALUE );
	
	foreach ( $CSV_MAP as $column => $mapping ) {
		if ( empty( $mapping ) ) {
			warnNoColumnMapping( $column );
			continue;
		}
		
		$currentColumn = $column; // set global current column
		
		if ( $column == 'ProjectId' ) {
			// set global current project
			$currentProject = $currentInputRow[ $mapping ];
			
			if ( empty( $previousRow ) || $previousRow[ 'ProjectId' ] != $currentProject ) {
				$projectCounter++;
			}
		}
		
		if ( is_array( $mapping ) ) {
			if ( key( $mapping ) != 'value' ) {
				$values = array_map(
					function ( $mapColumn ) use ( $currentInputRow, $CSV_MAP ) {
						if ( !isset( $CSV_MAP[ $mapColumn ] ) ) {
							warnNoColumnMapping( $mapColumn );
							return NOT_AVAILABLE_VALUE;
						}
						
						$originalColumn = $CSV_MAP[ $mapColumn ];
						
						if ( empty( $originalColumn ) ) {
							return NOT_AVAILABLE_VALUE;
						}
						
						if ( is_array( $originalColumn ) && key( $originalColumn ) == 'value' ) {
							$values = reset( $originalColumn );
							return reset( $values );
						}
						
						if ( !isset( $currentInputRow[ $originalColumn ] ) ) {
							warning( "No column in input file: '{$originalColumn}'." );
							return NOT_AVAILABLE_VALUE;
						}
						
						return $currentInputRow[ $originalColumn ];
					},
					reset( $mapping )
				);
			}
			else {
				$values = reset( $mapping );
			}
			
			$result[ $column ] = call_user_func_array(
				key( $mapping ),
				$values
			);
		}
		else {
			$result[ $column ] = $currentInputRow[ $mapping ];
		}
	}
	
	return $result;
}

/**
 * Returns the given value.
 * 
 * @param mixed $value
 * 
 * @return mixed
 */
function value( $value ) {
	return $value;
}

/**
 * Formats to a date string.
 * 
 * @param integer $year
 * @param integer $month
 * @param integer $day
 * 
 * @return string
 */
function createDate( $year, $month, $day ) {
	$values = array_map(
		function ( $value ) {
			return (int) $value;
		},
		array( $year, $month, $day )
	);

	$date = \DateTime::createFromFormat( 'Y-m-d', "{$values[ 0 ]}-{$values[ 1 ]}-{$values[ 2 ]}" );
	return $date->format( "Y-m-d" );
}

/**
 * Computes cumulative value.
 * 
 * @param integer $args ...
 * 
 * @return integer
 */
function cumulative() {
	global $currentColumn,
		$previousRow;
	
	if ( !empty( $previousRow ) ) {
		if ( !isset( $previousRow[ $currentColumn ] ) ) {
			error( "The previous row is missing a column: '{$currentColumn}'." );
			stop( STATUS_ERROR_GENERAL );
		}
		
		$values[] = $previousRow[ $currentColumn ];
	}
	else {
		$values = func_get_args();
	}
	
	return array_sum( $values );
}

/**
 * Sums the input arguments.
 * 
 * @param integer $args ...
 * 
 * @return integer
 */
function sum() {
	return array_sum( func_get_args() );
}

/**
 * Computes relative of team size.
 * 
 * @return integer
 */
function relativeTeamSize() {
	global $previousRow,
		$currentColumn,
		$currentProject;
	
	if ( empty( $previousRow ) || $previousRow[ 'ProjectId' ] != $currentProject ) {
		return 1;
	}
	
	$previousValue = $previousRow[ 'Active Developers' ];
	$newValue = func_get_arg( 0 );
	return $newValue - $previousValue;
}

// -------------------------------------------------------------------------------------------- //

// Read options

$shortOpts = array(
	'h',  // help
	'i:', // input file
	'm:', // maximum number of projects
	'o:'  // output file
);

$options = getopt( implode( '', $shortOpts ) );

if ( empty( $options ) ) {
	showHelp();
}

$inputFile = null;
$outputFile = null;
$maxProjects = null;

foreach ( $options as $optName => $optVal ) {
	switch ( $optName ) {
		case 'i':
			$inputFile = $optVal;
			break;
			
		case 'm':
			$maxProjects = (int) $optVal;
			break;
			
		case 'o':
			$outputFile = $optVal;
			break;
		
		case 'h':
		default:
			showHelp();
			break;
	}
}

// Check option values

if ( empty( $inputFile ) ) {
	error( "No input file specified." );
	stop( STATUS_ERROR_GENERAL );
}

if ( !file_exists( $inputFile ) ) {
	error( "No such file for input: '{$inputFile}'." );
	stop( STATUS_ERROR_GENERAL );
}

if ( !is_readable( $inputFile ) ) {
	error( "Input file not readable: '{$inputFile}'." );
	stop( STATUS_ERROR_GENERAL );
}

if ( $maxProjects < 0 ) {
	error( "Maximum projects must be a non-negative number." );
	stop( STATUS_ERROR_GENERAL );
}

// Prepare output stream

if ( !empty( $outputFile ) ) {
	$outputHandle = fopen( $outputFile, 'w' );
}
else {
	$outputHandle = STDOUT;
}

$inputHandle = fopen( $inputFile, 'r' );
$header = fgetcsv( $inputHandle );

fputcsv( $outputHandle, $CSV_OUT_HEADER, CSV_OUT_DELIMITER, CSV_OUT_ENCLOSURE );
$rowCounter = 0;
$projectCounter = 0;

while ( ( $values = fgetcsv( $inputHandle ) ) !== false ) {
	$row = array_combine( $header, $values );
	$outFields = mapRow( $row );
	$previousRow = $outFields; // set global previousRow

	if ( $maxProjects > 0 && $projectCounter > $maxProjects ) {
		$projectCounter--;
		break;
	}
	
	fputcsv( $outputHandle, $outFields, CSV_OUT_DELIMITER, CSV_OUT_ENCLOSURE );
	$rowCounter++;
}

fclose( $inputHandle );
fclose( $outputHandle );

info( "{$rowCounter} row(s) in {$projectCounter} project(s) converted." );
stop( STATUS_OK );
