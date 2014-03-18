#!/usr/bin/env php
<?php
/* This script caclulates the relative sample size per project after validation and cleansing. */

$sampleFile = '../../data/monthlyFactsAfterCleaningWithMetaDataAndId.csv';
$sourceFile = '../../data/monthlyFactsWithProperEnlistments.csv';
$outputFile = 'projectsValidation.csv';

$sampleFileHandle = fopen( $sampleFile, 'rb' );
$sampleHeader = fgetcsv( $sampleFileHandle );

$sampleData = array();

while ( !feof( $sampleFileHandle ) ) {
	$sampleLineData = fgetcsv( $sampleFileHandle );
	
	if ( empty( $sampleLineData ) ) {
		continue;
	}
	
	$sampleLineData = array_combine( $sampleHeader, $sampleLineData );
	
	if ( isset( $sampleData[ $sampleLineData[ 'project_name_fact' ] ] ) ) {
		$sampleData[ $sampleLineData[ 'project_name_fact' ] ][] = $sampleLineData;
	}
	else {
		$sampleData[ $sampleLineData[ 'project_name_fact' ] ] = array(
			$sampleLineData
		);
	}
}

fclose( $sampleFileHandle );

$sourceFileHandle = fopen( $sourceFile, 'rb' );
$sourceHeader = fgetcsv( $sourceFileHandle );

$factCount = 0;
fwrite( STDERR, "{$factCount} facts validated." );

while ( !feof( $sourceFileHandle ) ) {
	$sourceLineData = fgetcsv( $sourceFileHandle );
	
	if ( empty( $sourceLineData ) ) {
		continue;
	}
	
	$sourceLineData = array_combine( $sourceHeader, $sourceLineData );
	
	if ( !isset( $sampleData[ $sourceLineData[ 'project_name_fact' ] ] ) ) {
		continue;
	}
	
	$sampleLine = & $sampleData[ $sourceLineData[ 'project_name_fact' ] ];
	
	if ( isset( $sampleLine[ 'size' ] ) ) {
		$sampleLine[ 'size' ]++;
	}
	else {
		$sampleLine[ 'size' ] = 1;
	}
	
	$factCount++;
	fwrite( STDERR, "\r{$factCount} facts validated." );
}

fclose( $sourceFileHandle );
fwrite( STDERR, "\r{$factCount} facts validated.\n" );

$outputFileHandle = fopen( $outputFile, 'w' );
$outputHeader = array( 'project_id', 'project_name', 'data_size', 'sample_size', 'relative_size' );
fputcsv( $outputFileHandle, $outputHeader, ';' );

$sizes = array();

foreach ( $sampleData as $projectName => $facts ) {
	$dataSize = $facts[ 'size' ];
	$sampleSize = count( $facts ) - 1;
	$relativeSize = $sampleSize / $dataSize;
	
	$outputLine = array(
		'project_id' => $facts[ 0 ][ 'project_id' ],
		'project_name' => $projectName,
		'data_size' => $dataSize,
		'sample_size' => $sampleSize,
		'relative_size' => number_format( $relativeSize, 10, ',', '' )
	);
	
	fputcsv( $outputFileHandle, array_values( $outputLine ), ';' );
	
	$sizes[] = $relativeSize;
}

fclose( $outputFileHandle );

function mean( array $values ) {
	return array_sum( $values ) / count( $values );
}

fwrite( STDERR, count( $sampleData ) . " projects.\n" );
fwrite( STDERR, min( $sizes ) . " smallest coverage.\n" );
fwrite( STDERR, max( $sizes ) . " highest coverage.\n" );
fwrite( STDERR, mean( $sizes ) . " mean coverage.\n" );
fwrite( STDERR, "Done.\n" );
