#!/usr/bin/env php
<?php

const ADD_KEY = 'project_id';
const MATCH_KEY = 'project_name_fact';

$sourceFile		= '../../data/SampleProjectNamesListWithIds.csv';
$MATCH_KEY		= MATCH_KEY;
$projectsFile	= '../../data/monthlyFactsAfterCleaningWithMetaData.csv';
$finalFile		= 'monthlyFactsAfterCleaningWithMetaDataAndId.csv';

function readAllProjectIds( $sourceFile ) {
	if ( !file_exists( $sourceFile ) ) {
		fwrite( STDERR, "ERROR: Source file not found: '{$sourceFile}." . PHP_EOL );
		exit( 1 );
	}
	
	$projectData = array();
	$sourceHandle = fopen( $sourceFile, 'rb' ); // handle to projects ID file
	fgetcsv( $sourceHandle, null, ';', '"' ); // header
	
	while ( !feof( $sourceHandle ) ) {
		$lineData = fgetcsv( $sourceHandle, null, ';', '"' );
		
		if ( empty( $lineData ) ) {
			continue;
		}
		
		$projectData[ $lineData[ 1 ] ] = $lineData[ 0 ];
	}
	
	fclose( $sourceHandle );
	return $projectData;
}

$projectData = readAllProjectIds( $sourceFile ); // array( name => id )

$projectsHandle = fopen( $projectsFile, 'rb' ); // handle to projects monthly facts
$finalHandle = fopen( $finalFile, 'w' );

$monthlyHeaders = fgetcsv( $projectsHandle, null, ',', '"' );

if ( empty( $monthlyHeaders ) ) {
	fwrite( STDERR, "ERROR: No data found in '{$projectsFile}'." . PHP_EOL );
}

$finalHeaders = $monthlyHeaders;
$finalHeaders[] = ADD_KEY;

fputcsv( $finalHandle, $finalHeaders, ',', '"' );

$factCount = 0;
$skipCount = 0;
$projectIds = array();

fwrite( STDERR, "Processing..." . PHP_EOL );

while ( !feof( $projectsHandle ) ) {
	$monthlyData = fgetcsv( $projectsHandle, null, ',', '"' );
	
	if ( empty( $monthlyData ) ) {
		continue;
	}
	
	$monthlyData = array_combine( $monthlyHeaders, $monthlyData );
	
	if (
		empty( $monthlyData[ $MATCH_KEY ] )
		|| empty( $projectData[ $monthlyData[ $MATCH_KEY ] ] )
	) {
		$skipCount++;
		continue;
	}
	
	$projectId = $projectData[ $monthlyData[ $MATCH_KEY ] ];
	$monthlyData[ ADD_KEY ] = $projectId;
	
	if ( !isset( $projectIds[ $projectId ] ) ) {
		$projectIds[ $projectId ] = 1;
	}
	else {
		$projectIds[ $projectId ]++;
	}
	
	fputcsv( $finalHandle, $monthlyData, ',', '"' );
	
	$factCount++;
	fwrite( STDERR, "\rWritten {$factCount} facts to '{$finalFile}'." );
}

fclose( $finalHandle );
fclose( $projectsHandle );

fwrite( STDERR, "\rWritten {$factCount} facts to '{$finalFile}'." . PHP_EOL );
fwrite( STDERR, "{$skipCount} skipped facts." . PHP_EOL );
fwrite( STDERR, "Data of " . count( $projectIds ) . " projects written to file." . PHP_EOL );

$allProjectIds = array_values( $projectData );
$noDataProjects = array_diff( $allProjectIds, array_keys( $projectIds ) );

$projectNamesNoData = array();
$projectDataFlipped = array_flip( $projectData );

foreach ( $noDataProjects as $id ) {
	$projectNamesNoData[ $id ] = $projectDataFlipped[ $id ];
}

fwrite( STDERR, count( $projectNamesNoData ) . " projects have no data." . PHP_EOL );
fwrite( STDERR, "Details: " . var_export( $projectNamesNoData, true ) . PHP_EOL );
