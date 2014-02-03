/*!
 * main.js
 */
const START_DATE = new Date( 2014, 1, 1, 0, 0, 0, 0 );
const MS_IN_DAY = ( 1000 * 60 * 60 * 24 );
const PAGE_CONTAINER = document.getElementById( 'contents' );
const HEADING_CONTAINER = document.getElementById( 'page-title' );

( function () {
	document.addEventListener( 'DOMContentLoaded', function ( event ) {
		buildNavigation( document.getElementById( 'menu' ) );
		
		var page = parseHref( window.location.href );
		
		if ( page ) {
			loadPage( page, PAGE_CONTAINER );
			setHeading( new Date( page ).toDateString(), HEADING_CONTAINER );
		}
	} );
} )();

function buildNavigation( container ) {
	var start = START_DATE,
		now = new Date(),
		diff = now.valueOf() - start.valueOf(),
		diffDays = parseInt( diff / MS_IN_DAY ) + 1,
		list = document.createElement( 'ul' ),
		item,
		link;
	
	for ( var i = 0; i < diffDays; i++ ) {
		link = document.createElement( 'a' );
		var month = start.getMonth() + 1;
		link.setAttribute( 'href', '#' + start.getFullYear() + '-' + month + '-' + start.getDate() );
		link.appendChild( document.createTextNode( start.toDateString() ) );
		link.addEventListener( 'click', onMenuLinkClicked );

		item = document.createElement( 'li' );
		item.appendChild( link );
		
		list.appendChild( item );
		
		start.setDate( start.getDate() + 1 );
	}
	
	container.appendChild( list );
};

function parseHref( href ) {
	var pieces = href.split( '#' );
	return pieces[ 1 ];
};

function onMenuLinkClicked( event ) {
	var href = this.getAttribute( 'href' ),
		page = parseHref( href );

	loadPage( page, PAGE_CONTAINER );
	setHeading( new Date( page ).toDateString(), HEADING_CONTAINER );
};

function loadPage( page, container ) {
	if ( container.nodeName != 'IFRAME' ) {
		throw Error( 'Page container must be an iframe.' );
	}
	
	var file = page + '.txt';
	container.setAttribute( 'src', file );
};

function setHeading( title, element ) {
	element.innerHTML = '';
	element.appendChild( document.createTextNode( title ) );
};
