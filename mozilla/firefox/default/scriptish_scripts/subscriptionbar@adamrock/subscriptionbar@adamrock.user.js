// ==UserScript==
// @name           Subscription Bar
// @namespace      Adam Rock
// @include        http://*youtube.com/watch?v=*
// ==/UserScript==


if (/[\?&]playnext=1/i.test(document.location.href)) 
    document.location.replace(document.location.href.replace("playnext=1","playnext=0"));


GM_addStyle("#quicklist-bar{display: none}");
GM_addStyle("#quicklist-tray-container{display: none}");
GM_addStyle("#quicklist-bar-container{display: none}");