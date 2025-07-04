// Security & privacy
// https://github.com/arkenfox/user.js
user_pref("app.normandy.api_url", "");
user_pref("app.normandy.enabled", false);
user_pref("app.shield.optoutstudies.enabled", false);
user_pref("beacon.enabled", false);
user_pref("browser.aboutConfig.showWarning", false);
user_pref("browser.contentblocking.category", "strict");
user_pref("browser.disableResetPrompt", true);
user_pref("browser.newtabpage.activity-stream.showSponsored", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
user_pref("browser.newtabpage.enhanced", false);
user_pref("browser.newtabpage.introShown", true);
user_pref("browser.newtab.preload", false);
user_pref("browser.safebrowsing.appRepURL", "");
user_pref("browser.safebrowsing.blockedURIs.enabled", false);
user_pref("browser.safebrowsing.downloads.enabled", false);
user_pref("browser.safebrowsing.downloads.remote.enabled", false);
user_pref("browser.safebrowsing.downloads.remote.url", "");
user_pref("browser.safebrowsing.enabled", false);
user_pref("browser.safebrowsing.malware.enabled", false);
user_pref("browser.safebrowsing.phishing.enabled", false);
user_pref("browser.search.suggest.enabled", false);
user_pref("browser.selfsupport.url", "");
user_pref("browser.send_pings", false);
user_pref("browser.startup.homepage_override.mstone", "ignore");
user_pref("dom.battery.enabled", false);
user_pref("dom.private-attribution.submission.enabled", false);
user_pref("dom.push.enabled", false);
user_pref("dom.security.https_only_mode_ever_enabled", true);
user_pref("dom.security.https_only_mode", true);
user_pref("dom.serviceWorkers.enabled", false);
user_pref("dom.webnotifications.enabled", false);
user_pref("experiments.activeExperiment", false);
user_pref("experiments.enabled", false);
user_pref("experiments.manifest.uri", "");
user_pref("experiments.supported", false);
user_pref("extensions.getAddons.cache.enabled", false);
user_pref("extensions.getAddons.showPane", false);
user_pref("extensions.htmlaboutaddons.recommendations.enabled", false);
user_pref("extensions.pocket.enabled", false);
user_pref("extensions.shield-recipe-client.api_url", "");
user_pref("extensions.shield-recipe-client.enabled", false);
user_pref("extensions.webservice.discoverURL", "");
user_pref("geo.enabled", false);
user_pref("media.autoplay.default", 5);
user_pref("network.allow-experiments", false);
user_pref("network.captive-portal-service.enabled", false);
user_pref("network.trr.mode", 5);
user_pref("permissions.default.desktop-notification", 2);
user_pref("permissions.default.geo", 2);
user_pref("privacy.query_stripping", true);
user_pref("privacy.trackingprotection.enabled", true);
user_pref("signon.firefoxRelay.feature", "disabled");
user_pref("signon.rememberSignons", false);

// Enable telemetry
user_pref("browser.crashReports.unsubmittedCheck.autoSubmit2", true);
user_pref("browser.tabs.crashReporting.sendReport", true);
user_pref("datareporting.healthreport.uploadEnabled", true);
user_pref("datareporting.usage.uploadEnabled", true);
user_pref("datareporting.policy.dataSubmissionEnabled", true);

// Tracking Protection whitelist
user_pref("urlclassifier.trackingSkipURLs", "disqus.com, c.disquscdn.com, cdmu.disqus.com");

// Tabs
user_pref("browser.tabs.firefox-view", false);
user_pref("browser.tabs.hoverPreview.enabled", true);
user_pref("browser.tabs.hoverPreview.showThumbnails", false);
user_pref("browser.tabs.loadBookmarksInBackground", true);

// Scrolling
user_pref("general.smoothScroll.lines.durationMaxMS", 100);
user_pref("general.smoothScroll.lines.durationMinMS", 50);
user_pref("general.smoothScroll.mouseWheel.durationMaxMS", 100);
user_pref("general.smoothScroll.mouseWheel.durationMinMS", 50);
user_pref("widget.non-native-theme.scrollbar.style", 1);

// Location bar
user_pref("browser.urlbar.groupLabels.enabled", false);
user_pref("browser.urlbar.quicksuggest.enabled", false);
user_pref("browser.urlbar.shortcuts.bookmarks", false);
user_pref("browser.urlbar.shortcuts.history", false);
user_pref("browser.urlbar.shortcuts.tabs", false);
user_pref("browser.urlbar.suggest.calculator", true);
user_pref("browser.urlbar.trimURLs", false);

// Search
user_pref("browser.search.separatePrivateDefault", true);
user_pref("browser.search.separatePrivateDefault.ui.enabled", true);
user_pref("browser.search.suggest.enabled", false);
user_pref("browser.search.suggest.enabled.private", false);

// Keyboard and mouse
user_pref("ui.key.menuAccessKeyFocuses", false);
user_pref("ui.key.textcontrol.prefer_native_key_bindings_over_builtin_shortcut_key_definitions", false);
user_pref("browser.chrome.toolbar_tips.hide_on_keydown", 1);
user_pref("middlemouse.paste", false);

// Downloads
user_pref("browser.download.alwaysOpenPanel", false);
user_pref("browser.download.always_ask_before_handling_new_types", true);
user_pref("browser.download.clearHistoryOnDelete", 2);
user_pref("browser.download.manager.addToRecentDocs", false);
user_pref("browser.download.start_downloads_in_tmp_dir", true);

// Media
user_pref("media.ffmpeg.vaapi.enabled", true);
user_pref("media.hardwaremediakeys.enabled", false);
user_pref("pdfjs.ignoreDestinationZoom", true);

// Content
user_pref("browser.translations.enable", false);
user_pref("layout.spellcheckDefault", "disabled");
user_pref("font.name-list.emoji", "Noto Color Emoji");

// Disable containers
user_pref("privacy.userContext.enabled", false);

// Devtools
user_pref("devtools.accessibility.enabled", false);
user_pref("devtools.application.enabled", false);
user_pref("devtools.chrome.enabled", true);
user_pref("devtools.debugger.remote-enabled", true);
user_pref("devtools.screenshot.audio.enabled", false);
user_pref("devtools.screenshot.clipboard.enabled", true);
user_pref("devtools.toolbox.tabsOrder", "webconsole,inspector,netmonitor,jsdebugger,styleeditor,storage,performance,memory,accessibility");
user_pref("devtools.toolbox.zoomValue", "1.1");
user_pref("view_source.wrap_long_lines", true);

// Enable support for userChrome.css
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
