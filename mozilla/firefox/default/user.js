// Tabs
user_pref("browser.tabs.loadBookmarksInBackground", true);

// Location bar
user_pref("browser.urlbar.shortcuts.bookmarks", false);
user_pref("browser.urlbar.shortcuts.history", false);
user_pref("browser.urlbar.shortcuts.tabs", false);
user_pref("browser.urlbar.suggest.calculator", true);
user_pref("browser.urlbar.trimURLs", false);

// Search
user_pref("browser.search.hiddenOneOffs", "Google,DuckDuckGo");
user_pref("browser.search.separatePrivateDefault", true);
user_pref("browser.search.separatePrivateDefault.ui.enabled", true);
user_pref("browser.search.suggest.enabled", false);
user_pref("browser.search.suggest.enabled.private", false);

// Downloads
user_pref("browser.download.alwaysOpenPanel", false);
user_pref("browser.download.start_downloads_in_tmp_dir", true);

// Keyboard shortcuts
user_pref("ui.key.menuAccessKeyFocuses", false);
user_pref("ui.key.textcontrol.prefer_native_key_bindings_over_builtin_shortcut_key_definitions", true);

// Enable support for userChrome.css
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);

// Enable hardware video acceleration
user_pref("media.ffmpeg.vaapi.enabled", true);

// Don't ask for notification permissions
user_pref("permissions.default.desktop-notification", 2);

// Disable MPRIS integration
user_pref("media.hardwaremediakeys.enabled", false);

// Disable Pocket
user_pref("extensions.pocket.enabled", false);
