disable_ruby_docs() {
  export RUBY_CONFIGURE_OPTS="$RUBY_CONFIGURE_OPTS --disable-install-doc"
}

before_install disable_ruby_docs
