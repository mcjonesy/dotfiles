for mode in default insert visual
    fish_default_key_bindings -M $mode
end
fish_vi_key_bindings --no-erase default
for mode in insert default visual
    bind -M $mode \cf forward-char
end
fzf_key_bindings
set fish_escape_delay_ms 10
set -x fish_bind_mode default
