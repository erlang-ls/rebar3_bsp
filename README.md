rebar3_bsp
=====

Rebar3 support for the Build Server Protocol (aka BSP) 2.0.0.

https://build-server-protocol.github.io

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_bsp, {git, "git@github.com:erlang-ls/rebar3_erlang_ls.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:

    $ rebar3 bsp
