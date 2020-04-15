rebar3_erlang_ls
=====

Erlang LS rebar3 plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_erlang_ls, {git, "git@github.com:erlang-ls/rebar3_erlang_ls.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:

    $ rebar3 erlang_ls
    ===> Fetching erlang_ls
    ===> Compiling erlang_ls
    <Plugin Output>
