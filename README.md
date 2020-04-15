rebar3_erlang_ls
=====

rebar3 shell-like plugin that starts an agent to interact with the
Erlang LS language server.

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

Options
---

Currently, only _shortnames_ are supported. It is however possible to
customize the node name by using the standard _rebar3_ mechanism:

    {dist_node, [{sname, my_custom_name}]}.

The node name defaults to the name of the project directory.
