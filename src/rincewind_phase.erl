-module(rincewind_phase).

-type name() :: atom().
-type definition() :: #{name := name()}.

-opaque t() :: #{name := name()}.

-export_type([name/0, t/0, definition/0]).
