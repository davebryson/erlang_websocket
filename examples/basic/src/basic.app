{application, basic,
 [{description, "basic"},
  {vsn, "0.01"},
  {modules, [
    basic,
    basic_app,
    basic_sup,
    basic_web,
    basic_deps
  ]},
  {registered, []},
  {mod, {basic_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
