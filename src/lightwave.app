{application, lightwave,
 [{description, "lightwave"},
  {vsn, "0.1"},
  {modules, [
    lightwave,
    lightwave_app,
    lightwave_sup,
    lightwave_web,
    lightwave_deps
  ]},
  {registered, []},
  {mod, {lightwave_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
