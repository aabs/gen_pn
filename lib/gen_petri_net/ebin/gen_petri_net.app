%% This is the application resource file (.app file) for the petri_lib,
%% application.
{application, gen_petri_net, 
  [{description, "OTP Compliant Petri Net Implementation."},
   {vsn, "0.1.0"},
   {modules, [gen_petri_net_app, petri_test, gen_petri_net
              gen_petri_net_sup]},
   {registered,[petri_lib_sup]},
   {applications, [kernel, stdlib]},
   {mod, {petri_lib_app,[]}},
   {start_phases, []}]}.

