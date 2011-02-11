%% This is the application resource file (.app file) for the gen_pn,
%% application.
{application, gen_pn,
  [{description, "A Generic OTP-enabled Petri Net Server"},
   {vsn, "0.1.0"},
   {modules, [gen_pn_app,
              adj_list,
              adj_list_tests,
              gen_pn,
              petrinet,
              pn_tests,
              sample_pn,
              gen_pn_sup]},
   {registered,[gen_pn_sup]},
   {applications, [kernel, stdlib]},
   {mod, {gen_pn_app,[]}},
   {start_phases, []}]}.

