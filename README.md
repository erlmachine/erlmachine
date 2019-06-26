# erlmachine

Erlmachine - is an application level virtual machine based on Erlang/OTP. It provides a real-time allocation and allows to control hundreds, thousands low cost resource consuming and isolated code engines. You can set a wide range of rules, permissions, allocated resources (processor, memory, network, application data etc.). 
Erlmachine is used as high load, capacity emulation testing framework and also as an external programmable API engine.

Languages are inside of machines (Javascript, Erlang - both compiled to BEAM);  

Machines registry are inside of ecosystem  network  (via SYN);

Each machine has its own ID (GUID);

Machines allocator (via Poolboy);

Cowboy used as a machine state engine and driven by RESTfull design (via HTTP);

We improved a much funny concept about engines and gears. As the result an our machine consists basically of gears and engines. Gears are main building elements of an engine. An engine is one of the most important parts of a machine itself.  

All modules which don't represent running parts of machine must be named with a convenient part “gear_”. There are just tool modules.
Otherwise all running processes and its respective implementations must be named with a convenient part “engine_”. 


We are working on this, please wait for release
