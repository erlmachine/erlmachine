# erlmachine

Erlmachine - is an application level virtual machine based on Erlang/OTP. It provides a real-time allocation and allows to control hundreds, thousands low cost resource consuming and isolated code engines. You can set a wide range of rules, permissions, allocated resources (processor, memory, network, application data etc.). 
Erlmachine is used as high load, capacity emulation testing framework and also as an external programmable API engine.

Short owerview:

1. Languages are inside of machines (Javascript, Erlang - both compiled to BEAM);  

2. Machines registry are inside of ecosystem  network  (via SYN);

3. Each machine can have its own domain name inside a cluster and tagged by the unique serial number;

4. Each produced data piece can be tracked with its own tracking number inside a system.

5. Machines allocator (via Poolboy);

6. Cowboy used as a machine state engine and driven by RESTfull design (via HTTP);


We improved a much funny concept about tools, gears and gearboxes. As the result an our machine consists basically of tools and gears which can be used in your top level design elements - gearboxes. 

Tool is just an api module which can be reused as a library.

Gear is an OTP component which can be represented as a running proccess. It is the main structural element of gearbox.

Gearbox is a messaging system represented as a set of gears composed accordingly to your own design schema. Gearboxes can be combined together in large distributed messaging systems.


We are working on this, please wait for release
