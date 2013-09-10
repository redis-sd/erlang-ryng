ryng
====

[![Build Status](https://travis-ci.org/redis-sd/erlang-ryng.png?branch=master)](https://travis-ci.org/redis-sd/erlang-ryng)

Generic consistent hash algorithm handler for rings

## Example

```erl
$ erl -pa ebin -run ryng manual_start
Erlang R16B01 (erts-5.10.2) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.10.2  (abort with ^G)
1> ryng:new_ring([{name, my_ring}]).
{ok,<0.50.0>}
2> ryng:add_node(my_ring, node0).
ok
3> ryng:add_node(my_ring, node1, 1). %% weighted by 1, counts as 2 when there's one other node of weight 0
ok
4> ryng:sync_ring(my_ring).
ok
5> ryng:node_for(my_ring, erlang:now()).
{ok,node1}
6> ryng:node_for(my_ring, erlang:now()).
{ok,node1}
7> ryng:node_for(my_ring, erlang:now()).
{ok,node1}
8> ryng:node_for(my_ring, erlang:now()).
{ok,node0}
9> ryng:node_for(my_ring, erlang:make_ref()).
{ok,node1}
10> ryng:node_for(my_ring, erlang:make_ref()).
{ok,node1}
11> ryng:node_for(my_ring, erlang:make_ref()).
{ok,node1}
12> ryng:node_for(my_ring, erlang:make_ref()).
{ok,node0}
13> ryng:list_rings().                        
{ok,[#ring{name = my_ring,hash = sha,bits = 160,
           hasher = #Fun<ryng.0.126199387>,
           inc = 487167212443634306067894944238761006551977514325,
           max = 1461501637330902918203684832716283019655932542976,
           size = 3}]}
14> ryng:list_nodes(my_ring).
{ok,[#node{object = node0,weight = 0},
     #node{object = node1,weight = 1}]}
15> ryng:balance_summary(my_ring).
{ok,[{node0,0.3333333333333333},{node1,0.6666666666666666}]}
16> ryng:balance_check(my_ring, 1000000).
{ok,{1000000,3510033,3.510033},
    [{node0,333425,0.333425},{node1,666575,0.666575}]}
17> ryng:del_node(my_ring, node1).
ok
18> ryng:sync_ring(my_ring).
ok
19> ryng:balance_summary(my_ring).       
{ok,[{node0,1.0}]}
20> ryng:balance_check(my_ring, 1000000).
{ok,{1000000,3224708,3.224708},[{node0,1000000,1.0}]}
21> ryng:rm_ring(my_ring).               
ok
22> ryng:list_rings().
{ok,[]}
```
