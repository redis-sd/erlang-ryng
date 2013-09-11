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
3> %% weight = 1: counts as 2 when there's one other node of weight 0
3> ryng:add_node(my_ring, node1, 1).
ok
4> %% weight = 0, priority = 1: will only be selected if priority 0 is empty
4> ryng:add_node(my_ring, node2, 0, 1).
ok
5> ryng:sync_ring(my_ring).
ok
6> ryng:node_for(my_ring, erlang:now()).
{ok,node0}
7> ryng:node_for(my_ring, erlang:make_ref()).
{ok,node1}
8> ryng:node_for(my_ring, random:uniform()).
{ok,node1}
9> ryng:list_rings().
{ok,[#ring{name = my_ring,hash = sha,bits = 160,
           hasher = #Fun<ryng.2.946472>,
           max = 1461501637330902918203684832716283019655932542976,
           incrs = [{0,
                     487167212443634306067894944238761006551977514325},
                    {1,1461501637330902918203684832716283019655932542976}],
           sizes = [{0,3},{1,1}]}]}
10> ryng:list_nodes(my_ring).
{ok,[#node{object = node0,priority = 0,weight = 0},
     #node{object = node1,priority = 0,weight = 1},
     #node{object = node2,priority = 1,weight = 0}]}
11> ryng:balance_summary(my_ring).
{ok,[{0,node0,0.3333333333333333},
     {0,node1,0.6666666666666666},
     {1,node2,1.0}]}
12> ryng:balance_check(my_ring, 1000000).
{ok,{1000000,5492018,5.492018},
    [{node0,333425,0.333425},{node1,666575,0.666575}]}
13> ryng:del_node(my_ring, node1).
ok
14> ryng:sync_ring(my_ring).
ok
15> ryng:balance_summary(my_ring).
{ok,[{0,node0,1.0},{1,node2,1.0}]}
16> ryng:balance_check(my_ring, 1000000).
{ok,{1000000,5809765,5.809765},[{node0,1000000,1.0}]}
17> ryng:del_node(my_ring, node0).
ok
18> ryng:sync_ring(my_ring).
ok
19> ryng:balance_summary(my_ring).
{ok,[{1,node2,1.0}]}
20> ryng:balance_check(my_ring, 1000000).
{ok,{1000000,5959802,5.959802},[{node2,1000000,1.0}]}
21> ryng:rm_ring(my_ring).
ok
22> ryng:list_rings().
{ok,[]}
```
