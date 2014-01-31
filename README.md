ryng
====

[![Build Status](https://travis-ci.org/redis-sd/erlang-ryng.png?branch=master)](https://travis-ci.org/redis-sd/erlang-ryng)
[![Build Status](https://drone.io/github.com/redis-sd/erlang-ryng/status.png)](https://drone.io/github.com/redis-sd/erlang-ryng/latest)

Generic consistent hash algorithm handler for rings

## Example

```erl
$ erl -pa ebin -run ryng manual_start
Erlang R16B02 (erts-5.10.3) [source] [64-bit] [smp:2:2] [async-threads:10] [kernel-poll:false] [dtrace]

Eshell V5.10.3  (abort with ^G)
1> ryng:new_ring([{id, my_ring}]).
{ok,<0.47.0>}
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
{ok,node1}
7> ryng:node_for(my_ring, erlang:make_ref()).
{ok,node1}
8> ryng:node_for(my_ring, random:uniform()).
{ok,node1}
9> ryng:list_rings().
{ok,[{ryng_ring_v1,my_ring,sha,160,#Fun<ryng.2.107634887>,
                   1461501637330902918203684832716283019655932542976,
                   [{0,487167212443634306067894944238761006551977514325},
                    {1,1461501637330902918203684832716283019655932542976}],
                   [{0,3},{1,1}],
                   16402,20499,true,undefined}]}
10> ryng:list_nodes(my_ring).
{ok,[{ryng_node_v1,node0,0,0},
     {ryng_node_v1,node1,0,1},
     {ryng_node_v1,node2,1,0}]}
11> ryng:balance_summary(my_ring).
{ok,[{0,node0,0.3333333333333333},
     {0,node1,0.6666666666666666},
     {1,node2,1.0}]}
12> ryng:balance_check(my_ring, 1000000).
{ok,{1000000,2175268,2.175268},
    [{node0,333425,0.333425},{node1,666575,0.666575}]}
13> ryng:del_node(my_ring, node1).
ok
14> ryng:sync_ring(my_ring).
ok
15> ryng:balance_summary(my_ring).
{ok,[{0,node0,1.0},{1,node2,1.0}]}
16> ryng:balance_check(my_ring, 1000000).
{ok,{1000000,2782921,2.782921},[{node0,1000000,1.0}]}
17> ryng:del_node(my_ring, node0).
ok
18> ryng:sync_ring(my_ring).
ok
19> ryng:balance_summary(my_ring).
{ok,[{1,node2,1.0}]}
20> ryng:balance_check(my_ring, 1000000).
{ok,{1000000,1695015,1.695015},[{node2,1000000,1.0}]}
21> ryng:rm_ring(my_ring).
ok
22> ryng:list_rings().
{ok,[]}
```
