import ReassignedPriorities

gPrioritiesTuplesTest01 = gPrioritiesTuples [1,2,7,2,4] 1 == [(1,1),(2,2),(7,3),(2,4),(4,5)]

orderListbyValueTeste01 = orderListbyValue [(1,1),(2,2),(7,3),(2,4),(4,5)] == [(1,1),(2,2),(2,4),(4,5),(7,3)]

rPrioritiesTest01 = rPriorities [(1,1),(2,2),(2,4),(4,5),(7,3)] == [(1,1),(2,2),(2,4),(3,5),(4,3)]

orderListbyIndexTest01 = orderListbyIndex [(1,1),(2,2),(2,4),(3,5),(4,3)] == [1,2,4,2,3]

reassignedPrioritiesTest01 = reassignedPriorities [1,2,7,2,4] == [1,2,4,2,3]

testList = [gPrioritiesTuplesTest01, orderListbyValueTeste01, rPrioritiesTest01, orderListbyIndexTest01, reassignedPrioritiesTest01]

verify = map (\x -> if x then "Ok" else "Nok") testList