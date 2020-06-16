return 2 == 3
false
return 2.2 != 5
true
return "salam" == "salam"
true
return "salam" != aleyk
true
return null == null
true
return null != null
false
return true == true
true
return false != true
true
return [1, 2, 3] == [3, 4, 5, 6]
false
return [1, 2, 3] == [1, 4, 3]
false
return [1] != []
true
return [1, 2] != [1, 2]
false
return [2, 2, 2] == 2
true
return ["a", 2, 2] == 2
false
return ["ab", "ab"] == "ab"
true
return [true] == "ab"
false
return [null, null] == null
true
return [false] == null
false
return [null, 2] != 2
true
return [true, true] == true
true
return ["ab", "ac"] != "ac"
true
return 2.233 == 2.233
true
return 5 < 7
true
return 5 > 5.5
false
return "alpha" < "beta"
true
return "alpha" > "alphaz"
false
return [1, 2, 3] > 2
false
return [4, 3, "a"] > 2
Error
return ["ba", "be", "bo"] > "b"
true
return [1, "be", "bo"] > "b"
Error
return -false
true
return -true
false
return -2
-2
return -[1, 2, 3]
[-1, -2, -3]
return -[2, "a"]
Error
return -[false, true]
[true, false]

return "salam" + "aleyk" + "chetori"
"salamaleykchetori"
return [1, "salam"] + "a"
Error
return ["a", "b"] + "c"
["ac", "bc"]

