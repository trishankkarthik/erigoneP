bool flag = false, finish = false;

active proctype p() {
	do
	::  flag -> break
	::  else -> true
	od;
  finish = true
}

active proctype q() {
  do
  :: flag = true;
     flag = false
  od
}
