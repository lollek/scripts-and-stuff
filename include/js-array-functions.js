/* Created 2013-09-15 - Olle Kvarnstrom 
 * After a few days of JavaScript it seems like arrays are extremely lacking.
 * Adding some stuff that I think is needed */

/** shuffleList
 * Shuffles a list */
function shuffleList(list) {
  for (var i = list.length, j, tmp; i;) {
    j = Math.floor(Math.random() * i--);
    tmp = list[i];
    list[i] = list[j];
    list[j] = tmp;
  }
}

/** defragList
 * Removes all null elements from list */
function defragList(list) {
  for (var i = list.length; i;) 
    if (list[--i] == null) {
      for (var j = 0; j < i; j++)
        if (list[j] != null) {
          list[i] = list[j];
          list[j] = undefined;
        }
      if (i == j) break;
    }
  while(list.length && list[0] == null)
    list.shift();
}
