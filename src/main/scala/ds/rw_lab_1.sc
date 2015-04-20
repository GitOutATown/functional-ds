package ds

object rw_lab_1 {

	val pq = PriorityQueue(4,3,2,1,5)         //> pq  : ds.PriorityQueue[Int] = BinomialQueue(5, 1, 3, 4, 2)
	val min = pq.findMin                      //> min  : Int = 1
	val secondMin = pq.deleteMin.findMin      //> secondMin  : Int = 2
	val npq = pq + 0                          //> npq  : ds.PriorityQueue[Int] = BinomialQueue(0, 5, 1, 3, 4, 2)
	val thirdMin = npq.findMin                //> thirdMin  : Int = 0
	
	
	'''                                       //> res0: Char('\'') = '
}