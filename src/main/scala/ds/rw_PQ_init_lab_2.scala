package ds

object rw_PQ_init_lab_2 extends App {
  
	println("==> START, calling PriorityQueue instantiation...")
	val pq = PriorityQueue(4,3,2,1,5)
	println("--> pq: " + pq)
	
	println("\n--> calling min...")
	val min = pq.findMin
	println("--> min: " + min)
	
	println("\n--> calling pq.deleteMin.findMin...")
	val secondMin = pq.deleteMin.findMin
	println("--> secondMin: " + secondMin)
	
	println("\n--> calling pq + 0...")
	val npq = pq + 0
	println("--> new PQ: " + npq)
	
	println("\n--> calling npq.findMin...")
	val thirdMin = npq.findMin
	println("--> thirdMin: " + thirdMin)

}



