func standardTypes() {
	var i int = 42
	var b bool = true
	var s string = "Hello, Go!"

	println("Standard types:")
	println("int:", i)
	println("bool:", b)
	println("string:", s)
}

func loops() {
	println("\nLoops (For, Break, Continue):")

	for i := 0; i < 5; i++ {
		println(i)
		if i == 3 {
			break
		}
	}

	for i := 0; i < 5; i++ {
		if i == 2 {
			continue
		}
		println(i)
	}
}

func if_else() {
	println("\nIf-Else:")
	x := 10
	if x > 5 {
		println("x больше 5")
	} else {
		println("x не больше 5")
	}
}

func arrays() {
	println("\nArrays:")
	var arr [5]int

	for i := 0; i < len(arr); i++ {
		print(arr[i])
	}

	arr[2] = 5
	println()
	println(arr)
}

func sum2(a, b int) int {
	return a + b
}

func factorial(n int) int {
	if n == 1 {
		return 1
	} else {
		return n * factorial(n-1)
	}
}

func closureExample() {
	println("\nClosure Example:")

	counter := func() func() int {
		var count int
		return func() int {
			count++
			return count
		}
	}

	inc := counter()
	println(inc())
	println(inc())
	println(inc())
}

func goroutinesAndChannels() {
	println("\nGoroutines and Channels:")

	ch := make(chan string)

	go func(ch chan string) {
		ch <- "Hello from goroutine!"
	}(ch)

	println(<-ch)

	close(ch)
}

func deferExample() {
	println("\nDefer Example:")

	defer println("This is printed last.")

	println("This is printed first.")

	defer func() {
		if r := recover(); r != nil {
			println("Recovered from panic:", r)
		}
	}()

	panic("Something went wrong!")
}

func main() {
	standardTypes()
	loops()
	if_else()
	arrays()

	println("\n5 + 10 =", sum2(5, 10))
	println("factorial of 5:", factorial(5))

	closureExample()
	goroutinesAndChannels()
	deferExample()
}
