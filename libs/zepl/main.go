package main

import (
  "fmt"
  "os"
  "io"
  "net"
  "bufio"
)

func ListenForResponses(conn net.Conn, incoming chan string) {
  reader := bufio.NewReader(conn)
  for {
    buf, _ := reader.ReadString('\n')
    incoming <- string(buf)
  }
}

func ListenForStdin(outgoing chan string) {
  reader := bufio.NewReader(os.Stdin)
  for {
    jsonstr, e := reader.ReadString('\n')
    if e == io.EOF {
      jsonstr = "exit"
    }
    outgoing <- jsonstr
  }
}

func main() {
  conn, err := net.Dial("unix", "/tmp/zephyros.sock")
  if err != nil {
    fmt.Println("Can't connect. Is Zephyros running?")
    os.Exit(1)
  }

  incoming := make(chan string, 5)
  outgoing := make(chan string)

  go ListenForResponses(conn, incoming)
  go ListenForStdin(outgoing)

  MainLoop:
  for {
    fmt.Print("-> ")

    select {
    case request := <- outgoing:
      if request == "exit" {
        break MainLoop
      }
      fmt.Fprintf(conn, "%v", request)
      firstResponse := <- incoming
      fmt.Printf("<- %v\n", firstResponse)
    case asyncResponse := <- incoming:
      fmt.Printf("\n<- %v\n", asyncResponse)
    }
  }
}
