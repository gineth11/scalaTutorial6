object practical6 {
    def main(args: Array[String])={
        args(0) match{
            case "question1" =>    
                            {
                                // Question 01
                                def encrypt(x:String,y:Int):String = x.length() match{
                                    case 0 => ""
                                    case _ if(x.head <= 'Z' && x.head >= 'A') => (65 + (x.head.toChar.toInt+y-65)%26).toChar + encrypt(x.tail,y) // for capital letters
                                    case _ if(x.head <= 'z' && x.head >= 'a') => (97 + (x.head.toChar.toInt+y-97)%26).toChar + encrypt(x.tail,y) // for simple letters
                                    case _ => x.head + encrypt(x.tail,y)
                                }

                                // when decrypting, assumed that the key can be maximum 26.
                                def decrypt(x:String,y:Int):String = x.length() match{
                                    case 0 => ""
                                    case _ if(x.head <= 'Z' && x.head >= 'A') => (65 + (x.head.toChar.toInt+26-y-65)%26).toChar + decrypt(x.tail,y) // for capital letters
                                    case _ if(x.head <= 'z' && x.head >= 'a') => (97 + (x.head.toChar.toInt+26-y-97)%26).toChar + decrypt(x.tail,y) // for simple letters
                                    case _ => x.head + decrypt(x.tail,y)
                                }
                                
                                var input = scala.io.StdIn.readLine("\nEnter a string to encrypt: ")
                                print("Enter a key: ")
                                var key = scala.io.StdIn.readInt()
                                print(encrypt(input,key))
                                
                                input = scala.io.StdIn.readLine("\nEnter a string to decrypt: ")
                                print("Enter a key: ")
                                key = scala.io.StdIn.readInt()
                                print(decrypt(input,key))
                            }


            case "question2" =>    
                            {// Question 02

                                def encrypt(x:String,y:Int):String = x.length() match{
                                    case 0 => ""
                                    case _ if(x.head <= 'Z' && x.head >= 'A') => (65 + (x.head.toChar.toInt+y-65)%26).toChar + encrypt(x.tail,y) // for capital letters
                                    case _ if(x.head <= 'z' && x.head >= 'a') => (97 + (x.head.toChar.toInt+y-97)%26).toChar + encrypt(x.tail,y) // for simple letters
                                    case _ => x.head + encrypt(x.tail,y)
                                }

                                // when decrypting, assumed that the key can be maximum 26.
                                def decrypt(x:String,y:Int):String = x.length() match{
                                    case 0 => ""
                                    case _ if(x.head <= 'Z' && x.head >= 'A') => (65 + (x.head.toChar.toInt+26-y-65)%26).toChar + decrypt(x.tail,y) // for capital letters
                                    case _ if(x.head <= 'z' && x.head >= 'a') => (97 + (x.head.toChar.toInt+26-y-97)%26).toChar + decrypt(x.tail,y) // for simple letters
                                    case _ => x.head + decrypt(x.tail,y)
                                }

                                def cipher(x:String,key:Int, func: (String,Int)=>String):String = {
                                    // call the passed funciton above
                                    func(x,key)
                                }

                                println("\n\t\tEnter the string and a key")
                                var input = scala.io.StdIn.readLine("Enter the string: ")
                                print("Enter the key: ")
                                var key = scala.io.StdIn.readInt()
                                println("\t\t1 -> encrypt\n")
                                println("\t\t2 -> decrypt\n")
                                print("Enter the option: ")
                                var opt = scala.io.StdIn.readInt()
                                opt match{
                                    case 1 => println(cipher(input,key,encrypt))
                                    case 2 => println(cipher(input,key,decrypt))
                                } 
                            }
        }
    }
}
