package week2.bank

object Bank {
  val a = new BankAccount                         //> a  : week2.bank.BankAccount = week2.bank.BankAccount@33e27ed3
  a deposit 10
  a withdraw 5                                    //> res0: Int = 5
  a withdraw 7                                    //> java.lang.Error: insufficient funds
                                                  //| 	at week2.bank.BankAccount.withdraw(BankAccount.scala:12)
                                                  //| 	at week2.bank.Bank$$anonfun$main$1.apply$mcV$sp(week2.bank.Bank.scala:7)
                                                  //| 
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at week2.bank.Bank$.main(week2.bank.Bank.scala:3)
                                                  //| 	at week2.bank.Bank.main(week2.bank.Bank.scala)
}