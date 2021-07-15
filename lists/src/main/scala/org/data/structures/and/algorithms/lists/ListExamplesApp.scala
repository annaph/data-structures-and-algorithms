package org.data.structures.and.algorithms.lists

object ListExamplesApp extends App {

  import ListExamples._

  println("------------------------------------------------")
  println("Engineers:")
  println(engineers.mkString("\t===>\t", "\n\t===>\t", ""))
  println("------------------------------------------------")

  println("------------------------------------------------")
  println("Doctors:")
  println(doctors.mkString("\t===>\t", "\n\t===>\t", ""))
  println("------------------------------------------------")

  println("------------------------------------------------")
  println("Employees:")
  println(employees.mkString("\t===>\t", "\n\t===>\t", ""))
  println("------------------------------------------------")

  println("------------------------------------------------")
  println("Employees by category:")
  println(employees2.mkString("\t===>\t", "\n\t===>\t", ""))
  println("------------------------------------------------")

}

object ListExamples {
  val eng1: Engineer = Engineer(
    firstName = "Isaac",
    lastName = "Newton",
    department = "IT",
    salary = 4500.50,
    group = "Engineering")

  val eng2: Engineer = Engineer(
    firstName = "Albert",
    lastName = "Einstein",
    department = "Infra",
    salary = 4600.50,
    group = "Engineering")

  val doc1: Doctor = Doctor(
    firstName = "Michael",
    lastName = "Young",
    department = "Cardio",
    salary = 5000.50,
    group = "Medicine")

  val doc2: Doctor = Doctor(
    firstName = "Jeffrey",
    lastName = "Hall",
    department = "Pathology",
    salary = 5100.50,
    group = "Medicine")

  val engineers = List(eng1, eng2)

  val doctors = List(doc1, doc2)

  val employees: List[Employee] = engineers ::: doctors

  val employees2 = List(engineers, doctors)

}

trait Employee {

  def firstName: String

  def lastName: String

  def department: String

  def salary: Double

}

case class Engineer(firstName: String,
                    lastName: String,
                    department: String,
                    salary: Double,
                    group: String) extends Employee

case class Doctor(firstName: String,
                  lastName: String,
                  department: String,
                  salary: Double,
                  group: String) extends Employee
