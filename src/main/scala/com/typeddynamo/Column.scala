package com.typeddynamo

trait Column[T] {
  def name: String
  def evidence: TypeMapper[T]
}
