package com.typeddynamo

trait DynamoEntity[+T] {
  def hashPK: T
}
