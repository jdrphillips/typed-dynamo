package com.typeddynamo

case class DynamoException(msg: String) extends Exception(msg)
