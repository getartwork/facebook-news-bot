package com.gu.facebook_news_bot.utils

import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.struct.Tree

import scala.collection.mutable

object Parser {
  private val nlpProcessor = new CoreNLPProcessor()

  def warmUp = nlpProcessor.annotate("warm") //warm up now, to avoid delaying the response to the first user

  def getNouns(text: String): List[String] = {
    println(s"Trying to find nouns in '$text'")
    val doc = nlpProcessor.annotate(text)

    //Get all lemma->token mappings from all sentences
    /*val lemmaTokenMaps: List[(String, String)] = doc.sentences.flatMap { sentence =>
      sentence.lemmas.flatMap(lemmas => sentence.tags.map(tags => lemmas.zip(tags)))
    }.flatten.toList*/

    val t = doc.sentences.flatMap { sentence =>
      sentence.syntacticTree.map { tree =>

        println(tree.toStringDepth(true))

        val terms = getTerms2(tree)
        println(s"Got terms for sentence: $terms")
        terms
      }
    }.flatten
    println(s"FINAL: ${t.mkString(" AND ")}")

    //val nouns = lemmaTokenMaps.collect { case (lemma,token) if token.matches("^NN[A-Z]*") => lemma }
    //println(s"Found nouns: $t")
    t.toList
  }

  /**
    * for each child
    * - if noun - get only child, add to nouns
    * - else combine nouns and add to terms. f(child)
    * return terms + any remaining nouns combined
    */
  //TODO - this needs to be pure!!!
  private def getTerms(tree: Tree): mutable.MutableList[String] = {
    var terms: mutable.MutableList[String] = mutable.MutableList()
    var nouns: mutable.MutableList[String] = mutable.MutableList()

    tree.children.foreach { children =>
      children.foreach { child =>
        if (child.value.matches("^NN[A-Z]*")) {
          child.children.flatMap(c => c.headOption.map(_.value)).foreach(nouns += _)
        } else {
          if (nouns.nonEmpty) {
            println(s"Using nouns: $nouns")
            terms += nouns.mkString(" ")
            nouns = mutable.MutableList()
          }
          if (!child.isLeaf) terms ++= getTerms(child)
        }
      }
    }
    if (nouns.nonEmpty) terms += nouns.mkString(" ")
    terms
  }

  case class Terms(current: List[String], overall: List[String])

  /**
    * build up terms
    * at end, return terms.current
    */
  private def getTerms2(tree: Tree): List[String] = {

    tree.children.map { children =>
      val result: Terms = children.foldLeft(Terms(Nil,Nil)) { (terms, child) =>
        if (child.value.matches("^NN[A-Z]*")) {
          //Add noun to terms.current
          val upd = child.children.flatMap(c => c.headOption.map(_.value)).map { noun =>
            terms.copy(current = terms.current :+ noun)
          } getOrElse terms
          println(s"1. new terms: $terms")
          upd
        } else {
          //Combine any nouns in current
          //Get terms for child
          //Update overall
          val childTerms = getTerms2(child)
          val currentNoun = if (terms.current.isEmpty) None else Some(terms.current.mkString(" "))
          val toAppend = currentNoun.map(noun => childTerms.::(noun)).getOrElse(childTerms)

          val upd = terms.copy(overall = terms.overall ++ toAppend, current = Nil)
          println(s"2. new terms: $terms")
          upd
        }
      }
      //Combine any final nouns in current, return overall
      val currentNoun = if (result.current.isEmpty) None else Some(result.current.mkString(" "))
      val upd = currentNoun.map(result.overall.::(_)).getOrElse(result.overall)
      println(s"3. new terms: $upd")
      upd
    } getOrElse Nil
  }
}
