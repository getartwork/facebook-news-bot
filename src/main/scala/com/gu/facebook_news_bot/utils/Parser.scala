package com.gu.facebook_news_bot.utils

import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.struct.Tree

import scala.collection.immutable.Queue

object Parser {
  private val nlpProcessor = new CoreNLPProcessor()

  def warmUp = nlpProcessor.annotate("warm") //warm up now, to avoid delaying the response to the first user

  def getNouns(text: String): List[String] = {
    val doc = nlpProcessor.annotate(text)

    doc.sentences.toList.flatMap { sentence =>
      sentence.syntacticTree.map { tree =>

        println(tree.toStringDepth(true))

        val terms = getTerms(tree)
        println(s"Got terms for sentence: $terms")
        terms
      }
    }.flatten
  }

  /**
    * @param consecutive  nouns which should be considered a single term, e.g. "Donald Trump"
    * @param overall      the final set of terms in this tree
    */
  private case class Terms(consecutive: Queue[String], overall: Queue[String])

  private def getTerms(tree: Tree): Queue[String] = {
    tree.children.map { children =>
      val result: Terms = children.foldLeft(Terms(Queue(),Queue())) { (terms, child) =>
        if (child.value.matches("^NN[A-Z]*")) {
          //The child of this node is a noun, add it to consecutive list
          child.children.flatMap(c => c.headOption.map(_.value)).map { noun =>
            terms.copy(consecutive = terms.consecutive :+ noun)
          } getOrElse terms
        } else {
          /**
            * - Combine any consecutive nouns into a single term
            * - Recursively get any terms from this child tree
            */
          val childTerms = getTerms(child)
          val currentTerm = if (terms.consecutive.isEmpty) None else Some(terms.consecutive.mkString(" "))
          val toAppend = currentTerm.map(noun => childTerms :+ noun).getOrElse(childTerms)

          terms.copy(overall = terms.overall ++ toAppend, consecutive = Queue())
        }
      }

      val remainder = if (result.consecutive.isEmpty) None else Some(result.consecutive.mkString(" "))
      remainder.map(result.overall :+ _).getOrElse(result.overall)
    } getOrElse Queue()
  }
}
