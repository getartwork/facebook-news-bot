package com.gu.facebook_news_bot.services

import java.util.concurrent.TimeUnit

import com.github.benmanes.caffeine.cache.Caffeine
import com.gu.contentapi.client.GuardianContentClient
import com.gu.contentapi.client.model._
import com.gu.contentapi.client.model.v1.{Content, ItemResponse, SearchResponse}
import com.gu.facebook_news_bot.BotConfig
import com.gu.facebook_news_bot.utils.Loggers.appLogger
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.struct.Tree

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.matching.Regex

trait Capi {
  def getHeadlines(edition: String, topic: Option[Topic]): Future[Seq[Content]]

  def getMostViewed(edition: String, topic: Option[Topic]): Future[Seq[Content]]
}

object CapiImpl extends Capi {

  private lazy val client = new GuardianContentClient(BotConfig.capi.key)

  def getHeadlines(edition: String, topic: Option[Topic]): Future[Seq[Content]] =
    doQuery(edition, topic, _.showEditorsPicks(), _.editorsPicks)

  def getMostViewed(edition: String, topic: Option[Topic]): Future[Seq[Content]] =
    doQuery(edition, topic, _.showMostViewed(), _.mostViewed)

  def doQuery(edition: String, topic: Option[Topic], itemQueryModifier: ItemQuery => ItemQuery, getResults: (ItemResponse => Option[Seq[Content]])): Future[Seq[Content]] = {
    val query: ContentApiQuery = topic.map(_.getQuery(edition)).getOrElse(ItemQuery(edition))
    query match {
      case itemQuery @ ItemQuery(_,_) =>
        val ops = itemQueryModifier andThen commonItemQueryParams
        doItemQuery(ops(itemQuery), getResults)
      case searchQuery @ SearchQuery(_) => doSearchQuery(commonSearchQueryParams(searchQuery))
      case other =>
        appLogger.error(s"Unexpected ContentApiQuery type: $other")
        Future.successful(Nil)
    }
  }

  private def doItemQuery(query: ItemQuery, getResults: (ItemResponse => Option[Seq[Content]])): Future[Seq[Content]] = {
    CapiCache.get(query.toString).map(cached => Future.successful(cached)).getOrElse {
      client.getResponse(query) map { response: ItemResponse =>

        val results = getResults(response).filter(_.nonEmpty)
          .orElse(response.results)   //Fall back on main results
          .getOrElse(Nil)

        CapiCache.put(query.toString, results)
        results
      }
    }
  }

  private def doSearchQuery(query: SearchQuery): Future[Seq[Content]] = {
    CapiCache.get(query.toString).map(cached => Future.successful(cached)).getOrElse {
      client.getResponse(query) map { response: SearchResponse =>
        val results = response.results
        CapiCache.put(query.toString, results)
        results
      }
    }
  }

  /**
    * Duplication here because we can't use a parameterized type,
    * as capi-client defines e.g. ItemQuery with ShowParameters[ItemQuery]
    */
  private def commonItemQueryParams: ItemQuery => ItemQuery =
    _.showFields("standfirst")
    .tag("type/article")
    .tag("-tone/minutebyminute")
    .showElements("image")
    .pageSize(25)   //Matches the number of editors-picks/most-viewed, in case they aren't available

  private def commonSearchQueryParams: SearchQuery => SearchQuery =
    _.showFields("standfirst")
    .tag("type/article")
    .tag("-tone/minutebyminute")
    .showElements("image")
    .pageSize(25)

}

object CapiCache {

  private val cache = Caffeine.newBuilder()
    .expireAfterWrite(2, TimeUnit.MINUTES)
    .build[String, Seq[Content]]()

  def get(query: String): Option[Seq[Content]] = Option(cache.getIfPresent(query))
  def put(query: String, results: Seq[Content]): Unit = cache.put(query, results)
}

object Topic {
  def getTopic(text: String): Option[Topic] = {
    val lower = text.toLowerCase
    val topic = TopicList.find(topic => topic.pattern.findFirstIn(text).isDefined)
    topic.orElse(SearchTopic(text)) //Uppercase chars can help with finding proper nouns
  }

  private val TopicList: List[Topic] = List(
    PoliticsTopic,

    EditionSectionTopic(List("sport","sports"), "sport"),
    EditionSectionTopic(List("film","films","movies"), "film"),
    EditionSectionTopic(List("tv","television","radio"), "tv-and-radio"),
    EditionSectionTopic("business"),
    EditionSectionTopic(List("lifestyle", "life and style"), "lifeandstyle"),
    EditionSectionTopic("environment"),
    EditionSectionTopic("money"),
    EditionSectionTopic(List("tech", "technology"), "technology"),
    EditionSectionTopic("travel"),
    EditionSectionTopic("culture"),

    SectionTopic(List("football","soccer"), "football"),
    SectionTopic("music"),
    SectionTopic("books"),
    SectionTopic(List("art","design"), "artanddesign"),
    SectionTopic("stage"),
    SectionTopic("fashion"),
    SectionTopic("science"),

    SectionTagTopic(List("rugby","rugby union"), "sport", "rugby-union"),
    SectionTagTopic(List("formula 1","formula one","f1"), "sport", "formulaone"),
    SectionTagTopic(List("horse racing"), "sport", "horse-racing"),
    SectionTagTopic(List("rugby league"), "sport", "rugbyleague"),
    SectionTagTopic("sport", "cricket"),
    SectionTagTopic("sport", "tennis"),
    SectionTagTopic("sport", "golf"),
    SectionTagTopic("sport", "cycling"),
    SectionTagTopic("sport", "boxing"),
    SectionTagTopic("technology", "games"),
    SectionTagTopic(List("food","drink"), "lifeandstyle", "food-and-drink"),
    SectionTagTopic(List("health","wellbeing"), "lifeandstyle", "health-and-wellbeing"),
    SectionTagTopic("lifeandstyle", "family"),
    SectionTagTopic("lifeandstyle", "women"),
    SectionTagTopic(List("cats","cat facts"), "lifeandstyle", "cats"),
    SectionTagTopic(List("climate"), "environment", "climate-change")
  )
}

sealed trait Topic {
  val terms: List[String]
  lazy val pattern: Regex = ("""(^|\W)(""" + terms.mkString("|") + """)($|\W)""").r.unanchored
  def getQuery(edition: String): ContentApiQuery
  def name: String = terms.headOption.getOrElse("")
}

case class SectionTopic(terms: List[String], section: String) extends Topic {
  def getQuery(edition: String) = ItemQuery(section)
}
object SectionTopic {
  def apply(section: String): SectionTopic = SectionTopic(List(section), section)
}

case class EditionSectionTopic(terms: List[String], section: String) extends Topic {
  def getQuery(edition: String) = ItemQuery({
    if (List("us","au").contains(edition.toLowerCase)) s"$edition/$section"
    else s"uk/$section"   //uk edition by default
  })
}
object EditionSectionTopic {
  def apply(section: String): EditionSectionTopic = EditionSectionTopic(List(section), section)
}

case class SectionTagTopic(terms: List[String], section: String, tag: String) extends Topic {
  def getQuery(edition: String) = ItemQuery(s"$section/$tag")
}
object SectionTagTopic {
  def apply(section: String, tag: String): SectionTagTopic = SectionTagTopic(List(tag), section, tag)
}

case object PoliticsTopic extends Topic {
  val terms = List("politics")

  def getQuery(edition: String): ItemQuery = ItemQuery({
    edition.toLowerCase match {
      case "us" => "us-news/us-politics"
      case "au" => "australia-news/australian-politics"
      case _ => "politics"
    }
  })
}

//Special topic for searching CAPI for a set of terms
case class SearchTopic(terms: List[String]) extends Topic {
  def getQuery(edition: String) = {
    val quoted = terms.map(term => if (term.contains(" ")) "\"" + term + "\"" else term)
    val q = SearchQuery().q(quoted.mkString(" AND ")).orderBy("newest")
    println(s"getQuery: $q")
    q
  }

  //Store full query params in dynamodb
  override def name: String = terms.mkString(" AND ")
}
object SearchTopic {
  private val nlpProcessor = new CoreNLPProcessor()

  def warmUp = nlpProcessor.annotate("warm") //warm up now, to avoid delaying the response to the first user

  def apply(text: String): Option[SearchTopic] = {
    val filtered = text.replaceAll("([hH]+eadlines|[nN]+ews|[sS]+tories|[pP]+opular)","")
    println(s"Trying to find nouns in '$filtered'")
    val doc = nlpProcessor.annotate(filtered)

    //Get all lemma->token mappings from all sentences
    /*val lemmaTokenMaps: List[(String, String)] = doc.sentences.flatMap { sentence =>
      sentence.lemmas.flatMap(lemmas => sentence.tags.map(tags => lemmas.zip(tags)))
    }.flatten.toList*/

    val t = doc.sentences.flatMap { sentence =>
      sentence.syntacticTree.map { tree =>

        //TODO - this needs to be pure!!!
        def getTerms(tree: Tree): mutable.MutableList[String] = {
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

        val terms = getTerms(tree)
        println(s"Got terms for sentence: $terms")
        terms.toList
      }
    }.flatten
    println(s"FINAL: ${t.mkString(" AND ")}")

    //val nouns = lemmaTokenMaps.collect { case (lemma,token) if token.matches("^NN[A-Z]*") => lemma }
    //println(s"Found nouns: $t")
    if (t.nonEmpty) Some(SearchTopic(t.toList)) else None
  }
}
