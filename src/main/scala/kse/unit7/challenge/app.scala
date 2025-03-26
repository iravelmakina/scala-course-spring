package kse.unit7.challenge

import kse.unit7.challenge.adt.*
import kse.unit7.challenge.model.*
import kse.unit7.challenge.services.*

object app:

  def getPostsViews(apiKey: ApiKey): Try[List[PostView]] =
    for
      user  <- getUserProfile(apiKey)
      posts <- getPosts(user.userId)
      postViews <- posts.foldLeft(Try(List.empty[PostView])) { (accTry, post) =>
        accTry.flatMap { accList =>
          getPostView(post).map { postView =>
            accList :+ postView
          }
        }
      }
    yield postViews

  def getPostsViewDesugared(apiKey: ApiKey): Try[List[PostView]] =
    getUserProfile(apiKey).flatMap { user =>
      getPosts(user.userId).flatMap { posts =>
        posts.foldLeft(Try(List.empty[PostView])) { (accTry, post) =>
          accTry.flatMap { accList =>
            getPostView(post).map { postView =>
              accList :+ postView
            }
          }
        }
      }
    }

  def getPostView(post: Post): Try[PostView] =
    for
      comments <- getComments(post.postId)
      likes    <- getLikes(post.postId)
      shares   <- getShares(post.postId)
    yield PostView(post, comments, likes, shares)

  def getPostViewDesugared(post: Post): Try[PostView] =
    getComments(post.postId).flatMap { comments =>
      getLikes(post.postId).flatMap { likes =>
        getShares(post.postId).map { shares =>
          PostView(post, comments, likes, shares)
        }
      }
    }
