package com.geishatokyo.scalajs.enchant

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global => g}
import scala.scalajs.js.JSApp
import scala.util.Random


object ScalaJSExample extends JSApp {
  val playerImage = "images/space0.png"
  val gameOverImage = "images/gameover.png"

  val random = Random

  def lifeLabel(): Label = {
    val life = new Label()
    life.x = 400 - 64
    life.y = 20
    life.color = "black"
    life.font = "32px Meiryo"
    life
  }

  def setupPlayer(image: Surface): Sprite = {
    val player = new Sprite(32, 64)
    player.image = image
    player.x = 200 - 32
    player.y = 300
    player.frame = js.Array(0)
    player
  }

  def setupGameOver(image: Surface): Sprite = {
    val over = new Sprite(189, 97)
    over.image = image
    over.x = 125
    over.y = 125
    over.frame = js.Array(0)
    over
  }

  def surface(game: Core, image: String): Surface = game.assets.asInstanceOf[js.Dictionary[Surface]](image)

  def main(): Unit = {
    enchant()

    val game = new Core(400, 400)
    game.keybind(32, "space")
    game.preload(js.Array(playerImage, Enemy.Image, gameOverImage, Laser.Image))
    game.fps = 20

    var laser: Option[Laser] = None
    game.onload = { () =>
      val life = this.lifeLabel()
      val timeCounter = new Counter(0)
      val lifeCounter = new Counter(5)
      life.text = lifeCounter.count.toString
      val player = this.setupPlayer(this.surface(game, playerImage))
      var enemies = List[Enemy]()
      val gameOver = this.setupGameOver(this.surface(game, gameOverImage))
      game.rootScene.addChild(player)
      game.rootScene.addChild(life)
      game.rootScene.addChild(gameOver)

      player.addEventListener("enterframe", { e: Event =>
        if (lifeCounter.count <= 0) {
          player.visible = false
          gameOver.visible = true
        } else {
          if (timeCounter.count > 0) {
            timeCounter.decrement()
          } else {
            if(random.nextInt(20) == 0) {
              val enemy = Enemy(game)
              enemies = enemy :: enemies
            }
            player.visible = true
            gameOver.visible = false
          }
          if (game.input.left) player.x -= 10
          if (game.input.right) player.x += 10
          if (game.input.up) player.y -= 10
          if (game.input.down) player.y += 10
          if (game.input.space && laser.isEmpty) laser = Some(Laser.fromPlayer(game, player))
          laser.foreach(_.move())
          if(laser.exists(_.y < 0)) laser = None
          if (player.visible){
            if(player.x < 0) player.x = 0
            if(player.x > (400 - 32)) player.x = 400 - 32
            if(player.y < 0) player.y = 0
            if(player.y > (400 - 64)) player.y = 400 - 64
            enemies.map(_.move())
            enemies = enemies.filterNot(_.y > 400)
          }
          for {
            enemy <- enemies
            l <- laser
            if enemy.intersect(l.sprite)
          } {
            enemies = enemies.filterNot(_.intersect(l.sprite))
            enemy.remove()
            l.remove()
            laser = None
          }
          enemies.find(_.intersect(player)).foreach { enemy =>
            player.visible = false
            timeCounter.increment(10)
            player.x = 200 - 32
            player.y = 300
            lifeCounter.decrement()
            life.text = lifeCounter.count.toString
            enemies.foreach(_.remove())
            enemies = Nil
          }
        }
        js.Object
      })

    }

    game.start()
  }
}

case class Enemy(game: Core, sprite: Sprite) extends SpriteWrapper {
  def move(): Unit = {
    sprite.y += 5
    if(sprite.y > 400) remove()
  }
}

object Enemy extends SpriteWrapperObj {
  override val Image = "images/space1.png"
  val random = Random

  def apply(game: Core): Enemy = {
    val enemy = new Sprite(64, 64)
    enemy.image = surface(game)
    enemy.x = random.nextInt(400)
    enemy.y = 0
    enemy.frame = js.Array(0)
    game.rootScene.addChild(enemy)
    Enemy(game, enemy)
  }
}

case class Laser(game: Core, sprite: Sprite) extends SpriteWrapper {
  def move(): Unit = {
    sprite.y -= 20
    if(sprite.y < 0) remove()
  }
}

object Laser extends SpriteWrapperObj {
  override val Image = "images/bar.png"

  def fromPlayer(game: Core, player: Sprite): Laser = {
    val laser = new Sprite(1, 16)
    laser.image = surface(game)
    laser.x = player.x + 4
    laser.y = player.y - 16
    laser.frame = js.Array(0)
    game.rootScene.addChild(laser)
    Laser(game, laser)
  }
}

trait SpriteWrapperObj {
  def Image: String
  def surface(game: Core): Surface = game.assets.asInstanceOf[js.Dictionary[Surface]](Image)
}

trait SpriteWrapper {
  def game: Core
  def sprite: Sprite
  def x = sprite.x
  def y = sprite.y
  def intersect(other: Sprite) = sprite.intersect(other)
  def remove() = game.rootScene.removeChild(sprite)
}
