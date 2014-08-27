package com.ajay

import java.awt._
import javax.swing.{JFrame,JPanel}
import java.awt.Color
import java.awt.image.BufferedImage
import java.awt.geom.AffineTransform
import java.awt.event._
import java.lang._
import scala.collection._
import scala.swing.event.KeyEvent


object SpaceGame {
  	  val H = 640
	  val W = 480

	  
  
	  
  class ImmutableImage (
		val backingImage:BufferedImage){
	
		// Construct a blank image.
  		  def this(w:Int, h:Int) {
			this(new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB))
			
		}
	
		// Copy constructor.
		def this(src :ImmutableImage ) {
			this(new BufferedImage(src.backingImage.getColorModel(),
				src.backingImage.copyData(null), false, null))
		}
	
		// Clear the image.
		def clear(c :Color ) : ImmutableImage = {
			val copy = new ImmutableImage(this)
			val g = copy.backingImage.getGraphics()
			g.setColor(c)
			g.fillRect(0, 0, backingImage.getWidth(), backingImage.getHeight())
			copy
		}
	
		// Draw a filled circle.
		def fillCircle(x :Int , y :Int , r : Int,c : Color ) : ImmutableImage  = {
			val copy = new ImmutableImage(this)
			val g = copy.backingImage.getGraphics()
			g.setColor(c)
			g.fillOval(x - r, y - r, r * 2, r * 2)
			copy
		}
	}
	  
  class Bullet (
	val x:Int, val y : Int) {
  	      
		val radius = 5
		
		// Update the game state (repeatedly called for each game tick).
		def update(currentState : GameState ) : GameState  =  {
			var  bullets = currentState.bullets
			bullets = bullets - (this)
			if (y + radius >= 0)
				// Add a copy of the bullet which has moved up the screen slightly.
				bullets = bullets + (new Bullet(x, y - 5))
			currentState.setBullets(bullets)
		}
	
		def render(img : ImmutableImage ) : ImmutableImage  =  {
			return img.fillCircle(x, y, radius, Color.BLACK);
		}
	}
  
  
 
  class KeyboardState (
	val  depressedKeys:Set[Int] ){
  	    
	def this() = this(Set[Int]())

	def keyPressed(currentState : GameState , key : Int ) : GameState  = {
		currentState.setKeyboard(new KeyboardState(depressedKeys + key))
	}

	def keyReleased(currentState : GameState , key : Int) : GameState =  {
		currentState.setKeyboard(new KeyboardState(depressedKeys - key))
	}

	def isDown(key : Int) : Boolean = depressedKeys.contains(key)
}
  
  
  	  
  	  
  
  import java.awt.event.KeyEvent
  class Player (
	 val x : Int, val y : Int,
	val ticksUntilFire :Int) {

	// Construct a player at the starting position, ready to fire.
	def this() = this(W / 2, H - 50, 0)

	// Update the game state (repeatedly called for each game tick).
	 def update(currentState :GameState ) :GameState={
		// Update the player's position based on which keys are down.
		var newX :Int = x;
		if (currentState.keyboard.isDown(KeyEvent.VK_LEFT)
				|| currentState.keyboard.isDown(KeyEvent.VK_A))
			newX -= 2;
		if (currentState.keyboard.isDown(KeyEvent.VK_RIGHT)
				|| currentState.keyboard.isDown(KeyEvent.VK_D))
			newX += 2;

		// Update the time until the player can fire.
		var  newTicksUntilFire = ticksUntilFire;
		if (newTicksUntilFire > 0)
			newTicksUntilFire -= 1

		// Replace the old player with an updated player.
		val newPlayer = new Player(newX, y, newTicksUntilFire);
		currentState.setPlayer(newPlayer);
	}

	// Update the game state in response to a key press.
	def keyPressed(currentState :GameState  , key : Int): GameState  = {
	    var cs = currentState
		if (key == KeyEvent.VK_SPACE && ticksUntilFire == 0) {
			// Fire a bullet.
			val b = new Bullet(x, y);
			val newBullets = currentState.bullets + (b);
			
			cs = cs.setBullets(newBullets);

			// Make the player wait 25 ticks before firing again.
			cs = cs.setPlayer(new Player(x, y, 25));
		}
		cs
	}

	def render(img :ImmutableImage ) :ImmutableImage = {
		img.fillCircle(x, y, 20, Color.RED)
	}
}
  
  // An immutable, copy-on-write description of the entire game state.
class GameState(
	val player:Player, 
	val bullets:Set[Bullet],
	val keyboard:KeyboardState) {

//	def this(player1: Player , bullets1 : Set[Bullet] ,
//			keyboard1:KeyboardState ) {
//		this.Player = player1
//		//this.bullets = bullets1;
//		//this.keyboard = keyboard1;
//	}

	def this() {
		this(new Player(), Set[Bullet](), new KeyboardState());
	}

	def setPlayer( newPlayer:Player): GameState = {
		new GameState(newPlayer, bullets, keyboard)
	}

	def setBullets(newBullets : Set[Bullet]): GameState =  {
			new GameState(player, newBullets, keyboard)
	}

	def setKeyboard(newKeyboard :KeyboardState ) : GameState =  {
		new GameState(player, bullets, newKeyboard)
	}

	// Update the game state (repeatedly called for each game tick).
	def update() : GameState =  {
		var current = this
		current = current.player.update(current)
		for ( b <- current.bullets)
			current = b.update(current)
		current
	}

	// Update the game state in response to a key press.
	def keyPressed(key :Int ) :GameState  = {
		var current = this
		current = keyboard.keyPressed(current, key)
		current = player.keyPressed(current, key)
		current
	}

	// Update the game state in response to a key release.
	def keyReleased(key :Int ) : GameState ={
		var current = this
		current = keyboard.keyReleased(current, key)
		current
	}

	def render():ImmutableImage = {
		var img = new ImmutableImage(W,H)
		img = img.clear(Color.BLUE)
		img = player.render(img)
		for (b <- bullets)
			img = b.render(img)
		img
	}
}


  
    def main(args: Array[String]): Unit = {
    
      //var sI = new SpaceInvaders
      var currentState : GameState = new GameState();
       val f = new JFrame
       f.setBackground(Color.BLUE)
       f.setSize(H , W )
       f.setTitle("Play SpaceInvader by Tarun")
       f.setContentPane(new JPanel(){
         
         override def paintComponent(g : Graphics) ={
           val img : BufferedImage  = currentState.render().backingImage;
           //val img : BufferedImage  = new BufferedImage(gc.W, gc.H, BufferedImage.TYPE_INT_RGB);
           (g.asInstanceOf[Graphics2D]).drawRenderedImage(img,new AffineTransform());
			//((Graphics2D) g).drawRenderedImage(img, new AffineTransform());
         }
       })
       
       f.addKeyListener(new KeyAdapter(){
			override def keyPressed(e : KeyEvent ) {
				println("tarun"+ e.getKeyCode())
				currentState = currentState.keyPressed(e.getKeyCode())
			}
			
			override def keyReleased(e : KeyEvent ) {
			  println("tarun key released"+ e.getKeyCode())
				currentState = currentState.keyReleased(e.getKeyCode())
			}
       })
       f.setLocationByPlatform(true);
       f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
       f.setVisible(true)
       
       while(true){
    	   	currentState = currentState.update();
			f.repaint();
			try {
				Thread.sleep(20);
			} catch {
			  case e: InterruptedException => {} 
			}
       }
      
       
    }
    
    
}