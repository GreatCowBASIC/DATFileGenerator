''Title: graphics/sprite.bi
''
''About: License
''Copyright (c) 2007-2013, FreeBASIC Extended Library Development Group
''
''Distributed under the FreeBASIC Extended Library Group license. See
''accompanying file LICENSE.txt or copy at
''http://code.google.com/p/fb-extended-lib/wiki/License

#ifndef FBEXT_GFX_SPRITE_BI__
#define FBEXT_GFX_SPRITE_BI__ -1

#include once "fbgfx.bi"
#include once "ext/detail/common.bi"
#include once "ext/graphics/detail/common.bi"
#include once "ext/math/vector2.bi"


''Namespace: ext.gfx
namespace ext.gfx

''Enum: PPOPTIONS
''Used in the Sprite class to determine whether to use pixel perfect collision or not.
''
''usePP - The default, to use pixel perfect collision.
''noPP - only use rectangular collision.
''
enum PPOPTIONS
	usePP
	noPP
end enum



''Class: Sprite
''Fully featured sprite class with pixel perfect collision detection built-in
''
''See Also:
''<Simple Example of the Sprite Class>
type Sprite
public:
	''Sub: constructor
	''Creates a sprite object with a certain number of frames
	''
	''Parameters:
	''num - the number (1 based) of frames to create
	''
	declare constructor( byval num as uinteger )

	''Sub: default constructor
	''Creates an unitialized <Sprite> object. Used with <Init>.
	''
	declare constructor( )
	
	''Sub: copy constructor
	''Makes a deep copy of another <Sprite> object.
	declare constructor( byref rhs as Sprite )

	''Sub: Init
	''Used to initialize when using an array of <Sprite>
	''
	''Parameters:
	''num - the 1 based number of frames to create
	''
	declare sub Init( byval num as uinteger )

	''Function: FromSpritesheet
	''Loads a row of sprite images contained in a single image into the Sprite class.
	''
	''Parameters:
	''srci - pointer to FB.image containing the spritesheet data
	''startx - the x coordinate the first sprite begins at
	''starty - the y coordinate the first sprite begins at
	''spwidth - the width of the individual sprite (all must match)
	''spheight - the height of the individual sprite (all must match)
	''startindex - the (0 based) index in the Sprite object to begin adding sprites
	''numsp - the number of sprites to return
	''
	''Returns:
	''-1 = number of sprites does not match image width
	'' 0 = Sprite object cannot hold required number of sprites
	''>0 = the number of sprites loaded, should match numsp
	''
	declare function FromSpritesheet( byval srci as fb.image ptr, byval startx as uinteger, byval starty as uinteger, _
        byval spwidth as uinteger, byval spheight as uinteger, byval startindex as uinteger, byval numsp as integer ) _
        as integer
	
	''Function: GetImage
	''
	''Parameters:
	''index - the (0 based) frame to return
	''
	''Returns:
	''pointer to FB.IMAGE containing image data.
	''
	''Usage:
	''If you modify the image returned by this function you must then call <UpdateImage> to regenerate the collision mask.
	''
	declare function GetImage( byval index as uinteger ) as FB.IMAGE ptr

	''Sub: SetImage
	''Assigns a FB.IMAGE to a frame.
	''
	''Parameters:
	''index - the frame number to assign to (0 based), does not check if data already exists there.
	''img - the FB.IMAGE structure to assign to this frame.
	''
	''Usage:
	''This object will automatically free the image data when it is no longer needed, you should not deallocate this memory yourself.
	''
	declare sub SetImage( byval index as uinteger, byval img as FB.IMAGE ptr )

	''Sub: DuplicateImage
	''Creates a full copy of the Image
	''
	''Parameters:
	''from_index - the Image to copy. (0 based)
	''to_index - the empty index to copy to. (0 based)
	''
	declare sub DuplicateImage( byval from_index as uinteger, byval to_index as uinteger )

	''Sub: ReplaceImage
	''
	''Parameters:
	''index - the frame number to assign to (0 based)
	''img - the FB.IMAGE to replace with.
	''
	''Usage:
	''This method will free the previous frame if it exists.
	''
	declare sub ReplaceImage( byval index as uinteger, byval img as FB.IMAGE ptr )

	''Sub: DeleteImage
	''
	''Parameters:
	''index - the image to destroy (0 based)
	''
	declare sub DeleteImage( byval index as uinteger )

	''Sub: UpdateImage
	''Updates the collision mask for the specified frame.
	''
	''Parameters:
	''index - the frame to update the collision mask for (0 based).
	''
	''Usage:
	''You only need to call this subroutine if you modify the image returned by <GetImage>
	''
	declare sub UpdateImage( byval index as uinteger )

	''Function: isCollided
	''Determines if this Sprite object has collided with another Sprite object.
	''
	''Parameters:
	''spr - the second Sprite object to test collision with.
	''ppcol - optional value to determine collision method. Defaults to usePP.
	''_index_ - optional index into the second Sprite object to test collision on. (0 based)
	''
	''Returns:
	''ext.bool.true on collision, ext.bool.false otherwise.
	''
	''Usage:
	''This function first performs a bounding box collision check to decrease unnecessary calls to the pixel perfect routine.
	''
	declare function isCollided( byref spr as Sprite, byval ppcol as PPOPTIONS = usePP, byval _index_ as integer = -1 ) as ext.bool

	''Sub: RotateFrom
	''Rotates one frame to another frame.
	''
	''Parameters:
	''from_index - the source image index to rotate. (0 based)
	''to_index - the destination index to overwrite with the rotated image. (0 based)
	''angle - the angle to rotate the image to.
	''
	declare sub RotateFrom( byval from_index as uinteger, byval to_index as uinteger, byval angle as integer )

	''Sub: RotateFromImage
	''Rotates a FB.IMAGE to a frame.
	''
	''Parameters:
	''from_image - the source image to rotate.
	''to_index - the destination index to overwrite with the rotated image. (0 based)
	''angle - the angle to rotate the image to.
	''
	declare sub RotateFromImage( byval from_image as FB.IMAGE ptr, byval to_index as uinteger, byval angle as integer )

	''Sub: DrawImage
	''Draws the specified sprite either to an image buffer or to the screen.
	''
	''Parameters:
	''src_img - the Source image to draw.
	''dst_img - the destination to draw the image onto, defaults to null which will draw on the screen.
	''method - the method to use to draw the image from DrawMethods, defaults to XOR (XOR_) like FBGFX's PUT statement.
	''
	declare sub DrawImage( byval src_img as uinteger, byval dst_img as FB.IMAGE ptr = null, byval method as DrawMethods = DrawMethods.XOR_ )

	''Sub: Postition
	''Sets or Gets the position of the sprite for use with the draw and collision statements.
	''
	''Parameters:
	''_x_ - the x coordinate.
	''_y_ - the y coordinate.
	''
	''Usage:
	''To get the position pass one or both coordinates as negative (i.e. < 0) and the current values will
	''be returned in the variables passed.
	''
	declare sub Position overload( byref _x_ as single, byref _y_ as single )
	
	''Sub: Postition
	''Sets or Gets the position of the sprite for use with the draw and collision statements.
	''
	''Parameters:
	''_vec_ - <vector2>(single) containing the x and y coordinates.
	''
	''Usage:
	''To get the position pass a <vector2>(single) with one or both coordinates as negative (i.e. < 0) and the current values will
	''be returned in the <vector2> variable passed.
	''	
	declare sub Position ( byref _vec_ as ext.math.vec2f )
	
	''Sub: Update
	''Updates the position of the Sprite in relative terms.
	''
	''Parameters:
	''_x_ - relative x position to move sprite to.
	''_y_ - relative y position to move sprite to.
	''
	''Usage:
	''Passing a negative value for _x_ will cause the sprite to move "left", a negative value for _y_ will cause the
	''sprite to move "up".
	''
	declare sub Update overload( byval _x_diff as single = 0, byref _y_diff as single = 0 )

	''Sub: Update
	''Updates the position of the Sprite in relative terms.
	''
	''Parameters:
	''_vec_ - <vector2d> containing the relative position to move to.
	''
	''Usage:
	''Passing a negative value for x coordinate will cause the sprite to move "left", a negative value for the y coordinate will cause the
	''sprite to move "up".
	''
	declare sub Update( byval _vec_ as ext.math.vec2f )
	
	''Property: Count
	''Returns the number of frames this Sprite object holds (1 based)
	''
	declare property Count( ) as uinteger

	''Property: LastIndex
	''Returns the last referenced frame by <GetImage>
	''
	declare property LastIndex( ) as uinteger
	declare destructor( )

	'Externally needed but still private
	declare function GetColMap( byval index as uinteger ) as FB.IMAGE ptr
	declare static function Masker ( byval src_color as uinteger, byval dest_color as uinteger, byval xx as any ptr ) as uinteger
	declare static function CMasker ( byval src_color as uinteger, byval dest_color as uinteger, byval xx as any ptr ) as uinteger

private:

	'declare operator let ( byref rhs as Sprite )
	m_imgdata as FB.IMAGE ptr ptr
	m_coldata as FB.IMAGE ptr ptr
	m_size as uinteger
	m_lastindex as uinteger
	m_temp as FB.IMAGE ptr
	m_vec as ext.math.vec2f

end type


''Example: Simple Example of the Sprite Class
''(begin code)
''# include once "ext/graphics.bi"
''
''''This lets us not have to type ext.gfx before every Sprite object.
''using ext.gfx
''
''screenres 640,480,32
''
''''This is the normal way to dimension a ext.gfx.Sprite object.
''var mySprite1 = Sprite(5)
''
''''This is an alternative way that is compatible with arrays.
''dim mySprite2 as Sprite
''mySprite2.Init( 5 )
''
''''Functionally both ways are the same, but using the constructor method is preferred when not using an array of Sprites.
''
''
''''This loop will make our lead character, a large blue circle.
''for n as uinteger = 0 to 4
''
''	var crcl = imagecreate(100,100,&hFF00FF)
''
''	circle crcl, (50,50), 20 + (n*5), &h0000FF,,,, F
''
''	''You will notice we reuse the crcl variable name since Sprite becomes 
''	''responsible for freeing the image's memory when it is no longer needed.
''	mySprite1.SetImage(n, crcl)
''
''next
''
''''This loop creates our enemy, a smaller green circle.
''for n as uinteger = 0 to 4
''
''	var crcl2 = imagecreate(50,50,&hFF00FF)
''
''	circle crcl2, (25,25), 10 + (n*5), &h00FF00,,,,F
''	mySprite2.SetImage(n, crcl2)
''
''next
''
''mySprite1.Position( 100, 100 )
''mySprite2.Position( 200, 200 )
''
''var n = 0
''var tog = 0
''
''''Loop repeatedly until you press the ESC key on your keyboard.
''do while not multikey(FB.SC_ESCAPE)
''
''''Lock the screen before drawing to prevent flicker
''screenlock
''
''	cls
''
''	mySprite1.DrawImage n, null, TRANS_
''	''is equivalent to: put (x1,y1), mySprite1.GetImage(n), TRANS
''
''	mySprite2.DrawImage n, null, TRANS_
''	''is equivalent to: put (x2,y2), mySprite2.GetImage(n), TRANS
''
''	''Lets see if our hero has collided with our enemy.
''	''You could call the isCollided on either Sprite you wanted to check, it doesn't matter.
''	if mySprite1.isCollided( mySprite2 ) then
''		locate 1,1
''		print "Boom!"
''	end if
''screenunlock
''
''''Check for Arrow key presses and adjust our hero's location.
''if multikey(FB.SC_LEFT) then mySprite1.Update(-2)
''if multikey(FB.SC_RIGHT) then MySprite1.Update(2)
''if multikey(FB.SC_UP) then MySprite1.Update(,-2)
''if multikey(FB.SC_DOWN) then MySprite1.Update(,2)
''
''
''''Slow things down a bit so you can see everything.
''sleep 50
''
''''The tog variable is used to make the circles appear to pulsate.
''if tog = 0 then
''
''	n+=1
''
''else
''	n-=1
''
''endif
''
''''Reset tog if it goes out of bounds.
''if n < 0 then
''	n = 0
''	tog = 0
''
''end if
''
''if n > 4 then 
''	n = 4
''	tog = 1
''
''end if
''
''''Lather, Rinse, Repeat.
''loop
''(end code)

end namespace

#endif 'include guard