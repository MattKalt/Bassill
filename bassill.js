// A collection of effects you can use on _ANY_ variable that changes


//Original t, increments one per sample. The reverb, harmonifier, hihat, and snare need this.
t2 = t,

//Change t here, not below, or it messes with the snare/hihat sounds
t *= 8 / 49,

// Repeat x beats of y
// SUPER useful if you're writing complex beats/melodies
// Include this or the FXs won't work (or you could replace r(x, y) with Array(x).fill(y))
// r(1,[arrays]) also serves as a replacement for [arrays].flat()
r = repeat = (x, y) => Array( x ).fill( y ).flat( 9 ),

sp = (str, sep='') => str.split( sep ),
j = (arr, sep='') => arr.join( sep ),

//tra = transpose = (arr, amt) => arr.map(x=>x+amt),
tra = transpose = (x, amt) => Array.isArray(x)? x.map( e => e + amt ) : j( sp(x).map( e => e + amt ) ),

// Uses up a lot of chars and isn't /super/ readable, but a major timesaver when creating
// Particularly the NaN handing
m = mix = (x, vol=1, dist=0) => ( ( x * vol * ( 1 + dist ) ) % ( 256 * vol) )||0,

// Waveshaper distortion
ds = (x, amt) => x * (1 - amt) + (128 * ((x / 128) - 1) ** 3 + 128) * amt,

//seq = (arr, spd, T=t) => eval(arr[(T >> spd) % arr.length]), //more functionality, but drastic worse performance :(
seq = (arr, spd, T=t) => arr[(T >> spd) % arr.length],
mseq = (...x) => t * 2 ** (seq(...x) / 12),


// The Breakbeat drum machine. This is where the magic happens
// It sequences through an array and plays the corresponding number of beats
//    (1 = quarter note, 2 = 2 8th notes, etc)
// Something interesting happens when you don't use powers of 2, however:
//    You get strange and wonderful sounds
// the variables 's' and 'h' make it sound like a snare and a hihat, respectively
// most sounds are different timbres of the same note
// but if you replace 'T' with something other than t, such as any bytebeat melody,
// you can apply that timbre to the melody.
// Adding / &ing a breakbeat with a melody can also add attack to the notes of the melody
bt = beat = (arr, spd, vel = 2e4, vol = 1, T = t, oct = 0) =>
	m(vel / (T & (2 ** (spd - oct) / seq( arr, spd ) ) - 1), vol),

ls = sin(t2 / 9 & t2 >> 5), // long snare
//s = sin(t>>5), // acoustic-sounding grungy snare
//s = (((t*8/48)>>9) & 1) ? 0 : sin(t / 9 & t >> 5), // Snare
s = seq( [ls, 0], 9), // Snare
S = seq( [ls, 0], 8), // double snare
//s = sin((t | t * .7) >> 4), // quieter snare
//h = 1 & t * 441/480, // long Hihat
h = 1 & t2 * 441/480, // long Hihat
h = seq( [h,h,h,0], 8), //quieter, faster attack



// The FX rack, stores memory for use in effects
// Automatically keeps track of what's stored where
// If you see red (NaNs), raise 5e4 higher, or adjust your reverbs' 'dsp' variable
// Works best when FX are not inside conditionals (meaning the number of FX in use changes)
// But even then, should only create a momentary click/pop (might be more severe for reverb)
// You can also set it to [] and modify the effects to read m(fx[stuff]) to get around NaN issues
//    ^(this gets rid of the lag when editing, but sparse arrays might be slower during runtime)
t ? 0 : fx = r( 4e4, 0 ),
// Iterator, resets to 0 at every t
fxi = 0,

//dsp = downsample the bitrate of the reverb, dsp=2 cuts uses half as much space, 3 uses 1/3, etc
rv = reverb = (x, len = 16e3, feedb = .7, dry = .4, wet = 1, dsp = 2, T=t2) => (
	ech = y => fxi + ( 0|(y % len) / dsp ),
	x = x*dry + wet*fx [ech(T) ] || 0,
	t2 % dsp ? 0 : fx[ ech(t2) ] = x * feedb,
	fxi += 0|(len / dsp),
	x
),


lp = lopass = (x, f) => ( // f ~= frequency, but not 1:1
	// fx[fxi] is the value of the last sample
	// You will need to change the 'x % 256' if you're using signed or floatbeat
	x = min( max( x % 256, fx[fxi] - f), fx[fxi] + f), // Clamp the change since last sample between (-f, f)
	fx[fxi] = x,
	fxi++,
	x
),

// Sounds kinda off, and hipass+lopas=/=original when you use ^, but + sounds harsher
hp = hipass = (x, f) => (x % 256) ^ lp(x, f),

//sp = speed
lim = limiter = (x, sp = .1) => (
	x &= 255,
	mi = fx[fxi] = min( fx[fxi] + sp, x, 255),
	mx = fx[fxi + 1] = max( fx[fxi + 1] - sp, x, mi+9),
	fxi += 2,
	(x-mi) * 255/(mx-mi)
),

//downsample
//dsp = downsample = (x, res) => (
//	x = fx[fxi] = t2 & res ? x : fx[fxi],
//	x
//),

// Multi-voice melody: 'voices' is like a list of resonances
//mvm = (melody, speed, voices) => (
//	vcp = voices,
//	vcp.reduce((sum, i) =>
//		sum + m(i * t * 1.05946 ** melody[(t >> speed) % melody.length], .9 / vcp.length), 0)
//),



// XORs the input with its harmonics, controlled by the bits of a number ('tone')
// Unoptimized version
hm2= harmonify = (x,tone) => {
	o = 0;
	//len=8;
	len = log2(tone) + 1;
	for (i=0; i<len; i++) {
		o ^= ( 1 & (tone>>i) ) * (i+1)/2 * x
	}
	return o;
},


// Instead of computing on the fly, this version computes a wavetable at the start
// Side effects: you can't start the song at any t, and output is always full-volume
hm = harmonify = (x, tone, waveTableSize = 256 * t2/t | 0 ) => {
	//play from the buffer
	if( t2 > waveTableSize) {
		o = fx[ fxi + ( x * t2 / t & waveTableSize - 1) ];
		fxi += waveTableSize;
		return o
	}
	//fill the buffer
	for (i=0; i<8; i++) {
		fx[ fxi + t2 ] ^= ( 1 & (tone>>i) ) * (i+1)/2 * t
	}
	fxi += waveTableSize;
	//return x //not strictly necessary unless the wavetable size is large enough to notice silence at the start
},

//Basically just treat this like a black box and fiddle with the knobs at random
//For a more detailed exmplanation:
//	X, and the First 2 hexes of y, are the fun surprise knobs :)
//		Small changes in these values completely change the tone (most of the time)
//	The next 2 hexes of y control the harmonifier
// The next hex controls the *thump*/click/noise of the attack
// The next hex controls the decay
// The next 2 hexes control the lowpass
sy = synth = (melody, velTrack, speed, x, y, ...z)=>
	lp(
		min(
			m(
				hm(
					beat( [x], 10, 6e4, 1, melody, .02* ( (y>>24) & 255 ) )
				, ( y>>16 ) & 255, ...z
				)
			, .9,1
			)
			+ beat( velTrack, speed, 1e3 * ( (y>>12) & 15) )
		, beat( velTrack, speed, 1, 2e4 * ( (y>>8) & 15 ) )
		)
	, y&255
	),


//saw 2 sine
s2s = sinify = x => sin( x*PI/64 ) * 126 + 128,

v = vibrato = sin(t2>>10)/2,



//------------------ SEQUENCES -----------------------------------

//Do not take any of this out of the 't || ( )' statement
t || (

bsa = r(1 ,[
	0, 0, r(4, 12) 
]),

bsb = r(1, [
	bsa, 8, 7, bsa, 5, 3, bsa, 8, 7, 3, 3, 10, 12, 5, 5, 15, 17
]),

bsv = "2112211221122222",

wa = r(1, [
	r(6, 0), 12, 0, r(6, 0), 10, 5
]),


wb = r(1, [
	r(6, 0), 12, 0, r(5, 0), 3, 7, 15
]),

wc = r(1, [
	15, 15, 3, 3, 5, 5, 15, 17, 13, 13, 1, 1, 3, 3, 13, 15
]),

w = r(1, [
	wa, wb, wa, wc
]),

0
),


//---------------- MIXER

//BS = sinify( lp( mseq( bsb, 10 ), .1 + .05 * (t>>14) ) ) ,

//BS = sinify( lp( mseq( bsb, 10 ), .1 * 2 ** ((t>>12) / 12 ) ) ) ,

BS = x => sinify( lp( mseq( bsb, 10 ), x * 2 ** ( seq( bsb, 10) / 12) ) ) ,


BSvel = x => min ( 1, beat( bsv, 11, 1, x ) ),

//m( BS(.25) * lp( BSvel( 2e2 ), .01), .7) + m( BS(999) * lp( BSvel( 9e2 ), .01), .3)

Bas = m( BS(.25) * lp( BSvel( 2e2 ), .01), .7) + m( lp( BS(999), 3), .3),

//W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 1, 0x71060499, 1024 / PI * t2/t | 0), //octaved
//W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 1.01, 0x72060499, 128 * t2/t | 0), //cool acid-y shamisen
//W = synth( mseq( w, 10 ), [.5, .5, .5, .5, 1, 1, 1, 1], 10, 1, 0x72060409), //muffled but still 2 high
//W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 1.1, 0x73030409, 512 / PI * t2/t | 0), //sorta piano-ish
//W = synth( mseq( w, 10 ), [.5, .5, .5, .5, 1, 1, 1, 1], 10, 1.5, 0x79018509), //sorta corny guitar
//W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 1.1, 0x79018509, 1024 / PI * t2/t | 0), //guitar
//W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 1.5, 0x79018509), //guitar 2
//W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 1.1, 0x79098509, 256 / PI * t2/t | 0), //guitar oct up
//W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 1, 0x81018509, 1024 / PI * t2/t | 0), //upright bass-ish
//W = synth( mseq( w, 10 ), [.5, .5, .5, .5, 1, 1, 1, 1], 10, 2, 0x94010509), //just square
//W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 2.1, 0x9501F509), //slappy
W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 3.1, 0x9501F599, 1024 / PI * t2/t | 0), //weird
//W = synth( mseq( w, 10 ), [.5, .5, .5, .5, 1, 1, 1, 1], 10, 3.4, 0x9601FD99), //very trebly
//W = synth( mseq( w, 10 ), [.5, .5, .5, .5, 1, 1, 1, 1], 10, 3.4, 0x09600F99), //pure phone ring
//W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 3.3, 0x03010F01), //buzzy superbassy
//W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 3.3, 0x05010FF1), //square
//W = synth( mseq( w, 10 ), [.5, .5, .5, .5, 1, 1, 1, 1], 10, 3.3, 0x0E0204F1), //cool acid-y bass
W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 2.1, 0x95010699, 1024 / PI * t2/t | 0), //weird2
//W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 1, 0x2EEF0399, 1024 / PI * t2/t | 0), //octaved
//W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 1, 0x04E00101, 1024 * t2/t | 0),//trumpet w/ buzz


W


//suck:
//W = synth( mseq( w, 10 ), [.5, .5, .5, .5, 1, 1, 1, 1], 10, 3.3, 0x0D020FF1), //ultrahigh phone
//W = synth( sinify( mseq( w, 10 )) / 4, [.5, .5, .5, .5, 1, 1, 1, 1], 10, 1.3, 0x36020399, 1024 / PI * t2/t | 0), //what

//hp( W, .5 + sin( t / (2 << 11) ) / 2)

//W / 8 + Bas / 2


//,m( lp( BS(999), 3), .5)


//lp(min( BS, beat( bsv, 11, 1, 5e9 ) ), 5)

//BS
