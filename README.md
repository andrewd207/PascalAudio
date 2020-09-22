= Pascal Audio

Audio classes to decode and encode various formats including Ogg, Opus, Flac, 
m4a made for use with the Free Pascal compiler. It's also possible to use other 
external programs such as Sox, mpg321 or ffplay to decode any other format.
It can currently output audio to the _MM_ system of windows or _PulseAudio_ for 
linux.

Also there can be effects applied to the audio such as resampling via
_libresample_ or _libsamplerate_. LADSPA2 plugins are also possible to 
incorporate. As well as the noise removal filter from Audacity.

It is divided into two packages: PascalAudioIO and PascalAudioSuite.

== PascalAudioIO

This package contains the bindings and simple classes to directly use the 
libraries such as libogg or libflac etc. If you don't want to use the 
"Suite" which has a somewhat complicated threaded mechanism, then you need only
this.

== PascalAudioSuite

Using "Links" starting with a _Source_ link and ending with a _Destination_ 
link it's possible to decode and audio file and convert/play it or using any 
links. If the final link in the chan is PulseAudio then the decoded file will 
play on the speakers. Each link in the chain operates inside it's own thread 
and processes the data before handing off it's buffer to be handled by the next 
link.

To apply some effect to the decoded audio just insert some links into the 
middle of the chain. See the examples folder to see how simple this is.

== Adding new codecs or sources and destinations

Adding new codecs is really not that hard and can be done in a few minutes or 
hours as long as the binding already exists.


