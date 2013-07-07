Lalopmak Evil
============

A variant of colemak-evil.el/colemak.vim that takes some of Shai's ideas even further.  This mapping aims to be even more geometric/movement-based, exploiting the directional intuition that makes us so good at playing games.  Ergonomicness is also a goal, while mnemonicness is not.

A draft diagram can be found [near the top of this file](https://raw.github.com/lalopmak/lalopmak-evil/master/lalopmak-evil-libraries.el).



Setup
-----
1. [Install Evil](http://gitorious.org/evil/pages/Home#Install).
2. Download lalopmak-evil and put it somewhere in your load path.
3. Add `(load "lalopmak-evil")` to your Emacs init file.


If you wish to use this with QWERTY, add (defvar lalopmak-layout-map 'colemak-to-qwerty) to your init.el file, prior to the load.  If you want to use it with another layout, you currently have to define your own colemak-to-layout map.

Tips
----
Type :hints or :ars to bring up the hint screen.

Escape takes you into normal mode, an unfortunate historical accident.
I recommend defining an easy AltGr mapping (I use AltGr+t) since it will
be used a lot.  Alternatively, see patbl's [advice on key chords](https://github.com/patbl/colemak-evil/blob/master/README.md).


Colemak Evil
============

If you would, instead, like colemak-evil, please see the parent repository: https://github.com/patbl/colemak-evil

Colemak Evil is a set of remappings that implements some of
Shai Coleman's awesome Vim remappings in Emacs
([more information](http://forum.colemak.com/viewtopic.php?id=50)).

It's usable, but I'm an expert in neither Vim nor Emacs so you'll
likely encounter some funky behavior. If you have any improvements,
I'd be glad to integrate them.

Here are a few of the main differences from Shai's mappings:

* The only Vim mapping that works in insert mode is Esc (this avoids
  conflicts with Emacs's shortucts).
* Folding and several other features aren't implemented.
* Tab in insert mode doesn't take you into normal mode. 