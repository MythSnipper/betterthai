# betterthai
Convert English character input to IPA using Eglish pronunciation rules and then converting IPA to Thai in order to type Thai

As a person who is significantly better at English than his native language, and is very lazy, I complain about the horrible design of the Thai Kedmanee keyboard layout and struggle to type more than 10 words per minute with it. It is clear to me that this is not a problem that just affects me, but a significant portion of students in Thailand that attend international schools. Since I am taking Thai IB Lang&Lit A SL next year, I would be screwed if I didn't make an improved Thai keyboard before the end of summer break.

The major issues that plague all current Thai keyboards is the Thai language itself, as trying to fit 60 different characters onto a keyboard designed for around 30 characters in an efficient and logical way is nearly impossible. This issue is also caused by the Thai language itself as many sounds are assosiated by more than one consonant. The Kedmandee keyboard which is pretty much the sole Thai keyboard layout, has an uneven distribution of work between each finger. This further reduces efficiency and puts extra strain on the fingers that have to do most of the work.

This is a emacs plugin with a C++ backend that is designed to work with the emacs text editor and it works quite like the Chinese Pinyin input method.

Usage information
=================

better Thai v, copyright idk, License: MIT

Warning: this has been developed and tested solely on Arch Linux, use on other things at your own risk

Dependencies
----------
Arch Linux
packages: base base-devel bash gcc make emacs

How to use
----------
In an arch linux environment with dependencies installed, run using bash:

`git clone https://github.com/MythSnipper/betterthai`

`cd betterthai`

`make emacs-init emacs`


Now, open emacs and open a file you wish to edit.

Alt+X betterthai

After it has been ran press F1 to toggle between regular US keyboard and betterthai input method.

Credits
=======
Head Developers
---------------
* [Sidhiboon Pibulnakarintr](https://github.com/asianhen)
* [Haoming Lyu](https://github.com/MythSnipper)

Support Team
------------
[Jarupat Bulpakdi](https://github.com/mightythemight) - main liason 

Leonidas Paul Lambrides

[Niko Alois Attilakos](https://github.com/NotNoper)

Testers
-------
[Sidhiboon Pibulnakarintr](https://github.com/asianhen)

Still in progress of recruiting more 

Thanks
------
Greatest thanks to [Silas S. Brown](https://github.com/ssb22) for sugessting the use of espeak and emacs for this. That small piece of advice was invaluable to the sucess of this project

Trademarks
==========
Any trademarks we have mentioned without realising are trademarks of their respective holders.