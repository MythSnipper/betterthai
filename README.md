# betterthai
Convert English character input to IPA using English pronunciation rules and then converting IPA to Thai in order to type Thai.

As a person who is significantly better at English than his native language, and is very lazy, I complain about the horrible design of the Thai Kedmanee keyboard layout and struggle to type more than 10 words per minute with it. It is clear to me that this is not a problem that just affects me, but a significant portion of students in Thailand that attend international schools. Since I am taking Thai IB Lang & Lit A SL next year, I would be screwed if I didn't make an improved Thai keyboard before the end of summer break.

The major issues that plague all current Thai keyboards is the Thai language itself, as trying to fit 60 different characters onto a keyboard designed for around 30 characters in an efficient and logical way is nearly impossible. This issue is also caused by the Thai language itself as many sounds are associated by more than one consonant. The Kedmanee keyboard, which is pretty much the sole Thai keyboard layout, has an uneven distribution of work between each finger. This further reduces efficiency and puts extra strain on the fingers that have to do most of the work.

This is a emacs plugin with a C++ backend that is designed to work with the emacs text editor and it works quite like the Chinese Pinyin input method.

Usage information
=================

betterthai v1.0-Alpha, License: GPLv3

Warning: this has been developed and tested solely on Arch Linux, use it on other operating systems at your own risk.

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

if the thing doesn't work and you get something to do with json.cpp or rapid-fuzz.cpp, go to include and nuke the folders named json and rapid-fuzz

After nuking the folders, inside inlude; do 

`git clone https://github.com/nlohmann/json`

and then `git clone https://github.com/rapidfuzz/RapidFuzz`

then it should work. If problems still persist, spam ping the Head Devs and pray they care enough enough to help you. The response time depends on the amount of sidequests being done by the Head Devs.


Now, open emacs and open a file you wish to edit.

Alt+X betterthai

After it has been ran press F1 to toggle between regular US keyboard and betterthai input method.


* [Official Discord Server](https://discord.gg/mUKhGpWaWG)

Credits
=======
Head Developers
---------------
* [Sidhiboon Pibulnakarintr](https://github.com/asianhen)
* [Haoming Lyu](https://github.com/MythSnipper)

Support Team
------------
[Jarupat Bulpakdi](https://github.com/mightythemight) - main liason 

Leonidas Paul Lambrides - Helped us find a library to best implement levenshtein distance

[Niko Alois Attilakos](https://github.com/NotNoper) - Figured out how to create and parse json files

Testers
-------
["Gontzes Man"](https://github.com/Cxyoo367o) - Helped us identify the need for more detailed instructions

Still in progress of recruiting more 

Thanks
------
Greatest thanks to [Silas S. Brown](https://github.com/ssb22) for suggesting the use of espeak and emacs for this. That small piece of advice was invaluable to the sucess of this project

Trademarks
==========
Any trademarks we have mentioned without realising are trademarks of their respective holders.