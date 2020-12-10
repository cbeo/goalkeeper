# goalkeeper

A small web application backed by 

- [bknr.datastore](https://github.com/sharplispers/bknr.datastore) 
- [spinneret](https://github.com/ruricolist/spinneret)
- [LASS](https://github.com/Shinmera/LASS)
- and [lazybones](https://github.com/cbeo/lazybones), a small routing
  library built around [clack](https://github.com/fukamachi/clack)
  
Its purpose is to manage "games" that you run with a few friends. Each
game consists of a start date, a stop date, and a collection of goals
per player.  Players post "evidence" to their goals, and a goal is
considered to have been achieved when a majority of particpating
players mark it has having been met, given the evidence.

The scores are calculated as a percentage of goals met per
player. Whoever has the highest score at the end of the game is the
winner. An agreed upon prize is then to be awarded to the winner by
the losers.

It's all a very friendly little system. The application meant to take
advantage of the fact that it is sometimes easier to keep ourselves on
track if our friends are looking at what we're doing from time to
time.

## Design & Implemenation Notes

My design sense is quite poor.  UI is that of a classic web app -
i.e. built around submitting forms and watching pages reload. I use
some CSS3 features but no Javascript.

I built it in a single morning as a satisfying way to scratch an
itch. It has just enough functionality to be useful, but no more.

## Building 

If you want to run this on your own server for use with your own
friends, you can easily build `goalkeeper` under SBCL.

    sbcl --load build.lisp 
    
The above will produce a `goalkeeper` executable.  In order to
bootstrap users, you can put a file called `$HOME/goalkeeper.conf` and
fill it with the following:

    (:players
    	(("player1" "password1")
    	 ("player2" "password2")))
         
This willcreate initial users. Once the app is started, you can remove
`goalkeeper.conf`.  You also have the option of changing passwords
after logging in.  


    

## License

The code in this repo is AGPLv3.0 .

Any binary build of this project must contain licences for the
projects it relies upon - mostly MIT and BSD2 licenses.


