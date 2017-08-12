# haskell-smsn

Once you've got stack (the haskell tool) installed, you can set up this folder by running "stack build". Once it's set up, you can run "stack ghci" to bring up a GHCI REPL with all the relevant libraries loaded.

Once within GHCI, load the reader by running `:l smsn.hs` and then test it out on a file from [data-public](https://www.github.com/synchrony/data-public) by running

```
fileWithText =  "/SOMEWHERE-UP-TO-YOU/public/kh667uKCjKn8WzfB.smsn"
fileWithNoText = "/SOMEWHERE-UP-TO-YOU/public/zzYSwIonHKgH4EYm.smsn"

test fileWithNoText
test fileWithText
```
