# **Why LaTex**

## Problems with Word or whatever:
+ Formulas
+ Layout
+ No immediate reference to the figures
+ Bibliography
+ ...

### How does it work?
+ Start phrases with \
+ Arguments go inside graph parentesis
+ Comments symbol is %
+ The R "library" function is here called "usepackage"
+ To write the date you can directly put it, or, you can put % before it, so your laptop will use the date you created your document. Instead, to remove the date, just leave an empty space inside the graph parenthesis like this: date{}
+ Everytime you use the "begin" function, it must be followed by the "end" function
+ To start a new paragraph with an indentation, just leave an empty line after the previous one. If you don't want it, just write \noindent
+ To start a new paragraph, just use the functions \bigskip or \smallskip: it will leave a whole line between the previous and the current one.
+ To modify the defaul 10 pt dimension of the text in the document, just write [12] before the graph parenthesis of \documentclass{article}, like this: \documentclass[12pt]{article}
+ $ sign to do mathematics

>**Note**
>It is read "Latec"
