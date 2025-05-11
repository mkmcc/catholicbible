# catholicbible.el

**A command-line and Emacs Lisp interface for retrieving and formatting verses from [catholicbible.online](https://catholicbible.online)**  
Supports Knox, Douay-Rheims, and Vulgate translations with LaTeX and plaintext output.

## Features

- Retrieve verses by translation, book, chapter, and range
- Preserves paragraph breaks and skips footnotes
- Supports discontiguous verse ranges with ellipsis
- Outputs:
  - LaTeX (using the [`scripture`](https://ctan.org/pkg/scripture) package)
  - Plain text

## Usage

### commandline

```bash
./catholicbible TRANSLATION BOOK CHAPTER RANGE [--format FORMAT] [--out FILE]
```

```bash
./catholicbible knox Daniel 7 13-15 --tex

```

```latex
\begin{scripture}[Daniel 7:13-14]
\vs{13}Then I saw in my dream, how one came riding on the clouds of heaven, that was yet a son of man; came to where the Judge sat, crowned with age, and was ushered into his presence.
\vs{14}With that, power was given him, and glory, and sovereignty; obey him all must, men of every race and tribe and tongue; such a reign as his lasts for ever, such power as his the ages cannot diminish.
\end{scripture}
```

![typeset.](./img/Dn7_13-14.png)

```bash
./catholicbible douay_rheims Genesis 1 1-5 --format text --out genesis1.txt
```

1. In the beginning God created heaven, and earth. 2. And the earth
was void and empty, and darkness was upon the face of the deep; and
the spirit of God moved over the waters. 3. And God said: Be light
made. And light was made. 4. And God saw the light that it was good;
and he divided the light from the darkness. 5. And he called the
light Day, and the darkness Night; and there was evening and morning
one day.

### elisp

```emacs-lisp
(require 'catholicbible)

(catholicbible-format-verses-latex
 "knox" "Daniel" 5 "1-3,5")
```

```
"\begin{scripture}[Daniel 5:1-3,5]
\vs{1}Now turn we to king Baltassar, that made great cheer for courtiers of his a thousand, each man drinking wine as his rank entitled him.
\vs{2}And he, in his cups, would have the spoils of the old temple at Jerusalem brought in, cups of gold, cups of silver that his father Nabuchodonosor had carried away; king and court, wife and concubine should drink from them.
\vs{3}Brought in they were, all the spoils of Jerusalem; king and courtier, wife and concubine, drank from those vessels;


\dots


\vs{5}Then, in that hour, an apparition came to them. They saw the fingers of a man’s hand writing on the plaster of the palace wall, full in the lamp’s light; joints of a hand that wrote there the king could not choose but see.
\end{scripture}"
```


```
(require 'catholicbible)

(catholicbible-get-verses "vulgate" "Luke" 2 "1-10")
```

```emacs-lisp
((:verse 1 :text "Factum est autem in diebus illis, exiit edictum a Cæsare Augusto ut describeretur universus orbis.")
 (:verse 2 :text "Hæc descriptio prima facta est a præside Syriæ Cyrino:")
 (:verse 3 :text "et ibant omnes ut profiterentur singuli in suam civitatem.")
 (:verse 4 :text "Ascendit autem et Joseph a Galilæa de civitate Nazareth in Judæam, in civitatem David, quæ vocatur Bethlehem: eo quod esset de domo et familia David,")
 (:verse 5 :text "ut profiteretur cum Maria desponsata sibi uxore prægnante.")
 (:verse 6 :text "Factum est autem, cum essent ibi, impleti sunt dies ut pareret.")
 (:verse 7 :text "Et peperit filium suum primogenitum, et pannis eum involvit, et reclinavit eum in præsepio: quia non erat eis locus in diversorio.")
 (:verse 8 :text "Et pastores erant in regione eadem vigilantes, et custodientes vigilias noctis super gregem suum.")
 (:verse 9 :text "Et ecce angelus Domini stetit juxta illos, et claritas Dei circumfulsit illos, et timuerunt timore magno.")
 (:verse 10 :text "Et dixit illis angelus: Nolite timere: ecce enim evangelizo vobis gaudium magnum, quod erit omni populo:"))
```

<!-- Local Variables: -->
<!-- mode: markdown -->
<!-- End: -->
