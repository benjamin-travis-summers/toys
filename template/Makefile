DOCS=index.html

all: ${DOCS}

clean:
	rm -f ${DOCS}

%.html: %.md doc.css
	pandoc -s -c ./doc.css -i $< -o $@
