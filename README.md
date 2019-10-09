# Dociface: an R package for identifying and reconstructing elements from text documents

In this package, Documents are considered one or multi-page containers
for text and we are primarily focusing on PDF and scanned (OCR'ed)
documents. We don't exclude Word, OpenOffice, Pages and other forms
of word processing documents, or diagrams or even spreadsheets
(although these have much richer structure).

Dociface provides virtual classes and generic functions for
identifying document elements. Classes and methods specific to PDF
documents can be found in
[ReadPDF](https://github.com/dsidavis/ReadPDF), while those for OCR
documents can be found in
[Rtesseract](https://github.com/duncantl/Rtesseract). Methods specific
to certain types of documents, e.g. tabular data, academic papers,
etc., can be found in additional packages (in development).

# Essential functions

Dociface provides generics for identifying:

  - Text characteristics, including the bounding boxes of the text
  
  - Columns and column positions
  
  - Header and footer text
  
  - Line breaks, margins, and other "whitespace" elements

  - Shapes, lines, and figures
  
  - Font information (or text information, in the case of OCR
    documents)
	
  - Section titles/headers and text by section
  
Additionally, `Dociface` provides methods to plot one or multiple
`DocumentPage`s.

# Writing specific methods for new document types



