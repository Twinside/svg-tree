Rasterific-svg
==============

SVGTiny loader/renderer/serializer based on Rasterific.

Current capabilities
--------------------

The current version implements SVG Tiny1.2 with the exception of:

 * non-scaling stroke.
 * textArea element.

The implementation also implements feature from SVG 1.1 like:

 * Advanced text handling (text on path, dx/dy), but with
   low support for Unicode, right to left and vertical text.
 * CSS Styling, using CSS2 styling engine.
 * `pattern` element handling

