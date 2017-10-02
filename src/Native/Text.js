let _timjs$elm_collage$Native_Text = function() {

  //NOTE: We re-use the canvas object for better performance
  let canvas = document.createElement("canvas");
  let context = canvas.getContext("2d");

  /**
  * Uses canvas.measureText to compute and return the width of the given text of given font in pixels.
  *
  * @param {String} font The css font descriptor that text is to be rendered with (e.g. "bold 14px verdana").
  * @param {String} text The text to be rendered.
  *
  * @see https://stackoverflow.com/questions/118241/calculate-text-width-with-javascript/21015393#21015393
  */
  function width(font, text) {
    context.font = font;
    let metrics = context.measureText(text);
    return metrics.width;
  }

  return {
    width: F2(width)
  };

}();
