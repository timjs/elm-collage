var _timjs$elm_collage$Native_Collage = function() {

  let svgns = "http://www.w3.org/2000/svg";

  // var getSvgDimensions =
  //   typeof document !== null && document.body !== null
  //   ? getRealSvgDimensions
  //   : function(_) { return _elm_lang$core$Native_Utils.Tuple2(0, 0); };


  function getSvgDimensions(rawSvg) {
    //NOTE: Svg namespace needs to be there to render inner contents correctly!
    let temp = document.createElementNS(svgns, 'svg');
    temp.innerHTML = rawSvg;
    temp.style.visibility = 'hidden';

    document.body.appendChild(temp);

    //NOTE: We get the first child to be sure to get the dimensions of the inner child, not the Svg root.
    let elem = temp.firstChild;
    let rect = elem.getBoundingClientRect();

    document.body.removeChild(temp);

    //console.log("dims", elem, rect.width, rect.height);
    return _elm_lang$core$Native_Utils.Tuple2(rect.width, rect.height);
  }

  return {
    getSvgDimensions: getSvgDimensions
  };

}();
