HTMLWidgets.widget({

  name: 'taucharts',

  type: 'output',

  initialize: function(el, width, height) {

    return {
    };

  },

  renderValue: function(el, x, instance) {

    while (el.firstChild)
      el.removeChild(el.firstChild);

    var datasource = HTMLWidgets.dataframeToD3(x.datasource);

    dbg_x = x ;

    var sheet = window.document.styleSheets[0];
    // sheet.insertRule('strong { color: red; }', sheet.cssRules.length);

    var chart = new tauCharts.Chart({
      data: datasource,
      type: x.type,
      size: x.size,
      guide: x.guide,
      x: x.x,
      y: x.y,
      color: x.color
    });

    chart.renderTo('#'+el.id);

  },

  resize: function(el, width, height, instance) {

  }

});
