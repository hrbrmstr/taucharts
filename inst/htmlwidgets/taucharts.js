HTMLWidgets.widget({

  name: 'taucharts',

  type: 'output',

  initialize: function(el, width, height) {

    return {};

  },

  renderValue: function(el, x, instance) {

    while (el.firstChild)
      el.removeChild(el.firstChild);

    var datasource = HTMLWidgets.dataframeToD3(x.datasource);

    // for debugging data/parameter issues- remove for production version
    // dbg_x = x ;

    // we have to use CSS styles for the chart object colors so we
    // create a new stylesheet for every chart on a page (since there could be many charts)
    // by creating a sheet we're also ensured it will exist
    // then we further ensure proper CSS rule targeting by using the element id
    // along with the color style

    var sheet_element = document.createElement('style');
    document.head.appendChild(sheet_element) ;
    var sheet = sheet_element.sheet;
    if (x.forCSS !== null) {
      if (typeof(x.forCSS) === "string") {
        x.forCSS = [ x.forCSS ] ;
      }
      x.forCSS.map(function(v) {
        v = v.replace("{{ID}}", '#'+el.id+' ');
        sheet.insertRule(v, sheet.cssRules.length);
      });
    }

    if (x.forFonts !== null) {
      if (typeof(x.forFonts) === "string") {
        x.forFonts = [ x.forFonts ] ;
      }
      x.forFonts.map(function(v) {
        var headID = document.getElementsByTagName("head")[0];
        var cssNode = document.createElement('link');
        cssNode.type = 'text/css';
        cssNode.rel = 'stylesheet';
        cssNode.href = v;
        cssNode.media = 'screen';
        headID.appendChild(cssNode);
      });
    }

    // work with plugins
    var plugins = [];
    if(typeof(x.plugins) !== "undefined" && x.plugins.length){
      x.plugins.map( function(plugin) {
        if( plugin.type === "tooltip" ) {
          plugin.fields = Array.isArray(plugin.fields) ? plugin.fields : [plugin.fields];
          plugins.push(
            tauCharts.api.plugins.get('tooltip')(
              {fields: plugin.fields}
            )
          );
        }

        if( plugin.type === "legend" ){
          plugins.push(tauCharts.api.plugins.get('legend')());
        }

        if( plugin.type === "trendline" ){
          if (!Array.isArray(plugin.settings.models)){
            plugin.settings.models = [plugin.settings.models];
          }
          if (!Array.isArray(plugin.settings.type)){
            plugin.settings.type = plugin.settings.type;
          }
          plugins.push(tauCharts.api.plugins.get('trendline')(plugin.settings));
        }

        if( plugin.type === "quick-filter"){
          plugins.push(tauCharts.api.plugins.get('quick-filter')(plugin.fields));
        }

        if( plugin.type === "exportTo"){
          plugins.push(tauCharts.api.plugins.get('exportTo')(
           {cssPaths: plugin.cssPaths}
          ));
        }

        if( plugin.type === "annotations"){
          plugins.push(tauCharts.api.plugins.get('annotations')(
            {items: HTMLWidgets.dataframeToD3(plugin.items)}
          ));
        }

        if( plugin.type === "box-whiskers"){
          plugins.push(tauCharts.api.plugins.get('box-whiskers')(
            {flip: plugin.settings.flip, mode: plugin.settings.mode}
          ));
        }
      });
    }
    // Non-categorical facets do nothing
    x.dimensions.facets = 'category';

    instance.chart = new tauCharts.Chart({
      data: datasource,
      type: x.type,
      size: x.size,
      guide: x.guide,
      x: x.x,
      y: x.y,
      color: x.color,
      dimensions: x.dimensions,
      plugins: plugins
    });

    dbg_chart = instance.chart;

    instance.chart.renderTo('#'+el.id);

    // set up a container for tasks to perform after completion
    //  one example would be add callbacks for event handling
    //  styling
    if (!(typeof x.tasks === "undefined") ){
      if ( (typeof x.tasks.length === "undefined") ||
       (typeof x.tasks === "function" ) ) {
         // handle a function not enclosed in array
         // should be able to remove once using jsonlite
         x.tasks = [x.tasks];
      }
      x.tasks.map(function(t){
        // for each tasks call the task with el supplied as `this`
        t.call({el:el,chart:chart});
      });
    }

  },

  resize: function(el, width, height, instance) {
    instance.chart.refresh();
  }

});
