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

    // for debugging data/parameter issues- remove for production version
    dbg_x = x ;

    // we have to use CSS styles for the chart object colors so we
    // create a new stylesheet for every chart on a page (since there could be many charts)
    // by creating a sheet we're also ensured it will exist
    // then we further ensure proper CSS rule targeting by using the element id
    // along with the color style

    var sheet_element = document.createElement('style');
    document.head.appendChild(sheet_element) ;
    var sheet = sheet_element.sheet;
    if (x.forCSS !== null) {
      x.forCSS.map(function(v) {
        v = v.replace("{{ID}}", '#'+el.id+' ');
        sheet.insertRule(v, sheet.cssRules.length);
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
          plugins.push(tauCharts.api.plugins.get('trendline')(plugin.settings));
        }
      });
    }


    var chart = new tauCharts.Chart({
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

    dbg_chart = chart ;

    chart.renderTo('#'+el.id);

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

  }

});
