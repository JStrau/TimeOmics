$(document).on('click', '.selectable div table tbody tr', function(){
	var el = $(this);
	$(this).siblings().removeClass("rowsSelected");
	$(this).addClass("rowsSelected", this.clicked);
	el.trigger("change");
});	
var selectRowBinding = new Shiny.InputBinding();
$.extend(selectRowBinding, {
	find: function(scope) {
		return $(scope).find(".selectable");
	},
	getValue: function(el){
    tbl = $(el).find("table");
    //uncomment to return row number instead
	  //return $(tbl).children().children('.rowsSelected').index() + 1;
    var out = [];
    row = $(tbl).children().children('.rowsSelected');
    if(row.length == 0) return -1;
    $(row).find("td").each(function(cell){
      out[cell] = $(this).text();
    });
    return out;
	},
	setValue: function(el, value) {
	},
	subscribe: function(el, callback) {
		$(el).on("change.selectRowBinding", function(e) {
			callback();
		});
	},
	unsubscribe: function(el) {
	  $(el).off(".selectRowBinding");
	}
});
Shiny.inputBindings.register(selectRowBinding);