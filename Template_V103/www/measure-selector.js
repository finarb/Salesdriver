function defaultActions() {
      $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Tog') === 0; }).hide();
      $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Tog') === 0; }).next().hide();
      $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Product') === 0; }).hide();
      $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Product') === 0; }).next().hide();
      $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Product') === 0; }).next().next().hide();
      $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Product') === 0; }).next().next().next().hide();
      $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Product') === 0; }).next().next().next().next().hide();
      $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Value') === 0; }).next().hide();
      var arr = ["TDP WD","Avg SU Price","Promo SU Price","Reg SU Price","Any Promo","No Promo","VOF-Disp","VODisp-F","VOF+Disp","TPR Only"];
      var i = 0;
      var len = $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Value') === 0; }).length;
      $('.dt-button-collection a').filter(function() { return $(this).text().indexOf('Value') === 0; }).each(function () {
      if(i<len) {
      $(this).text(arr[i]);
      $(this).click(function(){
      $(this).next().trigger("click");
      });
      }
      i++;
  });
}
function modifyColumnVisibility () {
        $('a:contains("Measure Selector")').click(function(){
        setTimeout(function(){
        defaultActions();
        }, 1000);
        });
}
