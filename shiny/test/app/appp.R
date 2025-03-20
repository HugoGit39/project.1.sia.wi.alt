library(shiny)
library(DT)

wideTable <- as.data.frame(matrix(rnorm(1000), nrow = 10, ncol = 100))

# JavaScript for DoubleScroll
doubleScrollJS <- "
(function( $ ) {
  $.fn.doubleScroll = function(userOptions) {
    var options = {
      contentElement: undefined,
      scrollCss: {
        'overflow-x': 'auto',
        'overflow-y': 'hidden',
        'height': '20px'
      },
      contentCss: {
        'overflow-x': 'auto',
        'overflow-y': 'hidden'
      },
      onlyIfScroll: true,
      resetOnWindowResize: false,
      timeToWaitForResize: 30
    };

    $.extend(true, options, userOptions);

    $.extend(options, {
      topScrollBarMarkup: '<div class=\"doubleScroll-scroll-wrapper\"><div class=\"doubleScroll-scroll\"></div></div>',
      topScrollBarWrapperSelector: '.doubleScroll-scroll-wrapper',
      topScrollBarInnerSelector: '.doubleScroll-scroll'
    });

    var _showScrollBar = function($self, options) {
      if (options.onlyIfScroll && $self.get(0).scrollWidth <= Math.round($self.width())) {
        $self.prev(options.topScrollBarWrapperSelector).remove();
        return;
      }

      var $topScrollBar = $self.prev(options.topScrollBarWrapperSelector);

      if ($topScrollBar.length == 0) {
        $topScrollBar = $(options.topScrollBarMarkup);
        $self.before($topScrollBar);

        $topScrollBar.css(options.scrollCss);
        $(options.topScrollBarInnerSelector).css('height', '20px');
        $self.css(options.contentCss);

        var scrolling = false;

        $topScrollBar.bind('scroll.doubleScroll', function() {
          if (scrolling) {
            scrolling = false;
            return;
          }
          scrolling = true;
          $self.scrollLeft($topScrollBar.scrollLeft());
        });

        var selfScrollHandler = function() {
          if (scrolling) {
            scrolling = false;
            return;
          }
          scrolling = true;
          $topScrollBar.scrollLeft($self.scrollLeft());
        };
        $self.bind('scroll.doubleScroll', selfScrollHandler);
      }

      var $contentElement;
      if (options.contentElement !== undefined && $self.find(options.contentElement).length !== 0) {
        $contentElement = $self.find(options.contentElement);
      } else {
        $contentElement = $self.find('>:first-child');
      }

      $(options.topScrollBarInnerSelector, $topScrollBar).width($contentElement.outerWidth());
      $topScrollBar.width($self.width());
      $topScrollBar.scrollLeft($self.scrollLeft());
    };

    return this.each(function() {
      var $self = $(this);
      _showScrollBar($self, options);
      if (options.resetOnWindowResize) {
        var id;
        var handler = function(e) {
          _showScrollBar($self, options);
        };

        $(window).bind('resize.doubleScroll', function() {
          clearTimeout(id);
          id = setTimeout(handler, options.timeToWaitForResize);
        });
      }
    });
  };
}( jQuery ));
"

# JavaScript to apply DoubleScroll to the DT table
applyDoubleScrollJS <- "
$(document).ready(function(){
  $('#dtable').on('shiny:value', function(e){
    setTimeout(function(){
      $('#dtable table').wrap('<div id=\"scrolldiv\"></div>');
      $('#scrolldiv').doubleScroll({
        contentElement: $('table'),
        scrollCss: {
          'overflow-x': 'scroll',
          'overflow-y': 'hidden'
        },
        contentCss: {
          'overflow-x': 'scroll',
          'overflow-y': 'hidden'
        },
        resetOnWindowResize: true
      });
      setTimeout(function(){$(window).resize();}, 100);
    }, 0);
  });
});
"

# CSS for DoubleScroll
CSS <- "
.doubleScroll-scroll-wrapper {
  clear: both;
}
"

ui <- fluidPage(
  tags$head(
    tags$script(HTML(doubleScrollJS)),  # Embed doubleScroll JavaScript
    tags$script(HTML(applyDoubleScrollJS)),  # Apply doubleScroll to DT
    tags$style(HTML(CSS))  # Add custom CSS
  ),
  br(),
  DTOutput("dtable")
)

server <- function(input, output, session){
  output[["dtable"]] <- renderDT({
    datatable(wideTable)
  })
}

shinyApp(ui, server)
