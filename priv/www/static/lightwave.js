function dumpProps(obj, parent) {
   // Go through all the properties of the passed-in object
   for (var i in obj) {
      // if a parent (2nd parameter) was passed in, then use that to
      // build the message. Message includes i (the object's property name)
      // then the object's property value on a new line
      if (parent) { var msg = parent + "." + i + "\n" + obj[i]; } else { var msg = i + "\n" + obj[i]; }
      // Display the message. If the user clicks "OK", then continue. If they
      // click "CANCEL" then quit this level of recursion
      if (!confirm(msg)) { return; }
      // If this property (i) is an object, then recursively process the object
      if (typeof obj[i] == "object") {
         if (parent) { dumpProps(obj[i], parent + "." + i); } else { dumpProps(obj[i], i); }
      }
   }
}

wavename = window.location;
wavename = RegExp("http://[^/]*/?.*/([^/]+)/?").exec(wavename)[1];
wavename = wavename.toLowerCase();
var url = "/lightwave/" + wavename + "/";

(function($) {
    $(document).ready(function() {
        var curChanTick = "1";


        $("#chat").bind("message", function(event, message) {
            $(this).triggerHandler("append", [message]);

        }).bind("type", function(event, keys) {
            $(this).triggerHandler("appendType", [keys]);
        }).bind("error", function(event, message) {
            $(this).triggerHandler("append",[
                [
                    '<strong class="error">',
                    message,
                    '</strong>'
                ].join("")
            ]);
            $(this).triggerHandler("poll");
            
        }).bind("append", function(event, data) {
            $(".typer", this).each(function(){
              if ($(this).attr("data") == data.who) {
                $(this).remove();
              }
            });
            if (data.message == undefined) {
              return;
            }
            $("table", this).append([
                '<tr>',
                '<td class="timestamp">',data.timestamp, '</td>',
                '<td class="who">',data.who, '</td>',
                '<td>', data.message,'</td>',
                '</tr>'].join(""));
            this.scrollTop = $("table", this).get(0).offsetHeight;
        }).bind("appendType", function(event, data) {
            $(".typer", this).each(function(){
              if ($(this).attr("data") == data.who) {
                $(this).remove();
              }
            });
            if (data.message == "") {
              return;
            }
            $("table", this).append([
                '<tr class="typer" data="',data.who,'">',
                '<td class="timestamp">',data.timestamp,'</td>',
                '<td class="who">',data.who, '</td>',
                '<td>', data.message,'</td>',
                '</tr>'].join(""));
            this.scrollTop = $("table", this).get(0).offsetHeight;

        }).bind("poll", function(event) {
            var self = this;
            $.ajax({
                url: url + "get/" + curChanTick,
                dataType: "json",
                method: "get",
                success: function(arr) {
                    var ok = 0;
                    for (i = 0; i < arr.length; i++) {
                      data = arr[i];
                      if(data.status == "ok") {
                          curChanTick = data.tick + 1;
                          if (data.type == "message") {
                            $(self).triggerHandler("message", [data]);
                          } else if (data.type == "type") {
                            $(self).triggerHandler("type", [data]);
                          }
                          ok = 1
                      } else if(data.status == "error") {
                          $(self).triggerHandler("error", [data.error]);
                      } else {
                          alert("epic fail!");
                      }
                    }
                    if (ok) {
                      $(self).triggerHandler("poll");
                    }
                }
            });
        }).triggerHandler("poll");

        $("#chatter").submit(function(event){
            $.ajax({
                url: url + "chat",
                dataType: "json",
                type: "POST",
                data: "message="+$("#message").val()+"&who="+$("#who").val(),
                success: function(data) {
                    $("#message").val("").focus();
                    var chat = $("#chat");
                    if(data.status == "ok") {
                        chat.triggerHandler("posted", [data.message]);
                    } else if(data.status == "error") {
                        chat.triggerHandler("error", [data.error]);
                    } else {
                        alert("epic fail!");
                    }
                }
            });
            return false;
        });

        $("#message").keyup(function(event) {
            if (!typeTimer) {
              typeTimer = setTimeout('tellType()', 100);
            }
            return false;
        });
        $('#goto-wave').attr('value', wavename);

        $('#goto-wave').keydown(function(e){
          if(window.event)
            key = window.event.keyCode; //IE
          else
            key = e.which; //firefox
          if (key == 13) {
            $('#goto-wave-submit').click();
          }
        });
        $('#goto-wave-submit').click(function(){
            window.location = "/lightwave/" + $('#goto-wave').attr('value') + "/";
        });


        $("#message").attr("autocomplete", "off").focus();
    });
})(jQuery);

typeTimer = 0;
function tellTypeSuccess(data)
{
  var chat = $("#chat");
  if(data.status == "ok") {
    chat.triggerHandler("posted", [data.message]);
  } else if(data.status == "error") {
    chat.triggerHandler("error", [data.error]);
  } else {
    alert("epic fail!");
  }
}

function tellType()
{
  clearTimeout(typeTimer);
  typeTimer = 0;
  $.ajax({
    url: url + "type",
    dataType: "json",
    type: "POST",
    data: "keys="+$("#message").val()+"&who="+$("#who").val(),
    success: tellTypeSuccess
  });
}

// Show me the source easter egg.
$("#header").
/*    append([
            '<p class="source">',
                '(show me the <a href="#footer">source</a>.)',
            '</p>'
        ].join("")
    ). */
    click(function(event) {
        var target = event.target;
        if(target.nodeName.toLowerCase() === "a" &&
            $(target.parentNode).hasClass("source")) {
            
            $("#doc").append([
                '<pre id="source">',
                $("#script").
                    html().
                    replace(/</g, "&lt;").
                    replace(/>/g, "&gt;").
                    replace(/([.\-+()<>=;]|\s)(typeof|in|if|else|for|var|this|new|function)([.\-+()<>=;]|\s)/g, "$1<b>$2</b>$3").
                    replace(/(\/\/.*)/g, "<i>$1</i>").
                    replace(/('.*?')/g, "<em>$1</em>").
                    replace(/(".*?")/g, "<em>$1</em>").
                    replace(/(\d+)/g, "<strong>$1</strong>"),
                '</pre>'
            ].join(""));
            $(".source", this).remove();
        }
    });
