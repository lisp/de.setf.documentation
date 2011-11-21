;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation.xml; -*-


(in-package :de.setf.utility.implementation.xml)

(:documentation
  "This file implements xhmtl resource response interface for the introspction service."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (description
  "The functions implement the resource representations for the services resource.
 They adopt variations on several strategies. Where the request succeeds, they
 - computed results and generate a response document in-line
 - delegte to an external process and redirect to its result
 - delegate to an external document
 When the request fails, a failure 'receipt' is generated to accompany the unsuccessful response code.

 - /systems 
   Generate the overview  document inline based on the known project's system definitions,
   The document includes references to additional static and dynamic resources.

 - /system/*/controls.html
   Generate the controls for a single project to update the sources, build it, and view it.

 - /system/*/operation.*? 
   Builds the system with a specified implementation and generates response document which
   collects links to the build script and result transcripts. The latter is also exported under
   a generic location, so the client can likely ignore the response document and present the
   respective generic transcript resource directly.

 - /system/*/projection.*?
   Generates a view of the system through the specified projection and redirects to it.
   The result transcript is also exported. If the view exists, and postdates the build, nothing new
   need be generated."))

 

;;; let us see. in order to get this to work for jquery in firefox and safari
;;; - no &apos; entities even if it is xhtml
;;; - no cdata section, even ...
;;; - static html files only. dynamically generated - even with the javascript inline, causes errors:
;;;   $ undefined, JQuery undefined, and .data as well. that is, in the context of each of the - evidently read,
;;;   javascript passages. thus the workaround to externalize to a file and serve as static html from there.
;;;
;;; progressing further, the first attempt was a single page:
;;;
;;; accordion
;;;   configuration
;;;   new system
;;;   results : [creation update construction test modules packages calls tests]
;;;
;;; but javascript was not capable to handle rewriting the tab uri on the fly to present different systems.
;;; firefox and safari behaved differently, sometime reacted, somethimes not, signaled 'wrong_document_err' conditions
;;; on the occasions when the tabs even worked, and presented static file:/// content in manners different than
;;; http:// content. the usual.
;;;
;;; the second attempt. use a large element on the lower half of the page to display content as triggered
;;; the configuration controls. it turned out, that a problem similar to the load-order issue for chunked streams
;;; appeared for tabbed panes which were to load their own content from a tab element's href. if they
;;; load from a static file (as in file:///) they work, if they are served from a file - whether os x apache or
;;; cl-http, s hort "loading" label appears, but no content and the dom contains no panel element to have accepted
;;; the content. when loaded form a file such an element is present.
;;; sure, i must ahve done something wrong. it's just that it should not be possible to make such mistakes.
;;;
;;; next pass. jython or gwt might be a nice idea, but neither yields ready documentation for the javascript
;;; component of the architecture. next is dojo...
;;;
;;; in principle, dogo has a clearer implementation architecture. in one aspect, dom accessors and node
;;; manipulation, with html-based declarattions, and in the other, the javascript mirror. dojo.byId v/s
;;; digit.byId, onclick v/s onCLick, and the consequent parallel worlds. Were it possible to find definitive
;;; documentation in one place, all would be well. as it is, there are rafts of o.dojo... page which
;;; overwhelm search results and even the up-to-date descriptions are divided between examples at
;;; docs.dojocampus.org and the api documentation at api.dojotoolkit.org. why bother?
;;;
;;; in any case, the example do run - in contrast to jQuery both as copy-and-try static files and
;;; cut into generated pages. The aesthetic is not as spiffy, but it works. Keep in mind:
;;; - $ == dojo.query
;;; - it's dojo and dijit, not dogo or digit
;;; - "this._addView is not a function" often means that one applied a constructor rather than combine it 'new'
;;; - "Error: Could not load class 'dojox.layout.contentPane'. Did you spell the name correctly and use a full path, like 'dijit.form.Button'"
;;;   watch the capitalization
;;; - ;; ({}widgetId id) DON'T! : an element declared to be managed by dojo _must not_ have a declared widgetId.
;;;   if one is present the results are undefined. The activation process is observed to abort, do something else, or whatever.
;;; - the line number references for javascript errors on XHR document specifies the original document and is one or two higher
;;;   than the actual location
;;; - innerHTML applies to DOM nodes, but operators like selectChild applie sto the dijit wrapper object.
;;; - when a content pane loads a document, it strips any html wrapper and uses just the body as the inner html value.
;;;   the means that header style information is not applied and any script elements must be in the body.
;;; - errors in the load/error functions to an xhrGet vanish without a trace. the process just stops.

#|  can add a style element to the root svg element

<defs>
    <style type="text/css"><![CDATA[
      .node {
        fill: red;
        stroke: blue;
        stroke-width: 3
      }
    ]]></style>
  </defs>

|#

(defun /systems.* (request stream)
  (let* ((extension (or (url:extension request) "txt"))
         (mime-type
          (or (ignore-errors (mime:mime-type (http::mime-content-type-spec-for-pathname-type extension)))
              mime:text/plain))
         (system-names (sort (mapcar #'asdf:component-name (list-prototype-systems)) #'string-lessp)))
    (typecase mime-type
      (mime:text/plain
       (http:with-successful-response (stream :text)
         (format stream "~{~a~%~}" system-names)))
      (mime:text/csv
       (http:with-successful-response (stream :text)
         (format stream "~{~a~^,~}~%" system-names)))
      (t
       (www-utils:notify-log-window "Anomolous requestd encoding (~a ~a ~a)." request extension mime-type)
       (error 'http:document-not-found
              :url request
              :reason (format nil "Encoding not supported: ~s." mime-type))))))
      
      

(defun /systems (request stream)
  "Present a system overview. This encompasses:
 - a panel to add a system to the repository;
 - a panel which displays the current ecosystem and allows to choose an existing system;
 - panels are added per system to build/graph each one."
  
  (declare (ignore request))
  
  (http:with-successful-response (stream :html)
    (let ((*print-case* :downcase) (*print-pretty* t))
      (with-xml-writer (stream)
        (encode-xml-declaration)
        (encode-newline)
        (encode-document-type "xhtml" :public xqdm:+xhtml-public-identifier+
                              :system xqdm:+xhtml-system-identifier+)
        (xml ({xhtml}html ({xmlns}||  "http://www.w3.org/1999/xhtml")
                          ({xmlns}xlink  "http://www.w3.org/1999/xlink"))
             ({xhtml}head
              (({}meta ({}name "date") ({}content  (iso-time))))
              ({xhtml}title */systems-title*)
              (({}meta ({}http-equiv "Content-Type")
                       ({xhtml}content "text/xhtml;charset=utf-8")))
              (encode-stylesheet-resources)
              (({}style ({}type "text/css")) "
                 html, body { width: 100%; height: 100%; margin: 0; }
                 polygon { visibility: hidden; }
                 #headerContainer { padding: 8px;}
                 .controls *[href]:hover:not([disabled]) {border: outset 2px; }
                 .controls *[href]:not([disabled]) { border: inset 2px; }
                 .controls *[href]:[disabled] { border: solid white 2px; }
                 .controls { background-color: #e0e0e0; }
                 .controls {
                   color: black; background-color: transparent; 
                 }
                 .controls .label {clear: left; float: left; margin-left: 2px; height: 3ex;}
                 .controls .inputText {vertical-align: bottom;
                   margin-bottom: 4px;// width: 190px; height: 120px;
                   text-align: justify;
                 }
                 .buttons.runtime { width: 98%; position: absolute; bottom: 2px; }
                 .controls select {clear: right; float: right; }
                 .controls .button {clear: right; float: right; }
                 .controls .buttons {clear: right; float: right; }
                 .controls button {clear: right; float: right; }
                 .controls textarea {clear: both; width: 98%; }
                 .controls .clear {clear: both;}

                 // for report index panes
                 .filelist .file { margin-top: 10px; border-top: solid gray 1px; }
                 .filelist .file .name { font-weight: bold; }
                 .filelist .file .content { white-space: pre; margin-left: 48px; }
                 }"))
             
              
             (({xhtml}body ({}class *body-style-class*))
              
              (({xhtml}div ({}id "documentContainer") ({}dojoType "dijit.layout.BorderContainer")
                           ({}style "width: 100%; height: 100%;")
                           ({}design "headline") ({}gutters "false"))
               (({xhtml}div ({}id "headerContainer") ({}dojoType "dijit.layout.ContentPane")
                            ({}region "top") ({}splitter "true")
                            ({}style "width: 100%; height: 25%;")
                            ({}href "/html/index.html"))
                "")
               (({xhtml}div ({}id "bodyContainer") ({}dojoType "dijit.layout.BorderContainer")
                            ({}region "center")
                            ({}design "sidebar")
                            ({}gutters "false"))
                
                ;; left panels
                (({xhtml}div ({}id "controlContainer") ({}dojoType "dijit.layout.AccordionContainer")
                             ({}region "leading")  ({}splitter "true")
                             ({}style "width: 220;")
                             )
                 (({xhtml}div ({}id "overviewPane") ({}dojoType "dijit.layout.ContentPane")
                              ({}title "Projects") ({}style "padding: -2px;")
                              ({}class "configuration controls"))
                  (({xhtml}div ({}style ""))
                   (({xhtml}div ({}class "label") ({}style "height: 5ex;"))
                    "Select Project")
                   (({xhtml}button ({}id "showProjectButton") ({}name "showProjectButton")
                                   ({}style "position: absolute; top: 6px; right: 4px;")
                                   ({}operation "Show")
                                   ({}href "/system//")
                                   ({}dojoType "dijit.form.Button") ({}type "button"))
                    "Show")
                   (({xhtml}div ({}style "height: 24px;"))
                   (let ((system-names (sort (mapcar #'asdf:component-name (list-prototype-systems)) #'string-lessp)))
                     (xml ({xhtml}select ({}name "systemMenu") ({}id "systemMenu"))
                          (loop for name in  system-names
                                do (xml ({xhtml}option ({}value name)) (encode-string name)))))))

                  (({xhtml}div ({}style "position: absolute; bottom: 0px;"))
                   (({xhtml}div ({}class "label") ({}style "height: 5ex;"))
                    "New Project Name")
                   (({xhtml}div ({}class "button"))
                    (({xhtml}button ({}id "newButton") ({}name "newButton")
                                    ({}style "position: absolute; top: 2px; right: 4px;")
                                    ({}operation "New")
                                    ({}dojoType "dijit.form.Button") ({}type "button"))
                     "New"))
                   
                   (({xhtml}textarea ({}name "systemName") ({}id "systemName") ({}value "")) "")
                   (({xhtml}div ({}class "label")) "Repository URL")
                   (({xhtml}textarea ({}name "repositoryURL") ({}id "repositoryURL") ({}value "")) "")
                   (({xhtml}div ({}class "label")) "Repository type")
                   (({xhtml}select ({}name "typeMenu") ({}id "repositoryType"))
                    (({xhtml}option ({}value "DARCS")) "darcs")
                    (({xhtml}option ({}value "GIT")) "git")
                    (({xhtml}option ({}value "SVN")) "svn")))))
                  
                ;; right panels
                (({xhtml}div ({}id "displayContainer") ({}dojoType "dijit.layout.AccordionContainer")
                             ({}region "center"))
                 (({xhtml}div ({}id "overviewDisplay") ({}dojoType "dijit.layout.AccordionPane")
                              ({}title "Overview") ({}closable "true")
                              ;; ({}href *overview-graph*) ({}type "image/svg+xml")
                              )
                  (({xhtml}object ({}style "border: inset;")
                                  ({}data *overview-graph*) ({}type "image/svg+xml") 
                                  ({}scrolling "yes") ({}frameborder "0")
                                  ({}width "99%") ({}height "99%")) "opensource projects"))
                 (({xhtml}div ({}id "directoryDisplay") ({}dojoType "dijit.layout.AccordionPane")
                              ({}href "/lisp/source/") ({}title "Project Sources"))
                  "Project Sources")
                 (({xhtml}div ({}id "reportsPane") ({}dojoType "dijit.layout.AccordionPane")
                              ({}class "transcript")
                              ({}href "") ({}title "Reports"))
                  (({xhtml}div ({}id "reportsNavigator") ({}dojoType "dijit.layout.BorderContainer")
                               ({}style "width: 100%; height: 100%;") ({}design "sidebar"))
                   (({xhtml}div ({}id "reportsListAndContentPane") ({}dojoType "dijit.layout.AccordionContainer")
                                ({}region "center"))
                    (({xhtml}div ({}id "reportList") ({}dojoType "dijit.layout.AccordionPane")
                                 ({}title "Report List"))
                     "Report List")
                    (({xhtml}div ({}id "reportContent") ({}dojoType "dijit.layout.AccordionPane")
                                 ({}title "Report Content"))
                     "Report Content"))
                   (({xhtml}div ({}id "calendarPane") ({}dojoType "dijit.layout.ContentPane")
                                ({}region "right"))
                    (({}style ({}type "text/css")) "@include '/css/Calendar.css';")
                    
                    (({xhtml}div ({}id "reportCalendar")) "Report Calendar")))))))

              (encode-javascript-resources)
              (encode-javascript-requires '("dijit.layout.BorderContainer" "dijit.layout.AccordionContainer"
                                            "dijit.layout.TabContainer" "dijit.layout.ContentPane"
                                            "dijit.form.Button" "dojox.layout.ContentPane"
                                            "dojox.widget.Calendar" "dojo.fx"))
              ;; dojo startup
              (({}script ({}type "text/javascript")) "
  controlContainer = null;
  displayContainer = null;
  reportCalendar = null;
  reportsPane = null;
  reportsListAndContentPane = null;
  reportListPane = null;
  reportContentPane = null;
  reportContentItem = null;
  newButton = null;
  disabledName = 'disabled';

  // define global operators to generate/activate a pane to operate on a given project and to
  // present the result transcripts

  ensureProjectDisplayContainer = function (systemName) {
    // find or create the system's display pane
    displayContainer.getChildren().forEach( function(pane) {
        if (pane.attr('title') == systemName) {
          return(pane);
        }
      });
    var systemContainer =  new dijit.layout.TabContainer({ title: 'Display ' + systemName });
    displayContainer.addChild(systemContainer);
    displayContainer.selectChild(systemContainer);
    return(systemContainer);
  };

  function showSelectedProject () {
    // apply the current systemName selection to activate the respective controlPane.
    // if none is present, create a new one.
    showProject(dojo.byId('systemMenu').value);
  };

  function showProject(systemName) {
    var container = controlContainer;
    systemControlPane = dijit.byId(systemName + 'Controls');

    console.log('showSelectedProject: ' + systemControlPane + '.');
    if ( systemControlPane == undefined ) {
      // make anew if necessary
      var url = '/system/' + systemName + '/index.div';
      systemControlPane = new dojox.layout.ContentPane( {
        closable: true,
        title: systemName,
        id: systemName + 'Controls',
        href: url
      } );
      systemControlPane.closable = true;
      console.log('showSelectedProject: new: ' + systemControlPane + ' : ? ' + systemControlPane.closable + '.');
      container.addChild(systemControlPane);
    }
    container.selectChild(systemControlPane);
  };

  function showReportList(url) {
    reportListPane.setAttribute('label', url);
    reportListPane.setAttribute('title', url);
    reportListPane.setAttribute('href', url);
    displayContainer.selectChild(reportsPane);
    reportsListAndContentPane.selectChild(reportListPane);
  };

  function showReport(url) {
    reportContentItem.setAttribute('label', url);
    reportContentItem.setAttribute('title', url);
    reportContentPane.setAttribute('href', url);
    dojo.style(reportContentItem, 'white-space', 'pre');
    dojo.style(reportContentItem, 'white-space', 'pre');
    displayContainer.selectChild(reportsPane);
    reportsListAndContentPane.selectChild(reportContentPane);
  };

  function showHTTPResponse(data, httpCode, message) {
    console.log('showHTTPResponse: [' + httpCode + '] [' + message + ']');
    title = message + ((200 == httpCode) ? '' : ' (' + httpCode + ')');
    console.log('showHTTPResponse: [' +title + '] [' + data + ']');
    reportContentItem.setAttribute('label', title);
    reportContentItem.setAttribute('title', title);
    displayContainer.selectChild(reportsPane);
    reportsListAndContentPane.selectChild(reportContentPane);
    if ( '<' != data[0] ) {
      reportContentPane.attr('content', '<pre>' + data + '</pre>');
    } else {
      reportContentItem.innerHTML = data;
    }
    // console.log('HTTPResponse: [' + httpCode + '] [' + data + ']');
  };

  function disableLink(button) { button.attr(disabledName, true); };
  function enableLink(button) { button.attr(disabledName, false); };
  function isDisabled(button) { return true == button.attr(disabledName); };
  function disableLink(link) {
    if (link.attr) { link.attr(disabledName, true); }
    else { link.setAttribute(disabledName, true); };
  }
  function enableLink(link) {
    if (link.attr) { link.attr(disabledName, false); }
    else { link.removeAttribute(disabledName); };
  }
  function disableLinks(links) { links.forEach(disableLink); };
  function enableLinks(links) { links.forEach(enableLink); };
  function blinkLink(link) {
    fx = dojo.fadeOut({
      node: link,
      onEnd: function(){ dojo.fadeIn({ node: link }).play() }
    });
    fx.play();
    fx.play();
  };
  function bindLink(link, op) {
    // given a dom element, set onClick
    console.log('bind link: ' + link);
    link.onclick = function () { op(link); };
  };
  function bindButton(button, op) {
    // given a dijit object, set onClick
    console.log('bind button: ' + button);
    button.onClick = function () { op(button); };
  }
  function bindLinks(links, op) {
    links.forEach( function (link) { bindLink(link, op); } );
  };

  dojo.addOnLoad(function() {
    controlContainer = dijit.byId('controlContainer');
    displayContainer = dijit.byId('displayContainer');
    reportsPane = dijit.byId('reportsPane');
    reportsListAndContentPane = dijit.byId('reportsListAndContentPane');
    reportListPane = dijit.byId('reportList');
    reportContentPane = dijit.byId('reportContent');
    reportContentItem = dojo.byId('reportContent');
    var systemNameItem = dojo.byId('systemName');
    var repositoryURLItem = dojo.byId('repositoryURL');
    var repositoryTypeItem = dojo.byId('repositoryType');
    var systemName = '';
    var repositoryURL = '';
    var repositoryType = '';

    var requestOperation = function(url, operation) {
      // request an operation and display the response - whether success or failure, in the general propert pane.        
      var updateReportContentPane = function(data, ioargs) {
        var httpCode = ioargs.xhr.status;
        console.log('requestOp http code: [' + httpCode + '].');
        showHTTPResponse(data, httpCode, operation);
      };
      dojo.xhrGet({ url: url, sync: false, timeout: 30000,
                    load: updateReportContentPane, error: updateReportContentPane });
    };

    var extractRepositoryType = function(url) {
      if ( /darcs/.test(url) ) { return('DARCS'); }
      else if ( /git/.test(url) ) { return('GIT'); }
      else if ( /svn/.test(url) ) { return('SVN'); }
      else { return(null); }
    };

    var updateRepositoryParameters = function() {
      systemName = systemNameItem.value.replace(/\\s+/g,'');
      repositoryURL = repositoryURLItem.value.replace(/\\s+/g,'');
      repositoryType = extractRepositoryType(repositoryURL);
      if ( null != repositoryType ) {
        children = repositoryTypeItem.children;
        for (i = 0; i < children.length; i ++) {
          var item = children[i];
          console.log('child: ' + item + ' value: ' +  item.getAttribute('value') + '.');
          if (repositoryType == item.getAttribute('value')) {
            repositoryTypeItem.selectedIndex = i;
            break;
          }
        };
      } else {
        repositoryType = repositoryURLItem.value;
      }
      console.log('url: [' + repositoryURL + '] type [' + repositoryType + ']');
      if ( ('' != systemName) && ('' != repositoryURL) && ('' != repositoryType) ) {
        enableLink(newButton);
      } else {
        disableLink(newButton);
      }
    }
    var newAction = function(button) {
      if ( ! isDisabled(button) ) {
        var url = '/system/' + systemName + '/new&url=' + repositoryURL + '&type=' + repositoryType;
        button.title = url;
        console.log('requestNew: (' + url + ')');
        disableLink(button);
        requestOperation(url, button.attr('operation'));
        enableLink(button);
        showProject(systemName);
      } else {
        blinkLink(button);
      }
    };

    // bind the controls
    systemNameItem.onblur = updateRepositoryParameters;
    systemNameItem.value = '';
    repositoryURLItem.onblur = updateRepositoryParameters;
    repositoryURLItem.value = '';
    repositoryTypeItem.onblur = updateRepositoryParameters;
    newButton = dijit.byId('newButton');
    bindButton(newButton, newAction);
    disableLink(newButton);
    // bind the project selection menu
    dojo.byId('showProjectButton').onclick = showSelectedProject;
    // construct and attach the calendar
    reportCalendar = new dojox.widget.DailyCalendar({}, dojo.byId('reportCalendar'));
    dojo.connect(reportCalendar, 'onValueSelected', function(date) {
      month = (1 + date.getUTCMonth());
      day = date.getUTCDate();
      if (day < 10) { day = '0' + day; };
      if (month < 10) { month = '0' + month; };
      url = '/reports/' + date.getUTCFullYear() + '/' + month + '/' + day;
      console.log('report list: ' + url );
      showReportList(url);
    });
  }); ")))
        (encode-newline)))))

(defun encode-project-javascript (request stream &key system-name div-id xhr-request-p
                                  extent-textarea-id encoding-menu-id projection-menu-id relation-menu-id
                                  update-button-id)
  "Generate the javascript to manage the presentation of a single project."

  (declare (ignore request xhr-request-p))      ; keep it. in case.

  (xmlp:with-xml-writer (stream)
    (xml ({}script ({}type "text/javascript")) "
  console.log('js: " (encode-string div-id) " defining javascript ...');

  dojo.addOnLoad(function() {
    console.log('js:  addOnLoad " (encode-string div-id) " starting ...');

    // bind project-specific elements, widgets, &co.
    // activate the links - whereby the build and projection links require different actions.
    // connect the relation menu to the text field
    var systemName = '" (encode-string system-name) "';
    var extentItem = dojo.byId('" (encode-string extent-textarea-id) "');
    var originalExtent = '';
    var encodingItem = dojo.byId('" (encode-string encoding-menu-id) "')
    var projectionItem = dojo.byId('" (encode-string projection-menu-id) "')
    var relationItem = dojo.byId('" (encode-string relation-menu-id) "')
    var opLinks = dojo.query('#ops-" (encode-string div-id) " .link');
    var updateButton = null;
    var projectionLinks = dojo.query('#view-" (encode-string div-id) " .link');
    var projectDisplayContainer = null;
    var itemName = '" (encode-string div-id) "RepositoryURL';
    var repositoryURLItem = dojo.byId(itemName);
    console.log('name (' + itemName + ') -> ' + repositoryURLItem);
    itemName = '" (encode-string div-id) "RepositoryType';
    var repositoryTypeItem = dojo.byId(itemName);
    console.log('name (' + itemName + ') -> ' + repositoryTypeItem);

    var updateRelationExtent = function() {
      // for attachment to the relation menu to update the text area with the related items  
      var relation = relationItem.value;
      // just the objects
      if ( 'systems' == relation || 'modules' == relation || 'files' == relation) {
        var url ='/system/' + systemName + '/projection/' + relation + '.csv';
        console.log('extent url: [' + url + '] -> extentItem: ' + extentItem + '.');
        var updateExtent = function (data)  {
          console.log('extent data: [' + data + '] -> extentItem: ' + extentItem + '.');
          originalExtent = data;
          extentItem.value = data;
        };
        dojo.xhrGet({ url: url, load: updateExtent });
      } else {
        extentItem.value = '';
      }
    };

    var ensureProjectDisplayPane = function(name) {
      var pane = null;
      if ( null == projectDisplayContainer ) {
        projectDisplayContainer = ensureProjectDisplayContainer(systemName);
      }
      projectDisplayContainer.getChildren().forEach( function(oldPane) {
        if (oldPane.attr('title') == name) {
          pane = oldPane;
        }
      });
      if (null == pane) {
        pane = new dojox.layout.ContentPane({ title: name });
      }
      projectDisplayContainer.addChild(pane);
      return(pane);
    };

    var requestProjection = function(url, graphName, link) {
      // given a link to the abstract graph designator,  place a request.
      // if the request succeeds, create or locate the respective project graph pane and, set its content and display it
      // if the request fails, set the report list and report content items to display the transcript and display them.

      // either pass the location or do th xhr. attempts to do both deadlock in firefox
      // var pane = ensureProjectDisplayPane(graphName, url);
      // projectDisplayContainer.selectChild(pane);
      // return;
  
      var updateProjectDisplayPane = function(data, ioargs) {
        var httpCode = ioargs.xhr.status;
        console.log('requestProjection http code: [' + httpCode + '].');
        enableLink(link);
        var pane = ensureProjectDisplayPane(graphName, url);
        if ( '<' == data[0] ) {
          dojo.style(pane, 'white-space', 'normal');
        } else {
          dojo.style(pane, 'white-space', 'pre');
        }
        pane.attr('content', data);
        projectDisplayContainer.selectChild(pane);
      };

      var updateReportContentPane = function(errMsg, ioargs) {
        var httpCode = ioargs.xhr.status;
        var data = ioargs.xhr.responseText;
        console.log('requestProjection http code: [' + httpCode + '] error data: [' + data + '].');
        enableLink(link);
        showHTTPReport(data, httpCode, systemName + '-' + graph-name);
       }; 

      disableLink(link);
      dojo.xhrGet({ url: url, sync: false, timeout: 30000,
                    load: updateProjectDisplayPane, error: updateReportContentPane });
    };

    var requestOperation = function(url, operation, link) {
      // given a link to the abstract graph designator,  place a request.
      // if the request succeeds, create or locate the respective project graph pane and, set its content and display it
      // if the request fails, set the report list and report content items to display the transcript and display them.
        
      var updateReportContentPane = function(data, ioargs) {
        var httpCode = ioargs.xhr.status;
        console.log('requestOperation: http code: [' + httpCode + '].');
        //console.log('requestOperation: http data: [' + data + '].');
        enableLink(link);
        console.log('requestOperation: link enabled.');
        showHTTPResponse(data, httpCode, systemName + '-' + operation);
        console.log('requestOperation: http response shown.')
      };
      var refreshControlsPane = function(data, ioargs) {
        updateReportContentPane(data, ioargs);
        console.log('requestOperation: operation [' + operation + ']: ' + ('compile-op' == operation ));
        if ('compile-op' == operation ) {
          thisPane = dijit.byId(systemName + 'Controls');
          console.log('requestOperation: refresh this pane: ' + thisPane);
          //thisPane.setAttribute('href', thisPane.getAttribute('href'));
          thisPane.refresh();
          console.log('requestOperation: should not be here.');
        }
      }
      var reportError = function(errorMsg, ioargs) {
        // display the status and then refresh this pane
        console.log('reportError: error: [' + errorMsg + '].');
        var data = ioargs.xhr.responseText;
        updateReportContentPane(data, ioargs);
      }

      disableLink(link);
      dojo.xhrGet({ url: url, sync: false, timeout: 30000,
                    load: refreshControlsPane, error: reportError });
    };

    var projectionLinkAction = function(link) {
      // if the link is enabled, augment its generic url with the current settings and request the document
      // otherwise just alert
      console.log('p-link! ' + link);
      if ( link.getAttribute(disabledName) != 'true' ) {
        var url = link.getAttribute('href');
        var extent = extentItem.value.replace(/\\s+/g,'');
        var graphName = relationItem.value +
                  '-' + projectionItem.value +
                  '.' + encodingItem.value;
        url = url + '/' + graphName;
        if (originalExtent != extent && '' != extent) { // not necessary if whitespace is removed !(/^\s*$/.test(extent))) {
           url = url + '&extent=' + extent;
        };
        link.title = url;
        console.log('requestProjection: (' + url + ')');
        requestProjection(url, graphName, link);
      } else {
        fx = dojo.fadeOut({
          node: link,
          onEnd: function(){ dojo.fadeIn({ node: link }).play() }
        });
        fx.play();
        fx.play();
      }
    };

    var operationLinkAction = function(link) {
      if ( link.getAttribute(disabledName) != 'true' ) {
        var url = link.getAttribute('href');
        link.title = url;
        console.log('requestAction: (' + url + ')');
        requestOperation(url, link.getAttribute('operation'), link);
      } else {
        fx = dojo.fadeOut({
          node: link,
          onEnd: function(){ dojo.fadeIn({ node: link }).play() }
        });
        fx.play();
        fx.play();
      }
    };

    var updateAction = function(button) {
      if ( ! isDisabled(button) ) {
        var repositoryURL = repositoryURLItem.value.replace(/\\s+/g,'');
        var repositoryType = repositoryTypeItem.value;
        var url = '/system/' + systemName + '/update';
        if ( '' != repositoryURL ) { url = url + '&url=' + repositoryURL; };
        if ( '' != repositoryType ) { url = url  + '&type=' + repositoryType; };
        button.attr('title', url);
        console.log('requestUpdate: (' + url + ')');
        disableLink(button);
        requestOperation(url, button.attr('operation'), button);
        enableLink(button);
      } else {
        blinkButton(button);
      }
    };


    console.log('js: addOnLoad" (encode-string div-id) " setting handlers.');
    console.log('op links: ' + opLinks );
    relationItem.onclick = updateRelationExtent;
    updateRelationExtent();
    bindLinks(opLinks, operationLinkAction);
    console.log('P-links: ' + projectionLinks);
    bindLinks(projectionLinks, projectionLinkAction);
    var bName = '" (encode-string update-button-id) "';
    // updateButton = dijit.byId(bName);
    updateButton = new dijit.form.Button( { label: 'Update' }, bName);
    console.log('updateButton: (' + bName + ') ' + updateButton);
    bindButton(updateButton, updateAction);
    updateButton.attr('operation', 'Update');
    console.log('updateButton: ' + updateButton + ' op: ' + updateButton.attr('operation') + ' action: ' + updateButton.onClick);
    console.log('js: addOnLoad" (encode-string div-id) " complete.');
  });
  console.log('js: " (encode-string div-id) " defined.');")))


(defun /system/*/index.html (request stream &key system-name
                                     (system (or (find-system-prototype system-name)
                                                 (error 'http:document-not-found
                                                        :url request
                                                        :reason (format nil "System not found: ~s." system-name)))))
  "Generate an independent document for a project."

  (unless system-name (setf system-name (asdf:component-name system)))
  (let* ((*print-case* :downcase) (*print-pretty* t)
         (system-id (substitute #\- #\. system-name))
         (div-id (format nil "~a-document" system-id))
         (extent-textarea-id (format nil "~a-extent" system-id))
         (encoding-menu-id (format nil "~a-encoding" system-id))
         (projection-menu-id (format nil "~a-projection" system-id))
         (relation-menu-id (format nil "~a-relations" system-id))
         (update-button-id (format nil "update-~a-button" system-id)))
    (http:with-successful-response (stream :html)
      (xmlp:with-xml-writer (stream)
        (encode-xml-declaration)
        (encode-newline)
        (encode-document-type "xhtml" :public xqdm:+xhtml-public-identifier+
                              :system xqdm:+xhtml-system-identifier+)
        (xml ({xhtml}html ({xmlns}||  "http://www.w3.org/1999/xhtml"))
             ({xhtml}head
              (({}meta ({}name "date") ({}content  (iso-time))))
              ({xhtml}title */systems-title*)
              (({}meta ({}http-equiv "Content-Type")
                       ({xhtml}content "text/xhtml;charset=utf-8")))
              (encode-stylesheet-resources)
              (({}style ({}type "text/css")) "
                   html, body { width: 100%; height: 100%; margin: 0; }
                   .controls *[href]:hover { color: gray; border: solid black 1px; }
                   .controls *[href] { border: solid white 1px; }
                   .controls { background-color: #e0e0e0; }
                   .buttons { margin: 4px; width: 64px; position: absolute; right: 4px;}
                   .controls {
                     color: black; background-color: transparent; 
                   }
                   .controls .label {clear: left; float: left; margin-left: 2px; height: 3ex;}
                   .controls .inputText {vertical-align: bottom;
                     margin-bottom: 4px;// width: 190px; height: 120px;
                     text-align: justify;
                   }
                   .controls select {clear: right; float: right; }
                   .controls button {clear: right; float: right; }
                   .controls textarea {clear: both; width: 100%; }

                   // for report index panes
                   .filelist .file { margin-top: 10px; border-top: solid gray 1px; }
                   .filelist .file .name { font-weight: bold; }
                   .filelist .file .content { white-space: pre; margin-left: 48px; }
                   }")
              (encode-javascript-resources))
             (({xhtml}body ({}class "soria"))
              (encode-project-div request stream
                                  :system-name system-name
                                  :xhr-request-p nil)
              (({}script ({}type "text/javascript"))
               "console.log('" (encode-format "~a" div-id) " body loaded.');")
              ;; put the javascript in the body
              (encode-project-javascript request stream :system-name system-name
                                         :xhr-request-p nil
                                         :div-id  div-id :xhr-request-p nil
                                         :extent-textarea-id extent-textarea-id
                                         :encoding-menu-id encoding-menu-id
                                         :projection-menu-id projection-menu-id
                                         :relation-menu-id relation-menu-id
                                         :update-button-id update-button-id)))
        (encode-newline)))))


(defun /system/*/index.div (request stream &key system-name)
  "Generate a control div."

  (let ((*print-case* :downcase) (*print-pretty* t))
    (http:with-successful-response (stream :xml)
      (xmlp:with-xml-writer (stream)
        (encode-xml-declaration)
        (encode-newline)
        (encode-project-div request stream
                            :system-name system-name
                            :xhr-request-p (http::with-header-values (x-requested-with)
                                                                     (http::server-headers http::*server*)
                                             (string-equal x-requested-with "XMLHttpRequest")))))))


(defun encode-project-div (request stream &key system-name
                                   (system (or (find-system-prototype system-name)
                                               (error 'http:document-not-found
                                                      :url request
                                                      :reason (format nil "System not found: ~s." system-name))))
                                   (xhr-request-p t))
         
  "Generate a control div."
  
  (unless system-name (setf system-name (asdf:component-name system)))
  
  (let* ((system-id (substitute #\- #\. system-name))
         (metadata-pathname (definition-metadata-directory system))
         (runtime-binary-pathname #p"METADATA:bin;lisp;")
         (div-id (format nil "~a-controls" system-id))
         (extent-textarea-id (format nil "~a-extent" system-id))
         (encoding-menu-id (format nil "~a-encoding" system-id))
         (projection-menu-id (format nil "~a-projection" system-id))
         (relation-menu-id (format nil "~a-relations" system-id))
         (update-button-id (format nil "update-~a-button" system-id)))
    
    (xml ({xhtml}div ({xmlns}||  "http://www.w3.org/1999/xhtml")
                     ({xmlns}xlink  "http://www.w3.org/1999/xlink"))
         
         (({xhtml}div ({}id (list div-id "-documentContainer"))
                      ({}dojoType "dijit.layout.TabContainer")
                      ({}style "width: 100%; height: 100%;"))
          
          (({xhtml}div ({}id (list "update-" div-id)) ({}dojoType "dijit.layout.ContentPane")
                       ({}class " controls") ({}title "Update"))
           
           (({xhtml}div ({}class "button"))
            (({xhtml}button ({}id update-button-id) ({}name update-button-id)
                            ({}operation "Update")
                            ;; this failed for a dynamically loaded control pane, in the that
                            ;; widget was not yet defined when the onLoad function ran.
                            ;; workaround: defint the widget there programmatically.
                            ;; ({}dojoType "dijit.form.Button")
                            ({}type "button"))
             "Update"))
           (({xhtml}div ({}class "label")) "Repository URL")
           (({xhtml}textarea ({}name (list div-id "RepositoryURL")) ({}id (list div-id "RepositoryURL")) ({}value "")) "")
           (({xhtml}div ({}class "label")) "Repository type")
           (({xhtml}select ({}name "typeMenu") ({}id (list div-id "RepositoryType")))
            (({xhtml}option ({}value "DARCS")) "darcs")
            (({xhtml}option ({}value "GIT")) "git")
            (({xhtml}option ({}value "SVN")) "svn")))
          
          
          (({xhtml}div ({}name (list "ops-" div-id)) ({}id (list "ops-" div-id))
                       ({}dojoType "dijit.layout.ContentPane") ({}selected "true")
                       ({}class "controls")  ({}title "Ops"))
           (({xhtml}div ({}style "border-bottom: solid gray 1px; margin-bottom: 8px;"))
            (({xhtml}div ) "Build")
            (({xhtml}div ;;({}class "buttons runtime")
              ({}style " display: inline-block; padding-top: 12px;"))
             (({xhtml}span ({}style "font-size: x-small; float: right; height: 0px; position: relative; top: -12px; right: 4px;"))
              "with runtime")
             (let ((runtimes (directory (make-pathname :name :wild :type nil :defaults runtime-binary-pathname))))
               (if runtimes
                 (dolist (impl de.setf.utility.implementation::*build-implementations*)
                   (let ((id (format nil "buildButton~a~a" system-id impl)))
                     (xml ({xhtml}img ({}id id) ({}class "link build") ({}width "16") ({}height "16")
                                      ({}style "padding: 0px; margin: 0px; display: inline;")
                                      ({}src (format nil "/images/ico/~a.gif" impl))
                                      ({}disabled (unless (find impl runtimes :test #'string-equal :key #'pathname-name) "true"))
                                      ;; prime with a default reference
                                      ({}href (format nil "/system/~a/operation/compile-op/~a.image" system-name impl))
                                      ({}operation "compile-op")
                                      ({}tooltip impl) ({}alt impl) ({}title impl)))))
                 (encode-string "No runtimes available.")))))

           (({xhtml}div ({}style "border-bottom: solid gray 1px; margin-bottom: 8px;"))
            (({xhtml}div ) "Test")
            (({xhtml}div ;;({}class "buttons runtime")
              ({}style " display: inline-block; padding-top: 12px;"))
             (let ((images (directory (make-pathname :name :wild :type "image" :defaults metadata-pathname))))
               (if images
                 (xml ({xhtml}span ({}style "font-size: x-small; float: right; height: 0px; position: relative; top: -12px; right: 4px;"))
                      "with runtime")
                 (encode-string "no image available"))
               (when images
                 (dolist (impl de.setf.utility.implementation::*test-implementations*)
                   (when (find impl images :test #'string-equal :key #'pathname-name)
                     (let ((id (format nil "testButton~a~a" system-id impl)))
                       (xml ({xhtml}img ({}id id) ({}class "link test") ({}width "16") ({}height "16")
                                        ({}style "padding: 0px; margin: 0px; display: inline;")
                                        ({}src (format nil "/images/ico/~a.gif" impl))
                                        ;; prime with a default reference
                                        ({}href (format nil "/system/~a/operation/test-op/~a.image" system-name impl))
                                        ({}operation "test-op")
                                        ({}tooltip impl) ({}alt impl) ({}title impl)))))))))))
          
          (({xhtml}div ({}id (list "view-" div-id)) ({}dojoType "dijit.layout.ContentPane")
                       ({}class "controls") ({}title "View"))
           ;; (({xhtml}div ({}class "label")) "Views")
           (({xhtml}div ({}class "clear"))
            (({xhtml}div ({}class "label")) "Dimension")
            (({xhtml}select ({}name relation-menu-id) ({}id relation-menu-id))
             (({xhtml}option ({}value "systems") ({}selected "true")) "systems")
             (({xhtml}option ({}value "modules")) "modules")
             (({xhtml}option ({}value "files")) "files")
             (({xhtml}option ({}value "calls")) "calls")
             (({xhtml}option ({}value "packages")) "packages")
             (({xhtml}option ({}value "tests") ({}disabled "disabled")) "tests")))
           ({xhtml}div
            (({xhtml}div ({}class "label")) "Projection")
            (({xhtml}select ({}name projection-menu-id) ({}id projection-menu-id))
             (({xhtml}option ({}value "dot") ({}selected "true")) "dot")
             (({xhtml}option ({}value "twopi")) "twopi")
             (({xhtml}option ({}value "rdf")) "rdf")))
           ({xhtml}div
            (({xhtml}div ({}class "label")) "Encoding")
            (({xhtml}select ({}name encoding-menu-id) ({}id encoding-menu-id))
             (({xhtml}option ({}value "jpg")) "jpg")
             (({xhtml}option ({}value "svg") ({}selected "true")) "svg")
             (({xhtml}option ({}value "pdf")) "pdf")
             (({xhtml}option ({}value "n3")) "n3")))
           ({xhtml}div
            (({xhtml}div ({}class "label")) "Extent")
            (({xhtml}textarea ({}name extent-textarea-id) ({}id extent-textarea-id)
                              ({}value "") ({}class "inputText") ({}allow-returns "true")) ""))
           (({xhtml}div ;;({}class "buttons runtime")
              ({}style " display: inline-block; padding-top: 12px;"))
            (let ((images (directory (make-pathname :name :wild :type "image" :defaults metadata-pathname))))
              (if images
                 (xml ({xhtml}span ({}style "font-size: x-small; float: right; height: 0px; position: relative; top: -12px; right: 4px;"))
                      "with runtime")
                 (encode-string "no image available"))
              (when images
                (dolist (impl de.setf.utility.implementation::*graph-implementations*)
                  (when (find impl images :test #'string-equal :key #'pathname-name)
                    (let ((id (format nil "viewButton~a~a" system-id impl)))
                      (xml ({xhtml}img ({}id id) ({}class "link projection") ({}width "16") ({}height "16")
                                       ({}style "padding: 0px; margin: 0px; display: inline;")
                                       ({}src (format nil "/images/ico/~a.gif" impl))
                                       ;; prime with a default reference
                                       ({}href (format nil "/system/~a/projection/~a" system-name impl))
                                       ({}tooltip impl) ({}alt impl) ({}title impl)))))))))))

         (when xhr-request-p            ; put the javascript in the same element
           (encode-project-javascript request stream :system-name system-name
                                      :xhr-request-p  xhr-request-p
                                      :div-id  div-id :xhr-request-p nil
                                      :extent-textarea-id extent-textarea-id
                                      :encoding-menu-id encoding-menu-id
                                      :projection-menu-id projection-menu-id
                                      :relation-menu-id relation-menu-id
                                      :update-button-id update-button-id)))))





(defun /system/*/operation/ (request stream &key system-name operation
                                     (implementation de.setf.utility.implementation::*default.build-implementation*) (lisp-implementation implementation))
  "Run the build script - as generated for the project system and build runtime, save the results in
 the report tree, respond with a document which locates the script and maps the specific transcript to
 the generic transcript file."

  (setf operation (or (de.setf.utility.implementation::intern-system-operation operation)
                      (error 'http::bad-syntax-provided
                             :url request
                             :reason (format nil "invalid operation: ~s." operation))))
  (let ((system (or (find-system-prototype system-name)
                    (error 'http::document-not-found :reason (format nil "System not found: ~s." system-name)))))
    
    (multiple-value-bind (system return-code script transcript-file)
                         (perform-system-operation system operation :lisp-implementation lisp-implementation)
      (if (zerop return-code)
        (redirect-request request http::*server* transcript-file)
        (encode-system-operation-failure system request stream operation
                                         :return-code return-code
                                         :script script
                                         :transcript transcript-file)))))

(defun /system/*/new (request stream &key system-name url (repository-url url) type (repository-type type))
  (unless repository-url
    (error 'http::bad-syntax-provided :reason "A new system requires a repository url."))
  (unless (setf repository-type (de.setf.utility.implementation::intern-repository-type repository-type))
    (unless (setf repository-type (de.setf.utility.implementation::repository-type repository-url))
      (error 'http::bad-syntax-provided
             :url request
             :reason (format nil "invalid repository url: ~s." repository-url))))

  (if de.setf.utility.implementation::*permit-new-projects*
    (multiple-value-bind (system return-code script transcript-file)
                         (de.setf.utility.implementation::perform-system-new system-name
                                                                             :repository-url repository-url
                                                                             :repository-type repository-type)
      (if (zerop return-code)
        (redirect-request request http::*server* transcript-file)
        (encode-system-operation-failure system request stream :new
                                         :return-code return-code
                                         :script script
                                         :transcript transcript-file)))
    (error 'http::access-forbidden
           :url request
           :reason "New projects not permitted at this time.")))

(defun /system/*/update (request stream &key system-name url (repository-url url) type (repository-type type))
  (let ((system (or (find-system-prototype system-name)
                    (error 'http::document-not-found
                           :url request
                           :reason (format nil "System not found: ~s." system-name)))))
    (if repository-url
      (setf (system-repository-url system) repository-url)
      (setf repository-url (system-repository-url system)))
    (if repository-type
      (setf (system-repository-type system) (or (de.setf.utility.implementation::intern-repository-type repository-type)
                                                (error 'http::bad-syntax-provided
                                                       :reason (format nil "invalid repository url: ~s." repository-url))))
      (setf repository-type (system-repository-type system)))
    
    (if de.setf.utility.implementation::*permit-new-projects*
      (multiple-value-bind (system return-code script transcript-file)
                           (de.setf.utility.implementation::perform-system-update system
                                                                                  :repository-url repository-url
                                                                                  :repository-type repository-type)
        (if (zerop return-code)
          (redirect-request request http::*server* transcript-file)
          (encode-system-operation-failure system request stream :update
                                           :return-code return-code
                                           :script script
                                           :transcript transcript-file)))
      (error 'http::access-forbidden
           :url request
           :reason "Project updates not permitted at this time."))))


(defun /system/*/projection/ (request stream &key system-name (relation :module)
                                        (implementation de.setf.utility.implementation::*default.graph-implementation*)
                                        (lisp-implementation implementation)
                                        extent (projection :object) (encoding "text") (options '()))
  "Given a system, as designated by system-name, a relation, eg. modules or systems, a projection,
 eg. rdf or dot, and an encoding, eg. svg or n3, compute the relation graph, project it, and encoode
 it as specified. Any lisp-implementation argument must match a runtime for which a system image exists.
 An extent argument constrains the extent of the graph walk. Various combinations are
 - '(:module :object :text)' : a list of modules in the system
 - '(:system :object :text)' : a list of systems on which the system depends
 - '(:file :dot :svg)' : a system file graph encoded as '.dot' and rendered to '.svg'"

  (labels ((_mime-type ()
             (ignore-errors (mime:mime-type (http::mime-content-type-spec-for-pathname-type encoding)))))
    (let ((system (or (find-system-prototype system-name)
                    (error 'http::document-not-found
                           :url request
                           :reason (format nil "System not found: ~s." system-name))))
           (mime-type (or (_mime-type)
                         (error 'http::bad-syntax-provided
                                :url request
                                :reason (format nil "No encoding matches request: ~a." encoding)))))
      (unless (probe-file (make-pathname :name lisp-implementation :type "image"
                                         :defaults (definition-metadata-directory system)))
        (error 'http::document-not-found
               :url request
               :reason (format nil "Built image not found: ~s." system-name)))
      (setf projection (or (intern-request-projection projection)
                           :url request
                           (error 'http::bad-syntax-provided :reason (format nil "Invalid projection: ~s." projection))))
      (setf relation (or (intern-request-relation relation)
                           (error 'http::bad-syntax-provided
                                  :url request
                                  :reason (format nil "Invalid relation: ~s." relation))))
      
  
      (multiple-value-bind (system result script transcript-file)
                           (perform-system-projection system-name relation projection
                                                      :mime-type mime-type
                                                      :lisp-implementation lisp-implementation
                                                      :extent extent
                                                      :options options)
        (etypecase result
          (null                         ; nothing to say
           (http:with-successful-response (stream :text))
           (terpri stream))
          (pathname                     ; redirect to the generated document
           (typecase projection
             (rdf-projection
              (redirect-request request http::*server* result))
             (de.setf.utility.implementation::graphviz-projection
              ;; redirection is not sufficient, since the display context is in an existing document
              (let ((*print-case* :downcase) (*print-pretty* t)
                    (target (nth-value 1 (export-pathname result))))
                (http:with-successful-response (stream :html)
                  (xmlp:with-xml-writer (stream)
                    (encode-xml-declaration)
                    (encode-newline)
                    (encode-document-type "xhtml" :public xqdm:+xhtml-public-identifier+
                                          :system xqdm:+xhtml-system-identifier+)
                    (xml ({xhtml}html ({xmlns}||  "http://www.w3.org/1999/xhtml"))
                         ({xhtml}head )
                         ({xhtml}body
                          (({xhtml}object ({}style "border: inset;")
                                          ({}data target) ({}type "image/svg+xml") 
                                          ({}scrolling "yes") ({}frameborder "0")
                                          ({}width "99%") ({}height "99%"))
                           (encode-string (file-namestring result)))))))))
             (t
              (http:with-successful-response (stream :text)
                (format stream "~a~%" result)))))
                       
          (cons                         ; encode the result model as per the mime-type
           (encode-system-projection request stream system result projection mime-type))
          (integer                      ; script error code
           (encode-system-operation-failure system request stream `(,relation ,projection)
                                            :return-code result
                                            :script script
                                            :transcript transcript-file)))))))


(defun /reports/*/*/* (request stream &key day month year)
  (flet ((to-integer (x)
           (typecase x
             (string (parse-integer x :junk-allowed t))
             (integer x))))
    (unless (and (setf day (to-integer day))
                 (setf month (to-integer month))
                 (setf year (to-integer year)))
      (error 'http::bad-syntax-provided :reason (format nil "Invalid report date: ~a.~a.~a" day month year)))
    (setf day (format nil "~2,'0d" day)
          month (format nil "~2,'0d" month))
    (if (< year 2000) (setf year (+ year 1900)))
    (setf year (format nil "~4d" year))

    (let* ((directory-path (make-pathname :host "METADATA" :directory `(:absolute "reports" ,year ,month ,day)))
           (index-path (make-pathname :name "index" :type "html" :defaults directory-path)))
      (cond ((probe-file  directory-path)
            (unless (and (probe-file index-path) (>= (file-write-date index-path) (file-write-date directory-path)))
               (generate-report-index directory-path index-path))
             (redirect-request request http:*server* index-path))
            (t
             #+(or)                     ; reply as if it's ok
             (error 'http:document-not-found :url request :method :get
                    :reason (format nil "No reports on that date: ~a.~a.~a" day month year))
             (http:with-successful-response (stream :text)
               (format stream "No reports on that date: ~a.~a.~a" day month year)))))))


(defun /lisp/source/net/common-lisp/asdf/update (request stream)
  ;; mostly symbolic presence requirement
  (let ((system (or (find-system-prototype :asdf)
                    (error 'http::document-not-found
                           :url request
                           :reason (format nil "System not found: ~s." :asdf)))))
    (multiple-value-bind (system return-code script transcript-file)
                         (de.setf.utility.implementation::perform-asdf-update system)
      (if (zerop return-code)
        (redirect-request request http::*server* transcript-file)
        (encode-system-operation-failure system request stream :update
                                         :return-code return-code
                                         :script script
                                         :transcript transcript-file)))))

#|

(defun /system/*/graph/calls (request stream &key system-name implementation extent
                                    (projection "dot") (encoding "svg") (options '()))
  (declare (ignore request))
  (multiple-value-bind (system return-code graph-script graph-file transcript-file)
                       (graph-system-calls system-name implementation extent
                                           :projection projection
                                           :encoding encoding
                                           :options options)
    (if (zerop return-code)
      ;; encode the graph as an object element -- redirection fails, as the container just depicts the content as text
      (let ((graph-url-string (nth-value 1 (export-pathname graph-file))))
        (http:with-successful-response (stream :html)
          (let ((*print-case* :downcase) (*print-pretty* t))
            (xmlp:with-xml-writer (stream)
              (encode-xml-declaration)
              (encode-document-type "xhtml" :public xqdm:+xhtml-public-identifier+
                                    :system xqdm:+xhtml-system-identifier+)
              (xml ({xhtml}html ({xmlns}||  "http://www.w3.org/1999/xhtml")
                                ({xmlns}xlink  "http://www.w3.org/1999/xlink")) 
                   ({xhtml}head
                    (({}meta ({}name "date") ({}content  (iso-time))))
                    (({}meta ({}http-equiv "Content-Type")
                             ({xhtml}content "text/xhtml;charset=iso-8859-1"))))
                   
                   ({xhtml}body
                    (({xhtml}object ({}style "border: inset;")
                                    ({}data graph-url-string) ({}type "image/svg+xml") 
                                    ({}scrolling "no") ({}frameborder "0")
                                    ({}width "99%") ({}height "99%"))
                     (encode-format graph-url-string))))))))
      (encode-system-operation-results stream system "Graph Calls"
                                       :return-code return-code
                                       :aspects
                                       `(("Graph" , graph-file)
                                         ("Script" ,graph-script)
                                         ,@(when transcript-file
                                             `(("Transcript" ,transcript-file))))))))

(defmethod /system/*/graph/packages (request stream &key system-name implementation extent
                                    (projection "dot") (encoding "svg") (options '()))
  (declare (ignore request))
  (multiple-value-bind (system return-code graph-script graph-file transcript-file)
                       (graph-system-packages system-name implementation extent
                                           :projection projection
                                           :encoding encoding
                                           :options options)
    (if (zerop return-code)
      ;; encode the graph as an object element -- redirection fails, as the container just depicts the content as text
      (let ((graph-url-string (nth-value 1 (export-pathname graph-file))))
        (http:with-successful-response (stream :html)
          (let ((*print-case* :downcase) (*print-pretty* t))
            (with-xml-writer (stream)
              (encode-xml-declaration)
              (encode-document-type "xhtml" :public xqdm:+xhtml-public-identifier+
                                    :system xqdm:+xhtml-system-identifier+)
              (xml ({xhtml}html ({xmlns}||  "http://www.w3.org/1999/xhtml")
                                ({xmlns}xlink  "http://www.w3.org/1999/xlink")) 
                   ({xhtml}head
                    (({}meta ({}name "date") ({}content  (iso-time))))
                    (({}meta ({}http-equiv "Content-Type")
                             ({xhtml}content "text/xhtml;charset=iso-8859-1"))))
                   
                   ({xhtml}body
                    (({xhtml}object ({}style "border: inset;")
                                    ({}data graph-url-string) ({}type "image/svg+xml") 
                                    ({}scrolling "no") ({}frameborder "0")
                                    ({}width "99%") ({}height "99%"))
                     (encode-format graph-url-string))))))))
      (encode-system-operation-results stream system "Graph Calls"
                                       :return-code return-code
                                       :aspects
                                       `(("Graph" , graph-file)
                                         ("Script" ,graph-script)
                                         ,@(when transcript-file
                                             `(("Transcript" ,transcript-file))))))))

|#
