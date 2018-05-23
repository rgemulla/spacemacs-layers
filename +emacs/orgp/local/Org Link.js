// Quickly create org-mode links to Zotero from the Zotero application. Copy
// this file to the translators subdirectory in your Zotero data folder (e.g.,
// "~/Zotero/translators"), restart Zotero and set "Org Link" as default
// translator in Zotero's options. You can then use Ctrl+Shift+c to copy the
// selected item to the clipboard and subsequently paste a reference into an org
// file in Emacs.
//
// To be able to follow the created links in org-mode, use:
//
// (org-link-set-parameters
//  "zotero"
//  :follow (lambda (path) (browse-url (concat "zotero:" path))))

{
    "translatorID":"245d1ae0-5eb8-11e8-9c2d-fa7ae01bbebc",
    "translatorType":2,
    "label":"Org Link",
    "creator":"Rainer Gemulla",
    "target":"html",
    "minVersion":"2.0",
    "maxVersion":"",
    "priority":200,
    "inRepository":false,
    "lastUpdated":"2018-05-18 22:27:00"
}

function doExport() {
	  var item;
	  while (item = Zotero.nextItem()) {
		    Zotero.write("[[zotero://select/items/");
		    var library_id = item.libraryID ? item.libraryID : 0;
		    Zotero.write(library_id+"_"+item.key);
        Zotero.write("][");
		    let creator = item.creators[0].lastName;
        if (item.creators.length == 2) {
            creator += " and " + item.creators[1].lastName;
        } else if (item.creators.length>2) {
            creator += " et al.";
        }
        Zotero.write(creator);
        Zotero.write(" (");
        var date = Zotero.Utilities.strToDate(item.date)
        Zotero.write(date.year);
        Zotero.write("): ");
        Zotero.write(item.title);
        Zotero.write("]]");
    }
}
