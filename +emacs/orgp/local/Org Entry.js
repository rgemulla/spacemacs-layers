{
    "translatorID":"a46325b6-2cd2-40ce-a60d-d6a2c1edea47",
    "translatorType":2,
    "label":"Org Entry",
    "creator":"Rainer Gemulla",
    "target":"org",
    "minVersion":"2.0",
    "maxVersion":"",
    "priority":200,
    "inRepository":false,
    "lastUpdated":"2021-10-19 15:13:00"
}

// Quickly create a top-level org-mode entries (with archive folder set to
// storage location of Zotero and a link to Zotero) from the Zotero application.
//
// Copy this file to the translators subdirectory in your Zotero data folder
// (e.g., "~/Zotero/translators"), restart Zotero and set "Org Entry" as default
// translator in Zotero's options. You can then use Ctrl+Shift+c to copy the
// selected item to the clipboard and subsequently paste a reference into an org
// file in Emacs.
//
// To be able to follow the created links in org-mode, use:
//
// (org-link-set-parameters
//  "zotero"
//  :follow (lambda (path) (browse-url (concat "zotero:" path))))

function doExport() {
    var item;
    while (item = Zotero.nextItem()) {
        Zotero.write("* ");
        Zotero.write(item.title);
        Zotero.write(" (");
        let creator = item.creators[0].lastName;
        if (item.creators.length == 2) {
            creator += " and " + item.creators[1].lastName;
        } else if (item.creators.length>2) {
            creator += " et al.";
        }
        Zotero.write(creator);
        Zotero.write(", ");
        var date = Zotero.Utilities.strToDate(item.date);
        Zotero.write(date.year);
        Zotero.write(")");
        if (item.attachments.length) {
            Zotero.write(" :ATTACH:\n");
            Zotero.write(":PROPERTIES:\n");
            Zotero.write(":DIR: ~/Zotero/storage/");
            folder = item.attachments[0].uri.split('/').pop()
            Zotero.write(folder);
            Zotero.write("\n");
            Zotero.write(":ATTACHMENTS:");
            for (i=0; i<item.attachments.length; i++) {
                // this assumes that all items are in the same folder (which
                // they are usually not, but we often have only one item)
                Zotero.write(" ");
                Zotero.write(encodeURI(item.attachments[i].filename));
            }
            Zotero.write("\n");
            Zotero.write(":END:");
        }
        Zotero.write("\n")
        Zotero.write("[[zotero://select/items/");
        var library_id = item.libraryID ? item.libraryID : 0;
        Zotero.write(library_id+"_"+item.key);
        Zotero.write("][Link to Zotero]]\n");
    }
}
