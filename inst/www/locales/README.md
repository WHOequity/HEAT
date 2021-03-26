# Translations

> Currently, all code and files related to translations currently exist only on the master-nt-i18n branch. 

## Description

The **langs.csv** file demonstrates the new format for raw translations. The namespace, subject, and key columns must be left as is.
These columns are used by the application to find the appropriate translations.

If a new row needs to be added we (zrsa and who) need to coordinate to make sure all columns in the new row are filled in. WHO is
not expected to know or have the namespace, subject, or key. 

Important, any number of new language columns may be added, but the new column name must follow convention. 
Each language column name follows the pattern,

`<full language name> (<language abbreviation>)`

For example, "english (en)" or "french (fr)". The language abbreviation will appear as an option in the application's language selector 
dropdown menu and is used as the subfolder name under locales/. 

## Technical

The language folders and JSON files are created from the raw translations with the `bin/languages.R` script, found in the top-level project
folder.
