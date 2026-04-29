# to use this file, save a copy as "passwords.R"
# and fill in your user name and password

pwds <- function() {
	list(
		LSMS = list(username="name@email.com", password="password"),
		## DRYAD "Client ID" and "Secret" see https://datadryad.org/account 
		DRYAD = list(username="xq4xsE4lO$_((kssSSS_wCdU", password="XmJaJC__PP1x-x3EAQZvvxA"),
		## DATAVERSE: name/email/institution are used for guestbook responses; 
		## token is the Dataverse API token (sent as X-Dataverse-key) for restricted files.
		DATAVERSE = list(name="my name", email="name@email.com", position="volunteer", institution="ZzZzZ", token=NULL)
	)
}



