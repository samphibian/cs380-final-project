{-

Group
  name  String
  user  reference  --who owns the group (eg Sam's family is different than Tom's)
  users references --who is in the group (would be a problem if mult users with same name, but requiring a log in would fix that)

User
  name     String     --user's name (maybe split first/last but eh)
  email    String     --contact for the server
  contacts references --ways to contact the user

Contact
  type    String -- eg phone/email/mailing
  address String -- eg 123-456-7890/username@email.com
  estResp Float  -- estimated response time in hours

https://hbtvl.wordpress.com/2015/06/28/servant-persistent-and-dsls/

-}
