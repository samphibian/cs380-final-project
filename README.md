#final project

This project is the final project for CS380 - Modern Functional Programming as taught by Richard Eisenberg at Bryn Mawr College. It is designed to be a contact API.

In the finalProject.hs file, the server is set up and can be run by ghci finalProject.hs -> main. It can be killed using ctrl-c

API Endpoints

*note that listOf_ items are database entries and the database doesn't exist yet*

*all of the find_ functions and the definitions of User, Conflict, and Group are located in Models.hs*

*getUserConflict and the definitions of Schedule related items (excluding Conflict) are in ScheduleModel.hs*
* users
  * getUsers
    * Displays a list of Users
* users/:user_id
  * getUserById -> findUser :user_id listOfUsers
    * Displays the user with id :user_id
* users/:user_id/groups
  * getGroupsByUser -> findGroupsByUser :user_id listOfGroups
    * Displays all of the Groups of a particular user
* users/:user_id/:member_name
  * getGroupByMember -> findGroupByMember :member_id (findGroupsByUser :user_id listOfGroups)
    * Displays the Group that a particular member belongs to
* users/:user_id/:member_name/contacts
  * getContactsByMember -> getAllContactsByMember :member_id (findUser :user_id listOfUsers) (unsafePerformIO (getUserConflict schedules x))\*
    * Displays all of the contact methods and their estimated response time for a particular member

\* note that getUserConflict gets the Conflicts of a user based on the current date.. Since that information is necessary for some of the pure functions, I used unsafePerformIO rather than depurify the functions.

I had difficulties getting started due to problems accessing localhost via SSH-ing and due to having conflicting packages locally on the lab machines. After figuring out what the problems were, I spent a while setting up a VirtualBox machine that I could work from and install packages to without having to worry about previous packages.

Thanks to servant's tutorials and documentation, setting up the server itself was quite simple. However, I had problems having both /users and /groups api calls, so I ended up scrapping that and just making it so that groups can only be found in the context of users. I also spent a lot of time experimenting with different databases to try to add \users\new ..., \users\groups\new ..., and \users\:id\conflicts\new endpoints, but was unable to get it to work.

TODO:
  * add data manipulation/saving abilities by adding a database
  
cabal packages:
  * servant
  * servant-blaze
  * servant-cassava
  * servant-server
  * persistent
  * persistent-sqlite
  * persistent-template
  * aeson-compat
  * lucid
  * wai
