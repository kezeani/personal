# 'World Cup Guide' Design

## Website Design

### General Layout

The layout is constructed in 'layout.html'.

Every webpage has a maroon red background and a black navigation bar (navbar). I chose the red background because the two main colors of the 2022 World Cup are red and white. The black navbar contrasts the logo well and compliments the red.

I used Bootstrap to implement the navbar and imported the world cup logo from the internet.

The three navigation tabs on the left, teams, players, and predictor game, are constant. The two tabs on the right change depending on if the user is logged in or not. If the user is not logged in, the two links travel to register and login tabs. If logged in, the user sees a logout tab and their username which links to a dropdown menu of the user's bookmarked players and a page to change their password.

### Register/Login

The register and login pages consist of username and password forms using input boxes and a submit button. The register page has one username bar, a password bar and a confirm bar.

The change password page is identical to the register page with three bars, old password, new password, and a confirmation bar.

### Homepage

The homepage is constructed in 'index.html'.

The homepage is made up of three Bootstrap cards that link to the players, teams, and predictor pages. The buttons were also implemented using Bootstrap.

### Teams

The teams tab is constructed in 'teams.html'.

The groups are displayed using 8 Bootstrap tables of identical makeup. The nations and flags are hard coded in each table. In every table, the fifa ranking header is a link to view FIFA's official ranking.

### Players

The players tab is constructed in 'players.html'.

Initially, the players page displays one input form for name searching and 4 forms for various filters. After pressing the search button, the application runs a SQL query through the database worldcup.db which I put together manually. Worldcup.db contains the information of every single player at the World Cup as well as information about the nations. When the page is requested using the post method, all elected filters are put into the query and a table is printed that displays each row of the search. Every player's name is accompanied by a link to a google search of that player.

If the user is logged in, there is a bookmark button for each player in the rightmost cell of the row. In worldcup.db there is a bookmark table that records every bookmark that each user makes. When the user selects a player to bookmark, that player's information is entered into the bookmark table. Conversely, when the bookmark is removed, that row of data is removed from the table.

#### Bookmarks

The bookmarks tab is constructed in 'bookmarks.html'.

The bookmark tab can only be accessed if the user is logged in. On this page, there is a table that displays the user's bookmarked players in a similar layout to the players tab. To do this, the app runs a SQL query looking through the bookmarks table for all bookmarks made by a user with the current session id.

### Predictor Game

The Predictor Game utilizes 6 html pages that represent different stages of the tournament.

After clicking the predictor navigation item, the group stage, built in 'group.html' is displayed. Similar to groups, this page is made up of eight tables, one for each group. Along with each nation and their flag, each table is accompanied by two select forms where the user can make their selections. Following the submission of these forms, each team is stored in an array of teams which is used on the "Round of 16" page and the user is redirected to said page.

World Cup knockout matches are predetermined. For example, 1st place in Group A is guaranteed to play second place in Group B. So, the Round of 16 page, built in 'ro16.html', uses jinja placeholders and takes the information submitted from the previous page to display the appropriate teams for the round. From there, the page is similar, with each table containing a select for users to choose their winners. This pattern of variable storing and subsequent display continues for every stage until the final. After the user submits their winner, the information is used to display the winner page, built in 'winner.html'. On this page, the user sees their winner as well as a table containing every player on the team that they selected.

### Apology

The site apology messages are stored in 'apology.html'

If the user makes errors on the website, such as invalid password submission, or selecting the same team to come both first and second in the same group, the site displays an error message informing the user of their issue.