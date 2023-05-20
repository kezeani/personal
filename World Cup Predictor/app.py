import os
import cs50

from cs50 import SQL
from flask import Flask, redirect, render_template, request, session
from flask_session import Session
from tempfile import mkdtemp
from werkzeug.security import check_password_hash, generate_password_hash
from functools import wraps

app = Flask(__name__)

db = SQL("sqlite:///worldcup.db")

app.config["SESSION_PERMANENT"] = False
app.config["SESSION_TYPE"] = "filesystem"
Session(app)

#Apology function from CS50 finance PSET
def apology(message, code=400):
    """Render message as an apology to user."""
    def escape(s):
        """
        Escape special characters.

        https://github.com/jacebrowning/memegen#special-characters
        """
        for old, new in [("-", "--"), (" ", "-"), ("_", "__"), ("?", "~q"),
                         ("%", "~p"), ("#", "~h"), ("/", "~s"), ("\"", "''")]:
            s = s.replace(old, new)
        return s
    return render_template("apology.html", top=code, bottom=escape(message)), code

#Login required function from CS50 finance PSET
def login_required(f):
    @wraps(f)
    def decorated_function(*args, **kwargs):
        if session.get("user_id") is None:
            return redirect("/login")
        return f(*args, **kwargs)
    return decorated_function

@app.route("/changepassword", methods=["GET", "POST"])
@login_required
def changepassword():
    if request.method == "POST":
        # Variables
        oldpassword = request.form.get("oldpassword")
        newpassword = request.form.get("newpassword")
        confirmation = request.form.get("confirmation")

        # Ensure fields are filled and valid
        if not oldpassword:
            return apology("Please Enter Old Password", 403)

        elif not newpassword:
            return apology("Please Enter New Password", 403)

        elif not confirmation:
            return apology("Please Confirm New Password", 403)

        elif (confirmation != newpassword):
            return apology("Confirmation does not match", 403)

        # Check if oldpassword is valid
        realhash = db.execute("SELECT * FROM users WHERE id = ?", session["user_id"])
        if not check_password_hash(realhash[0]["hash"], oldpassword):
            return apology("Incorrect Password", 403)
        else:
            newhash = generate_password_hash(newpassword)
            db.execute("UPDATE users SET hash = ? WHERE id = ?", newhash, session["user_id"])

            return redirect("/login")
    else:
        return render_template("password.html")




@app.route('/')
def home():
    return render_template("index.html")

@app.route('/bookmarks', methods=["GET", "POST"])
@login_required
def bookmarks():
    #Connects all bookmarks made by the active user
    bookmarks = db.execute("SELECT * FROM bookmarks WHERE user_id = ?", session["user_id"])
    for row in bookmarks:
        row["link"] = "https://www.google.com/search?q=" + row["name"]
        row["nation"] = db.execute("SELECT name FROM nations WHERE id = (SELECT nation_id FROM players WHERE id = ?)", row["player_id"])[0]["name"]

    #Finds the identity of every bookmarked player and puts their IDs in an array
    ids = db.execute("SELECT player_id FROM bookmarks WHERE user_id = ?", session["user_id"])
    list = []
    for row in ids:
        list.append(row["player_id"])

    #Collects the ID of the player to be bookmarked or unbookmarked
    bookmark = request.form.get("book")
    unbookmark = request.form.get("unbook")

    #If user tries to bookmark player
    if bookmark:
        ids = db.execute("SELECT player_id FROM bookmarks WHERE user_id = ?", session["user_id"])
        list = []
        for row in ids:
            list.append(row["player_id"])

        #Check if player is already bookmarked to avoid duplication
        check = db.execute("SELECT player_id FROM bookmarks WHERE user_id = ? AND player_id = ?", session["user_id"], bookmark)
        if check:
            return render_template("bookmarks.html", stat=0, bookmarks=bookmarks, list=list)

        #Collecct each variable to insert into bookmarks table
        bookname = db.execute("SELECT name FROM players WHERE id = ?", bookmark)[0]["name"]
        booknation = db.execute("SELECT name FROM nations WHERE id IN (SELECT nation_id FROM players WHERE id = ?)", bookmark)[0]["name"]
        bookposition = db.execute("SELECT position FROM players WHERE id = ?", bookmark)[0]["position"]
        bookage = db.execute("SELECT age FROM players WHERE id = ?", bookmark)[0]["age"]
        db.execute("INSERT INTO bookmarks (user_id, player_id, name, nation, position, age) VALUES(?, ?, ?, ?, ?, ?)",
                    session["user_id"], bookmark, bookname, booknation, bookposition, bookage)

        bookmarks = db.execute("SELECT * FROM bookmarks WHERE user_id = ?", session["user_id"])
        ids = db.execute("SELECT player_id FROM bookmarks WHERE user_id = ?", session["user_id"])
        list = []
        for row in ids:
            list.append(row["player_id"])

        return render_template("bookmarks.html", bookmarks=bookmarks, list=list)

    #If user wants to remove bookmark
    if unbookmark:
        db.execute("DELETE FROM bookmarks WHERE player_id = ?", unbookmark)
        ids = db.execute("SELECT player_id FROM bookmarks WHERE user_id = ?", session["user_id"])
        list = []
        for row in ids:
            list.append(row["player_id"])
        return render_template("bookmarks.html", bookmarks=bookmarks, list=list)

    return render_template("bookmarks.html", bookmarks=bookmarks, list=list)

@app.route("/login", methods=["GET", "POST"])
def login():
    """Log user in"""

    # Forget any user_id
    session.clear()

    # User reached route via POST (as by submitting a form via POST)
    if request.method == "POST":

        # Ensure username was submitted
        if not request.form.get("username"):
            return apology("must provide username", 403)

        # Ensure password was submitted
        elif not request.form.get("password"):
            return apology("must provide password", 403)

        # Query database for username
        rows = db.execute("SELECT * FROM users WHERE username = ?", request.form.get("username"))

        # Ensure username exists and password is correct
        if len(rows) != 1 or not check_password_hash(rows[0]["hash"], request.form.get("password")):
            return apology("invalid username and/or password", 403)

        # Remember which user has logged in
        session["user_id"] = rows[0]["id"]
        session["username"] = rows[0]["username"]

        # Redirect user to home page
        return redirect("/")

    # User reached route via GET (as by clicking a link or via redirect)
    else:
        return render_template("login.html")


@app.route('/group', methods=["GET", "POST"])
def group():
    return render_template("group.html")


@app.route("/logout")
def logout():
    """Log user out"""

    # Forget any user_id
    session.clear()


    # Redirect user to login form
    return redirect("/")

@app.route('/players', methods=["GET", "POST"])
def players():
    #collects nations and players to put in forms
    nations = db.execute("SELECT name FROM nations ORDER BY name")
    positions = db.execute("SELECT DISTINCT position FROM players")

    if request.method == "POST":
        # Insert id of every team if not specified
        if request.form.get("nation") == "Any Nation":
           nation_id = [x for x in range(1, 33)]
        else:
            nation_id = [db.execute("SELECT id FROM nations WHERE name = ?", request.form.get("nation"))[0]["id"]]

        #If name not specified enter % to search for all names
        if not request.form.get("name"):
            name = "%"
        else:
            name = "%" + request.form.get("name") + "%"

        #Variables to display in forms
        plainname = request.form.get("name")
        original = request.form.get("position")
        minage = request.form.get("minage")
        maxage = request.form.get("maxage")
        nation = request.form.get("nation")

        if not minage:
            minage = 0

        if not maxage:
            maxage = 50

        if original == "Any Position":
            position = ["GK", "DF", "MF", "FW"]
        else:
            position = original

        #Find all players who fit criteria
        search = db.execute("SELECT * FROM players WHERE name LIKE ? AND nation_id IN (?) AND position IN (?) AND age >= ? AND age <= ? ORDER BY name", name, nation_id, position, minage, maxage)
        for row in search:
                    row["link"] = "https://www.google.com/search?q=" + row["name"]
                    row["nation"] = db.execute("SELECT name FROM nations WHERE id = (SELECT nation_id FROM players WHERE id = ?)", row["id"])[0]["name"]

        #If the user is logged in bookmark code is used
        try:
            bookmark = request.form.get("book")
            unbookmark = request.form.get("unbook")
            ids = db.execute("SELECT player_id FROM bookmarks WHERE user_id = ?", session["user_id"])
            list = []
            for row in ids:
                list.append(row["player_id"])

            if bookmark:
                if bookmark in list:
                    return render_template("players.html", stat=0, search=search, nations=nations, positions=positions, nation=nation, position=original, minage=minage, maxage=maxage, name=name, plainname=plainname, list=list)

                bookname = db.execute("SELECT name FROM players WHERE id = ?", bookmark)[0]["name"]
                booknation = db.execute("SELECT name FROM nations WHERE id IN (SELECT nation_id FROM players WHERE id = ?)", bookmark)[0]["name"]
                bookposition = db.execute("SELECT position FROM players WHERE id = ?", bookmark)[0]["position"]
                bookage = db.execute("SELECT age FROM players WHERE id = ?", bookmark)[0]["age"]
                db.execute("INSERT INTO bookmarks (user_id, player_id, name, nation, position, age) VALUES(?, ?, ?, ?, ?, ?)",
                            session["user_id"], bookmark, bookname, booknation, bookposition, bookage)
                return redirect("/bookmarks")

            if unbookmark:
                db.execute("DELETE FROM bookmarks WHERE player_id = ?", unbookmark)
                ids = db.execute("SELECT player_id FROM bookmarks WHERE user_id = ?", session["user_id"])
                list = []
                for row in ids:
                    list.append(row["player_id"])
                return render_template("bookmarks.html", bookmarks=bookmarks, list=list)
        except:
                list = []
                return render_template("players.html", stat=0, search=search, nations=nations, positions=positions, nation=nation, position=original, minage=minage, maxage=maxage, name=name, plainname=plainname, list=list)

        return render_template("players.html", stat=0, search=search, nations=nations, positions=positions, nation=nation, position=original, minage=minage, maxage=maxage, name=name, plainname=plainname, list=list)
    else:
        return render_template("players.html", nations=nations, positions=positions, position="Any Position", nation="Any Nation", stat=1)


@app.route("/register", methods=["GET", "POST"])
def register():
    """Register user"""
    if request.method == "POST":
        # Variables
        username = request.form.get("username")
        password = request.form.get("password")
        confirmation = request.form.get("confirmation")

        # Ensure all fields are filled and valid
        if not username:
            return apology("must provide username", 400)

        elif not password:
            return apology("must provide password", 400)

        elif not confirmation:
            return apology("must provide confirmation", 400)

        elif (confirmation != password):
            return apology("Confirmation does not match", 400)

        users = db.execute("SELECT username FROM users WHERE username = ?", username)
        if (users):
            return apology("username already exists", 400)

        # Generate hash
        hashpassword = generate_password_hash(password)

        db.execute("INSERT INTO users (username, hash) VALUES(?, ?)", username, hashpassword)

        return redirect("/login")

    else:
        return render_template("register.html")

@app.route('/ro16', methods=["POST"])
def ro16():
    #Team Variables
    teams = []

    #If form not submitted or duplicate names, return apology
    for x in range(1, 17):
        if not request.form.get(str(x)):
            return apology("Invalid Team Selection", 400)
        if request.form.get(str(x)) in teams:
            return apology("Invalid Team Selection", 400)

        #Add team to array for future use
        teams.append(db.execute("SELECT name, image FROM nations WHERE name = ?", request.form.get(str(x)))[0])

    return render_template("ro16.html", a1=teams[0], a2=teams[1], b1=teams[2], b2=teams[3], c1=teams[4], c2=teams[5], d1=teams[6], d2=teams[7], e1=teams[8], e2=teams[9], f1=teams[10], f2=teams[11], g1=teams[12], g2=teams[13], h1=teams[14], h2=teams[15])

@app.route('/quarters', methods=["POST"])
def quarters():
    teams = []

    #If form not submitted or duplicate names, return apology
    for x in range(1, 9):
        if not request.form.get(str(x)):
            return apology("Invalid Team Selection", 400)
        if request.form.get(str(x)) in teams:
            return apology("Invalid Team Selection", 400)

        #Add team to array for future use
        teams.append(db.execute("SELECT name, image FROM nations WHERE name = ?", request.form.get(str(x)))[0])

    return render_template("quarters.html", a=teams[0], b=teams[1], c=teams[2], d=teams[3], e=teams[4], f=teams[5], g=teams[6], h=teams[7])

@app.route('/semis', methods=["POST"])
def semis():
    teams = []

    #If form not submitted or duplicate names, return apology
    for x in range(1, 5):
        if not request.form.get(str(x)):
            return apology("Invalid Team Selection", 400)
        if request.form.get(str(x)) in teams:
            return apology("Invalid Team Selection", 400)

        #Add team to array for future use
        teams.append(db.execute("SELECT name, image FROM nations WHERE name = ?", request.form.get(str(x)))[0])

    return render_template("semis.html", a=teams[0], b=teams[1], c=teams[2], d=teams[3])

@app.route('/final', methods=["POST"])
def final():
    teams = []

    #If form not submitted or duplicate names, return apology
    for x in range(1, 3):
        if not request.form.get(str(x)):
            return apology("Invalid Team Selection", 400)
        if request.form.get(str(x)) in teams:
            return apology("Invalid Team Selection", 400)

        #Add team to array for future use
        teams.append(db.execute("SELECT name, image FROM nations WHERE name = ?", request.form.get(str(x)))[0])

    return render_template("final.html", a=teams[0], b=teams[1])

@app.route('/winner', methods=["POST"])
def winner():

    #Display selected winner
    teams = []
    teams.append(db.execute("SELECT name, image FROM nations WHERE name = ?", request.form.get("1"))[0])

    #Display players on the selected winnern
    players = db.execute("SELECT * FROM players WHERE nation_id IN (SELECT id FROM nations WHERE name = ?)", request.form.get("1"))
    for row in players:
        row["link"] = "https://www.google.com/search?q=" + row["name"]
        row["nation"] = db.execute("SELECT name FROM nations WHERE id = (SELECT nation_id FROM players WHERE id = ?)", row["id"])[0]["name"]


    return render_template("winner.html", a=teams[0], players=players)

@app.route('/teams', methods=["GET"])
def teams():
    return render_template("teams.html")