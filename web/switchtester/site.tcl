#!/bin/env wapptclsh

source env.tcl
source wing.tcl

# system reqs
source /usr/lib/tcl8/8.6/http-2.8.11.tm

package require wapp

sqlite3 db $::env(DATABASE_PATH)

proc whoisfront {} {
    return [::http::data [::http::geturl $::env(FRONT_TEXT_URL)]]
}

proc wapp-default {} {
    set B [wapp-param BASE_URL]
    wingcss

    wapp-trim {
        <section class="container">
        <h1>switchtester</h1>
        <p>You have stumbled across a web application related to tracking remote perception. In this case, there is a person, they have multiple beings in the same body. Your job (should you decide to participate in this completely voluntary study) is to simply tell which one of those multiple beings is in control of the body they inhabit. The goal of this is to prove to Maya that the spoop (tm) is a powerful force to be respected.</p>
        <ul>
        <li><a href="%html($B)/data">Submit data</a></li>
        <li><a href="%html($B)/observers">Register as an observer</a></li>
        <li><a href="%html($B)/results">Results</a></li>
        </ul>
        </section>
    }
}

proc errorpage {code message} {
    wapp-reply-code $code
    wapp-trim {
        <h1>error</h1>
        <p>%html($message)</p>
    }
}

proc wapp-page-results {} {
    set B [wapp-param BASE_URL]
    wingcss
    wapp-trim {
        <section class="container">
        <table>
        <tr>
        <td>ID</td>
        <td>Timestamp</td>
        <td>Guess</td>
        <td>Actual</td>
        <td>Observer</td>
        </tr>
    }
    db eval {SELECT
               mea.id
             , datetime(mea.ts, 'unixepoch') AS ts
             , guess.name AS gname
             , actual.name AS aname
             , observer.name AS oname
             FROM
               measurements AS mea
             INNER JOIN systemmates AS guess ON guess.rowid=mea.systemmate_id
             INNER JOIN systemmates AS actual ON actual.rowid=mea.actual_systemmate_id
             INNER JOIN observers AS observer ON observer.rowid=mea.observer_id
    } values {
        set rid $values(id)
        set ts $values(ts)
        set guess $values(gname)
        set actual $values(aname)
        set observer $values(oname)

        wapp-trim {
            <tr>
            <td>%html($rid)</rd>
            <td>%html($ts)</td>
            <td>%html($guess)</td>
            <td>%html($actual)</td>
            <td>%html($observer)</td>
            </tr>
        }
    }

    wapp-trim {
        </table>
        </section>
    }
}

proc wapp-page-observers-submit {} {
    wapp-allow-xorigin-params

    set B [wapp-param BASE_URL]
    set name [wapp-param display]
    set discord [wapp-param discord]
    set method [wapp-param REQUEST_METHOD]

    if {[string match {POST} $method] == 0} {
        # bad method
        errorpage 405 {use POST}
        return
    }

    db eval {BEGIN}
    db eval {INSERT INTO observers(name, discord_tag) VALUES ($name, $discord)}
    db eval {COMMIT}

    wingcss
    wapp-trim {
        <section class="container">
        <h1>Thank you for submitting this, %html($name)</h1>
        <p>If you need to be contacted over your submission, we will do so via Discord at %html($discord).</p>
        <a href="%html($B)/data">Click here to submit data</a>
        </section>
    }
}

proc wapp-page-observers {} {
    wingcss

    set B [wapp-param BASE_URL]

    wapp-trim {
        <section class="container">
        <form action="%html($B)/observers-submit" method="POST">
        Display name (public):<br />
        <input required type="text" name="display" value="" /><br />
        Discord tag (verification use only):<br />
        <input required type="text" name="discord" value="" /><br />
        <input type="submit" value="Submit" />
        </form>
        </section>
    }
}

proc wapp-page-data {} {
    wingcss

    set B [wapp-param BASE_URL]

    wapp-trim {
        <section class="container">
        <form action="%html($B)/data-submit" method="POST">
        Systemmate: <select name="systemmate">
    }

    db eval {SELECT rowid, name FROM systemmates} values {
        set id $values(rowid)
        set name $values(name)

        wapp-trim {
            <option value="%html($id)">%html($name)</option>
        }
    }

    wapp-trim {
        </select> <br />

        Observer (<a href="%html($B)/observers">click here</a> to add yourself): <select name="observer">
    }

    db eval {SELECT rowid, name FROM observers} values {
        set id $values(rowid)
        set name $values(name)

        wapp-trim {
            <option value="%html($id)">%html($name)</option>
        }
    }

    wapp-trim {
        </select>
        <input type="submit" value="Submit" />
        </form>
        </section>
    }
}

proc wapp-page-data-submit {} {
    wapp-allow-xorigin-params

    set method [wapp-param REQUEST_METHOD]
    if {[string match {POST} $method] == 0} {
        # bad method
        errorpage 405 {use POST}
        return
    }

    set B [wapp-param BASE_URL]
    set systemTime [clock seconds]
    set systemmate [wapp-param systemmate]
    set observer [wapp-param observer]
    set actual_front [whoisfront]

    db eval {BEGIN;}
    db eval {INSERT INTO
               measurements(ts, observer_id, systemmate_id, actual_systemmate_id)
             VALUES
               ($systemTime, $observer, $systemmate, $actual_front);
    }
    db eval {COMMIT;}

    wapp-reply-code 200
    wingcss
    wapp-trim {
        <section class="container">
        <h1>Thank you</h1>
        <a href='%html($B)/results'>See the results</a>

        <h2>Your score</h2>
        <table>
        <tr>
        <td>Your guess</td>
    }

    db eval {SELECT name FROM systemmates WHERE rowid=$systemmate} val {
        set name $val(name)

        wapp-trim {
            <td>%html($name)</td>
        }
    }

    wapp-trim {
        </tr>
        <tr>
        <td>Actual front</td>
    }

    db eval {SELECT name FROM systemmates WHERE rowid=$actual_front} val {
        set name $val(name)

        wapp-trim {
            <td>%html($name)</td>
        }
    }

    wapp-trim {
        </tr>
        </table>
        </section>
    }
}

wapp-start $::argv
