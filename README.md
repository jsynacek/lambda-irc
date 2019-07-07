# 𝛌irc

A friendly IRC bot, eager to give you fortune cookies.

## Installation

```
$ git clone https://github.com/jsynacek/lambda-irc.git
```
```
$ cd lambda-irc
$ stack install
```
The program is installed to `$HOME/.local/bin/` by default.

## Usage

Cookies are loaded from `$HOME/.fortunes`, one cookie per line. If that file cannot be read for some reason, several hard-coded cookies are provided by default.

```
$ lambda-irc
< parsed irc message stream incoming ... >
```

Afterwards, any `PRIVMSG` starting with the string `!fortune` sent directly to the bot will yield a fortune cookie.

## Command dispatch

Limited control of the bot is available via a unix socket at `/tmp/lambda-irc.sock`.

#### Recognized commands

These are the recognized commands. Their arguments are specified as described in [RFC1459](https://tools.ietf.org/html/rfc1459#section-2.3.1), see `<params>`.
- `/mesg`
- `/join`
- `/part`
- `/quit`

#### Example

The following is an example usage of the command dispatch using `nc`. The blank lines are not part of the session, they are added for clarity.
```
$ nc -U /tmp/lambda-irc.sock

Give me a cookie!
unknown command: Give me a cookie!

/join #haskell-beginners
<no output, bot joins the channel>

/mesg #haskell-beginners :Hi, I'm worth a fortune!
<no output, bot sends a message to the channel>

/part #haskell-beginners
<no output, bot leaves the channel>

/quit :I'm tired...
<no output, bot disconnects from the server>
```
