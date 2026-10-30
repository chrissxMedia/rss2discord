# rss2discord

Poll RSS and Atom feeds every ten minutes, send new posts to Discord webhooks,
and edit sent messages when their content changes.

Copy [feeds.example.csv](feeds.example.csv) to `feeds.csv` and edit it, with one
feed per line and no header:

```text
feed_url,webhook_url[,prefix]
```

The optional prefix goes before the title, separated by a space, and supports
mentions such as `@everyone` or `<@&ROLE_ID>`. Quote fields containing commas.
With Docker, edit `data/feeds.csv` after setup. Restart after edits.

Feed URLs must be unique. Keep `/data/history.sqlite3` to preserve history
across restarts. New feeds skip existing posts.

```sh
mkdir -p data
cp -n feeds.example.csv data/feeds.csv
chmod 600 data/feeds.csv
docker run --stop-timeout 70 -v "$PWD/data:/data" chrissx/rss2discord:latest
```

SIGTERM and SIGINT stop polling after the current item finishes and close the
database. The 70-second stop timeout lets a request finish its 60-second timeout
before Docker sends SIGKILL.
