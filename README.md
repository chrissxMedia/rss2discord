# rss2discord

Poll RSS and Atom feeds every ten minutes and send new posts to Discord
webhooks.

Copy [feeds.example.csv](feeds.example.csv) to `feeds.csv` and edit it, with one
feed per line and no header:

```text
name,feed_url,webhook_url[,prefix]
```

The optional prefix goes before the title, separated by a space, and supports
mentions such as `@everyone` or `<@&ROLE_ID>`. Quote fields containing commas.
With Docker, edit `data/feeds.csv` after setup. Restart after edits.

Keep feed names unchanged to preserve the history in `/data/<name>.txt`. New
feeds skip existing posts and send only new ones to Discord.

```sh
mkdir -p data
cp -n feeds.example.csv data/feeds.csv
chmod 600 data/feeds.csv
docker run -v "$PWD/data:/data" chrissx/rss2discord:latest
```
