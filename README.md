# rss2discord

Poll RSS and Atom feeds every ten minutes and send new posts to a Discord
webhook. Set `DISCORD_WEBHOOK_URL` to the webhook URL.

Put one feed per line in [feeds.csv](feeds.csv), name first and URL second,
without a header. With Docker, edit `data/feeds.csv` after setup. Restart after
edits.

Keep feed names unchanged to preserve the history in `/data/<name>.txt`. New
feeds skip existing posts and send only new ones to Discord.

```sh
mkdir -p data
cp -n feeds.csv data/
docker run -e DISCORD_WEBHOOK_URL=XXX \
  -v "$PWD/data:/data" chrissx/rss2discord:latest
```
