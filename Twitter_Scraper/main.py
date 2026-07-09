"""
Entry point: scrape tweets then run the three-layer sentiment pipeline.

Usage
-----
    python main.py <username> [--max-tweets N] [--no-dl] [--csv PATH]

Examples
--------
    python main.py elonmusk
    python main.py NASA --max-tweets 50
    python main.py openai --no-dl            # skip DistilBERT (faster)
    python main.py openai --csv results.csv  # save full DataFrame to CSV
"""

from __future__ import annotations

import argparse
import json
import sys

from scraper import scrape_profile_tweets
from sentiment_analysis import analyze_tweets, summarize


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Scrape tweets and run sentiment analysis."
    )
    parser.add_argument("username", help="Twitter/X username (with or without @)")
    parser.add_argument(
        "--max-tweets",
        type=int,
        default=25,
        metavar="N",
        help="Maximum number of tweets to collect (default: 25)",
    )
    parser.add_argument(
        "--no-dl",
        action="store_true",
        help="Skip the DistilBERT deep-learning layer (faster, uses only TextBlob + VADER)",
    )
    parser.add_argument(
        "--csv",
        metavar="PATH",
        default=None,
        help="If provided, save the full results DataFrame to this CSV file",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()

    print(f"[1/3] Scraping tweets for @{args.username.lstrip('@')} …")
    tweets = scrape_profile_tweets(args.username, max_tweets=args.max_tweets)

    if not tweets:
        print("No tweets found. The profile may be private or the username is wrong.")
        sys.exit(1)

    print(f"      Collected {len(tweets)} tweet(s).")

    dl_msg = "TextBlob + VADER only (--no-dl)" if args.no_dl else "TextBlob + VADER + DistilBERT"
    print(f"[2/3] Running sentiment analysis ({dl_msg}) …")
    df = analyze_tweets(tweets, use_dl=not args.no_dl)

    print("[3/3] Results\n")

    # Print a compact per-tweet table to stdout
    display_cols = ["username", "final_label", "final_confidence",
                    "vader_compound", "textblob_polarity", "text"]
    available = [c for c in display_cols if c in df.columns]
    print(df[available].to_string(index=False, max_colwidth=60))

    print("\n── Summary ────────────────────────────────────────────")
    summary = summarize(df)
    print(json.dumps(summary, indent=2))

    if args.csv:
        df.to_csv(args.csv, index=False)
        print(f"\nFull results saved to: {args.csv}")


if __name__ == "__main__":
    main()
