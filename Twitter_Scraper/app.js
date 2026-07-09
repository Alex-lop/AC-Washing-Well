const form = document.querySelector("#scrape-form");
const resultsBody = document.querySelector("#results-body");
const downloadCsvButton = document.querySelector("#download-csv");
const downloadJsonButton = document.querySelector("#download-json");
const tweetCount = document.querySelector("#tweet-count");
const likeCount = document.querySelector("#like-count");
const replyCount = document.querySelector("#reply-count");
const scrollSquigglePath = document.querySelector("#scroll-squiggle-progress");
const scrollSquiggleDot = document.querySelector("#scroll-squiggle-dot");

let currentResults = [];

const sampleTweets = [
  {
    username: "OpenAI",
    text: "Launching a new way to explore public conversation data with clearer filters and cleaner exports.",
    createdAt: "2026-06-18 13:42",
    likeCount: 128,
    replyCount: 14,
    retweetCount: 31,
    url: "https://x.com/OpenAI/status/123",
  },
  {
    username: "OpenAI",
    text: "Data previews should be easy to scan before you commit to a full download.",
    createdAt: "2026-06-18 12:10",
    likeCount: 89,
    replyCount: 7,
    retweetCount: 16,
    url: "https://x.com/OpenAI/status/124",
  },
  {
    username: "OpenAI",
    text: "CSV and JSON exports make it simple to move scraped records into analysis tools.",
    createdAt: "2026-06-17 16:05",
    likeCount: 204,
    replyCount: 22,
    retweetCount: 44,
    url: "https://x.com/OpenAI/status/125",
  },
];

function mixColor(startColor, endColor, amount) {
  const start = startColor.match(/\w\w/g).map((channel) => parseInt(channel, 16));
  const end = endColor.match(/\w\w/g).map((channel) => parseInt(channel, 16));
  const mixed = start.map((channel, index) => Math.round(channel + (end[index] - channel) * amount));

  return `#${mixed.map((channel) => channel.toString(16).padStart(2, "0")).join("")}`;
}

function getScrollProgress() {
  const scrollableHeight = document.documentElement.scrollHeight - window.innerHeight;

  if (scrollableHeight <= 0) {
    return 1;
  }

  return Math.min(window.scrollY / scrollableHeight, 1);
}

function updateScrollSquiggle() {
  if (!scrollSquigglePath || !scrollSquiggleDot) {
    return;
  }

  const pathLength = scrollSquigglePath.getTotalLength();
  const progress = getScrollProgress();
  const currentPoint = scrollSquigglePath.getPointAtLength(pathLength * progress);
  const activeColor = mixColor("8fd3ff", "c7a0ff", progress);

  scrollSquigglePath.style.strokeDasharray = pathLength;
  scrollSquigglePath.style.strokeDashoffset = pathLength * (1 - progress);
  scrollSquiggleDot.setAttribute("cx", currentPoint.x);
  scrollSquiggleDot.setAttribute("cy", currentPoint.y);
  scrollSquiggleDot.classList.toggle("is-visible", progress > 0.02);
  scrollSquiggleDot.classList.toggle("is-complete", progress >= 0.99);
  document.documentElement.style.setProperty("--scroll-color", activeColor);
}

function normalizeUsername(sourceValue) {
  if (sourceValue.includes("x.com/") || sourceValue.includes("twitter.com/")) {
    const pathParts = sourceValue.split("/").filter(Boolean);
    const usernameIndex = pathParts.findIndex((part) => part.includes("x.com") || part.includes("twitter.com")) + 1;
    return pathParts[usernameIndex] || "sample_user";
  }

  return sourceValue.trim().replace(/^@/, "") || "sample_user";
}

function buildPreviewRows(formData) {
  const username = normalizeUsername(formData.get("sourceValue"));
  const maxTweets = Number(formData.get("maxTweets")) || sampleTweets.length;
  const sourceType = formData.get("sourceType");
  const sourceValue = formData.get("sourceValue");

  return sampleTweets.slice(0, maxTweets).map((tweet, index) => ({
    ...tweet,
    username: sourceType === "search" ? `search_result_${index + 1}` : username,
    sourceType,
    sourceValue,
  }));
}

function renderResults(results) {
  resultsBody.replaceChildren();

  if (results.length === 0) {
    const row = document.createElement("tr");
    const cell = document.createElement("td");

    row.className = "empty-row";
    cell.colSpan = 7;
    cell.textContent = "No preview data yet.";
    row.append(cell);
    resultsBody.append(row);
    return;
  }

  results.forEach((tweet) => {
    const row = document.createElement("tr");
    const values = [
      `@${tweet.username}`,
      tweet.text,
      tweet.createdAt,
      tweet.likeCount,
      tweet.replyCount,
      tweet.retweetCount,
    ];

    values.forEach((value, index) => {
      const cell = document.createElement("td");

      if (index === 1) {
        cell.className = "tweet-text";
      }

      cell.textContent = value;
      row.append(cell);
    });

    const linkCell = document.createElement("td");
    const link = document.createElement("a");

    link.href = tweet.url;
    link.target = "_blank";
    link.rel = "noreferrer";
    link.textContent = "View";
    linkCell.append(link);
    row.append(linkCell);
    resultsBody.append(row);
  });
}

function updateSummary(results) {
  tweetCount.textContent = results.length;
  likeCount.textContent = results.reduce((total, tweet) => total + tweet.likeCount, 0);
  replyCount.textContent = results.reduce((total, tweet) => total + tweet.replyCount, 0);
}

function setDownloadState(enabled) {
  downloadCsvButton.disabled = !enabled;
  downloadJsonButton.disabled = !enabled;
}

function downloadFile(filename, content, type) {
  const blob = new Blob([content], { type });
  const url = URL.createObjectURL(blob);
  const link = document.createElement("a");

  link.href = url;
  link.download = filename;
  link.click();
  URL.revokeObjectURL(url);
}

function escapeCsvValue(value) {
  const stringValue = String(value ?? "");
  return `"${stringValue.replaceAll('"', '""')}"`;
}

function convertToCsv(results) {
  const headers = ["username", "text", "createdAt", "likeCount", "replyCount", "retweetCount", "url"];
  const rows = results.map((tweet) => headers.map((header) => escapeCsvValue(tweet[header])).join(","));

  return [headers.join(","), ...rows].join("\n");
}

form.addEventListener("submit", (event) => {
  event.preventDefault();

  currentResults = buildPreviewRows(new FormData(form));
  renderResults(currentResults);
  updateSummary(currentResults);
  setDownloadState(currentResults.length > 0);
});

form.addEventListener("reset", () => {
  currentResults = [];
  renderResults(currentResults);
  updateSummary(currentResults);
  setDownloadState(false);
});

downloadCsvButton.addEventListener("click", () => {
  downloadFile("tweet-results.csv", convertToCsv(currentResults), "text/csv");
});

downloadJsonButton.addEventListener("click", () => {
  downloadFile("tweet-results.json", JSON.stringify(currentResults, null, 2), "application/json");
});

window.addEventListener("scroll", updateScrollSquiggle, { passive: true });
window.addEventListener("resize", updateScrollSquiggle);
updateScrollSquiggle();
