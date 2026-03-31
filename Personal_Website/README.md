# Personal Website

This is a clean starter template for a personal website built with plain HTML, CSS, and JavaScript.

## 1) Project structure

```
Personal_website/
  index.html
  styles.css
  script.js
  README.md
```

## 2) Run locally

From the `Personal_website` folder:

```bash
python3 -m http.server 5500
```

Open: [http://localhost:5500](http://localhost:5500)

## 3) Customize your content

- Update your name and intro in `index.html`
- Edit project cards in the Projects section
- Change colors and spacing in `styles.css`
- Add your social links in the Contact section

## 4) Deploy your site (simple options)

### Option A: GitHub Pages

1. Push this folder to a GitHub repo (for example: `personal-website`)
2. In GitHub, go to **Settings -> Pages**
3. Under **Build and deployment**, set:
   - **Source:** Deploy from a branch
   - **Branch:** `main` and `/ (root)`
4. Save and wait for the Pages URL

If your repo is named `username.github.io`, your site URL can be:
`https://username.github.io`

Otherwise it is usually:
`https://username.github.io/repo-name`

### Option B: Netlify (drag-and-drop easiest)

1. Go to [https://app.netlify.com/drop](https://app.netlify.com/drop)
2. Drag your `Personal_website` folder into the page
3. Netlify gives you a live URL immediately

### Option C: Vercel

1. Push code to GitHub
2. Import repo at [https://vercel.com/new](https://vercel.com/new)
3. Framework preset: **Other**
4. Deploy

## 5) Use your own custom domain

After deployment:

1. Buy a domain (Namecheap, Cloudflare, Google Domains, etc.)
2. In your hosting provider, add the custom domain (for example, `alexlopez.dev`)
3. In your domain DNS:
   - Add a **CNAME** for `www` pointing to the host target
   - Add **A records** for root domain (`@`) if your host requires them
4. Wait for DNS propagation (a few minutes to 24 hours)
5. Enable HTTPS in the hosting dashboard

## 6) Next upgrades

- Add a resume PDF download button
- Add a blog page
- Add a contact form with Formspree or Netlify Forms
- Add analytics (Plausible or Google Analytics)
