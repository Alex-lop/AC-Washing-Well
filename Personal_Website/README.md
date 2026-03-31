# Personal Website

A simple static personal website built from scratch with HTML, CSS, and JavaScript.

## 1) Run locally

From this folder:

```bash
python3 -m http.server 5500
```

Then open:

`http://localhost:5500`

## 2) Customize content

Update:

- `index.html` for text, sections, and links
- `styles.css` for colors and layout
- `script.js` for small interactions

## 3) Host for free (GitHub Pages)

1. Create a new GitHub repo named `personal-website` (or any name).
2. Push this folder to that repo:

```bash
git init
git add .
git commit -m "Initial personal website"
git branch -M main
git remote add origin https://github.com/<your-username>/personal-website.git
git push -u origin main
```

3. In GitHub repo settings:
   - Go to **Pages**
   - Under **Build and deployment** choose:
     - Source: **Deploy from a branch**
     - Branch: **main**
     - Folder: **/(root)**

4. Wait 1-2 minutes and your site will be live at:
   - `https://<your-username>.github.io/personal-website/`

## 4) Optional custom domain

1. Buy domain from a registrar (Namecheap, Porkbun, Google Domains alternatives, etc.).
2. In GitHub Pages settings, add your domain.
3. Add DNS records at your registrar:
   - `A` records to GitHub Pages IPs
   - `CNAME` record for `www` pointing to `<your-username>.github.io`

GitHub will automatically handle HTTPS once DNS is set correctly.

## 5) Alternative hosting (even easier deploy UX)

- Netlify: drag-and-drop folder or connect GitHub repo
- Vercel: import GitHub repo and deploy

Both auto-redeploy when you push changes.
