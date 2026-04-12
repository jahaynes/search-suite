/**
 * Simple component loader for shared HTML fragments.
 * Loads header, footer, and sidebar into placeholder elements.
 */
async function loadComponent(id, url) {
  const placeholder = document.getElementById(id);
  if (!placeholder) return;
  try {
    const resp = await fetch(url);
    if (!resp.ok) throw new Error(`Failed to load ${url}: ${resp.status}`);
    const html = await resp.text();
    placeholder.innerHTML = html;
    // Highlight current page in nav/sidebar after loading
    highlightCurrentLink(placeholder);
  } catch (err) {
    console.error(err);
    placeholder.innerHTML = `<p style="color:red">Error loading component: ${url}</p>`;
  }
}

function highlightCurrentLink(container) {
  const links = container.querySelectorAll('a');
  const current = window.location.pathname;
  links.forEach(link => {
    const href = link.getAttribute('href');
    if (href && current.endsWith(href)) {
      link.style.fontWeight = 'bold';
      link.setAttribute('aria-current', 'page');
    }
  });
}

function initComponents() {
  const base = document.querySelector('meta[name="component-base"]');
  const basePath = base ? base.content : 'components';

  const promises = [];

  const header = document.getElementById('header');
  if (header) promises.push(loadComponent('header', `${basePath}/header.html`));

  const footer = document.getElementById('footer');
  if (footer) promises.push(loadComponent('footer', `${basePath}/footer.html`));

  const sidebar = document.getElementById('sidebar');
  if (sidebar) {
    const src = sidebar.getAttribute('data-src');
    if (src) promises.push(loadComponent('sidebar', `${basePath}/${src}`));
  }

  return Promise.all(promises);
}

document.addEventListener('DOMContentLoaded', initComponents);