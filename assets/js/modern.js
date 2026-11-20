/* Shared website interactions: theme, publications, filtering and BibTeX copying. */
(async () => {
  const root = document.documentElement;
  const toggle = document.querySelector('.theme-toggle');
  let saved; try { saved = localStorage.getItem('academic-theme'); } catch (_) {}
  if (saved === 'dark' || saved === 'light') root.dataset.theme = saved;
  const current = () => root.dataset.theme || 'light';
  const update = () => {
    if (!toggle) return;
    const dark = current() === 'dark';
    toggle.textContent = dark ? '☀ Light' : '☾ Dark';
    toggle.setAttribute('aria-label', `Switch to ${dark ? 'light' : 'dark'} theme`);
  };
  if (toggle) {
    toggle.hidden = false;
    update();
    toggle.addEventListener('click', () => {
      root.dataset.theme = current() === 'dark' ? 'light' : 'dark';
      try { localStorage.setItem('academic-theme', root.dataset.theme); } catch (_) {}
      update();
    });
  }

// Load centralized publications before initializing controls

if (typeof loadPublications === 'function') {

    try {
        await loadPublications();

    } catch (error) {

        console.error(error);

        document.querySelectorAll('[data-publications]')
            .forEach(list => {
                const message = document.createElement('li');
                message.textContent =
                    'Publications could not be loaded.';
                list.replaceChildren(message);
            });

        return;
    }

}

  for (const button of document.querySelectorAll('[data-copy-bib]')) {
    button.hidden = false;
    button.addEventListener('click', async () => {
      const value = button.closest('details').querySelector('pre').textContent;
      try {
        await navigator.clipboard.writeText(value);
        button.textContent = 'Copied';
      } catch (_) {
        const range = document.createRange();
        range.selectNodeContents(button.closest('details').querySelector('pre'));
        const selection = window.getSelection();
        selection.removeAllRanges();
        selection.addRange(range);
        button.textContent = 'Select and copy';
      }
      setTimeout(() => { button.textContent = 'Copy BibTeX'; }, 1200);
    });
  }

  for (const details of document.querySelectorAll('.pub-actions details.inline-details')) {
    details.addEventListener('toggle', () => {
      if (!details.open) return;
      for (const sibling of details.parentElement.querySelectorAll('details.inline-details')) {
        if (sibling !== details) sibling.open = false;
      }
    });
  }

  for (const tools of document.querySelectorAll('.publication-tools')) {
    const section = tools.closest('section');
    const items = [...section.querySelectorAll('.publication')];
    const search = tools.querySelector('[data-pub-search]');
    const year = tools.querySelector('[data-pub-year]');
    const count = tools.querySelector('[data-pub-count]');
    const normalize = value => value.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase();
    const indexed = items.map(item => [item, normalize(item.querySelector('p').textContent)]);
    const years = [...new Set(items.map(item => item.dataset.year).filter(Boolean))].sort((a,b) => Number(b)-Number(a));
    years.forEach(value => year.add(new Option(value, value)));
    function filter() {
      const words = normalize(search.value.trim()).split(/\s+/).filter(Boolean);
      let visible = 0;
      for (const [item, text] of indexed) {
        const match = (!year.value || item.dataset.year === year.value) && words.every(word => text.includes(word));
        item.hidden = !match;
        if (match) visible++;
      }
      count.textContent = `${visible} of ${items.length} publications${visible ? '' : ' — no matching results'}`;
    }
    search.addEventListener('input', filter);
    year.addEventListener('change', filter);
    tools.querySelector('[data-pub-reset]').addEventListener('click', () => { search.value=''; year.value=''; filter(); search.focus(); });
    tools.hidden = false;
    filter();
  }
})();
