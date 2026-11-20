
async function loadPublications() {

    const response = await fetch('assets/data/publications.json');

    if (!response.ok) {
        throw new Error('Unable to load publications');
    }

    const publications = await response.json();

    document.querySelectorAll('[data-publications]').forEach(list => {

        const website = list.dataset.publications;
        const category = list.dataset.category;

        const selected = publications
            .filter(p => p[website] === true)
            .filter(p => !category || p.category === category)
            .sort((a, b) =>
                b.year - a.year );

        list.replaceChildren();

        selected.forEach(paper => {
            list.appendChild(createPublication(paper));
        });

    });

}

function createPublication(paper) {

    const li = document.createElement('li');

    li.className = 'publication';
    li.dataset.year = paper.year;

    const citation = document.createElement('p');

    citation.textContent = paper.citation ||
        `${paper.authors.join(', ')}. ${paper.title}. ` +
        `${paper.venue}. ${paper.year}.`;

    li.appendChild(citation);

    const actions = document.createElement('div');
    actions.className = 'pub-actions';

    // Paper button

    if (paper.paper) {
        actions.appendChild(
            createPublicationLink('Paper', paper.paper)
        );
    }

    // BibTeX button

    if (paper.bibtex) {

        const details = document.createElement('details');
        details.className = 'inline-details';

        const summary = document.createElement('summary');
        summary.textContent = 'BibTeX';

        const pre = document.createElement('pre');
        pre.textContent = paper.bibtex;

        const copy = document.createElement('button');
        copy.type = 'button';
        copy.setAttribute('data-copy-bib', '');
        copy.hidden = true;
        copy.textContent = 'Copy BibTeX';

        details.append(summary, pre, copy);

        actions.appendChild(details);
    }

    // Abstract button

    if (paper.abstract || paper.abstract_url) {

        const details = document.createElement('details');
        details.className = 'inline-details abstract-details';

        const summary = document.createElement('summary');
        summary.textContent = 'Abstract';

        const panel = document.createElement('div');
        panel.className = 'details-panel';

        if (paper.abstract) {

            const paragraph = document.createElement('p');
            paragraph.textContent = paper.abstract;
            panel.appendChild(paragraph);

        } else {

            panel.appendChild(
                createPublicationLink(
                    'Read abstract',
                    paper.abstract_url
                )
            );

        }

        details.append(summary, panel);
        actions.appendChild(details);
    }

    // Source code button

    if (paper.code) {
        actions.appendChild(
            createPublicationLink('Code', paper.code)
        );
    }

    // Forthcoming papers

    if (paper.status === 'forthcoming') {

        const status = document.createElement('span');
        status.className = 'pending-link';
        status.textContent = 'Forthcoming';

        actions.appendChild(status);
    }

    li.appendChild(actions);

    return li;
}

function createPublicationLink(label, url) {

    const link = document.createElement('a');

    link.textContent = label;
    link.href = url;
    link.target = '_blank';
    link.rel = 'noopener noreferrer';

    return link;
}
