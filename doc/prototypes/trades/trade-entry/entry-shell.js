'use strict';
/* Puts the trade-entry pages on the Trades group's shell: the area menu, the
 * breadcrumb, the pills and the prototype bar. The booking and pricing scripts
 * draw into .app, which moves under the shell's main element. */
(function () {
    const app = document.querySelector('.app');
    const pricing = /pricing\.html$/.test(location.pathname);
    const main = shell({
        title: pricing ? 'Price a structure' : 'Structure editor', name: 'trade-entry',
        question: pricing ? 'How is a structure priced, as a whole or leg by leg?' : 'How are a structure and its components booked?',
        crumbs: [['Home', '#'], ['Trades', '../trade-page/'], ['Structures and packages', '../structure-page/'], [pricing ? 'Price a structure' : 'Structure editor']],
        states: [['structure editor', 'index.html', !pricing], ['price a structure', 'pricing.html', pricing]],
        says: pricing
            ? 'The pricing screen reads a market data snapshot, and Refresh reads the next one. Nothing ticks by itself. A new trade goes to Book a trade, and the price of a structure goes back to the editor as a change.'
            : 'The structure editor edits a working copy. A change is not written until Save, and Save asks for the kind of change, the reason and the commentary. Try as reader to see the refused screen.'
    });
    main.append(app);
})();
