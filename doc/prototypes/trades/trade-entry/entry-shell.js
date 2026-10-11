'use strict';
/* Puts the trade-entry pages on the Trades group's shell: the area menu, the
 * breadcrumb, the pills and the prototype bar. The booking and pricing scripts
 * draw into .app, which moves under the shell's main element. */
(function () {
    const app = document.querySelector('.app');
    const pricing = /pricing\.html$/.test(location.pathname);
    const main = shell({
        title: pricing ? 'Price a structure' : 'Structure editor', name: 'trade-entry', noRoles: true,
        question: pricing ? 'How is a structure priced, as a whole or leg by leg?' : 'How are a structure and its components booked?',
        crumbs: [['Home', '#'], ['Trades', '../trade-page/'], ['Structures and packages', '../structure-page/'], [pricing ? 'Price a structure' : 'Structure editor']],
        states: [['structure editor', 'index.html', !pricing], ['price a structure', 'pricing.html', pricing]],
        says: pricing
            ? 'The dynamic half of the trade-entry work: market data ticks and the trader moves vols and rates. Structure mode prices the deal as one unit. Leg mode prices each trade on its own, which is how a package is priced.'
            : 'The first prototype of this work, now on the Trades shell. Strategy mode fixes the leg set and enforces the strategy invariant. Package mode offers every product type and enforces nothing structural. A component change bumps the structure version and authorisation does not.'
    });
    main.append(app);
})();
