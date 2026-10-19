/* Shared script of the convention management prototypes.
 *
 * Self-contained: plain JavaScript, mock data, no framework, nothing that
 * outlives the page. Each prototype folder carries an identical copy, so it
 * opens on its own, and the prototype inventory check keeps the copies equal.
 *
 * It holds the mock convention documents, the interface parts the prototypes
 * share (shell, rail, select, table, pager, dialog) and the icons, which are
 * the Fluent paths of projects/ores.web/packages/web/src/assets/icons. */

(function (root) {
    'use strict';

    var M = {};

    var ICONS = {"add": "M10 2.5C10.2761 2.5 10.5 2.72386 10.5 3V9.5H17C17.2761 9.5 17.5 9.72386 17.5 10C17.5 10.2761 17.2761 10.5 17 10.5H10.5V17C10.5 17.2761 10.2761 17.5 10 17.5C9.72386 17.5 9.5 17.2761 9.5 17V10.5H3C2.72386 10.5 2.5 10.2761 2.5 10C2.5 9.72386 2.72386 9.5 3 9.5H9.5V3C9.5 2.72386 9.72386 2.5 10 2.5Z", "edit": "M17.1813 2.92689C16.029 1.71505 14.1046 1.69077 12.9222 2.87317L3.54735 12.2475C3.21952 12.5754 2.99198 12.9899 2.89142 13.4424L2.0138 17.3923C1.97672 17.5592 2.02748 17.7335 2.14838 17.8544C2.26928 17.9753 2.44356 18.026 2.61044 17.9889L6.53683 17.1157C7.00426 17.0118 7.43237 16.7767 7.77096 16.4381L17.129 7.08003C18.27 5.939 18.2932 4.09631 17.1813 2.92689ZM13.6293 3.58029C14.4142 2.79538 15.6917 2.8115 16.4566 3.61596C17.1947 4.39225 17.1793 5.61548 16.4218 6.37293L15.7506 7.04418L12.958 4.25155L13.6293 3.58029ZM12.2508 4.95864L15.0435 7.7513L7.06385 15.731C6.8597 15.9352 6.60158 16.0769 6.31975 16.1396L3.16044 16.8421L3.86762 13.6593C3.92692 13.3924 4.0611 13.148 4.25444 12.9547L12.2508 4.95864Z", "delete": "M8.5 4H11.5C11.5 3.17157 10.8284 2.5 10 2.5C9.17157 2.5 8.5 3.17157 8.5 4ZM7.5 4C7.5 2.61929 8.61929 1.5 10 1.5C11.3807 1.5 12.5 2.61929 12.5 4H17.5C17.7761 4 18 4.22386 18 4.5C18 4.77614 17.7761 5 17.5 5H16.4456L15.2521 15.3439C15.0774 16.8576 13.7957 18 12.2719 18H7.72813C6.20431 18 4.92256 16.8576 4.7479 15.3439L3.55437 5H2.5C2.22386 5 2 4.77614 2 4.5C2 4.22386 2.22386 4 2.5 4H7.5ZM5.74131 15.2292C5.85775 16.2384 6.71225 17 7.72813 17H12.2719C13.2878 17 14.1422 16.2384 14.2587 15.2292L15.439 5H4.56101L5.74131 15.2292ZM8.5 7.5C8.77614 7.5 9 7.72386 9 8V14C9 14.2761 8.77614 14.5 8.5 14.5C8.22386 14.5 8 14.2761 8 14V8C8 7.72386 8.22386 7.5 8.5 7.5ZM12 8C12 7.72386 11.7761 7.5 11.5 7.5C11.2239 7.5 11 7.72386 11 8V14C11 14.2761 11.2239 14.5 11.5 14.5C11.7761 14.5 12 14.2761 12 14V8Z", "history": "M10 4C13.3137 4 16 6.68629 16 10C16 13.3137 13.3137 16 10 16C6.68629 16 4 13.3137 4 10C4 9.84443 4.00591 9.69034 4.0175 9.53793C4.03845 9.26258 3.83222 9.02239 3.55687 9.00144C3.28152 8.98049 3.04133 9.18673 3.02038 9.46207C3.00687 9.6397 3 9.8191 3 10C3 13.866 6.13401 17 10 17C13.866 17 17 13.866 17 10C17 6.13401 13.866 3 10 3C8.04094 3 6.27012 3.80499 5 5.10109V3.5C5 3.22386 4.77614 3 4.5 3C4.22386 3 4 3.22386 4 3.5V6.5C4 6.77614 4.22386 7 4.5 7H7.5C7.77614 7 8 6.77614 8 6.5C8 6.22386 7.77614 6 7.5 6H5.52772C6.62683 4.77191 8.2234 4 10 4ZM10 6.5C10 6.22386 9.77614 6 9.5 6C9.22386 6 9 6.22386 9 6.5V10.5C9 10.7761 9.22386 11 9.5 11H12.5C12.7761 11 13 10.7761 13 10.5C13 10.2239 12.7761 10 12.5 10H10V6.5Z", "arrow_clockwise": "M4 10C4 6.68629 6.68629 4 10 4C11.7766 4 13.3732 4.77191 14.4723 6H12.5C12.2239 6 12 6.22386 12 6.5C12 6.77614 12.2239 7 12.5 7H15.5C15.7761 7 16 6.77614 16 6.5V3.5C16 3.22386 15.7761 3 15.5 3C15.2239 3 15 3.22386 15 3.5V5.10109C13.7299 3.80499 11.9591 3 10 3C6.13401 3 3 6.13401 3 10C3 13.866 6.13401 17 10 17C13.866 17 17 13.866 17 10C17 9.8191 16.9931 9.6397 16.9796 9.46207C16.9587 9.18673 16.7185 8.98049 16.4431 9.00144C16.1678 9.02239 15.9615 9.26258 15.9825 9.53793C15.9941 9.69034 16 9.84443 16 10C16 13.3137 13.3137 16 10 16C6.68629 16 4 13.3137 4 10Z", "save": "M3 5C3 3.89543 3.89543 3 5 3H13.3787C13.9091 3 14.4178 3.21071 14.7929 3.58579L16.4142 5.20711C16.7893 5.58218 17 6.09089 17 6.62132V15C17 16.1046 16.1046 17 15 17H5C3.89543 17 3 16.1046 3 15V5ZM5 4C4.44772 4 4 4.44772 4 5V15C4 15.5523 4.44772 16 5 16L5 11.5C5 10.6716 5.67157 10 6.5 10H13.5C14.3284 10 15 10.6716 15 11.5V16C15.5523 16 16 15.5523 16 15V6.62132C16 6.3561 15.8946 6.10175 15.7071 5.91421L14.0858 4.29289C13.8983 4.10536 13.6439 4 13.3787 4L13 4V6.5C13 7.32843 12.3284 8 11.5 8L7.5 8C6.67157 8 6 7.32843 6 6.5L6 4H5ZM7 4L7 6.5C7 6.77614 7.22386 7 7.5 7L11.5 7C11.7761 7 12 6.77614 12 6.5V4L7 4ZM14 16V11.5C14 11.2239 13.7761 11 13.5 11H6.5C6.22386 11 6 11.2239 6 11.5V16H14Z", "dismiss": "M4.08859 4.21569L4.14645 4.14645C4.32001 3.97288 4.58944 3.9536 4.78431 4.08859L4.85355 4.14645L10 9.293L15.1464 4.14645C15.32 3.97288 15.5894 3.9536 15.7843 4.08859L15.8536 4.14645C16.0271 4.32001 16.0464 4.58944 15.9114 4.78431L15.8536 4.85355L10.707 10L15.8536 15.1464C16.0271 15.32 16.0464 15.5894 15.9114 15.7843L15.8536 15.8536C15.68 16.0271 15.4106 16.0464 15.2157 15.9114L15.1464 15.8536L10 10.707L4.85355 15.8536C4.67999 16.0271 4.41056 16.0464 4.21569 15.9114L4.14645 15.8536C3.97288 15.68 3.9536 15.4106 4.08859 15.2157L4.14645 15.1464L9.293 10L4.14645 4.85355C3.97288 4.67999 3.9536 4.41056 4.08859 4.21569L4.14645 4.14645L4.08859 4.21569Z", "search": "M13.7291 14.4362C12.5924 15.411 11.115 16 9.5 16C5.91015 16 3 13.0899 3 9.5C3 5.91015 5.91015 3 9.5 3C13.0899 3 16 5.91015 16 9.5C16 11.115 15.411 12.5924 14.4361 13.7292L17.8535 17.1465C18.0487 17.3417 18.0487 17.6583 17.8535 17.8536C17.6799 18.0271 17.4105 18.0464 17.2156 17.9114L17.1464 17.8536L13.7291 14.4362ZM13.0196 13.7266C13.276 13.5128 13.5127 13.2761 13.7265 13.0197C14.5216 12.0659 15 10.8388 15 9.5C15 6.46243 12.5376 4 9.5 4C6.46243 4 4 6.46243 4 9.5C4 12.5376 6.46243 15 9.5 15C10.8388 15 12.0658 14.5217 13.0196 13.7266Z", "filter": "M7.5 13H12.5C12.7761 13 13 13.2239 13 13.5C13 13.7455 12.8231 13.9496 12.5899 13.9919L12.5 14H7.5C7.22386 14 7 13.7761 7 13.5C7 13.2545 7.17688 13.0504 7.41012 13.0081L7.5 13H12.5H7.5ZM5.5 9H14.5C14.7761 9 15 9.22386 15 9.5C15 9.74546 14.8231 9.94961 14.5899 9.99194L14.5 10H5.5C5.22386 10 5 9.77614 5 9.5C5 9.25454 5.17688 9.05039 5.41012 9.00806L5.5 9H14.5H5.5ZM3.5 5H16.5C16.7761 5 17 5.22386 17 5.5C17 5.74546 16.8231 5.94961 16.5899 5.99194L16.5 6H3.5C3.22386 6 3 5.77614 3 5.5C3 5.25454 3.17688 5.05039 3.41012 5.00806L3.5 5H16.5H3.5Z", "lock_closed": "M10 13C10.5523 13 11 12.5523 11 12C11 11.4477 10.5523 11 10 11C9.44771 11 9 11.4477 9 12C9 12.5523 9.44771 13 10 13ZM6 6H7V5C7 3.34315 8.34315 2 10 2C11.6569 2 13 3.34315 13 5V6H14C15.6569 6 17 7.34315 17 9V15C17 16.6569 15.6569 18 14 18H6C4.34315 18 3 16.6569 3 15V9C3 7.34315 4.34315 6 6 6ZM10 3C8.89543 3 8 3.89543 8 5V6H12V5C12 3.89543 11.1046 3 10 3ZM16 9C16 7.89543 15.1046 7 14 7H6C4.89543 7 4 7.89543 4 9V15C4 16.1046 4.89543 17 6 17H14C15.1046 17 16 16.1046 16 15V9Z", "checkmark_circle": "M10 2C14.4183 2 18 5.58172 18 10C18 14.4183 14.4183 18 10 18C5.58172 18 2 14.4183 2 10C2 5.58172 5.58172 2 10 2ZM10 3C6.13401 3 3 6.13401 3 10C3 13.866 6.13401 17 10 17C13.866 17 17 13.866 17 10C17 6.13401 13.866 3 10 3ZM13.3584 7.64645C13.532 7.82001 13.5513 8.08944 13.4163 8.28431L13.3584 8.35355L9.35355 12.3584C9.17999 12.532 8.91056 12.5513 8.71569 12.4163L8.64645 12.3584L6.64645 10.3584C6.45118 10.1632 6.45118 9.84658 6.64645 9.65131C6.82001 9.47775 7.08944 9.45846 7.28431 9.59346L7.35355 9.65131L9 11.298L12.6513 7.64645C12.8466 7.45118 13.1632 7.45118 13.3584 7.64645Z", "arrow_rotate_counterclockwise": "M17 10C17 6.13401 13.866 3 10 3C6.13401 3 3 6.13401 3 10C3 12.7916 4.63419 15.2026 7 16.3265V14.5C7 14.2239 7.22386 14 7.5 14C7.77614 14 8 14.2239 8 14.5V17.5C8 17.7761 7.77614 18 7.5 18H4.5C4.22386 18 4 17.7761 4 17.5C4 17.2239 4.22386 17 4.5 17H6.12422C3.66539 15.6357 2 13.0128 2 10C2 5.58172 5.58172 2 10 2C14.4183 2 18 5.58172 18 10C18 10.2761 17.7761 10.5 17.5 10.5C17.2239 10.5 17 10.2761 17 10ZM10 12C8.89543 12 8 11.1046 8 10C8 8.89543 8.89543 8 10 8C11.1046 8 12 8.89543 12 10C12 11.1046 11.1046 12 10 12ZM10 11C10.5523 11 11 10.5523 11 10C11 9.44772 10.5523 9 10 9C9.44772 9 9 9.44772 9 10C9 10.5523 9.44772 11 10 11Z", "info": "M10.4921 8.91012C10.4497 8.67687 10.2456 8.49999 10.0001 8.49999C9.72397 8.49999 9.50011 8.72385 9.50011 8.99999V13.5021L9.50817 13.592C9.55051 13.8253 9.75465 14.0021 10.0001 14.0021C10.2763 14.0021 10.5001 13.7783 10.5001 13.5021V8.99999L10.4921 8.91012ZM10.7988 6.74999C10.7988 6.33578 10.463 5.99999 10.0488 5.99999C9.63461 5.99999 9.29883 6.33578 9.29883 6.74999C9.29883 7.16421 9.63461 7.49999 10.0488 7.49999C10.463 7.49999 10.7988 7.16421 10.7988 6.74999ZM18 10C18 5.58172 14.4183 2 10 2C5.58172 2 2 5.58172 2 10C2 14.4183 5.58172 18 10 18C14.4183 18 18 14.4183 18 10ZM3 10C3 6.13401 6.13401 3 10 3C13.866 3 17 6.13401 17 10C17 13.866 13.866 17 10 17C6.13401 17 3 13.866 3 10Z", "warning": "M9.56195 3.26181C9.75109 2.91271 10.2521 2.91273 10.4412 3.26186L16.9418 15.2628C17.1222 15.5959 16.881 16.0009 16.5021 16.0009H3.49942C3.12051 16.0009 2.8793 15.5959 3.0598 15.2627L9.56195 3.26181ZM11.3205 2.78557C10.7532 1.73821 9.25014 1.73813 8.68271 2.78544L2.18056 14.7864C1.63905 15.7858 2.3627 17.0009 3.49942 17.0009H16.5021C17.6388 17.0009 18.3624 15.786 17.821 14.7865L11.3205 2.78557ZM10.5 7.50023C10.5 7.22409 10.2761 7.00023 9.99996 7.00023C9.72382 7.00023 9.49996 7.22409 9.49996 7.50023V11.5002C9.49996 11.7764 9.72382 12.0002 9.99996 12.0002C10.2761 12.0002 10.5 11.7764 10.5 11.5002V7.50023ZM10.75 13.7502C10.75 14.1644 10.4142 14.5002 9.99996 14.5002C9.58575 14.5002 9.24996 14.1644 9.24996 13.7502C9.24996 13.336 9.58575 13.0002 9.99996 13.0002C10.4142 13.0002 10.75 13.336 10.75 13.7502Z"};

    /* ---------------------------------------------------------------- */
    /* Small helpers                                                      */
    /* ---------------------------------------------------------------- */

    M.esc = function (s) {
        return String(s === null || s === undefined ? '' : s)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;')
            .replace(/>/g, '&gt;').replace(/"/g, '&quot;');
    };

    M.icon = function (name) {
        return '<svg class="ico" viewBox="0 0 20 20" aria-hidden="true"><path d="' +
            (ICONS[name] || '') + '"/></svg>';
    };

    M.params = function () { return new URLSearchParams(root.location.search); };

    /* The demo clock: every relative time reads against it. */
    M.NOW = Date.parse('2026-10-19T12:00:00Z');

    M.relTime = function (iso) {
        var t = Date.parse(iso);
        var s = Math.max(0, Math.round((M.NOW - t) / 1000));
        var out;
        function unit(n, w) { return n + ' ' + w + (n === 1 ? '' : 's') + ' ago'; }
        if (s < 3600) { out = unit(Math.max(1, Math.round(s / 60)), 'minute'); }
        else if (s < 86400) { out = unit(Math.round(s / 3600), 'hour'); }
        else if (s < 86400 * 60) { out = unit(Math.round(s / 86400), 'day'); }
        else { out = unit(Math.round(s / (86400 * 30)), 'month'); }
        return '<time datetime="' + M.esc(iso) + '" title="' +
            M.esc(iso.replace('T', ' ').replace('Z', ' UTC')) + '">' + out + '</time>';
    };

    M.dash = function (v) {
        return v === '' || v === null || v === undefined
            ? '<span class="muted-dash">—</span>' : M.esc(v);
    };

    M.badge = function (kind, text) {
        return '<span class="badge ' + kind + '">' + M.esc(text) + '</span>';
    };

    M.btn = function (act, label, opts) {
        opts = opts || {};
        return '<button type="button" class="btn ' + (opts.cls || '') + '" data-act="' + act + '"' +
            (opts.arg !== undefined ? ' data-arg="' + M.esc(opts.arg) + '"' : '') +
            (opts.disabled ? ' disabled' : '') + '>' +
            (opts.icon ? M.icon(opts.icon) : '') + M.esc(label) + '</button>';
    };

    M.accessMark = function (editable) {
        return '<span class="access" title="' + (editable
            ? 'You can change this.' : 'You can read this but not change it.') + '">' +
            M.icon(editable ? 'edit' : 'lock_closed') + (editable ? 'Editable' : 'Read only') + '</span>';
    };

    M.notice = function (kind, html) { return '<div class="notice ' + kind + '">' + html + '</div>'; };

    /* ---------------------------------------------------------------- */
    /* The shell, the breadcrumb and the page header                      */
    /* ---------------------------------------------------------------- */

    M.AREAS = ['Home', 'Reference data', 'Organisation', 'Market data', 'Trades', 'Requests', 'Operations'];

    M.shell = function (area, person) {
        return '<header class="shell-head"><span class="brand">ORE Studio</span><nav>' +
            M.AREAS.map(function (a) {
                return '<a href="#" onclick="return false"' + (a === area ? ' class="on"' : '') + '>' + a + '</a>';
            }).join('') + '</nav><span class="who"><b>' + M.esc(person.name) + '</b> · ' +
            M.esc(person.part) + ' · ACME</span></header>';
    };

    M.crumbs = function (items) {
        return '<div class="crumbs">' + items.map(function (c, i) {
            var last = i === items.length - 1;
            return (c.act && !last ? '<a href="#" data-act="' + c.act + '">' + M.esc(c.t) + '</a>' : M.esc(c.t)) +
                (last ? '' : '<span class="sep">›</span>');
        }).join('') + '</div>';
    };

    M.pagehead = function (title, lead, actions) {
        return '<div class="pagehead"><div><h1>' + M.esc(title) + '</h1>' +
            (lead ? '<p class="lead">' + lead + '</p>' : '') + '</div><div class="actions">' +
            (actions || '') + '</div></div>';
    };

    /* ---------------------------------------------------------------- */
    /* The journey rail                                                   */
    /* ---------------------------------------------------------------- */

    M.rail = function (steps, current) {
        return '<nav class="railnav" aria-label="Steps"><ol>' + steps.map(function (s, i) {
            var cls = i < current ? 'done' : (i === current ? 'current' : 'ahead');
            return '<li><button type="button" class="railentry ' + cls + '" data-act="goto" data-arg="' + i + '"' +
                (i > current ? ' disabled' : '') + (i === current ? ' aria-current="step"' : '') + '>' +
                '<span class="railmark ' + cls + '">' + (i < current ? '✓' : (i + 1)) + '</span>' +
                M.esc(s) + '</button></li>';
        }).join('') + '</ol></nav>';
    };

    /* ---------------------------------------------------------------- */
    /* A custom select, since a native one cannot be styled              */
    /* ---------------------------------------------------------------- */

    M.select = function (id, options, value, opts) {
        opts = opts || {};
        var cur = options.filter(function (o) { return o.v === value; })[0];
        return '<div class="sel' + (opts.inline ? ' inline' : '') + '" data-sel="' + M.esc(id) + '">' +
            '<button type="button" class="sel-trigger" aria-haspopup="listbox" aria-expanded="false">' +
            '<span>' + (cur ? M.esc(cur.t) : '<span class="faint">' + M.esc(opts.placeholder || 'Choose…') + '</span>') +
            '</span><span class="chev">▾</span></button>' +
            '<div class="sel-menu" role="listbox" hidden>' + options.map(function (o) {
                return '<button type="button" role="option" data-v="' + M.esc(o.v) + '"' +
                    (o.v === value ? ' class="on"' : '') + '>' + M.esc(o.t) + '</button>';
            }).join('') + '</div></div>';
    };

    /* Wire every select under root; onPick(id, value) is called on a choice. */
    M.wireSelects = function (rootEl, onPick) {
        rootEl.querySelectorAll('.sel').forEach(function (sel) {
            var trig = sel.querySelector('.sel-trigger');
            var menu = sel.querySelector('.sel-menu');
            trig.addEventListener('click', function (e) {
                e.stopPropagation();
                var open = menu.hidden;
                document.querySelectorAll('.sel-menu').forEach(function (m) { m.hidden = true; });
                menu.hidden = !open;
                trig.setAttribute('aria-expanded', String(open));
            });
            menu.addEventListener('click', function (e) {
                var b = e.target.closest('button[data-v]');
                if (!b) { return; }
                menu.hidden = true;
                onPick(sel.getAttribute('data-sel'), b.getAttribute('data-v'));
            });
        });
    };
    document.addEventListener('click', function () {
        document.querySelectorAll('.sel-menu').forEach(function (m) { m.hidden = true; });
    });

    /* ---------------------------------------------------------------- */
    /* Table and pager                                                    */
    /* ---------------------------------------------------------------- */

    M.PAGE_SIZES = [15, 25, 50, 100];

    /* cols: [{id, h, cls, cell(row)}]. sort: {id, dir} or null. */
    M.table = function (cols, rows, o) {
        o = o || {};
        return '<table class="rows' + (o.refreshing ? ' refreshing' : '') + '"><thead><tr>' +
            cols.map(function (c) {
                var on = o.sort && o.sort.id === c.id;
                return '<th class="' + (c.cls || '') + '" aria-sort="' + (on ? (o.sort.dir > 0 ? 'ascending' : 'descending') : 'none') + '">' +
                    (o.plain ? M.esc(c.h) : '<button type="button" class="btn ghost small" data-act="sort" data-arg="' + c.id + '">' +
                    M.esc(c.h) + (on ? (o.sort.dir > 0 ? ' ↑' : ' ↓') : '') + '</button>') + '</th>';
            }).join('') + '</tr></thead><tbody>' +
            rows.map(function (r, i) {
                return '<tr class="' + (o.plain ? '' : 'pick') + (o.isOn && o.isOn(r) ? ' on' : '') + '"' + (o.plain ? '' : ' tabindex="0"') + ' data-row="' + i + '">' +
                    cols.map(function (c) { return '<td class="' + (c.cls || '') + '">' + c.cell(r) + '</td>'; }).join('') + '</tr>';
            }).join('') + '</tbody></table>';
    };

    M.sortRows = function (rows, cols, sort) {
        if (!sort) { return rows.slice(); }
        var c = cols.filter(function (x) { return x.id === sort.id; })[0];
        var val = c.val || function (r) { return r[c.id]; };
        return rows.slice().sort(function (a, b) {
            var x = val(a), y = val(b);
            return (x < y ? -1 : (x > y ? 1 : 0)) * sort.dir;
        });
    };

    M.pager = function (st, total) {
        var pages = Math.max(1, Math.ceil(total / st.size));
        var from = total === 0 ? 0 : st.page * st.size + 1;
        var to = Math.min(total, (st.page + 1) * st.size);
        return '<div class="pager"><span class="count">' + from + '–' + to + ' of ' + total + '</span>' +
            M.btn('pg', 'First', { cls: 'small ghost', arg: 'first', disabled: st.page === 0 }) +
            M.btn('pg', 'Previous', { cls: 'small ghost', arg: 'prev', disabled: st.page === 0 }) +
            M.btn('pg', 'Next', { cls: 'small ghost', arg: 'next', disabled: st.page >= pages - 1 }) +
            M.btn('pg', 'Last', { cls: 'small ghost', arg: 'last', disabled: st.page >= pages - 1 }) +
            '<span>Page size</span>' + M.select('pagesize', M.PAGE_SIZES.map(function (n) {
                return { v: String(n), t: String(n) };
            }), String(st.size), { inline: true }) + '</div>';
    };

    M.pageStep = function (st, how, total) {
        var last = Math.max(0, Math.ceil(total / st.size) - 1);
        if (how === 'first') { st.page = 0; }
        if (how === 'prev') { st.page = Math.max(0, st.page - 1); }
        if (how === 'next') { st.page = Math.min(last, st.page + 1); }
        if (how === 'last') { st.page = last; }
    };

    /* ---------------------------------------------------------------- */
    /* Dialogs                                                            */
    /* ---------------------------------------------------------------- */

    M.dialog = function (html, wide) {
        M.closeDialog();
        var s = document.createElement('div');
        s.className = 'scrim';
        s.id = 'dialog-scrim';
        s.innerHTML = '<div class="dialog' + (wide ? ' wide' : '') + '" role="dialog" aria-modal="true">' + html + '</div>';
        document.body.appendChild(s);
        var first = s.querySelector('input, textarea, .sel-trigger, button');
        if (first) { first.focus(); }
        return s;
    };
    M.closeDialog = function () {
        var s = document.getElementById('dialog-scrim');
        if (s) { s.remove(); }
    };
    document.addEventListener('keydown', function (e) {
        if (e.key === 'Escape') { M.closeDialog(); }
    });

    /* ---------------------------------------------------------------- */
    /* The people the prototypes act as                                   */
    /* ---------------------------------------------------------------- */

    M.PEOPLE = {
        'm.okafor': { id: 'm.okafor', name: 'M. Okafor', part: 'Operations', decides: [] },
        'j.ruiz': { id: 'j.ruiz', name: 'J. Ruiz', part: 'Market Risk', decides: ['market_risk'] },
        't.brandt': { id: 't.brandt', name: 'T. Brandt', part: 'Market Risk', decides: ['market_risk'] },
        'k.mori': { id: 'k.mori', name: 'K. Mori', part: 'Trading desk', decides: [] }
    };

    M.CHANGE_REASONS = {
        amend: [
            { v: 'common.rectification', t: 'Rectification' },
            { v: 'common.non_material_update', t: 'No change in substance' },
            { v: 'common.market_practice', t: 'Market practice changed' }
        ],
        delete: [
            { v: 'common.obsolete', t: 'Obsolete' },
            { v: 'common.duplicate', t: 'Duplicate' },
            { v: 'common.entered_in_error', t: 'Entered in error' }
        ]
    };

    /* ---------------------------------------------------------------- */
    /* The mock convention documents                                      */
    /* ---------------------------------------------------------------- */

    var ACRONYMS = { ois: 'OIS', cds: 'CDS', bma: 'BMA', cms: 'CMS', fx: 'FX', fra: 'FRA', ibor: 'IBOR' };
    function label(k) {
        var t = k.split('_').map(function (w) { return ACRONYMS[w] || w; }).join(' ');
        return t.charAt(0).toUpperCase() + t.slice(1);
    }

    /* The 26 kinds the conventions screen lists: 23 families, the composite
       tenor basis swap, and the tenor and currency pair kinds. The four
       without a party column are held once for the tenant. */
    var PARTY = ['average_ois', 'bma_basis_swap', 'bond_yield', 'cds', 'cms_spread_option', 'commodity_forward',
        'commodity_future', 'cross_currency_basis', 'cross_currency_fix_float', 'deposit', 'fra', 'future', 'fx_option',
        'inflation_swap', 'intraday_power_load', 'ois', 'swap', 'swap_index', 'tenor_basis_swap',
        'tenor_basis_two_swap', 'zero', 'zero_inflation_index'];
    var TENANT = ['currency_pair', 'ibor_index', 'overnight_index', 'tenor'];
    M.KINDS = PARTY.map(function (k) { return { key: k, label: label(k), party: true }; })
        .concat(TENANT.map(function (k) { return { key: k, label: label(k), party: false }; }))
        .sort(function (a, b) { return a.key < b.key ? -1 : (a.key > b.key ? 1 : 0); });
    M.kind = function (key) { return M.KINDS.filter(function (k) { return k.key === key; })[0]; };

    var CAL = ['AED', 'EUR', 'USD', 'GBP', 'JPY', 'CHF', 'SGD', 'HKD'];
    var IDX = { AED: 'AED-EIBOR-6M', EUR: 'EUR-EURIBOR-6M', USD: 'USD-SOFR', GBP: 'GBP-SONIA',
        JPY: 'JPY-TONA', CHF: 'CHF-SARON', SGD: 'SGD-SORA', HKD: 'HKD-HIBOR-3M' };
    var FREQ = ['Annual', 'Semi-Annual', 'Quarterly', 'Monthly'];
    var BDC = ['Following', 'ModifiedFollowing', 'Preceding'];
    var DCF = ['ACT/360', 'ACT/365', '30/360'];
    var SUB = ['', 'Compounding', 'Averaging'];

    function opts(list) { return list.map(function (x) { return { v: x, t: x }; }); }

    /* The term columns the prototypes draw. A pick holds the code of a list
       another record kind owns, and a text column holds a value of its own. */
    M.COLUMNS = {
        swap: [
            { id: 'fixed_calendar', h: 'Fixed calendar', pick: 'calendar_names', options: opts(CAL) },
            { id: 'fixed_frequency', h: 'Fixed frequency', pick: 'payment_frequencies', options: opts(FREQ) },
            { id: 'fixed_convention', h: 'Fixed convention', pick: 'business_day_convention_types', options: opts(BDC) },
            { id: 'fixed_day_count', h: 'Fixed day count fraction', pick: 'day_count_fraction_types', options: opts(DCF) },
            { id: 'index', h: 'Index', pick: 'floating_index_types', options: opts(CAL.map(function (c) { return IDX[c]; })) },
            { id: 'float_frequency', h: 'Float frequency', pick: 'payment_frequencies', options: opts(FREQ) },
            { id: 'sub_periods', h: 'Sub-periods coupon type', pick: 'sub_periods_coupon_types', options: opts(SUB.filter(Boolean)), optional: true }
        ],
        deposit: [
            { id: 'index_based', h: 'Index based', pick: 'yes_no', options: opts(['Yes', 'No']) },
            { id: 'calendar', h: 'Calendar', pick: 'calendar_names', options: opts(CAL) },
            { id: 'convention', h: 'Convention', pick: 'business_day_convention_types', options: opts(BDC) },
            { id: 'day_count', h: 'Day count fraction', pick: 'day_count_fraction_types', options: opts(DCF) },
            { id: 'settlement_days', h: 'Settlement days', pick: 'settlement_days', options: opts(['0', '1', '2']) }
        ],
        ibor_index: [
            { id: 'fixing_calendar', h: 'Fixing calendar', pick: 'calendar_names', options: opts(CAL) },
            { id: 'day_count', h: 'Day count fraction', pick: 'day_count_fraction_types', options: opts(DCF) },
            { id: 'settlement_days', h: 'Settlement days', pick: 'settlement_days', options: opts(['0', '1', '2']) },
            { id: 'convention', h: 'Business day rule', pick: 'business_day_convention_types', options: opts(BDC) }
        ]
    };
    M.modelled = function (kind) { return !!M.COLUMNS[kind]; };

    function pad(n, w) { var s = String(n); while (s.length < w) { s = '0' + s; } return s; }

    function swapDocs() {
        var combos = [];
        for (var i = 0; i < 288; i++) {
            var n = (i * 97) % 288;
            var c = { cal: CAL[n % 8], freq: FREQ[Math.floor(n / 8) % 4], bdc: BDC[Math.floor(n / 32) % 3], dcf: DCF[Math.floor(n / 96) % 3] };
            var curated = c.cal === 'AED' && c.freq === 'Annual' && c.bdc === 'ModifiedFollowing' && c.dcf === 'ACT/365';
            if (!curated) { combos.push(c); }
        }
        combos = [{ cal: 'AED', freq: 'Annual', bdc: 'ModifiedFollowing', dcf: 'ACT/365' }].concat(combos.slice(0, 200));
        var sets = combos.map(function (c, i) {
            return {
                fixed_calendar: c.cal, fixed_frequency: c.freq, fixed_convention: c.bdc, fixed_day_count: c.dcf,
                index: IDX[c.cal], float_frequency: i === 0 ? 'Semi-Annual' : FREQ[(i * 3 + 1) % 4],
                sub_periods: i === 0 ? '' : SUB[i % 3]
            };
        });
        var docs = [];
        for (var d = 0; d < 320; d++) {
            var terms = d < 201 ? sets[d] : sets[(d * 7) % 201];
            var key = d === 0 ? 'AED-EIBOR-6M-SWAP' : terms.fixed_calendar + '-' + terms.index.split('-').slice(1).join('-') + '-SWAP-' + pad(d, 3);
            var authored = d % 41 === 7;
            docs.push({
                kind: 'swap', key: key, version: d === 0 ? 2 : 1, terms: terms,
                source: authored ? 'Authored' : 'Curated dataset',
                modifiedBy: authored ? 'm.okafor' : 'system',
                recordedAt: authored ? '2026-10-12T09:20:00Z' : '2026-10-02T06:00:00Z',
                reason: authored ? 'system.new_record' : 'system.external_data_import',
                commentary: authored ? 'Authored for a counterparty agreement' : 'Published from ORE Instrument Conventions'
            });
        }
        return docs;
    }

    function depositDocs() {
        var docs = [];
        var tenors = ['ON', '1W', '1M'];
        CAL.forEach(function (c, ci) {
            tenors.forEach(function (t, ti) {
                var based = ti === 2;
                docs.push({
                    kind: 'deposit', key: c + '-DEPO-' + t, version: 1,
                    terms: { index_based: based ? 'Yes' : 'No', calendar: based ? '' : c, convention: based ? '' : BDC[ci % 3],
                        day_count: based ? '' : DCF[(ci + ti) % 3], settlement_days: based ? '' : String(ti % 3) },
                    source: 'Curated dataset', modifiedBy: 'system', recordedAt: '2026-10-02T06:00:00Z',
                    reason: 'system.external_data_import', commentary: 'Published from ORE Instrument Conventions'
                });
            });
        });
        return docs;
    }

    function iborDocs() {
        return CAL.map(function (c, i) {
            return {
                kind: 'ibor_index', key: IDX[c], version: 1,
                terms: { fixing_calendar: c, day_count: DCF[i % 3], settlement_days: String(i % 3), convention: BDC[i % 3] },
                source: 'Curated dataset', modifiedBy: 'system', recordedAt: '2026-10-02T06:00:00Z',
                reason: 'system.external_data_import', commentary: 'Published from ORE Instrument Conventions'
            };
        });
    }

    function genericDocs(kind) {
        var out = [];
        for (var i = 1; i <= 3; i++) {
            out.push({
                kind: kind, key: kind.toUpperCase().replace(/_/g, '-') + '-STD-' + i, version: 1, terms: {},
                source: 'Curated dataset', modifiedBy: 'system', recordedAt: '2026-10-02T06:00:00Z',
                reason: 'system.external_data_import', commentary: 'Published from ORE Instrument Conventions'
            });
        }
        return out;
    }

    var CACHE = {};
    M.docs = function (kind) {
        if (!CACHE[kind]) {
            CACHE[kind] = kind === 'swap' ? swapDocs() : kind === 'deposit' ? depositDocs()
                : kind === 'ibor_index' ? iborDocs() : genericDocs(kind);
        }
        return CACHE[kind];
    };
    M.doc = function (kind, key) {
        return M.docs(kind).filter(function (d) { return d.key === key; })[0];
    };

    /* The terms of a document in words, so two documents can be told apart. */
    M.words = function (d) {
        var cols = M.COLUMNS[d.kind];
        if (!cols) { return 'Terms are not drawn in this prototype'; }
        var t = d.terms;
        if (d.kind === 'swap') {
            return [t.fixed_calendar, t.fixed_frequency.toLowerCase(), t.fixed_day_count, t.index].join(', ');
        }
        if (d.kind === 'deposit') {
            return t.index_based === 'Yes' ? 'Follows its index' : [t.calendar, t.day_count, t.settlement_days + ' settlement days'].join(', ');
        }
        return [t.fixing_calendar, t.day_count, t.settlement_days + ' settlement days'].join(', ');
    };

    /* How many distinct term sets a kind states. */
    M.termSets = function (kind) {
        var seen = {};
        M.docs(kind).forEach(function (d) { seen[JSON.stringify(d.terms)] = 1; });
        return Object.keys(seen).length;
    };

    M.sameTerms = function (a, b) { return JSON.stringify(a.terms) === JSON.stringify(b.terms); };

    /* The prototype bar at the foot: states as buttons, and the gap toggle. */
    M.bar = function (items, current) {
        return '<span class="label">View</span>' + items.map(function (it) {
            return '<button type="button" data-act="bar" data-arg="' + M.esc(it.v) + '"' +
                (it.v === current ? ' class="on"' : '') + '>' + M.esc(it.t) + '</button>';
        }).join('') + '<button type="button" data-act="gaps">' +
            (document.body.classList.contains('nogaps') ? 'Show server gaps' : 'Hide server gaps') + '</button>';
    };

    M.gap = function (html) {
        return '<div class="gap"><b>Server gap.</b> ' + html + '</div>';
    };

    root.Mgmt = M;
})(window);
