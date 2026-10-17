/* Choose when a report runs. Self-contained: plain JavaScript, mock data, no
 * framework, no build step, and nothing that outlives the page.
 *
 * See doc/knowledge/journeys/reporting/journey_choose_when_a_report_runs.org.
 *
 * The journey's question is where scheduling lives. All three variants carry
 * the same fixture and the same actions, and disagree only about structure: A a
 * field group on the report's own row, B a list of schedules, C a week of fire
 * times. Nothing here reads a server.
 *
 * Four actions, and the page keeps them apart: Run now starts one run and leaves
 * the schedule alone; Pause stops the schedule firing and keeps it; Play starts
 * it firing again; Unschedule takes the schedule away and leaves the definition
 * on demand. Run now and Unschedule exist as operations. Pause and Play do not:
 * the scheduler job's is_active is where that state would live, so the paused
 * row this fixture draws is a fixture and the page says so.
 *
 * Two structural facts the copy never hides: a schedule belongs to one report
 * definition, and a definition belongs to one party, so this screen reaches
 * ACMCOR's five definitions and no others; and only a tenant administrator may
 * schedule, so the second role state draws every cadence read-only.
 *
 * The next fire times are the one thing drawn that a build cannot draw. They
 * are computed in this page from the expression, and every place they appear
 * says so, because no operation reads a scheduler job. */

(function () {
    'use strict';

    /* The fixture's clock. Fixed, so the dates a reviewer sees do not move
       under them; the page states the same time in the header. */
    var NOW = new Date(2026, 9, 20, 14, 32, 0);
    var WEEK_START = new Date(2026, 9, 19, 0, 0, 0);
    var DAY_NAMES = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
    var MONTH_NAMES = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];

    var PARTY = { code: 'ACMCOR', name: 'ACME Corporation', city: 'London' };

    /* Parties the tenant holds and this screen cannot reach. The cadence of
       their definitions is not shown, because no read here reaches it either. */
    var OTHER_PARTIES = [
        { code: 'ACCOUK', name: 'ACME UK Ltd', city: 'London', definitions: ['Liquidity Coverage'] },
        { code: 'ACCOUS', name: 'ACME US LLC', city: 'New York', definitions: ['Headline Position'] },
        { code: 'ACCOHK', name: 'ACME Hong Kong Ltd', city: 'Hong Kong', definitions: ['Board Risk Dashboard'] }
    ];

    var ROLES = {
        admin: { person: 'Anneke Vos', label: 'Tenant administrator', maySchedule: true },
        analyst: { person: 'Tom Beck', label: 'Reporting analyst', maySchedule: false }
    };

    /* The five definitions of the party the person works in. Two carry a live
       schedule, one is drawn paused, and two carry nothing, so a live schedule,
       a paused one and an on-demand one sit beside each other. `runs` is the
       count report_instances.list returns; there is no last-fired time
       anywhere, because no read serves one. `paused` is a fixture: no operation
       reads or writes the scheduler job's is_active. */
    var DEFINITIONS = [
        {
            id: 'rpt-8f31a0c4',
            name: 'Group Consolidated Exposure',
            short: 'GCE',
            expression: '0 6 * * *',
            jobId: 'sched.job.41c9f0a7',
            paused: false,
            runs: 26
        },
        {
            id: 'rpt-2b7de915',
            name: 'Bucketed DV01',
            short: 'BDV',
            expression: '0 */2 * * *',
            jobId: 'sched.job.9e02b6d3',
            paused: false,
            runs: 1204
        },
        {
            id: 'rpt-c04a7f28',
            name: 'Large Exposures',
            short: 'LEX',
            expression: '0 6 * * 1',
            jobId: 'sched.job.7d18aa52',
            paused: true,
            runs: 8
        },
        {
            id: 'rpt-5e91cc36',
            name: 'Leverage Ratio',
            short: 'LEV',
            expression: null,
            jobId: null,
            paused: false,
            runs: 14
        },
        {
            id: 'rpt-a1f70d82',
            name: 'Data Quality Completeness',
            short: 'DQC',
            expression: null,
            jobId: null,
            paused: false,
            runs: 41
        }
    ];

    var CADENCES = [
        { id: 'daily', label: 'Every day' },
        { id: 'every2h', label: 'Every two hours' },
        { id: 'weekly', label: 'Every Monday' }
    ];

    var VARIANTS = [
        {
            id: 'A',
            name: 'On the report',
            gist: 'Scheduling is a field group on the report definition\u2019s own row: one cadence, one time, edited in place, with its next fire times under it.'
        },
        {
            id: 'B',
            name: 'Schedule list',
            gist: 'Scheduling is a list of schedules, one per report, sorted by when it next fires, so "what runs tomorrow morning" is one view.'
        },
        {
            id: 'C',
            name: 'Calendar',
            gist: 'Scheduling is a week of fire times: a report sits in the hours it fires, so a clash and a quiet day are visible before they happen.'
        }
    ];

    var GAPS = [
        {
            title: 'No pause and no play',
            body: 'The scheduler job carries is_active in ores_scheduler_job_definitions_tbl, so the data can express a paused schedule, and nothing exposes it to a reporting screen. reporting.v1.ops.schedule_report_definitions and reporting.v1.ops.unschedule_report_definitions are the only two schedule operations, and neither sets is_active. A pause operation and a play operation are the missing pair, and a read of the flag is the missing read: without it the screen cannot tell a paused schedule from a live one. The paused row on this page is a fixture drawn from the journey, and no subject is invented for it.'
        },
        {
            title: 'No read of the scheduler job',
            body: 'The definition stores scheduler_job_id and no operation in this set reads the job behind it. A real screen can state that a definition is scheduled and repeat its expression; it cannot state the job\u2019s state, its next due time, or the scheduler\u2019s own reading of the expression. None of the three variants can be built around a scheduler read that does not exist.'
        },
        {
            title: 'The next fire times are drawn, not read',
            body: 'Every date on this page is computed in the browser from the expression. That is the prototype\u2019s concession: a person judges a cadence by the dates it produces, and no read here produces them. A build cannot show these dates until one does, so a build would show the expression alone and say why.'
        },
        {
            title: 'No time zone on the schedule',
            body: 'report_definition carries a cron expression and no time zone. Whose clock "06:00" means is unstated, so the rows above are the installation\u2019s clock, not ACMCOR\u2019s. ACCOUS keeps New York hours: its morning is a report at 11:00 London, and nothing on the definition can say so.'
        },
        {
            title: 'The firing is stored and unreadable',
            body: 'report_trigger_event records every firing with its definition, its trigger type, its job and its time. No read of it is served over NATS, so the screen cannot say when a schedule last fired, whether it fired at all, or which run a firing produced. A person waiting for the first scheduled run has the expression and nothing else.'
        },
        {
            title: 'The write permission has no code',
            body: 'The two schedule operations are named in this journey and have no permission code. TenantAdmin and SuperAdmin hold "*" and can call them; no role below the administrator can be granted the capability, because there is no code to grant. The administrator-only rule on this screen is a design rule, not an enforced one.'
        },
        {
            title: 'No screen at all, today',
            body: 'There is no BFF route for reporting and no reporting UI in the application. This page is the first drawing of a surface for five journeys, and the schedule panel has no row in any build to attach to.'
        }
    ];

    var DRAFT_DEFAULT = { cadence: 'daily', time: '06:00' };

    /* The one sentence the page repeats wherever a paused schedule is drawn,
       because the state is drawn and not served. */
    var PAUSE_FIXTURE = 'Pause and Play have no operation today. The scheduler job carries ' +
        'is_active in ores_scheduler_job_definitions_tbl, which is where a paused state would ' +
        'live, and nothing reads or writes it from a reporting screen. The paused schedule below ' +
        'is a fixture.';

    var S = {
        variant: 'A',
        role: 'admin',
        open: null,
        confirm: null,
        drafts: {},
        defs: null,
        activity: 'rest',
        showState: false,
        log: []
    };

    // ------------------------------------------------------------- fixture

    function copyDefinitions() {
        return DEFINITIONS.map(function (definition) {
            return {
                id: definition.id,
                name: definition.name,
                short: definition.short,
                expression: definition.expression,
                jobId: definition.jobId,
                paused: definition.paused,
                runs: definition.runs
            };
        });
    }

    function defById(id) {
        var found = S.defs.filter(function (definition) { return definition.id === id; });
        return found.length === 1 ? found[0] : null;
    }

    /* Has a schedule: an expression and a scheduler job. A paused definition
       keeps both. */
    function scheduled() {
        return S.defs.filter(function (definition) { return definition.expression !== null; });
    }

    /* Fires today: scheduled and not paused. Only these reach a next fire time,
       the week grid or a clash. */
    function live() {
        return S.defs.filter(function (definition) {
            return definition.expression !== null && !definition.paused;
        });
    }

    function pausedList() {
        return S.defs.filter(function (definition) {
            return definition.expression !== null && definition.paused;
        });
    }

    function onDemand() {
        return S.defs.filter(function (definition) { return definition.expression === null; });
    }

    // ------------------------------------------------------- expressions

    /* The three shapes the fixture uses: a fixed minute, an hour that is one
       value or a step, and a day of the week that is any day or one day. An
       expression this does not understand returns null, and every reader says
       so rather than guessing. */
    function parseExpr(expression) {
        if (!expression) return null;
        var parts = String(expression).trim().split(/\s+/);
        if (parts.length !== 5) return null;
        if (!/^\d+$/.test(parts[0])) return null;
        if (parts[2] !== '*' || parts[3] !== '*') return null;
        if (parts[4] !== '*' && !/^\d+$/.test(parts[4])) return null;

        var minute = parseInt(parts[0], 10);
        if (minute > 59) return null;

        var hours = [];
        var hour = parts[1];
        var index;
        if (hour === '*') {
            for (index = 0; index < 24; index += 1) hours.push(index);
        } else if (/^\*\/\d+$/.test(hour)) {
            var step = parseInt(hour.slice(2), 10);
            if (step < 1) return null;
            for (index = 0; index < 24; index += 1) {
                if (index % step === 0) hours.push(index);
            }
        } else if (/^\d+$/.test(hour)) {
            var at = parseInt(hour, 10);
            if (at > 23) return null;
            hours.push(at);
        } else {
            return null;
        }

        return {
            minute: minute,
            hours: hours,
            day: parts[4] === '*' ? null : parseInt(parts[4], 10)
        };
    }

    function draftOf(id) {
        if (!S.drafts[id]) {
            var definition = defById(id);
            var parsed = parseExpr(definition.expression);
            if (!parsed) {
                S.drafts[id] = { cadence: DRAFT_DEFAULT.cadence, time: DRAFT_DEFAULT.time };
            } else if (parsed.hours.length > 1) {
                S.drafts[id] = { cadence: 'every2h', time: '00:00' };
            } else {
                S.drafts[id] = {
                    cadence: parsed.day === null ? 'daily' : 'weekly',
                    time: pad(parsed.hours[0]) + ':' + pad(parsed.minute)
                };
            }
        }
        return S.drafts[id];
    }

    function exprOf(draft) {
        var bits = String(draft.time).split(':');
        var hour = parseInt(bits[0], 10);
        var minute = parseInt(bits[1], 10);
        if (isNaN(hour) || isNaN(minute)) return null;
        if (draft.cadence === 'every2h') return '0 */2 * * *';
        if (draft.cadence === 'weekly') return minute + ' ' + hour + ' * * 1';
        return minute + ' ' + hour + ' * * *';
    }

    function cadenceLabel(expression) {
        var parsed = parseExpr(expression);
        if (!parsed) return 'an expression this prototype cannot read';
        if (parsed.hours.length > 1) return 'every two hours, on the hour';
        var time = pad(parsed.hours[0]) + ':' + pad(parsed.minute);
        if (parsed.day === null) return 'every day at ' + time;
        return 'every ' + fullDay(parsed.day) + ' at ' + time;
    }

    function fullDay(index) {
        return ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'][index];
    }

    // ------------------------------------------------------------ dates

    function pad(number) {
        return String(number).length < 2 ? '0' + number : String(number);
    }

    function startOfDay(date) {
        return new Date(date.getFullYear(), date.getMonth(), date.getDate(), 0, 0, 0, 0);
    }

    /* The next `count` firings after the fixture clock. Candidate times come
       from the parsed expression, so the list is short and the answer is
       exact for the three shapes the fixture uses. */
    function previewDates(expression, count) {
        var parsed = parseExpr(expression);
        if (!parsed) return [];
        var found = [];
        var dayOffset;
        var hourIndex;
        var hour;
        for (dayOffset = 0; dayOffset < 22 && found.length < count; dayOffset += 1) {
            var dayStart = new Date(startOfDay(NOW).getTime() + dayOffset * 86400000);
            if (parsed.day !== null && dayStart.getDay() !== parsed.day) continue;
            for (hourIndex = 0; hourIndex < parsed.hours.length; hourIndex += 1) {
                hour = parsed.hours[hourIndex];
                var candidate = new Date(dayStart.getFullYear(), dayStart.getMonth(), dayStart.getDate(), hour, parsed.minute, 0, 0);
                if (candidate.getTime() <= NOW.getTime()) continue;
                found.push(candidate);
                if (found.length >= count) break;
            }
        }
        return found;
    }

    function relativeLabel(date) {
        var minutes = Math.round((date.getTime() - NOW.getTime()) / 60000);
        if (minutes < 60) return 'in ' + minutes + ' min';
        if (minutes < 2880) return 'in ' + Math.round(minutes / 60) + ' h';
        return 'in ' + Math.round(minutes / 1440) + ' d';
    }

    function fmtDate(date) {
        return DAY_NAMES[date.getDay()] + ' ' + date.getDate() + ' ' + MONTH_NAMES[date.getMonth()] + ', ' + pad(date.getHours()) + ':' + pad(date.getMinutes());
    }

    function fmtTime(date) {
        return pad(date.getHours()) + ':' + pad(date.getMinutes());
    }

    function firesWithin(expression, hours) {
        var parsed = parseExpr(expression);
        if (!parsed) return 0;
        var count = 0;
        var dayOffset;
        var hourIndex;
        for (dayOffset = 0; dayOffset < 3; dayOffset += 1) {
            var dayStart = new Date(startOfDay(NOW).getTime() + dayOffset * 86400000);
            if (parsed.day !== null && dayStart.getDay() !== parsed.day) continue;
            for (hourIndex = 0; hourIndex < parsed.hours.length; hourIndex += 1) {
                var candidate = new Date(dayStart.getFullYear(), dayStart.getMonth(), dayStart.getDate(), parsed.hours[hourIndex], parsed.minute, 0, 0);
                var minutes = (candidate.getTime() - NOW.getTime()) / 60000;
                if (minutes > 0 && minutes <= hours * 60) count += 1;
            }
        }
        return count;
    }

    /* Which day/hour pairs a schedule occupies in the fixture week. Column 0
       is Monday, matching the grid. */
    function weekSlots(expression) {
        var parsed = parseExpr(expression);
        if (!parsed) return [];
        var slots = [];
        var column;
        var hourIndex;
        for (column = 0; column < 7; column += 1) {
            var day = new Date(WEEK_START.getTime() + column * 86400000);
            if (parsed.day !== null && day.getDay() !== parsed.day) continue;
            for (hourIndex = 0; hourIndex < parsed.hours.length; hourIndex += 1) {
                slots.push({ column: column, hour: parsed.hours[hourIndex], minute: parsed.minute });
            }
        }
        return slots;
    }

    /* The cells of the fixture week that hold more than one schedule, gathered
       into one line per set of reports, so seven colliding mornings read as one
       fact and not as seven sentences. */
    function clashReport() {
        var byCell = {};
        var order = [];
        live().forEach(function (definition) {
            weekSlots(definition.expression).forEach(function (slot) {
                var key = slot.column + ':' + slot.hour;
                if (!byCell[key]) {
                    byCell[key] = [];
                    order.push(key);
                }
                byCell[key].push(definition);
            });
        });

        var groups = {};
        var groupOrder = [];
        var count = 0;
        order.forEach(function (key) {
            var here = byCell[key];
            if (here.length < 2) return;
            count += 1;
            var bits = key.split(':');
            var names = here.map(function (definition) { return definition.name; }).sort();
            var groupKey = bits[1] + '|' + names.join('|');
            if (!groups[groupKey]) {
                groups[groupKey] = { hour: parseInt(bits[1], 10), names: names, columns: [] };
                groupOrder.push(groupKey);
            }
            groups[groupKey].columns.push(parseInt(bits[0], 10));
        });

        return {
            count: count,
            groups: groupOrder.map(function (key) { return groups[key]; })
        };
    }

    function nameList(names) {
        if (names.length < 2) return names.join('');
        return names.slice(0, -1).join(', ') + ' and ' + names[names.length - 1];
    }

    function columnDate(column) {
        return new Date(WEEK_START.getTime() + column * 86400000);
    }

    // ------------------------------------------------------------ helpers

    function esc(value) {
        return String(value === null || value === undefined ? '' : value)
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function variant() {
        var found = VARIANTS.filter(function (entry) { return entry.id === S.variant; });
        return found.length === 1 ? found[0] : VARIANTS[0];
    }

    function role() {
        return ROLES[S.role] || ROLES.admin;
    }

    function maySchedule() {
        return role().maySchedule;
    }

    function nextOf(definition) {
        var dates = previewDates(definition.expression, 1);
        return dates.length === 1 ? dates[0] : null;
    }

    function sortedByNext(list) {
        return list.slice().sort(function (left, right) {
            var a = nextOf(left);
            var b = nextOf(right);
            if (a === null || b === null) return a === b ? left.name.localeCompare(right.name) : (a === null ? 1 : -1);
            return a.getTime() - b.getTime();
        });
    }

    function logStep(text) {
        S.log.push(text);
    }

    function unscheduleCopy(definition) {
        return 'Unscheduling takes the recurrence and the definition\u2019s scheduler job away, and the definition becomes on demand, so a person can still run it now. It is not a pause: nothing is left to resume. Its ' +
            definition.runs + ' runs on record stay, with their outputs and errors, and a run already in flight is not cancelled: no operation in this journey stops a run.';
    }

    function runNowCopy(definition) {
        return 'Run now starts one run at once and leaves the schedule alone. The expression, the scheduler job and the next firing are unchanged: the schedule fires again as it did before. The definition\u2019s concurrency policy decides what happens if a run is already in flight. No run already produced is touched, and this action creates history rather than removing it. Its ' +
            definition.runs + ' runs on record stay.';
    }

    function pauseCopy(definition) {
        return 'Pausing keeps the schedule and stops it firing. The expression stays on the definition, the scheduler job stays, and nothing fires until the schedule is played again. The next fire time is unknown while it is paused, so the screen states no date. Runs already produced stay, and a run in flight is not cancelled. ' +
            PAUSE_FIXTURE;
    }

    function playCopy(definition) {
        return 'Playing starts firing again on the same schedule: the same expression, the same scheduler job, and the next time the expression names. Nothing is re-entered, and playing does not itself create a run. Runs already produced stay. ' +
            PAUSE_FIXTURE;
    }

    // --------------------------------------------------------- shared parts

    function appHeader() {
        return '<header class="appheader"><div class="appheader-inner">' +
            '<span class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></span>' +
            '<nav class="appnav"><a href="../index.org">Prototypes</a>' +
            '<a href="#" class="here">Reporting</a></nav>' +
            '<span class="modechip">' + esc(role().label) + ' \u00b7 party ' + esc(PARTY.code) + '</span>' +
            '</div></header>';
    }

    function pageHead() {
        return '<header class="pagehead"><div>' +
            '<div class="crumbs"><a href="../index.org">Prototypes</a><span>/</span>' +
            '<span>Reporting</span><span>/</span><span>Choose when a report runs</span></div>' +
            '<h1>When a report runs</h1>' +
            '<p class="lede">A schedule belongs to one report definition, and a definition belongs to one party. ' +
            'These are ' + esc(PARTY.code) + '\u2019s ' + S.defs.length + ' definitions, because that is the party ' +
            'the signed-in person works in. <b>Run now</b> starts one run and leaves the schedule alone. ' +
            '<b>Pause</b> keeps the schedule and stops it firing, and <b>Play</b> starts it firing again. ' +
            '<b>Unschedule</b> takes the schedule away. Giving one a schedule sets when its run starts, and nothing else.</p>' +
            '</div><div class="actions">' +
            '<span class="updated">Installation clock ' + esc(fmtDate(NOW)) + '</span>' +
            '<button class="btn" data-act="toggle-state">Show state</button>' +
            '<a class="btn linklike" href="../index.org">Back to prototypes</a>' +
            '</div></header>';
    }

    function fixtureNote() {
        return '<div class="notice warn"><b>PROTOTYPE, MOCK DATA.</b> ' +
            'The cadences, the paused schedule and the next fire times below are a fixture. Nothing on this page reads a server. ' +
            'There is no scheduler read for this screen, so a build cannot show a job\u2019s state, its last firing ' +
            'or its next due time; the dates here are computed from the expression to show what a person is judging. ' +
            '<b>Pause and Play have no operation today</b>: the scheduler job\u2019s <b>is_active</b> in ' +
            '<span class="mono">ores_scheduler_job_definitions_tbl</span> is where that state would live, and nothing ' +
            'reads or writes it from here. The paused schedule below is a fixture. ' +
            'The schedule is a cron expression with <b>no time zone</b>, so "06:00" is the installation\u2019s clock and ' +
            'not the party\u2019s. There is no reporting UI in the application today: this page is the first drawing ' +
            'of a surface for five journeys.</div>';
    }

    function editor(id) {
        var definition = defById(id);
        var draft = draftOf(id);
        var expression = exprOf(draft);
        var dates = previewDates(expression, 3);
        var parsed = parseExpr(expression);
        var atDisabled = draft.cadence === 'every2h';
        var isChange = definition.expression !== null;

        var cadenceOptions = CADENCES.map(function (cadence) {
            return '<option value="' + cadence.id + '"' + (draft.cadence === cadence.id ? ' selected' : '') + '>' +
                esc(cadence.label) + '</option>';
        }).join('');

        return '<div class="sched-edit" data-editor="' + esc(id) + '">' +
            '<div class="fields">' +
            '<span class="field"><label for="cad-' + esc(id) + '">Repeats</label>' +
            '<select id="cad-' + esc(id) + '" data-act="cadence" data-id="' + esc(id) + '">' + cadenceOptions + '</select></span>' +
            '<span class="field"><label for="at-' + esc(id) + '">At</label>' +
            '<input id="at-' + esc(id) + '" type="time" value="' + esc(pad(parsed ? parsed.hours[0] : 6) + ':' + pad(parsed ? parsed.minute : 0)) + '"' +
            (atDisabled ? ' disabled' : '') + ' data-act="time" data-id="' + esc(id) + '"></span>' +
            '<span class="hint">' + (atDisabled
                ? 'Every two hours from midnight, so the time of day is not part of the expression.'
                : 'The minute and hour of the expression.') + '</span>' +
            '</div>' +
            '<div class="exprline"><span>Expression:</span>' +
            (expression === null
                ? '<span class="mono">unreadable</span>'
                : '<span class="mono">' + esc(expression) + '</span>') +
            '<span class="tag muted">sent as schedule_expression</span></div>' +
            '<div class="nextfires">' +
            '<div class="previewhead"><span class="hint">' + (definition.paused
                ? 'Next fire times, computed here \u2014 nothing fires while this schedule is paused'
                : 'Next fire times, computed here \u2014 no scheduler read exists') + '</span></div>' +
            (dates.length === 0
                ? '<p class="hint">This prototype cannot preview that expression. The service validates it when the write arrives; the scheduler evaluates it.</p>'
                : '<ul class="nextlist">' + dates.map(function (date) {
                    return '<li><span class="bullet">\u2022</span><span>' + esc(fmtDate(date)) + '</span>' +
                        '<span class="faint">' + esc(relativeLabel(date)) + '</span></li>';
                }).join('') + '</ul>' +
                (definition.paused
                    ? '<p class="hint">These are the times the expression would produce after Play.</p>'
                    : '')) +
            '</div>' +
            '<div class="actions">' +
            (maySchedule()
                ? '<button class="btn primary" data-act="ask-schedule" data-id="' + esc(id) + '">' +
                  (isChange ? 'Change the schedule\u2026' : 'Schedule this report\u2026') + '</button>' +
                  runNowButton(definition, 'btn') +
                  (isChange
                      ? pausePlayButton(definition, 'btn') + unscheduleButton(definition, 'btn')
                      : '')
                : '<span class="tag muted">Read-only: only a tenant administrator may schedule</span>') +
            '<button class="btn ghost" data-act="close-editor">Close</button>' +
            '</div></div>';
    }

    function nextFireList(definition, count) {
        if (definition.paused) {
            return '<span class="tag warn">will not fire</span> <span class="faint">while paused</span>';
        }
        var dates = previewDates(definition.expression, count);
        if (dates.length === 0) {
            return '<span class="faint">This prototype cannot preview that expression.</span>';
        }
        return '<ul class="nextlist">' + dates.map(function (date) {
            return '<li><span class="bullet">\u2022</span><span>' + esc(fmtDate(date)) + '</span>' +
                '<span class="faint">' + esc(relativeLabel(date)) + '</span></li>';
        }).join('') + '</ul>';
    }

    /* The four actions, drawn as buttons. Run now and Unschedule send an
       operation that exists. Pause and Play send nothing: no operation sets the
       scheduler job's is_active, and the tooltip says so. */
    function actionButton(act, label, id, extraClass, title) {
        return '<button class="' + extraClass + '" data-act="' + act + '" data-id="' + esc(id) + '"' +
            (title ? ' title="' + esc(title) + '"' : '') + '>' + esc(label) + '</button>';
    }

    function runNowButton(definition, extraClass) {
        return actionButton('ask-runnow', 'Run now', definition.id, extraClass,
            'Starts one run at once. The schedule is not changed.');
    }

    function pausePlayButton(definition, extraClass) {
        if (definition.paused) {
            return actionButton('ask-play', 'Play', definition.id, extraClass,
                'Starts firing again on the same schedule. No operation exists for this today.');
        }
        return actionButton('ask-pause', 'Pause', definition.id, extraClass,
            'Keeps the schedule and stops it firing. No operation exists for this today.');
    }

    function unscheduleButton(definition, extraClass) {
        return actionButton('ask-unschedule', 'Unschedule', definition.id, extraClass,
            'Takes the schedule away and leaves the definition on demand. The runs stay.');
    }

    function scheduleCell(definition) {
        if (definition.expression === null) {
            return '<span class="tag muted">on demand</span> <span class="faint">nothing scheduled</span>';
        }
        var state = definition.paused
            ? '<span class="tag warn">paused</span>'
            : '<span class="tag accent">scheduled</span>';
        return '<div class="cellgroup">' + state +
            '<span class="expr mono">' + esc(definition.expression) + '</span></div>' +
            '<div class="next">' + esc(cadenceLabel(definition.expression)) + '</div>' +
            '<div class="next">job ' + esc(definition.jobId) + '</div>' +
            (definition.paused
                ? '<div class="faint">the schedule stays; it fires again after Play</div>'
                : '');
    }

    function actionCell(definition) {
        if (!maySchedule()) {
            return '<span class="tag muted">read-only</span>';
        }
        var open = S.open === definition.id;
        return '<div class="cellgroup">' +
            '<button class="btn small" data-act="open-editor" data-id="' + esc(definition.id) + '">' +
            (open ? 'Close' : (definition.expression === null ? 'Set a schedule' : 'Edit schedule')) + '</button>' +
            (open
                ? ''
                : runNowButton(definition, 'btn small') +
                  (definition.expression === null ? '' : pausePlayButton(definition, 'btn small') +
                    unscheduleButton(definition, 'btn small'))) +
            '</div>';
    }

    // ------------------------------------------------------------ variant A

    function variantA() {
        var rows = S.defs.map(function (definition) {
            var main = '<tr>' +
                '<td><span class="reportname">' + esc(definition.name) + '</span>' +
                '<span class="reportid">' + esc(definition.id) + '</span></td>' +
                '<td><span class="tag muted">' + esc(PARTY.code) + '</span></td>' +
                '<td class="schedcell">' + scheduleCell(definition) + '</td>' +
                '<td class="schedcell">' + (definition.expression === null
                    ? '<span class="faint">\u2014</span>'
                    : nextFireList(definition, 3)) + '</td>' +
                '<td>' + actionCell(definition) + '</td>' +
                '</tr>';
            var editorRow = S.open === definition.id
                ? '<tr class="editorrow"><td colspan="5">' + editor(definition.id) + '</td></tr>'
                : '';
            return '<tbody>' + main + editorRow + '</tbody>';
        }).join('');

        return '<section class="card">' +
            '<header class="sectionhead"><h2>Report definitions \u2014 ' + esc(PARTY.code) + '</h2>' +
            '<div class="meta"><span>' + S.defs.length + ' definitions</span>' +
            '<span class="tag accent">' + live().length + ' scheduled</span>' +
            (pausedList().length === 0
                ? ''
                : '<span class="tag warn">' + pausedList().length + ' paused</span>') +
            '<span class="tag muted">' + onDemand().length + ' on demand</span></div></header>' +
            '<div class="table-wrap"><table><thead><tr>' +
            '<th>Report definition</th><th>Party</th><th>Schedule</th><th>Next fire (mock preview)</th><th>Actions</th>' +
            '</tr></thead>' + rows + '</table></div>' +
            '<p class="rowfoot">The schedule is a field of the definition, so it is edited where the definition is read. ' +
            'The field group opens under the row it belongs to, and the row reads <em>scheduled</em>, <em>paused</em> ' +
            'or <em>on demand</em>. Run now starts a run and leaves the schedule alone; Pause keeps the schedule and ' +
            'stops it firing; Play fires it again; Unschedule takes it away and leaves the definition on demand. A ' +
            'definition with no schedule is not a special case: it is the same field group with nothing in it.</p>' +
            '</section>';
    }

    // ------------------------------------------------------------ variant B

    function variantB() {
        var list = sortedByNext(live()).concat(pausedList());
        var soon = live().reduce(function (total, definition) {
            return total + firesWithin(definition.expression, 24);
        }, 0);
        var soonNames = live().map(function (definition) {
            var count = firesWithin(definition.expression, 24);
            return count === 0 ? null : definition.name + ' ' + count + ' time' + (count === 1 ? '' : 's');
        }).filter(function (text) { return text !== null; }).join(', ');

        var rows = list.map(function (definition) {
            var next = nextOf(definition);
            var open = S.open === definition.id;
            return '<div class="schedrow">' +
                '<div class="when">' + (definition.paused
                    ? '<span class="tag warn">paused</span><span class="rel">will not fire</span>'
                    : (next === null ? '\u2014' : esc(fmtDate(next))) +
                      '<span class="rel">' + (next === null ? 'cannot preview' : esc(relativeLabel(next))) + '</span>') +
                '</div>' +
                '<div class="which"><span class="name">' + esc(definition.name) + '</span>' +
                '<span class="detail"><span class="tag muted">' + esc(PARTY.code) + '</span>' +
                '<span class="mono">' + esc(definition.expression) + '</span>' +
                '<span>' + esc(cadenceLabel(definition.expression)) + '</span>' +
                '<span class="faint">job ' + esc(definition.jobId) + '</span>' +
                '<span class="faint">' + definition.runs + ' runs on record</span>' +
                (definition.paused ? '<span class="faint">schedule kept, nothing fires until Play</span>' : '') +
                '</span></div>' +
                '<div class="rowacts">' +
                (maySchedule()
                    ? '<button class="btn small" data-act="open-editor" data-id="' + esc(definition.id) + '">' +
                      (open ? 'Close' : 'Edit') + '</button>' +
                      (open
                          ? ''
                          : runNowButton(definition, 'btn small') + pausePlayButton(definition, 'btn small') +
                            unscheduleButton(definition, 'btn small'))
                    : '<span class="tag muted">read-only</span>') +
                '</div>' +
                (open ? '<div class="host">' + editor(definition.id) + '</div>' : '') +
                '</div>';
        }).join('');

        var idle = onDemand().map(function (definition) {
            var open = S.open === definition.id;
            return '<div class="tray-item">' +
                '<div class="trayrow">' +
                '<div><span class="name">' + esc(definition.name) + '</span>' +
                '<div class="detail">' + esc(PARTY.code) + ' \u00b7 ' + definition.runs + ' runs on record \u00b7 nothing scheduled, so the definition runs only when a person runs it</div>' +
                '</div>' +
                '<div class="rowacts">' +
                (maySchedule()
                    ? '<button class="btn small" data-act="open-editor" data-id="' + esc(definition.id) + '">' +
                      (open ? 'Close' : 'Give it a schedule') + '</button>' +
                      (open ? '' : runNowButton(definition, 'btn small'))
                    : '<span class="tag muted">read-only</span>') +
                '</div></div>' +
                (open ? '<div class="tray-editor">' + editor(definition.id) + '</div>' : '') +
                '</div>';
        }).join('');

        return '<section class="card">' +
            '<header class="sectionhead"><h2>Schedules \u2014 ' + esc(PARTY.code) + '</h2>' +
            '<div class="meta"><span>' + scheduled().length + ' of ' + S.defs.length + ' definitions carry a schedule</span>' +
            '<span class="tag accent">' + live().length + ' firing</span>' +
            (pausedList().length === 0
                ? ''
                : '<span class="tag warn">' + pausedList().length + ' paused</span>') +
            '<span class="tag accent">sorted by next fire</span></div></header>' +
            '<div class="nexthead">In the next 24 hours <b>' + soon + ' runs start</b>' +
            '<span class="faint">' + (soonNames === '' ? 'nothing fires' : '\u2014 ' + esc(soonNames)) + '</span></div>' +
            (rows === '' ? '<p class="hint" style="padding:12px 0">No definition carries a schedule.</p>' : rows) +
            '<p class="rowfoot">Ordered by the next time each schedule fires, so a person answers "what runs tomorrow ' +
            'morning" by reading down one column. A paused schedule has no next fire, so it sits at the end with its ' +
            'expression still shown. The schedule is the row here, not the report: the report\u2019s name is ' +
            'the schedule\u2019s subject and nothing else, and a definition without a schedule is not a row at all.</p>' +
            '</section>' +
            '<section class="card">' +
            '<header class="sectionhead"><h2>Not scheduled \u2014 ' + onDemand().length + ' definitions</h2>' +
            '<div class="meta">on demand, run by hand today</div></header>' +
            '<div class="tray">' + idle + '</div>' +
            '<p class="rowfoot">These sit outside the list because they have no schedule to scan. Giving one a schedule ' +
            'is the same write as changing one.</p>' +
            '</section>';
    }

    // ------------------------------------------------------------ variant C

    function variantC() {
        var cells = [];
        var column;
        var hour;
        var placed = {};
        live().forEach(function (definition) {
            weekSlots(definition.expression).forEach(function (slot) {
                var key = slot.column + ':' + slot.hour;
                if (!placed[key]) placed[key] = [];
                placed[key].push(definition);
            });
        });

        var head = '<div class="cal-head"><span></span>' + (function () {
            var out = '';
            for (column = 0; column < 7; column += 1) {
                var day = columnDate(column);
                out += '<span>' + DAY_NAMES[day.getDay()] + ' <span class="dom">' + day.getDate() + '</span></span>';
            }
            return out;
        }()) + '</div>';

        for (hour = 0; hour < 24; hour += 1) {
            cells.push('<div class="cal-row"><span class="cal-h">' + pad(hour) + ':00</span>' + (function () {
                var out = '';
                for (column = 0; column < 7; column += 1) {
                    var key = column + ':' + hour;
                    var here = placed[key] || [];
                    var weekend = column >= 5;
                    var chips = here.map(function (definition) {
                        var kind = parseExpr(definition.expression).hours.length > 1
                            ? 'intra'
                            : (parseExpr(definition.expression).day === null ? '' : 'week');
                        return '<span class="chip ' + kind + '" data-act="open-editor" data-id="' + esc(definition.id) + '" ' +
                            'title="' + esc(definition.name + ' \u00b7 ' + definition.expression) + '">' +
                            esc(definition.short) + '</span>';
                    }).join('');
                    out += '<div class="cell' + (weekend ? ' we' : '') + '">' + chips + '</div>';
                }
                return out;
            }()) + '</div>');
        }

        var clash = clashReport();
        var emptyHours = [];
        for (hour = 0; hour < 24; hour += 1) {
            var any = Object.keys(placed).some(function (key) {
                return parseInt(key.split(':')[1], 10) === hour;
            });
            if (!any) emptyHours.push(pad(hour));
        }
        var weekendCount = Object.keys(placed).filter(function (key) {
            return parseInt(key.split(':')[0], 10) >= 5;
        }).length;

        var keyRows = S.defs.map(function (definition) {
            var count = definition.expression === null ? 0 : weekSlots(definition.expression).length;
            var open = S.open === definition.id;
            return '<div class="keyrow">' +
                '<span class="kcode">' + esc(definition.short) + '</span>' +
                '<span><span class="kname">' + esc(definition.name) + '</span>' +
                '<span class="kdetail">' + (definition.expression === null
                    ? 'no schedule \u2014 on demand, not on the grid'
                    : esc(cadenceLabel(definition.expression)) + ' \u00b7 <span class="mono">' + esc(definition.expression) + '</span> \u00b7 ' +
                      (definition.paused
                          ? 'paused \u2014 the schedule stays and nothing fires, so it is not on the grid'
                          : count + ' firings this week')) +
                '</span></span>' +
                '<span class="keyacts">' + (maySchedule()
                    ? '<button class="btn small ghost" data-act="open-editor" data-id="' + esc(definition.id) + '">' +
                      (open ? 'Close' : (definition.expression === null ? 'Place it' : 'Change slot')) + '</button>' +
                      (open
                          ? ''
                          : runNowButton(definition, 'btn small ghost') +
                            (definition.expression === null
                                ? ''
                                : pausePlayButton(definition, 'btn small ghost') +
                                  unscheduleButton(definition, 'btn small ghost')))
                    : '<span class="faint">read-only</span>') + '</span>' +
                '</div>';
        }).join('');

        var clashLine = clash.count === 0
            ? '<span class="tag ok">no clash</span> No two schedules fire in the same hour this week.'
            : '<span class="tag warn">' + clash.count + (clash.count === 1 ? ' clash' : ' clashes') + '</span> ' +
              clash.groups.map(function (group) {
                  var dayText;
                  if (group.columns.length === 7) dayText = 'every day of the week';
                  else if (group.columns.length >= 4) dayText = group.columns.length + ' days of the week';
                  else dayText = group.columns.map(function (column) {
                      return fullDay(columnDate(column).getDay());
                  }).join(', ');
                  return '<b>' + esc(nameList(group.names)) + '</b> at ' + pad(group.hour) + ':00 on ' + dayText;
              }).join('; ') +
              '. The scheduler holds one job per definition and compares none with another, so neither report waits ' +
              'for the other; the two-hourly schedule is what puts it in the daily slot.';

        var legend = live().map(function (definition) {
            var parsed = parseExpr(definition.expression);
            var kind = parsed.hours.length > 1 ? 'intra' : (parsed.day === null ? '' : 'week');
            return '<span class="item"><span class="chip ' + kind + '">' + esc(definition.short) + '</span>' +
                esc(definition.name) + '</span>';
        }).join('');

        return '<section class="card">' +
            '<header class="sectionhead"><h2>When reports fire \u2014 week of 19\u201325 October 2026</h2>' +
            '<div class="meta"><span>' + live().length + ' schedules on the grid</span>' +
            (pausedList().length === 0
                ? ''
                : '<span class="tag warn">' + pausedList().length + ' paused, off the grid</span>') +
            '<span class="tag muted">installation clock, no time zone stored</span></div></header>' +
            '<div class="cal-note">' + clashLine + '</div>' +
            '<div class="cal-note legend">' + legend + '</div>' +
            '<div class="cal">' + head + cells.join('') + '</div>' +
            '<div class="cal-foot"><span>Quiet: ' + emptyHours.length + ' of 24 hours carry nothing on any day (' +
            esc(emptyHours.slice(0, 6).join(', ')) + '\u2026). Saturday and Sunday carry ' + weekendCount +
            ' of the ' + Object.keys(placed).length + ' firings.</span>' +
            '<span class="faint">Click a report to change what fires in it.</span></div>' +
            (S.open !== null ? '<div style="margin-top:14px">' + editor(S.open) + '</div>' : '') +
            '<p class="rowfoot">The column is a day and the row is an hour, so a clash is two reports in one cell and a ' +
            'quiet day is an empty column. The rows are the installation\u2019s clock. The schedule stores no time zone, ' +
            'so the grid cannot say whose week this is; ' + esc(PARTY.code) + ' keeps London hours and ACCOUS keeps ' +
            'New York\u2019s, and the same grid is drawn for both.</p></section>' +
            '<section class="card">' +
            '<header class="sectionhead"><h2>On the grid</h2>' +
            '<div class="meta">one code per report definition</div></header>' +
            '<div class="key">' + keyRows + '</div>' +
            '<p class="rowfoot">A report fires in every cell its cadence names. A paused schedule fires in none, so it ' +
            'has no cell either; its expression stays on the definition and it returns to the grid after Play. A ' +
            'definition with no schedule has no cell, ' +
            'so an empty grid and a grid of on-demand reports would look the same \u2014 the second list is what tells ' +
            'them apart.</p></section>';
    }

    // ------------------------------------------------------ shared panels

    function reachCard() {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>What each action reaches</h2>' +
            '<div class="meta">Run now, Pause, Play and Unschedule reach different things</div></header>' +
            '<div class="reach">' +
            '<div class="does"><h3>It changes</h3><ul>' +
            '<li>Schedule and change: the definition\u2019s <span class="mono">schedule_expression</span>, and the scheduler job it names</li>' +
            '<li>Unschedule: removes both, so the definition is on demand and has nothing to resume</li>' +
            '<li>Run now: creates one run at once and leaves the schedule exactly as it was</li>' +
            '<li>Pause and Play, if the operation existed: the scheduler job\u2019s <span class="mono">is_active</span>, and nothing else</li>' +
            '<li>The definition\u2019s next version \u2014 definitions are bitemporal, so the earlier version stays readable</li>' +
            '</ul></div>' +
            '<div class="not"><h3>It does not change</h3><ul>' +
            '<li>The report\u2019s inputs, or the data they are read from</li>' +
            '<li>The report\u2019s portfolio, or what it computes over</li>' +
            '<li>The report\u2019s outputs, or the template that writes them</li>' +
            '<li>Runs already produced, or their archives: ' +
            (scheduled().length > 0
                ? scheduled().map(function (definition) { return definition.runs; }).reduce(function (a, b) { return a + b; }, 0) + ' runs stay'
                : 'runs on record stay') + '</li>' +
            '<li>A run in flight: no operation in this journey stops one</li>' +
            '<li>Any definition in another party, and no definition but the one written</li>' +
            '<li>The definition itself when the schedule is paused: the paused state would live on the job, not on the definition</li>' +
            '</ul></div></div></section>';
    }

    function scopeCard() {
        var rows = OTHER_PARTIES.map(function (party) {
            return '<tr class="unreachable"><td><span class="tag muted">' + esc(party.code) + '</span></td>' +
                '<td>' + esc(party.name) + '</td>' +
                '<td>' + esc(party.city) + '</td>' +
                '<td>' + party.definitions.map(function (name) { return esc(name); }).join(', ') + '</td>' +
                '<td><span class="faint">not reachable from here</span></td></tr>';
        }).join('');

        return '<section class="card">' +
            '<header class="sectionhead"><h2>Definitions this screen cannot reach</h2>' +
            '<div class="meta">party row-level security</div></header>' +
            '<div class="table-wrap"><table><thead><tr>' +
            '<th>Party</th><th>Legal entity</th><th>Keeps</th><th>Holds</th><th></th>' +
            '</tr></thead><tbody>' + rows + '</tbody></table></div>' +
            '<p class="rowfoot">A schedule belongs to one definition and a definition belongs to one party, so there is no ' +
            'path from this screen to another party\u2019s report: not a hidden one, and not one to add later. The party ' +
            'is not a filter over a tenant-wide list, because no such list is served. To schedule ' +
            'ACCOUK\u2019s Liquidity Coverage, a person works in ACCOUK. Their cadence is not shown here either, ' +
            'because no read here reaches it.</p></section>';
    }

    function gapCard() {
        return '<section class="card gaps">' +
            '<header class="sectionhead"><h2>What this screen cannot show</h2>' +
            '<span class="faint" style="font-size:12px">the journey\u2019s own list of what is missing</span></header>' +
            '<dl>' + GAPS.map(function (gap) {
                return '<div><dt>' + esc(gap.title) + '</dt><dd>' + esc(gap.body) + '</dd></div>';
            }).join('') + '</dl></section>';
    }

    /* One dialog per action, because the four actions reach different things and
       a single "are you sure" would conflate them again. */
    function dialog() {
        if (S.confirm === null) return '';
        var definition = defById(S.confirm.id);
        var action = S.confirm.action;
        var draft = draftOf(definition.id);
        var draftExpression = exprOf(draft);
        var current = definition.expression;
        var currentDates = current === null ? [] : previewDates(current, 3);
        var draftDates = previewDates(draftExpression, 3);
        var nowPreview = current === null
            ? 'nothing: the definition runs only when a person runs it'
            : (definition.paused
                ? 'nothing: the schedule is paused and will not fire'
                : (currentDates.length === 0
                    ? 'this prototype cannot preview that expression'
                    : currentDates.map(fmtDate).join(' \u00b7 ')));

        var plan = {
            title: '',
            expression: '',
            cadence: '',
            nextfires: '',
            operation: '',
            notice: '',
            hint: '',
            confirm: '',
            warn: false
        };

        if (action === 'schedule') {
            plan.title = current === null ? 'Schedule this report?' : 'Change this schedule?';
            plan.expression = draftExpression === null ? '\u2014' : draftExpression;
            plan.cadence = cadenceLabel(draftExpression);
            plan.nextfires = draftDates.length === 0
                ? 'this prototype cannot preview that expression'
                : draftDates.map(fmtDate).join(' \u00b7 ');
            plan.operation = 'reporting.v1.ops.schedule_report_definitions';
            plan.notice = 'This sets when the run starts and nothing else. It does not run the report now: the first ' +
                'run comes from the first firing. It does not change the report\u2019s inputs, its portfolio, or its ' +
                'outputs. It does not touch runs already produced, and it does not stop a run in flight. ' +
                'No time zone is stored on the schedule, so this expression is read on the installation\u2019s clock.';
            plan.hint = 'This prototype sends nothing. In a build the service validates the expression, the definition ' +
                'records its scheduler job, and the screen reads the definition back, so the panel shows the stored value ' +
                'and not the value the person typed.';
            plan.confirm = 'Confirm: schedule it';
        } else if (action === 'runnow') {
            plan.title = 'Run this report now?';
            plan.expression = current === null ? '\u2014' : current;
            plan.cadence = current === null ? 'no schedule: the definition is on demand' : cadenceLabel(current);
            plan.nextfires = nowPreview;
            plan.operation = 'reporting.v1.ops.trigger_report_instance';
            plan.notice = runNowCopy(definition);
            plan.hint = 'This prototype sends nothing. In a build the service applies the definition\u2019s concurrency ' +
                'policy, creates a run, and dispatches the execution workflow; the screen then hands the person to the run.';
            plan.confirm = 'Confirm: run it now';
        } else if (action === 'pause') {
            plan.title = 'Pause this schedule?';
            plan.expression = current;
            plan.cadence = cadenceLabel(current);
            plan.nextfires = 'unknown: nothing fires while the schedule is paused';
            plan.operation = 'none: no pause operation exists, and the state would be the job\u2019s is_active';
            plan.notice = pauseCopy(definition);
            plan.hint = 'This prototype changes the row to a fixture. No operation sets the scheduler job\u2019s ' +
                'is_active, so a build could not draw this state until one is added and the flag is readable.';
            plan.confirm = 'Confirm: pause the schedule';
            plan.warn = true;
        } else if (action === 'play') {
            plan.title = 'Play this schedule?';
            plan.expression = current;
            plan.cadence = cadenceLabel(current);
            plan.nextfires = currentDates.length === 0
                ? 'this prototype cannot preview that expression'
                : currentDates.map(fmtDate).join(' \u00b7 ');
            plan.operation = 'none: no play operation exists, and the state would be the job\u2019s is_active';
            plan.notice = playCopy(definition);
            plan.hint = 'This prototype changes the row to a fixture. No operation sets the scheduler job\u2019s ' +
                'is_active, so a build could not draw this state until one is added.';
            plan.confirm = 'Confirm: play the schedule';
            plan.warn = true;
        } else {
            plan.title = 'Unschedule this report?';
            plan.expression = current;
            plan.cadence = 'the schedule is taken away';
            plan.nextfires = 'nothing: the definition becomes on demand';
            plan.operation = 'reporting.v1.ops.unschedule_report_definitions';
            plan.notice = unscheduleCopy(definition);
            plan.hint = 'This prototype sends nothing. In a build the service removes the schedule, and the screen ' +
                'reads the definition back and states the result.';
            plan.confirm = 'Confirm: unschedule';
            plan.warn = true;
        }

        return '<div class="backdrop"><div class="dialog" role="dialog" aria-modal="true">' +
            '<h2>' + esc(plan.title) + '</h2>' +
            '<p class="confirmline"><b>' + esc(definition.name) + '</b> \u00b7 report definition ' +
            '<span class="mono">' + esc(definition.id) + '</span> \u00b7 party <b>' + esc(PARTY.code) + '</b> \u00b7 ' +
            esc(PARTY.name) + '</p>' +
            '<dl>' +
            '<dt>Schedule expression</dt><dd class="mono">' + esc(plan.expression) + '</dd>' +
            '<dt>Cadence</dt><dd>' + esc(plan.cadence) + '</dd>' +
            '<dt>Next fire times (mock preview)</dt><dd>' + esc(plan.nextfires) + '</dd>' +
            '<dt>Operation</dt><dd class="mono">' + esc(plan.operation) + '</dd>' +
            '</dl>' +
            '<div class="notice' + (plan.warn ? ' warn' : '') + '">' + esc(plan.notice) + '</div>' +
            '<div class="actions">' +
            '<button class="btn" data-act="confirm-cancel">Cancel</button>' +
            '<button class="btn primary" data-act="confirm-go">' + esc(plan.confirm) + '</button>' +
            '</div>' +
            '<p class="hint">' + esc(plan.hint) + '</p>' +
            '</div></div>';
    }

    // --------------------------------------------------------------- bars

    function roleButtons() {
        return '<span class="sep">|</span><span class="label">role</span>' +
            '<button data-act="role" data-role="admin"' + (S.role === 'admin' ? ' class="on"' : '') + '>Administrator</button>' +
            '<button data-act="role" data-role="analyst"' + (S.role === 'analyst' ? ' class="on"' : '') + '>Analyst</button>';
    }

    function renderBar() {
        var variantButtons = VARIANTS.map(function (entry) {
            return '<button data-act="variant" data-variant="' + entry.id + '"' +
                (S.variant === entry.id ? ' class="on"' : '') + '>' + esc(entry.id) + '</button>';
        }).join('');

        var stateLine = '<span class="label">variant <b>' + esc(S.variant) + '</b> \u00b7 ' +
            esc(variant().name) + ' \u00b7 state: <b>' + esc(S.activity) + '</b> \u00b7 ' +
            esc(role().label) + ' ' + esc(role().person) + ' on party ' + esc(PARTY.code) + '</span>';

        var statePanel = !S.showState ? '' : '<div class="state-panel">' +
            '<div class="state-grid">' +
            '<span>fixture clock: <b>' + esc(fmtDate(NOW)) + '</b></span>' +
            '<span>party: <b>' + esc(PARTY.code) + '</b></span>' +
            '<span>definitions: <b>' + S.defs.length + '</b></span>' +
            '<span>scheduled: <b>' + live().length + '</b></span>' +
            '<span>paused: <b>' + pausedList().length + '</b></span>' +
            '<span>on demand: <b>' + onDemand().length + '</b></span>' +
            '<span>role may schedule: <b>' + (maySchedule() ? 'yes' : 'no') + '</b></span>' +
            '</div>' +
            '<p class="state-note">One party, because a definition belongs to one party. The cadences, the paused state ' +
            'and the fire times are a fixture; nothing reads a server, no read returns a scheduler job, the job\u2019s ' +
            'is_active has no operation, and the schedule stores no time zone.</p>' +
            (S.log.length === 0
                ? '<p class="state-note">No action yet.</p>'
                : '<ol>' + S.log.map(function (entry, index) {
                    return '<li>' + (index + 1) + '. ' + esc(entry) + '</li>';
                }).join('') + '</ol>') +
            '</div>';

        document.getElementById('proto-bar').innerHTML =
            '<div class="bar-line">' +
            '<span class="tag-prototype">Prototype</span>' + variantButtons +
            '<span class="sep">|</span>' + stateLine +
            roleButtons() +
            '<span class="gist">' + esc(variant().gist) + '</span>' +
            '<button data-act="toggle-state">' + (S.showState ? 'Hide state' : 'Show state') + '</button>' +
            '</div>' + statePanel;
    }

    function renderNote() {
        document.getElementById('proto-note').textContent =
            'PROTOTYPE \u00b7 mock data, nothing reads a server \u00b7 variant ' + S.variant + ' live \u00b7 state ' +
            S.activity + ' \u00b7 ' + role().person + ', ' + role().label + ', party ' + PARTY.code;
    }

    function render() {
        var body = pageHead() +
            fixtureNote() +
            (S.variant === 'A' ? variantA() : (S.variant === 'B' ? variantB() : variantC())) +
            (!maySchedule()
                ? '<div class="notice">Read-only. Your role is ' + esc(role().label) + ', and only a tenant administrator ' +
                  'may set a schedule. The cadence, the expression and the next fire times are shown to you; the write ' +
                  'controls are not. Ask a tenant administrator of ' + esc(PARTY.code) + ' to change it.</div>'
                : '') +
            reachCard() +
            scopeCard() +
            gapCard();

        document.getElementById('app').innerHTML =
            '<div class="shell">' + appHeader() + '<main>' + body + '</main></div>' +
            dialog();

        renderNote();
        renderBar();
    }

    // ------------------------------------------------------------ actions

    function setActivity(text) {
        S.activity = text;
    }

    function openEditor(id) {
        var definition = defById(id);
        if (!definition || !maySchedule()) return;
        S.open = S.open === id ? null : id;
        S.confirm = null;
        setActivity(S.open === null ? 'rest' : 'editing ' + definition.name);
        logStep('opened the schedule editor for ' + definition.name);
        render();
        writeParams();
    }

    function readParams() {
        var params = new URLSearchParams(window.location.search);
        var wanted = (params.get('variant') || '').toUpperCase();
        if (VARIANTS.some(function (entry) { return entry.id === wanted; })) S.variant = wanted;
        var roleParam = params.get('role');
        if (roleParam === 'admin' || roleParam === 'analyst') S.role = roleParam;
    }

    function writeParams() {
        var params = new URLSearchParams();
        params.set('variant', S.variant);
        params.set('role', S.role);
        window.history.replaceState(null, '', window.location.pathname + '?' + params.toString());
    }

    document.addEventListener('click', function (event) {
        var element = event.target.closest('[data-act]');
        if (!element) return;
        if (element.tagName !== 'BUTTON' && !element.classList.contains('chip')) return;
        event.preventDefault();
        var act = element.getAttribute('data-act');
        var id = element.getAttribute('data-id');

        if (act === 'variant') {
            S.variant = element.getAttribute('data-variant');
            S.open = null;
            S.confirm = null;
            setActivity('rest');
            logStep('switched to variant ' + S.variant);
        } else if (act === 'role') {
            S.role = element.getAttribute('data-role');
            S.open = null;
            S.confirm = null;
            setActivity(maySchedule() ? 'rest' : 'read-only: ' + role().label);
            logStep('role set to ' + role().label.toLowerCase());
        } else if (act === 'open-editor') {
            openEditor(id);
            return;
        } else if (act === 'close-editor') {
            S.open = null;
            setActivity('rest');
            render();
            writeParams();
            return;
        } else if (act === 'ask-schedule') {
            S.confirm = { id: id, action: 'schedule' };
            setActivity('confirming schedule \u00b7 ' + defById(id).name);
            logStep('asked to schedule ' + defById(id).name + ' as ' + exprOf(draftOf(id)));
        } else if (act === 'ask-runnow') {
            S.confirm = { id: id, action: 'runnow' };
            setActivity('confirming run now \u00b7 ' + defById(id).name);
            logStep('asked to run ' + defById(id).name + ' now, leaving the schedule alone');
        } else if (act === 'ask-pause') {
            S.confirm = { id: id, action: 'pause' };
            setActivity('confirming pause \u00b7 ' + defById(id).name);
            logStep('asked to pause the schedule of ' + defById(id).name + ' \u2014 no pause operation exists');
        } else if (act === 'ask-play') {
            S.confirm = { id: id, action: 'play' };
            setActivity('confirming play \u00b7 ' + defById(id).name);
            logStep('asked to play the schedule of ' + defById(id).name + ' \u2014 no play operation exists');
        } else if (act === 'ask-unschedule') {
            S.confirm = { id: id, action: 'unschedule' };
            setActivity('confirming unschedule \u00b7 ' + defById(id).name);
            logStep('asked to unschedule ' + defById(id).name + ' \u2014 the schedule goes, the definition stays');
        } else if (act === 'confirm-cancel') {
            S.confirm = null;
            setActivity(S.open === null ? 'rest' : 'editing ' + defById(S.open).name);
            logStep('cancelled, nothing sent');
        } else if (act === 'confirm-go') {
            confirmWrite();
            return;
        } else if (act === 'toggle-state') {
            S.showState = !S.showState;
            render();
            return;
        }
        render();
        writeParams();
    });

    document.addEventListener('change', function (event) {
        var element = event.target.closest('[data-act]');
        if (!element || (element.tagName !== 'SELECT' && element.tagName !== 'INPUT')) return;
        var act = element.getAttribute('data-act');
        var id = element.getAttribute('data-id');
        if (act !== 'cadence' && act !== 'time') return;
        var draft = draftOf(id);
        if (act === 'cadence') draft.cadence = element.value;
        if (act === 'time') draft.time = element.value;
        setActivity('editing ' + defById(id).name + ' \u00b7 ' + exprOf(draft));
        render();
    });

    function confirmWrite() {
        var definition = defById(S.confirm.id);
        var action = S.confirm.action;
        if (action === 'unschedule') {
            definition.expression = null;
            definition.jobId = null;
            definition.paused = false;
            logStep('would send reporting.v1.ops.unschedule_report_definitions for ' + definition.name + ' \u2014 nothing is sent \u00b7 ' + definition.runs + ' runs stay');
            setActivity('unscheduled \u00b7 ' + definition.name + ' is on demand');
        } else if (action === 'runnow') {
            logStep('would send reporting.v1.ops.trigger_report_instance for ' + definition.name + ' \u2014 nothing is sent \u00b7 the schedule is unchanged');
            setActivity('running now \u00b7 ' + definition.name + ', one run started');
        } else if (action === 'pause') {
            definition.paused = true;
            logStep('would pause the schedule of ' + definition.name + ' by setting the job\u2019s is_active to false \u2014 no operation exists, nothing is sent \u00b7 ' + definition.expression + ' is kept');
            setActivity('paused \u00b7 ' + definition.name + ' keeps ' + definition.expression);
        } else if (action === 'play') {
            definition.paused = false;
            logStep('would play the schedule of ' + definition.name + ' by setting the job\u2019s is_active to true \u2014 no operation exists, nothing is sent \u00b7 ' + definition.expression + ' is unchanged');
            setActivity('playing \u00b7 ' + definition.name + ', ' + cadenceLabel(definition.expression));
        } else {
            var expression = exprOf(draftOf(definition.id));
            definition.expression = expression;
            definition.jobId = 'sched.job.' + (Math.floor(Math.random() * 0xffffff) + 0x100000).toString(16);
            definition.paused = false;
            logStep('would send reporting.v1.ops.schedule_report_definitions for ' + definition.name + ' as ' + expression + ' \u2014 nothing is sent');
            setActivity('scheduled \u00b7 ' + definition.name + ', ' + cadenceLabel(expression));
        }
        S.confirm = null;
        S.open = null;
        render();
        writeParams();
    }

    S.defs = copyDefinitions();
    readParams();
    if (!maySchedule()) setActivity('read-only: ' + role().label);
    render();
    writeParams();
}());
