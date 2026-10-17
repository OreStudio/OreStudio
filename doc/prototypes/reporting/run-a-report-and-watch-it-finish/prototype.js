/* Reporting: run a report and watch it finish. Self-contained: plain
 * JavaScript, mock data, no framework, no build step, nothing that outlives
 * the page. Opens from file://.
 *
 * The journey this answers to is
 * doc/knowledge/journeys/reporting/journey_run_a_report_and_watch_it_finish.org.
 *
 * It answers one question: how do the five reporting journeys become one
 * reporting surface without a screen per journey? For this journey the question
 * is how a person starts a run and then knows when to stop waiting. Three
 * structures answer it, and they disagree about structure rather than colour:
 *
 *   A, modal-and-notify. The catalogue stays the page. A dialog confirms the
 *      run, then the page returns to the catalogue and a strip at the top
 *      reports the run while the person works.
 *   B, run-theatre. Starting a run opens a dedicated view of that one run: the
 *      six phases left to right, the current one live, and no way out until
 *      the run is terminal.
 *   C, queue-first. Starting a run drops a row into a queue of the person's
 *      runs. The queue is the screen, and several runs advance together.
 *
 * Everything on the page is a fixture. No reporting read serves the step a run
 * is on -- the phases animate on a scripted clock so a person flipping variants
 * can watch a run advance. The pipeline is six phases and seven workflow steps,
 * and the page says which is which: pre-processing is four workflow steps,
 * retrieval and post-processing are one workflow step. Execution runs on a
 * worker node, so ORE Studio submits the work and waits. Only finalisation
 * touches the report instance's state machine. */

(function () {
    'use strict';

    var PHASE_MS = 700;
    var TICK_MS = 50;
    var RUN_MS = PHASE_MS * 6;

    /* ------------------------------------------------------------ phases */

    /* Six phases in their fixed order. `platform` marks execution: it happens
     * on a worker node and no ORE Studio service performs it. */
    var PHASES = [
        { key: 'preproc', name: 'Pre-processing', short: 'pre' },
        { key: 'submit', name: 'Submission', short: 'submit' },
        { key: 'execute', name: 'Execution', short: 'exec', platform: true },
        { key: 'retrieve', name: 'Retrieval', short: 'retrieve', note: 'with post-processing, one workflow step' },
        { key: 'postproc', name: 'Post-processing', short: 'post', note: 'with retrieval, one workflow step' },
        { key: 'finalise', name: 'Finalisation', short: 'finalise', stateMachine: true }
    ];

    /* Seven workflow steps, in the order the executing shape runs them. Each
     * names the phase it runs in. A run that substitutes pre-processing starts
     * at step 4' instead of steps 1 to 4. */
    var STEPS_EXECUTE = [
        { op: 'reporting.v1.ops.gather_trades', phase: 'Pre-processing' },
        { op: 'reporting.v1.ops.gather_market_data', phase: 'Pre-processing' },
        { op: 'reporting.v1.ops.assemble_bundle', phase: 'Pre-processing' },
        { op: 'ore.v1.ops.prepare_ore_package', phase: 'Pre-processing' },
        { op: 'compute.v1.ops.submit_compute', phase: 'Submission' },
        { op: 'reporting.v1.ops.collect_compute_results', phase: 'Retrieval and post-processing' },
        { op: 'reporting.v1.ops.finalise_report', phase: 'Finalisation' }
    ];

    var STEPS_SUBSTITUTE = [
        { op: 'reporting.v1.ops.resolve_prepared_input', phase: 'Pre-processing', substituted: 'prepare_ore_package' },
        { op: 'reporting.v1.ops.ignore_compute_results', phase: 'Retrieval and post-processing', substituted: 'collect_compute_results' },
        { op: 'reporting.v1.ops.finalise_report', phase: 'Finalisation' }
    ];

    var PARTIES = ['ACMCOR', 'ACCOUK', 'ACCOUS', 'ACCOHK'];

    /* The definitions a person can run. The first three are the demonstration
     * runs, each with its own ending: clean, completed while carrying errors,
     * and failed. `substitutes` draws the four-step chain beside the seven-step
     * one the journey asks the prototype to show. */
    var DEFS = [
        {
            id: 'd-headline',
            name: 'Headline Position',
            party: 'ACMCOR',
            type: 'Position',
            schedule: 'On demand',
            scope: 'book ACME-IRS-01, market data 2026-10-16',
            executes: 'pre-processing executes; post-processing executes',
            steps: 'execute',
            latest: 'completed',
            latestAt: '2026-10-16 14:11:02'
        },
        {
            id: 'd-large-exposures',
            name: 'Large Exposures',
            party: 'ACMCOR',
            type: 'Exposure',
            schedule: 'Weekdays 05:00',
            scope: 'portfolio ACME-RATES-EU, market data 2026-10-16',
            executes: 'pre-processing executes; post-processing executes',
            steps: 'execute',
            latest: 'completed with errors',
            latestAt: '2026-10-16 05:00:44'
        },
        {
            id: 'd-liquidity-coverage',
            name: 'Liquidity Coverage',
            party: 'ACCOUK',
            type: 'Regulatory',
            schedule: 'Weekdays 05:15',
            scope: 'portfolio ACME-LIQ-UK, market data 2026-10-16',
            executes: 'pre-processing executes; post-processing executes',
            steps: 'execute',
            latest: 'failed',
            latestAt: '2026-10-16 05:16:31'
        },
        {
            id: 'd-group-consolidated',
            name: 'Group Consolidated Exposure',
            party: 'ACMCOR',
            type: 'Exposure',
            schedule: 'Month end 23:00',
            scope: 'portfolio ACME-GROUP, market data 2026-10-16',
            executes: 'pre-processing executes; post-processing executes',
            steps: 'execute',
            latest: 'completed',
            latestAt: '2026-09-30 23:04:18'
        },
        {
            id: 'd-liquidity-buffer',
            name: 'Bucketed DV01',
            party: 'ACCOUS',
            type: 'Liquidity',
            schedule: 'On demand',
            scope: 'portfolio ACME-LIQ-US, market data 2026-10-16',
            executes: 'pre-processing executes; post-processing executes',
            steps: 'execute',
            latest: 'completed',
            latestAt: '2026-10-15 18:22:09'
        },
        {
            id: 'd-smoke',
            name: 'Board Risk Dashboard',
            party: 'ACCOHK',
            type: 'Dashboard',
            schedule: 'On demand',
            scope: 'portfolio ACME-BOARD-HK, market data 2026-10-16',
            executes: 'pre-processing resolves a prepared input; post-processing is ignored',
            steps: 'substitute',
            latest: 'completed',
            latestAt: '2026-10-14 09:40:02',
            substitution: true
        },
        {
            id: 'd-leverage',
            name: 'Leverage Ratio',
            party: 'ACCOUK',
            type: 'Regulatory',
            schedule: 'Weekdays 05:30',
            scope: 'portfolio ACME-RATES-UK, market data 2026-10-16',
            executes: 'pre-processing executes; post-processing executes',
            steps: 'execute',
            latest: 'completed',
            latestAt: '2026-10-16 05:31:12'
        }
    ];

    /* The three demonstration runs, by ending. `failAt` is the phase the run
     * dies in: the engine or the collection did not produce what the next
     * phase needs, and the run terminates with a report-scoped error rather
     * than sitting mid-flight. */
    var BLUEPRINTS = [
        {
            defId: 'd-headline',
            ending: 'clean',
            terminalPhase: false
        },
        {
            defId: 'd-large-exposures',
            ending: 'errors',
            terminalPhase: false,
            errors: 2,
            worst: 'Warning'
        },
        {
            defId: 'd-liquidity-coverage',
            ending: 'failed',
            terminalPhase: false,
            failAt: 3
        }
    ];

    var ERROR_ROWS = [
        { severity: 'Warning', at: '14:32:06', body: 'trade 7a41c9d2 would not build: missing market data for ORES-IRS-9Y-USD', scope: 'ores.ore, step prepare_ore_package' },
        { severity: 'Warning', at: '14:32:08', body: 'the engine wrote 1 warning to its own log; the report still executed', scope: 'worker node, step submit_compute' }
    ];

    var FAILED_ERROR = {
        severity: 'Error',
        at: '14:32:07',
        body: 'the compute batch did not close: workunit f7c1 stayed dispatched on node grid-03',
        scope: 'Report'
    };

    var VARIANTS = [
        {
            id: 'A',
            name: 'A \u00b7 modal-and-notify',
            gist: 'Starting a run is a dialog over the catalogue, and the run reports itself in a strip at the top, so the person keeps working.',
            argues: 'The catalogue is the screen. The run is a strip, not a destination: confirm in a dialog, return to where you were, and read the outcome wherever you are.'
        },
        {
            id: 'B',
            name: 'B \u00b7 run-theatre',
            gist: 'Starting a run opens a dedicated view of that one run; the six phases advance left to right and the view does not leave until the run is terminal.',
            argues: 'A run is a destination while it runs. The theatre shows the six phases and the step the run is on, and it holds the person there until the run reaches a terminal state.'
        },
        {
            id: 'C',
            name: 'C \u00b7 queue-first',
            gist: 'Starting a run drops a row into a queue of the person\u2019s runs; each row advances in place, and several runs can be watched together.',
            argues: 'The queue is the screen. A run is a row that advances in place, so starting three is the same gesture as starting one, and no run hides the others.'
        }
    ];

    /* --------------------------------------------------------------- state */

    var S = {
        variant: 'A',
        view: null,          /* variant-specific: A 'catalogue' | 'run', B 'gate' | 'run' */
        defId: 'd-headline', /* the definition the picker names, C only */
        runs: [],
        nextId: 0,
        confirm: null,       /* the definition a dialog or gate holds, or null */
        stripDismissed: false,
        clock: 0,
        timer: null,
        timerOrigin: 0,
        speed: 1,
        ending: null,
        autoStarted: false,
        log: []
    };

    function variantById(id) { return VARIANTS.filter(function (v) { return v.id === id; })[0]; }
    function defById(id) { return DEFS.filter(function (d) { return d.id === id; })[0]; }
    function blueprintFor(index) {
        var b = BLUEPRINTS[index % BLUEPRINTS.length];
        return { defId: b.defId, ending: b.ending, failAt: b.failAt, errors: b.errors, worst: b.worst };
    }

    function esc(value) {
        return String(value).replace(/[&<>"']/g, function (c) {
            return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c];
        });
    }

    function pad(n) { return (n < 10 ? '0' : '') + n; }

    /* The scripted clock, a run's elapsed milliseconds, and what that elapsed
     * time means. `endedAt` is a run's own scripted end, clamped so the last
     * tick lands exactly on the terminal state. */
    function runDuration(run) {
        return run.endedAt - run.startedAt;
    }

    /* Elapsed is clamped to the run's own duration. `endedAt` is an offset from
     * the start of the session's scripted clock, not from the run's start, so
     * the clamp is against the duration: a queued row that started late must
     * still stop at its own terminal state. */
    function elapsedOf(run) {
        return Math.max(0, Math.min(runDuration(run), S.clock - run.startedAt));
    }

    function phaseIndexAt(ms) {
        if (ms >= RUN_MS) return 6;
        return Math.floor(ms / PHASE_MS);
    }

    function fmtClock(ms) {
        var secs = ms / 1000;
        if (secs < 60) return secs.toFixed(1) + ' s';
        return Math.floor(secs / 60) + ':' + ((secs % 60 < 10 ? '0' : '') + (secs % 60).toFixed(1));
    }

    function wallTime(base, ms) {
        var total = base + ms / 1000;
        var h = Math.floor(total / 3600);
        var m = Math.floor((total % 3600) / 60);
        var s = total % 60;
        return pad(h) + ':' + pad(m) + ':' + (s < 10 ? '0' : '') + s.toFixed(3);
    }

    var BASE_SECONDS = 14 * 3600 + 32 * 60 + 5;

    /* --------------------------------------------------------------- runs */

    function startRun(blueprint) {
        var def = defById(blueprint.defId);
        var index = S.nextId++;
        var run = {
            id: index,
            defId: def.id,
            party: def.party,
            name: def.name,
            steps: def.steps,
            scope: def.scope,
            executes: def.executes,
            schedule: def.schedule,
            type: def.type,
            ending: blueprint.ending,
            errors: blueprint.errors || 0,
            worst: blueprint.worst,
            failAt: blueprint.failAt,
            startedAt: S.clock,
            endedAt: S.clock + (blueprint.failAt === undefined ? RUN_MS : (blueprint.failAt + 1) * PHASE_MS),
            triggerId: 'b1e7' + pad(index) + 'c4',
            id8: 'ri-' + (0x9fccf6c8 + index * 7919).toString(16)
        };
        S.runs.push(run);
        S.log.push('Run now \u00b7 ' + def.name + ' \u00b7 ' + def.party +
            ' \u00b7 reporting.v1.ops.trigger_report_instance answered ' + run.id8);
        return run;
    }

    function dropQueueDemo() {
        startRun({ defId: 'd-headline', ending: 'clean' });
        startRun({ defId: 'd-large-exposures', ending: 'errors', errors: 2, worst: 'Warning' });
        startRun({ defId: 'd-liquidity-coverage', ending: 'failed', failAt: 3 });
        S.runs[S.runs.length - 3].startedAt = 0;
        S.runs[S.runs.length - 3].endedAt = RUN_MS;
        S.runs[S.runs.length - 2].startedAt = 1400;
        S.runs[S.runs.length - 2].endedAt = 1400 + RUN_MS;
        S.runs[S.runs.length - 1].startedAt = 2800;
        S.runs[S.runs.length - 1].endedAt = 2800 + 4 * PHASE_MS;
        S.log.push('Demo \u00b7 three runs dropped into the queue: one clean, one carrying errors, one failing');
    }

    function startDemo() {
        S.stripDismissed = false;
        S.confirm = null;
        if (S.variant === 'C') {
            if (S.timer !== null) clearInterval(S.timer);
            dropQueueDemo();
        } else {
            startRun(blueprintFor(S.runs.length));
        }
        if (S.variant === 'A') S.view = 'catalogue';
        else if (S.variant === 'B') S.view = 'run';
        restartClock();
        render();
    }

    function currentRun() {
        return S.runs.length ? S.runs[S.runs.length - 1] : null;
    }

    function runState(run) {
        if (!run) return 'idle';
        return S.clock >= run.endedAt ? run.ending : 'running';
    }

    function runPhaseIndex(run) {
        if (!run) return -1;
        return phaseIndexAt(elapsedOf(run));
    }

    /* The state of one phase in one run, which is the whole vocabulary the
     * page needs: done, running, unattended (execution, the worker node),
     * blocked (the run died before reaching it), failed (the run died here) and
     * terminal, which means the run reached a terminal state in that phase. */
    function phaseState(run, phaseKey, index) {
        var phase = PHASES[index];
        var state = runState(run);
        var at = runPhaseIndex(run);
        var terminal = state !== 'running' && state !== 'idle';

        if (terminal) {
            if (state === 'failed') {
                if (run.failAt !== undefined) {
                    if (index < run.failAt) return 'done';
                    if (index === run.failAt) return 'failed';
                    return 'blocked';
                }
            }
            if (index < at) return 'done';
            if (index === at) return state === 'failed' ? 'terminal-bad' : (state === 'errors' ? 'terminal-err' : 'terminal-ok');
            return 'blocked';
        }

        if (at > index) return 'done';
        if (at === index) return phase.platform ? 'unattended' : 'running';
        return 'pending';
    }

    function runFailTerminal(run) { return run.ending === 'failed'; }

    function phaseLabel(state) {
        return {
            pending: 'waiting',
            running: 'running',
            unattended: 'on a worker node',
            done: 'done',
            failed: 'failed here',
            blocked: 'not reached',
            'terminal-ok': 'completed clean',
            'terminal-err': 'completed, errors',
            'terminal-bad': 'failed'
        }[state] || state;
    }

    /* ------------------------------------------------------------ rendering */

    function phaseBar(run, compact) {
        return '<div class="phasebar' + (compact ? ' compact' : '') + '">' +
            PHASES.map(function (phase, index) {
                var state = phaseState(run, phase.key, index);
                var title = phase.name + ' \u00b7 ' + phaseLabel(state) +
                    (phase.platform ? ' \u00b7 worker node, not an ORE Studio service' : '') +
                    (phase.stateMachine ? ' \u00b7 the only phase that touches the state machine' : '') +
                    (phase.note ? ' \u00b7 ' + phase.note : '');
                return '<div class="ph ' + state + '" title="' + esc(title) + '">' +
                    '<span class="ph-n">' + (index + 1) + '</span>' +
                    '<span class="ph-name">' + esc(compact ? phase.short : phase.name) + '</span>' +
                    '</div>';
            }).join('') + '</div>';
    }

    function stateTag(run) {
        var state = runState(run);
        if (state === 'running') {
            return '<span class="tag accent"><span class="live-dot"></span>running</span>';
        }
        if (state === 'clean') return '<span class="tag ok">completed</span>';
        if (state === 'errors') return '<span class="tag warn">completed, carrying errors</span>';
        if (state === 'failed') return '<span class="tag bad">failed</span>';
        return '<span class="tag muted">not started</span>';
    }

    function elapsedLine(run) {
        if (!run) return 'no run';
        return runState(run) === 'running'
            ? fmtClock(elapsedOf(run)) + ' elapsed'
            : fmtClock(run.endedAt - run.startedAt) + ' took';
    }

    /* The terminal panel: the run's state, its output message, its times, the
     * errors it carried, and what it left in storage. One panel for all three
     * endings, because all three are terminal states of one entity. */
    function terminalPanel(run, options) {
        options = options || {};
        var state = runState(run);
        if (state === 'running') return '';
        var clean = state === 'clean';
        var errors = state === 'errors';
        var failed = state === 'failed';
        var cls = clean ? 'ok' : (failed ? 'bad' : 'err');
        var duration = fmtClock(run.endedAt - run.startedAt);
        var started = wallTime(BASE_SECONDS, run.startedAt);
        var completed = wallTime(BASE_SECONDS, run.endedAt);
        var msg = failed ? 'Report execution failed.' : 'Report execution completed.';

        var head = failed
            ? '<span class="tag bad">failed</span><h3>Run failed</h3>'
            : (errors
                ? '<span class="tag warn">completed, carrying errors</span><h3>Run completed while carrying errors</h3>'
                : '<span class="tag ok">completed</span><h3>Run completed clean</h3>');

        var facts = '<div class="t-facts">' +
            '<div class="f"><span class="fk">started_at</span><span class="fv">2026-10-16 ' + started + '</span></div>' +
            '<div class="f"><span class="fk">completed_at</span><span class="fv">2026-10-16 ' + completed + '</span></div>' +
            '<div class="f"><span class="fk">duration</span><span class="fv">' + duration + '</span></div>' +
            '<div class="f"><span class="fk">trigger_id \u00b7 on-demand</span><span class="fv">' + esc(run.triggerId) + '</span></div>' +
            (failed
                ? '<div class="f"><span class="fk">output_storage_key</span><span class="fv">none \u00b7 no archive was written</span></div>' +
                  '<div class="f"><span class="fk">errors</span><span class="fv">1 error \u00b7 Report scope</span></div>'
                : '<div class="f"><span class="fk">output_storage_key</span><span class="fv">reporting/runs/' + esc(run.id8) + '/output.tar.gz</span></div>' +
                  (errors
                      ? '<div class="f"><span class="fk">errors</span><span class="fv">' + run.errors + ' errors \u00b7 worst severity ' + esc(run.worst) + '</span></div>'
                      : '<div class="f"><span class="fk">errors</span><span class="fv">none recorded</span></div>')) +
            '</div>';

        var carrying = errors
            ? '<div class="notice warn"><b>Completed while carrying errors.</b> The engine finished and wrote its report, ' +
              'and said in its own log that a trade would not build. ' + run.errors + ' errors, worst severity ' + esc(run.worst) +
              '. This is not a failure: the run completed. <a href="../../../../knowledge/journeys/reporting/journey_find_out_why_a_run_went_wrong.org">Find out why a run went wrong</a>' +
              ' holds the errors. The trading desk cannot read them today: <span class="mono">reporting::report_error_events:read</span> sits at TenantAdmin.</div>'
            : '';

        var failure = failed
            ? '<div class="notice err"><b>Report-scoped error.</b> <span class="mono">' + esc(FAILED_ERROR.body) + '</span> ' +
              'The workflow compensated every step and <span class="mono">reporting.v1.ops.fail_report</span> closed the run at ' +
              'phase 4. It did not wait mid-flight, and it left no archive to open.</div>'
            : '';

        var actions = '<div class="t-actions">' +
            (failed
                ? '<a class="btn linklike" href="../../../../knowledge/journeys/reporting/journey_find_out_why_a_run_went_wrong.org">Read the run\u2019s errors</a>' +
                  '<button class="btn primary" data-act="start-demo">Run it again</button>'
                : '<a class="btn linklike" href="../../../../knowledge/journeys/reporting/journey_read_what_a_run_produced.org">Read what the run produced</a>' +
                  (errors ? '<a class="btn linklike" href="../../../../knowledge/journeys/reporting/journey_find_out_why_a_run_went_wrong.org">Find out why</a>' : '') +
                  '<button class="btn" data-act="start-demo">Run another</button>') +
            '<span class="hint">The definition\u2019s row now shows this run as its latest.</span>' +
            '</div>';

        return '<div class="terminal ' + cls + '">' +
            '<div class="t-head">' + head + '<span class="spacer"></span>' +
            '<span class="bigclock">' + duration + '</span></div>' +
            carrying + failure +
            '<div class="output"><span class="q">output_message</span>\u201c' + msg + '\u201d</div>' +
            (errors
                ? '<div class="steplist">' + ERROR_ROWS.slice(0, run.errors).map(function (e) {
                    return '<div class="srow"><span class="sn">' + esc(e.severity) + '</span>' +
                        '<span class="so">' + esc(e.body) + '</span>' +
                        '<span class="sr">' + esc(e.scope) + ' \u00b7 ' + esc(e.at) + '</span></div>';
                }).join('') + '</div>'
                : '') +
            facts + actions +
            '</div>';
    }

    /* The plan: six phases and the seven steps beneath them, with the
     * substitution shape beside it. All three variants carry it, because it is
     * the one fact all three must state. */
    function planPanel(def) {
        def = def || DEFS[0];
        var substitute = def.steps === 'substitute';
        var rows = [
            { name: 'Pre-processing', n: 1, steps: substitute
                ? [{ op: 'reporting.v1.ops.resolve_prepared_input', repl: true }]
                : [
                    { op: 'reporting.v1.ops.gather_trades' },
                    { op: 'reporting.v1.ops.gather_market_data' },
                    { op: 'reporting.v1.ops.assemble_bundle' },
                    { op: 'ore.v1.ops.prepare_ore_package' }
                ], note: substitute ? 'one workflow step, substituting the four' : 'four workflow steps, not one' },
            { name: 'Submission', n: 2, steps: [{ op: 'compute.v1.ops.submit_compute' }], note: 'one workflow step' },
            { name: 'Execution', n: 3, platform: true, steps: [{ op: 'run on a worker node' }], note: 'not a workflow step, and not an ORE Studio service' },
            { name: 'Retrieval', n: 4, steps: [], note: 'with post-processing, one workflow step' },
            { name: 'Post-processing', n: 5, steps: [{ op: substitute ? 'reporting.v1.ops.ignore_compute_results' : 'reporting.v1.ops.collect_compute_results' }], note: 'one workflow step, shared with retrieval' },
            { name: 'Finalisation', n: 6, steps: [{ op: 'reporting.v1.ops.finalise_report' }], note: 'the only phase that touches the state machine' }
        ];

        return '<section class="card" id="plan">' +
            '<header class="sectionhead"><h2>The pipeline: six phases, seven workflow steps</h2>' +
            '<span class="meta"><span class="tag">6 phases</span><span class="tag">' + (substitute ? '3' : '7') + ' steps in this shape</span></span></header>' +
            '<div class="counts">' +
            '<span class="hint">Phases are the pipeline\u2019s. Steps are what the definition\u2019s run configuration selects. ' +
            'Pre-processing is <b style="color:var(--ink);font-weight:400">four workflow steps, not one</b>; retrieval and post-processing are ' +
            '<b style="color:var(--ink);font-weight:400">one workflow step, not two</b>. Execution is neither: it runs on a worker node.</span>' +
            '</div>' +
            '<div class="ladder">' + rows.map(function (row) {
                return '<div class="lrow' + (row.platform ? ' platform' : '') + '">' +
                    '<div class="lname"><span class="lnum">' + row.n + '</span>' + esc(row.name) +
                    (row.platform ? '<span class="tag platform">worker node</span>' : '') + '</div>' +
                    '<div class="lsteps">' + (row.steps.length
                        ? row.steps.map(function (s) {
                            return '<span class="lstep">' + (s.repl ? '<b>' + esc(s.op) + '</b>' : esc(s.op)) + '</span>';
                        }).join('')
                        : '<span class="hint">the same workflow step as retrieval</span>') + '</div>' +
                    '<div class="lnote">' + esc(row.note) + '</div>' +
                    '</div>';
            }).join('') + '</div>' +
            '<p class="rowfoot">' + (substitute
                ? 'This definition substitutes both ends: it resolves a prepared input instead of gathering, and ignores the results instead of collecting them, so three steps run where seven do. The phases do not change.'
                : 'This definition executes both substitutable phases, so seven steps run. A definition that substitutes pre-processing starts at <span class="mono">resolve_prepared_input</span>; one that ignores post-processing uses <span class="mono">ignore_compute_results</span>. A run that cannot finish uses <span class="mono">fail_report</span>.') +
            '</p>' +
            '<div class="notice warn" style="margin-top:14px"><b>Honesty note.</b> In production this ladder is a plan, not a progress read: ' +
            'the run\u2019s state and times are reporting\u2019s, but the step in flight lives in <span class="mono">ores.workflow</span> and no reporting read serves it. ' +
            'This prototype animates the phases on a scripted timer so the waiting is observable; the real screen must not fake a moving bar until the read exists.</div>' +
            '</section>';
    }

    function gapsPanel() {
        var gaps = [
            {
                title: 'No per-step progress',
                body: 'The reporting service states the run\u2019s state and its times. The step the workflow is on lives in ores.workflow, and no reporting read serves it. A read carrying the workflow instance\u2019s current step is the missing piece.'
            },
            {
                title: 'The run does not name its workflow instance',
                body: 'report_instance carries trigger_id, fsm_state_id, the times and the output, and no field for the workflow instance that runs it. Even with step progress available, the screen has no key to reach it from the run.'
            },
            {
                title: 'The trigger is written and unreadable',
                body: 'The press records a report_trigger_event with the definition, type, person and time, and no read of it exists over NATS. The run\u2019s trigger_id names it, and nothing resolves that id to a row.'
            },
            {
                title: 'No cancellation',
                body: 'A press creates a run, and no operation stops one. A run started by mistake, or one waiting on a worker node, has to be waited out. This prototype offers no Stop action rather than one the service would refuse.'
            },
            {
                title: 'The error permissions sit too high',
                body: 'reporting::report_error_events:read is held by no role below TenantAdmin. A trading desk that runs a report cannot read the errors that say it completed carrying problems, so the terminal panel\u2019s count and link wait on that grant.'
            },
            {
                title: 'The concurrency policy is the definition\u2019s',
                body: 'The definition carries concurrency_policy and the screen does not evaluate it. Variant C draws a queue the person owns; the service decides whether a trigger is refused, and the reply\u2019s shape for a refusal is not part of this journey.'
            }
        ];
        return '<section class="card gaps">' +
            '<header class="sectionhead"><h2>What this mock cannot show</h2>' +
            '<span class="faint" style="font-size:12px">each gap names the journey that records it</span></header>' +
            '<dl>' + gaps.map(function (gap) {
                return '<div><dt>' + esc(gap.title) + '</dt><dd>' + esc(gap.body) + '</dd></div>';
            }).join('') + '</dl></section>';
    }

    function arguesPanel(variant) {
        return '<section class="card">' +
            '<header class="sectionhead"><h2>What this variant argues</h2>' +
            '<span class="meta"><span class="tag accent">variant ' + esc(variant.id) + '</span>' +
            '<span class="faint" style="font-size:12px">switch with the buttons in the foot bar, or ?variant=A|B|C</span></span></header>' +
            '<p class="dim" style="font-size:13px;max-width:86ch">' + esc(variant.argues) + '</p>' +
            '<div class="argues" style="margin-top:14px">' +
            '<div class="arg"><h3>Same facts, all three</h3><p>Six phases, seven steps, execution outside the platform, finalisation alone at the state machine, a terminal state with the output message, and completion that can carry errors.</p></div>' +
            '<div class="arg"><h3>Where it disagrees</h3><p>' + esc(disagreement(variant.id)) + '</p></div>' +
            '</div></section>';
    }

    function disagreement(id) {
        if (id === 'A') return 'Leaving the run. A sends the person back to the catalogue and makes the run report itself; the run is interruptible and passive.';
        if (id === 'B') return 'Leaving the run. B makes one run a destination and holds the person there until it is terminal; the run is interruptible and the page is busy.';
        return 'The count. C assumes a person starts several runs and watches them together, so the unit of attention is the queue, not the run.';
    }

    /* ---------------------------------------------------------- variant A */

    function pageA() {
        var run = currentRun();
        var state = run ? runState(run) : 'idle';
        var showingStrip = run && !S.stripDismissed;
        var stripTerminal = state !== 'running' && state !== 'idle';

        return '<div class="shell' + (showingStrip ? ' has-strip' : '') + '">' +
            appHeader('Reports', 'variant A \u00b7 modal-and-notify') +
            (showingStrip ? runStripA(run, state) : '') +
            '<main>' +
            pageHeadA() +
            fixtureNote() +
            cataloguePanel(run) +
            (run && stripTerminal ? '<section class="card"><header class="sectionhead"><h2>Last run</h2>' +
                '<span class="meta"><span class="tag muted">run ' + esc(run.id8) + '</span></span></header>' +
                terminalPanel(run, { inline: true }) +
                '<p class="rowfoot">The strip and this panel say the same thing, because the strip can be dismissed and the run must still be readable. ' +
                'A person who dismissed the strip finds the run from the definition\u2019s latest column.</p></section>' : '') +
            planPanel(DEFS.filter(function (d) { return d.id === (S.confirm || (run && run.defId) || 'd-headline'); })[0]) +
            arguesPanel(variantById('A')) +
            gapsPanel() +
            '</main></div>' +
            (S.confirm ? confirmDialog(S.confirm) : '');
    }

    function pageHeadA() {
        return '<header class="pagehead"><div>' +
            '<div class="crumbs"><a href="../index.org">Prototypes</a><span>\u203a</span><a href="../../../../knowledge/journeys/reporting/reporting.org">Reporting</a><span>\u203a</span><span>Run a report</span></div>' +
            '<h1>Reports</h1>' +
            '<p class="lede">The definitions your party runs, each with the scope a run will read and the configuration it will use. ' +
            'Run one on demand, then carry on where you were: the run reports itself.</p>' +
            '</div><div class="actions">' +
            '<button class="btn primary" data-act="start-demo">Run the demonstration</button>' +
            '<a class="btn linklike" href="../index.org">Back to prototypes</a>' +
            '</div></header>';
    }

    function runStripA(run, state) {
        var terminal = state !== 'running';
        var cls = terminal ? ' terminal-' + (state === 'clean' ? 'ok' : (state === 'failed' ? 'bad' : 'err')) : '';
        var text;
        if (!terminal) {
            var at = runPhaseIndex(run);
            text = 'phase ' + (at + 1) + ' of 6 \u00b7 <b>' + esc(PHASES[at].name) + '</b> \u00b7 <b>' + fmtClock(elapsedOf(run)) + '</b>';
        } else if (state === 'clean') {
            text = '<b>Report execution completed.</b> ' + esc(run.name) + ' \u00b7 took ' + fmtClock(run.endedAt - run.startedAt);
        } else if (state === 'errors') {
            text = '<b>Report execution completed.</b> ' + esc(run.name) + ' completed while carrying ' + run.errors + ' errors, worst ' + esc(run.worst) + ' \u00b7 took ' + fmtClock(run.endedAt - run.startedAt);
        } else {
            text = '<b>Report execution failed.</b> ' + esc(run.name) + ' \u00b7 report-scoped error at phase 4 \u00b7 no archive';
        }

        return '<div class="runstrip' + cls + '"><div class="strip-inner">' +
            '<span class="strip-tag">' + (terminal ? 'run finished' : 'run in flight') + '</span>' +
            '<span class="strip-body">' +
            '<span class="strip-text">' + esc(run.name) + ' \u00b7 ' + esc(run.party) + '</span>' +
            phaseBar(run, true) +
            '<span class="strip-text">' + text + '</span>' +
            '</span>' +
            '<span class="strip-actions">' +
            (terminal
                ? '<button data-act="start-demo">Run again</button>'
                : '<button data-act="jump">Watch it</button>') +
            '<button class="strip-dismiss" data-act="dismiss-strip" title="Dismiss the strip; the run stays readable on the catalogue">\u00d7</button>' +
            '</span></div></div>';
    }

    function cataloguePanel(run) {
        var rows = DEFS.map(function (def) {
            var isThis = run && run.defId === def.id && run.id === S.runs[S.runs.length - 1].id;
            var latest = def.latest;
            var latestTag = latest === 'completed' ? '<span class="tag ok">completed</span>'
                : latest === 'completed with errors' ? '<span class="tag warn">completed, errors</span>'
                : '<span class="tag bad">failed</span>';
            return '<tr' + (isThis ? '' : '') + '>' +
                '<td class="def-name">' + esc(def.name) + '<span class="sub">' + esc(def.type) + ' \u00b7 ' + esc(def.party) + '</span>' +
                (def.substitution ? '<span class="tag accent" style="margin-top:4px">substitutes both ends</span>' : '') + '</td>' +
                '<td class="scope">' + esc(def.scope) + '</td>' +
                '<td><div class="runconfig"><span>' + esc(def.executes) + '</span><span class="faint">' + (def.steps === 'substitute' ? '3 workflow steps' : '7 workflow steps') + '</span></div></td>' +
                '<td><div class="latest">' + latestTag +
                '<span class="faint">' + esc(def.latestAt) + '</span>' +
                (isThis && run
                    ? '<span class="lastrun">' + stateTag(run) + ' <span class="faint">this session \u00b7 ' + elapsedLine(run) + '</span></span>'
                    : '') + '</div></td>' +
                '<td><button class="btn" data-act="ask-run" data-def="' + esc(def.id) + '"' +
                (S.confirm ? ' disabled' : '') + '>Run now</button></td>' +
                '</tr>';
        }).join('');

        return '<section class="card">' +
            '<header class="sectionhead"><h2>The definitions this party runs</h2>' +
            '<span class="meta"><span class="tag">' + DEFS.length + ' definitions</span>' +
            '<span class="faint" style="font-size:12px">scope and run configuration come from the definition\u2019s configuration</span></span></header>' +
            '<div class="table-wrap"><table class="defs"><thead><tr>' +
            '<th>Definition</th><th>Scope the run will read</th><th>Run configuration</th><th>Latest run</th><th></th>' +
            '</tr></thead><tbody>' + rows + '</tbody></table></div>' +
            '<p class="rowfoot">Press <b>Run now</b> and the system asks you to confirm, restating the definition, its scope and its run configuration: a run costs compute and writes rows. ' +
            'The catalogue does not change while the run does.</p>' +
            '</section>';
    }

    function confirmDialog(def) {
        return '<div class="scrim" data-act="scrim">' +
            '<div class="dialog" role="dialog" aria-modal="true" aria-label="Confirm the run">' +
            '<div class="dialog-head"><h2 class="dialog-title">Run ' + esc(def.name) + ' now?</h2>' +
            '<p class="dialog-sub">One run of this definition, as it stands. It costs compute and writes rows.</p></div>' +
            '<div class="dialog-body">' +
            '<table class="kv"><tbody>' +
            '<tr><td>Definition</td><td>' + esc(def.name) + ' \u00b7 ' + esc(def.type) + ' \u00b7 ' + esc(def.party) + '</td></tr>' +
            '<tr><td>Scope the run will read</td><td>' + esc(def.scope) + '</td></tr>' +
            '<tr><td>Schedule it carries</td><td>' + esc(def.schedule) + ' \u00b7 this press is on-demand</td></tr>' +
            '<tr><td>Run configuration</td><td>' + esc(def.executes) + '</td></tr>' +
            '<tr><td>Latest run</td><td>' + esc(def.latest) + ' \u00b7 ' + esc(def.latestAt) + '</td></tr>' +
            '</tbody></table>' +
            '<div><p class="hint" style="margin-bottom:6px">The workflow steps this run will take: <b style="color:var(--ink);font-weight:500">' +
            (def.steps === 'substitute' ? '3 steps' : '7 steps') + '</b>, across six phases.</p>' +
            '<div class="steplist">' + (def.steps === 'substitute' ? STEPS_SUBSTITUTE : STEPS_EXECUTE).map(function (s, i) {
                return '<div class="srow' + (s.substituted ? ' repl' : '') + '">' +
                    '<span class="sn">' + (i + 1) + '</span>' +
                    '<span class="so">' + esc(s.op) + '</span>' +
                    '<span class="sr">' + (s.substituted ? 'replaces ' + esc(s.substituted) : '') + ' \u00b7 ' + esc(s.phase) + '</span>' +
                    '</div>';
            }).join('') + '</div></div>' +
            '<div class="notice">On confirmation the screen sends one request, <span class="mono">reporting.v1.ops.trigger_report_instance</span>, naming the definition. ' +
            'The service records the trigger, creates the run and starts the workflow. This press is the screen\u2019s only write.</div>' +
            '</div>' +
            '<div class="dialog-foot">' +
            '<button class="btn ghost" data-act="cancel-confirm">Cancel</button>' +
            '<button class="btn primary" data-act="confirm-run">Confirm and run</button>' +
            '</div></div></div>';
    }

    /* ---------------------------------------------------------- variant B */

    function pageB() {
        var run = currentRun();
        if (!run) return gatePageB();

        var state = runState(run);
        var terminal = state !== 'running';
        var at = runPhaseIndex(run);
        var current = at < 6 ? PHASES[at] : PHASES[5];
        var phaseSteps = function (phaseName) {
            var list = (run.steps === 'substitute' ? STEPS_SUBSTITUTE : STEPS_EXECUTE)
                .filter(function (s) { return s.phase === phaseName; });
            return list.map(function (s) { return s.op; }).join(' \u00b7 ');
        };

        var cards = PHASES.map(function (phase, index) {
            var st = phaseState(run, phase.key, index);
            var op = phase.platform ? 'the worker node; not an ORE Studio service'
                : (index === 4 && phaseSteps('Post-processing') === '' ? 'shares retrieval\u2019s step'
                    : (index === 3 ? 'shares post-processing\u2019s step' : phaseSteps(phase.name)));
            return '<div class="tphase ' + st + '">' +
                '<div class="tp-head"><span class="tp-n">' + (index + 1) + '</span>' +
                (phase.platform ? '<span class="tag platform">worker node</span>' : '') +
                (phase.stateMachine ? '<span class="tag">state machine</span>' : '') + '</div>' +
                '<div class="tp-name">' + esc(phase.name) + '</div>' +
                '<div class="tp-step">' + esc(op) + '</div>' +
                '<div class="tp-state">' + esc(phaseLabel(st)) + '</div>' +
                '</div>';
        }).join('');

        var live = !terminal
            ? '<div class="current-card ' + (current.platform ? 'platform' : 'live') + '">' +
              '<div class="cur-top"><span class="tag accent">phase ' + (at + 1) + ' of 6</span>' +
              '<span class="cur-name">' + esc(current.name) + '</span>' +
              '<span class="cur-op">' + esc(current.platform ? 'the worker node, not an ORE Studio service' : (phaseSteps(current.name) || 'shares the neighbouring phase\u2019s step')) + '</span>' +
              '<span class="bigclock" style="margin-left:auto">' + fmtClock(elapsedOf(run)) + '</span></div>' +
              (current.platform
                  ? '<p class="cur-note">Execution is not a step any ORE Studio service performs: the services submitted the work and now wait for the grid. ' +
                    'The screen cannot say how far along the engine is, and this phase is marked so nobody reads it as an ORE Studio phase.</p>'
                  : current.stateMachine
                      ? '<p class="cur-note">Finalisation is the only phase that touches the state machine: it marks the instance completed and stamps its completion time.</p>'
                      : '<p class="cur-note">' + esc(current.note || 'The workflow engine dispatched this step and is waiting for its reply.') + '</p>') +
              '</div>'
            : '';

        var gate = terminal ? '' :
            '<div class="gate warn"><span class="gate-lead">This view holds the run.</span>' +
            'Theatre variant: there is no way back to the catalogue until the run is terminal. ' +
            '<span class="faint">A run can outlast the session, so the page also states that leaving and returning finds the same run by id.</span></div>';

        return '<div class="shell">' +
            appHeader('Report run', 'variant B \u00b7 run-theatre') +
            '<main>' +
            '<header class="pagehead"><div>' +
            '<div class="crumbs"><a href="../index.org">Prototypes</a><span>\u203a</span><a href="../../../../knowledge/journeys/reporting/reporting.org">Reporting</a><span>\u203a</span><span>Run ' + esc(run.name) + '</span></div>' +
            '<h1>Running ' + esc(run.name) + '</h1>' +
            '<p class="lede">' + esc(run.scope) + '. One on-demand run of the definition, followed phase by phase to its terminal state.</p>' +
            '</div><div class="actions">' +
            (terminal
                ? '<button class="btn primary" data-act="start-demo">Run another</button>' +
                  '<span class="hint">The theatre’s catalogue is variant A: switch with A in the foot bar.</span>'
                : '<a class="btn linklike" href="?variant=A">Leave to the catalogue (variant A)</a>') +
            '</div></header>' +
            fixtureNote() +
            gate +
            '<section class="theatre-run">' +
            '<div class="runhead"><div><h2>' + esc(run.name) + ' \u00b7 ' + esc(run.party) + '</h2>' +
            '<p class="sub">run ' + esc(run.id8) + ' \u00b7 kind on-demand \u00b7 trigger ' + esc(run.triggerId) + ' \u00b7 started ' +
            wallTime(BASE_SECONDS, run.startedAt) + '</p></div>' +
            '<div class="right">' + stateTag(run) + '<span class="bigclock">' + fmtClock(elapsedOf(run)) + '</span></div></div>' +
            '<div class="card" style="padding:14px"><div class="theatre">' + cards + '</div></div>' +
            live +
            (terminal ? terminalPanel(run) : '') +
            '</section>' +
            '<div class="card"><header class="sectionhead"><h2>Seven steps, six phases</h2>' +
            '<span class="meta"><span class="tag">' + (run.steps === 'substitute' ? '3' : '7') + ' steps in this run\u2019s shape</span></span></header>' +
            '<div class="steplist">' + (run.steps === 'substitute' ? STEPS_SUBSTITUTE : STEPS_EXECUTE).map(function (s, i) {
                return '<div class="srow' + (s.substituted ? ' repl' : '') + '"><span class="sn">' + (i + 1) + '</span>' +
                    '<span class="so">' + esc(s.op) + '</span>' +
                    '<span class="sr">' + esc(s.phase) + (s.substituted ? ' \u00b7 replaces ' + esc(s.substituted) : '') + '</span></div>';
            }).join('') +
            '<div class="srow"><span class="sn">\u2014</span><span class="so">run on a worker node</span>' +
            '<span class="sr">Execution \u00b7 not a workflow step, and not an ORE Studio service</span></div>' +
            '</div></div>' +
            planPanel(defById(run.defId)) +
            arguesPanel(variantById('B')) +
            gapsPanel() +
            '</main></div>';
    }

    function gatePageB() {
        var def = defById(S.confirm || S.defId);
        return '<div class="shell">' +
            appHeader('Report run', 'variant B \u00b7 run-theatre') +
            '<main>' +
            '<header class="pagehead"><div>' +
            '<div class="crumbs"><a href="../index.org">Prototypes</a><span>\u203a</span><a href="../../../../knowledge/journeys/reporting/reporting.org">Reporting</a><span>\u203a</span><span>Start a run</span></div>' +
            '<h1>Start a run of ' + esc(def.name) + '</h1>' +
            '<p class="lede">In this variant the press leaves the catalogue: the run takes over the page until it is terminal.</p>' +
            '</div><div class="actions"><a class="btn linklike" href="?variant=A">The catalogue (variant A)</a></div></header>' +
            fixtureNote() +
            '<section class="card gate-panel">' +
            '<header class="sectionhead"><h2>Confirm the run</h2><span class="meta"><span class="tag accent">variant B</span></span></header>' +
            '<table class="kv"><tbody>' +
            '<tr><td>Definition</td><td>' + esc(def.name) + ' \u00b7 ' + esc(def.type) + ' \u00b7 ' + esc(def.party) + '</td></tr>' +
            '<tr><td>Scope the run will read</td><td>' + esc(def.scope) + '</td></tr>' +
            '<tr><td>Run configuration</td><td>' + esc(def.executes) + '</td></tr>' +
            '<tr><td>What happens next</td><td>The page becomes the run: six phases left to right, the current one live, until a terminal state.</td></tr>' +
            '</tbody></table>' +
            '<div class="gate"><span class="gate-lead">On confirmation the page leaves the catalogue.</span>' +
            'Variant B argues that a run is a destination. Variant A argues the opposite: there, the same press returns to the catalogue and the run reports itself in a strip.</div>' +
            '<div class="t-actions"><button class="btn ghost" data-act="goto-a">Cancel</button>' +
            '<button class="btn primary" data-act="start-demo">Confirm and run</button></div>' +
            '</section>' +
            planPanel(def) +
            arguesPanel(variantById('B')) +
            gapsPanel() +
            '</main></div>';
    }

    /* ---------------------------------------------------------- variant C */

    function pageC() {
        var running = S.runs.filter(function (r) { return runState(r) === 'running'; }).length;
        var pick = DEFS.filter(function (d) { return d.id === S.defId; })[0] || DEFS[0];

        var rows = S.runs.map(function (run, index) {
            var state = runState(run);
            var at = runPhaseIndex(run);
            var terminal = state !== 'running';
            var now = at < 6 ? PHASES[at] : PHASES[5];
            var steps = (run.steps === 'substitute' ? STEPS_SUBSTITUTE : STEPS_EXECUTE)
                .filter(function (s) { return s.phase === now.name || (now.name === 'Retrieval' && s.phase.indexOf('Retrieval') === 0); });
            var result = '';
            if (state === 'clean') result = '<span class="tag ok">completed</span><span class="msg">Report execution completed.</span>';
            else if (state === 'errors') result = '<span class="tag warn">completed, ' + run.errors + ' errors \u00b7 worst ' + esc(run.worst) + '</span><span class="msg">Report execution completed.</span>';
            else if (state === 'failed') result = '<span class="tag bad">failed \u00b7 report-scoped error</span><span class="msg">Report execution failed. \u00b7 no archive</span>';
            else result = '<span class="faint">waiting for a terminal state</span>';

            return '<tr class="' + (terminal ? 'terminal' : '') + (state === 'failed' ? ' failed-row' : '') + '">' +
                '<td class="q-def">' + esc(run.name) + '<span class="sub">' + esc(run.party) + ' \u00b7 ' + esc(run.scope) + '</span></td>' +
                '<td class="q-state">' + stateTag(run) + '</td>' +
                '<td class="q-phase">' + phaseBar(run, false) +
                '<div class="q-now">' +
                (terminal ? '<span class="now-note">' + (state === 'failed' ? 'terminated at phase ' + (run.failAt + 1) + ' \u00b7 the run did not wait mid-flight' : 'all six phases reached') + '</span>'
                    : '<span class="tag ' + (now.platform ? 'platform' : 'accent') + '">phase ' + (at + 1) + ' \u00b7 ' + esc(now.name) + '</span>' +
                      '<span class="now-op">' + esc(now.platform ? 'worker node, not an ORE Studio service' : (steps.map(function (s) { return s.op; }).join(' \u00b7 ') || 'shares the step below')) + '</span>') +
                '</div></td>' +
                '<td class="q-elapsed">' + elapsedLine(run) + '</td>' +
                '<td class="q-result">' + result + '</td>' +
                '<td>' + (terminal
                    ? '<button class="btn ghost" data-act="again" data-run="' + run.id + '">Run again</button>'
                    : '<button class="btn ghost" disabled>Running</button>') + '</td>' +
                '</tr>';
        }).join('');

        var empty = S.runs.length === 0
            ? '<div class="qempty">No runs in this session. Pick a definition and drop a row.<br>' +
              '<span class="hint">In this variant, starting a run is not a dialog: the row is the confirmation. Press <b>Start demo</b> to watch all three endings advance together.</span></div>'
            : '';

        return '<div class="shell">' +
            appHeader('Report runs', 'variant C \u00b7 queue-first') +
            '<main>' +
            '<header class="pagehead"><div>' +
            '<div class="crumbs"><a href="../index.org">Prototypes</a><span>\u203a</span><a href="../../../../knowledge/journeys/reporting/reporting.org">Reporting</a><span>\u203a</span><span>Runs</span></div>' +
            '<h1>Your report runs</h1>' +
            '<p class="lede">The runs you started this session. Each row advances in place to its own terminal state, so the queue answers the question the single run cannot: what am I waiting for now?</p>' +
            '</div><div class="actions">' +
            '<button class="btn primary" data-act="start-demo">Start demo</button>' +
            '<a class="btn linklike" href="../index.org">Back to prototypes</a>' +
            '</div></header>' +
            fixtureNote() +
            '<section class="card">' +
            '<header class="sectionhead"><h2>Start a run</h2>' +
            '<span class="meta"><span class="tag accent">' + running + ' running</span><span class="tag">' + S.runs.length + ' in this session</span></span></header>' +
            '<div class="queuebar">' +
            '<div class="field"><label for="def-pick">Definition</label>' +
            '<select id="def-pick" data-act="pick">' + DEFS.map(function (d) {
                return '<option value="' + esc(d.id) + '"' + (d.id === pick.id ? ' selected' : '') + '>' +
                    esc(d.name) + ' \u00b7 ' + esc(d.party) + (d.steps === 'substitute' ? ' \u00b7 substitutes both ends' : '') + '</option>';
            }).join('') + '</select></div>' +
            '<button class="btn primary" data-act="start-picked">Run now</button>' +
            '<span class="spacer"></span>' +
            '<span class="hint">No dialog: the press drops a row, and the row is the confirmation. ' +
            'The definition names the scope, so it is shown on the row.</span>' +
            '</div>' +
            '<p class="rowfoot" style="margin-top:12px">' + esc(pick.scope) + ' \u00b7 ' + esc(pick.executes) + ' \u00b7 ' +
            (pick.steps === 'substitute' ? '3 workflow steps' : '7 workflow steps') + '</p>' +
            '</section>' +
            '<section class="card">' +
            '<header class="sectionhead"><h2>Queue</h2>' +
            '<span class="meta"><span class="faint" style="font-size:12px">' + S.runs.length + ' rows \u00b7 every row carries its own clock</span></span></header>' +
            (S.runs.length
                ? '<div class="table-wrap"><table class="queue"><thead><tr>' +
                  '<th>Definition and scope</th><th>State</th><th>Phases</th><th>Elapsed</th><th>Terminal state</th><th></th>' +
                  '</tr></thead><tbody>' + rows + '</tbody></table></div>'
                : empty) +
            (S.runs.length
                ? '<p class="rowfoot">The queue is the person\u2019s, not the service\u2019s. The definition carries <span class="mono">concurrency_policy</span> and the screen does not evaluate it: ' +
                  'a trigger the service refuses would come back as a refusal, not as a queue position this screen invents.</p>'
                : '') +
            '</section>' +
            planPanel(pick) +
            arguesPanel(variantById('C')) +
            gapsPanel() +
            '</main></div>';
    }

    /* --------------------------------------------------------- shared bits */

    function appHeader(here, mode) {
        return '<header class="appheader"><div class="appheader-inner">' +
            '<span class="brand"><span class="mark">O</span><span class="name">ORE Studio</span></span>' +
            '<nav class="appnav"><a href="../index.org">Prototypes</a>' +
            '<a href="../../../../knowledge/journeys/reporting/reporting.org">Reporting</a>' +
            '<a class="here" href="#">' + esc(here) + '</a></nav>' +
            '<span class="modechip">' + esc(mode) + '</span>' +
            '</div></header>';
    }

    function fixtureNote() {
        return '<div class="notice warn"><b>Fixture.</b> The six phases are scripted on a timer, ' +
            esc(SCRIPT_SPEED()) + '; nothing on this page reads a server; and there is no reporting UI in the application today. ' +
            'The per-phase position the ladder shows does not exist: in production it lives in <span class="mono">ores.workflow</span>, ' +
            'and no reporting read serves it. Parties, definitions, times, error counts and output are mock data.</div>';
    }

    function SCRIPT_SPEED() {
        return 'at 1\u00d7 a phase takes ' + PHASE_MS + ' ms, so one run takes ' + (RUN_MS / 1000).toFixed(1) + ' s';
    }

    /* --------------------------------------------------------------- chrome */

    function render() {
        var variant = variantById(S.variant);
        if (S.variant === 'A') document.getElementById('app').innerHTML = pageA();
        else if (S.variant === 'B') document.getElementById('app').innerHTML = pageB();
        else document.getElementById('app').innerHTML = pageC();
        renderNote(variant);
        renderBar(variant);
    }

    function runSummary() {
        var run = currentRun();
        if (!run) return { state: 'idle', phase: 'no run', elapsed: '0.0 s', run: null };
        var state = runState(run);
        var at = runPhaseIndex(run);
        return {
            state: state,
            phase: state === 'running' ? (PHASES[at] ? (at + 1) + '/6 ' + PHASES[at].name : '6/6') : 'terminal \u00b7 ' + state,
            elapsed: fmtClock(elapsedOf(run)),
            run: run
        };
    }

    function renderNote(variant) {
        var summary = runSummary();
        document.getElementById('proto-note').innerHTML =
            '<span class="note-tag">Prototype</span>' +
            '<span>Mock data \u00b7 no server is read \u00b7 no reporting UI exists in the application today</span>' +
            '<span class="note-live">\u25b8 variant ' + esc(variant.id) + ' \u00b7 ' + esc(variant.name.replace(/^[ABC] \u00b7 /, '')) +
            ' \u00b7 run ' + esc(summary.phase) + '</span>';
    }

    function renderBar(variant) {
        var summary = runSummary();
        var buttons = VARIANTS.map(function (v) {
            return '<button data-act="variant" data-variant="' + v.id + '"' + (S.variant === v.id ? ' class="on"' : '') + '>' + v.id + '</button>';
        }).join('');
        var dotCls = summary.state === 'idle' ? 'idle'
            : summary.state === 'running' ? ''
            : summary.state === 'failed' ? 'err' : 'done';
        var stateWord = summary.state === 'idle' ? 'no run yet'
            : summary.state === 'running' ? 'running'
            : summary.state === 'clean' ? 'terminal \u00b7 completed clean'
            : summary.state === 'errors' ? 'terminal \u00b7 completed carrying errors'
            : 'terminal \u00b7 failed';

        document.getElementById('proto-bar').innerHTML =
            '<div class="bar-line">' +
            '<span class="tag-prototype">Prototype</span>' + buttons +
            '<span class="sep">|</span>' +
            '<button data-act="start-demo">Run demo</button>' +
            '<button data-act="replay">Replay</button>' +
            '<span class="gist">' + esc(variant.gist) + '</span>' +
            '</div>' +
            '<div class="clockline">' +
            '<span class="live-dot ' + dotCls + '"></span>' +
            '<span class="label">variant <b>' + esc(variant.id) + '</b></span>' +
            '<span class="sep">\u00b7</span>' +
            '<span class="label">run <b>' + esc(summary.run ? summary.run.name : '\u2014') + '</b></span>' +
            '<span class="sep">\u00b7</span>' +
            '<span class="label">phase <b>' + esc(summary.phase) + '</b></span>' +
            '<span class="sep">\u00b7</span>' +
            '<span class="label">elapsed scripted <b>' + esc(summary.elapsed) + '</b></span>' +
            '<span class="sep">\u00b7</span>' +
            '<span class="label">' + esc(stateWord) + '</span>' +
            '<span class="sep">\u00b7</span>' +
            '<span class="label faint">total scripted <b>' + esc(fmtClock(S.clock)) + '</b></span>' +
            '</div>';
    }

    /* ---------------------------------------------------------------- clock */

    function restartClock() {
        if (S.timer !== null) clearInterval(S.timer);
        S.timerOrigin = performance.now();
        S.timer = setInterval(tick, TICK_MS);
    }

    function tick() {
        S.clock = performance.now() - S.timerOrigin;

        var live = S.runs.some(function (run) { return runState(run) === 'running'; });
        if (!live) {
            clearInterval(S.timer);
            S.timer = null;
        }
        render();
    }

    /* ------------------------------------------------------------- actions */

    function askRun(def) {
        S.confirm = def;
        S.view = 'catalogue';
        render();
    }

    function confirmRun() {
        var def = S.confirm;
        if (!def) return;
        var blueprint = blueprintForDefinition(def.id);
        S.confirm = null;
        S.stripDismissed = false;
        startRun(blueprint);
        if (S.variant === 'A') S.view = 'catalogue';
        else if (S.variant === 'B') S.view = 'run';
        restartClock();
        render();
    }

    /* The ending follows the definition, so a person running the same
     * definition twice gets the same ending; the demo cycles through all
     * three endings by cycling the definitions. */
    function blueprintForDefinition(defId) {
        var match = BLUEPRINTS.filter(function (b) { return b.defId === defId; })[0];
        if (match) return blueprintFor(BLUEPRINTS.indexOf(match));
        return { defId: defId, ending: 'clean' };
    }

    function blueprintEndingFor(def) {
        if (def.id === 'd-headline') return 'clean';
        if (def.id === 'd-large-exposures') return 'errors';
        if (def.id === 'd-liquidity-coverage') return 'failed';
        return 'clean';
    }

    function setVariant(id) {
        S.variant = id;
        S.confirm = null;
        S.stripDismissed = false;
        if (id === 'B') S.view = S.runs.length ? 'run' : 'gate';
        else S.view = 'catalogue';
        if (S.runs.some(function (run) { return runState(run) === 'running'; }) && S.timer === null) {
            S.timerOrigin = performance.now() - S.clock;
            S.timer = setInterval(tick, TICK_MS);
        }
        render();
        writeParams();
    }

    function replay() {
        if (S.timer !== null) clearInterval(S.timer);
        S.timer = null;
        S.clock = 0;
        S.runs = [];
        S.nextId = 0;
        S.confirm = null;
        S.stripDismissed = false;
        S.log = [];
        S.log.push('Replay \u00b7 the scripted clock and every run reset');
        if (S.variant === 'B') S.view = 'gate';
        render();
        startDemo();
    }

    function readParams() {
        var p = new URLSearchParams(window.location.search);
        var v = p.get('variant');
        if (variantById(v)) S.variant = v;
        if (p.get('view') === 'catalogue' && S.variant === 'B') S.view = 'catalogue';
        var speed = parseFloat(p.get('speed'));
        if (speed > 0 && speed <= 4) {
            S.speed = speed;
        }
        var ending = p.get('ending');
        if (ending === 'clean' || ending === 'errors' || ending === 'failed') S.ending = ending;
        var dialog = p.get('dialog');
        if (dialog && defById(dialog)) S.confirm = defById(dialog);
    }

    function writeParams() {
        var p = new URLSearchParams(window.location.search);
        p.set('variant', S.variant);
        window.history.replaceState(null, '', window.location.pathname + '?' + p.toString());
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest('[data-act]');
        if (!el) return;
        var act = el.getAttribute('data-act');

        if (act === 'scrim' && ev.target !== el) return;
        if (act === 'start-demo' || act === 'scrim' || act === 'confirm-run' ||
            act === 'variant' || act === 'pick') {
            ev.preventDefault();
        }
        if (el.tagName === 'SELECT') return;

        if (act === 'variant') {
            setVariant(el.getAttribute('data-variant'));
        } else if (act === 'ask-run') {
            askRun(defById(el.getAttribute('data-def')));
        } else if (act === 'cancel-confirm') {
            S.confirm = null;
            render();
        } else if (act === 'confirm-run') {
            confirmRun();
        } else if (act === 'scrim') {
            S.confirm = null;
            render();
        } else if (act === 'start-demo') {
            startDemo();
            writeParams();
        } else if (act === 'replay') {
            replay();
            writeParams();
        } else if (act === 'dismiss-strip') {
            S.stripDismissed = true;
            render();
        } else if (act === 'jump') {
            S.variant = 'B';
            S.view = 'run';
            render();
            writeParams();
        } else if (act === 'goto-a') {
            setVariant('A');
        } else if (act === 'again') {
            var run = S.runs.filter(function (r) { return r.id === parseInt(el.getAttribute('data-run'), 10); })[0];
            if (run) {
                S.confirm = null;
                startRun({ defId: run.defId, ending: run.ending, failAt: run.failAt, errors: run.errors, worst: run.worst });
                restartClock();
                render();
            }
        } else if (act === 'start-picked') {
            var def = defById(S.defId) || DEFS[0];
            startRun(blueprintForDefinition(def.id));
            restartClock();
            render();
        }
    });

    document.addEventListener('change', function (ev) {
        var el = ev.target;
        if (el.getAttribute && el.getAttribute('data-act') === 'pick') {
            S.defId = el.value;
            render();
        }
    });

    document.addEventListener('keydown', function (ev) {
        if (ev.key === 'Escape' && S.confirm) {
            S.confirm = null;
            render();
        }
    });

    readParams();
    if (S.variant === 'B') S.view = S.runs.length ? 'run' : 'gate';

    /* The demonstration starts itself once, so a person opening the page sees
     * a run advance without pressing anything. Variant C starts its three
     * endings instead of one run, because the queue is what it argues. The
     * rows' scripted offsets are set before the clock starts, so a row that
     * should already be terminal reads terminal on the first frame. */
    function boot() {
        S.runs = [];
        S.nextId = 0;
        S.clock = 0;
        if (S.variant === 'C') {
            dropQueueDemo();
        } else if (S.ending) {
            var chosen = BLUEPRINTS.filter(function (b) { return b.ending === S.ending; })[0];
            startRun({ defId: chosen.defId, ending: chosen.ending, failAt: chosen.failAt, errors: chosen.errors, worst: chosen.worst });
            if (S.variant === 'B') S.view = 'run';
        } else {
            startRun(blueprintFor(0));
        }
        S.autoStarted = true;
        restartClock();
        render();
    }

    boot();
})();
