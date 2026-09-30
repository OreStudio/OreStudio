/*
 * ores-dsh-environment — browser half.
 *
 * Hand-written, no build step, no dependencies beyond the platform `react`
 * module. The frozen interface is projects/ores.dsh_environment/CONTRACT.md.
 */
window.__ModuleLoader__.load({
  id: 'ores-dsh-environment',
  factory: (require) => {
    var module = { exports: {} }
    var exports = module.exports

    const React = require('react')
    const h = React.createElement

    const TAG = 'ores-dsh-environment'
    const MONO = 'ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, monospace'
    const ACCENT = 'var(--dsw-alias-state-business-primary)'
    const WARN = 'var(--dsw-alias-state-warn-primary)'

    /* The service palette, keyed by the state ids the host sends. The host
     * owns the vocabulary and the order; this table owns only the colour.
     * `stopped` is grey because it is the state an operator chooses, and
     * `failed` and `missing` are both red but for different reasons: one is
     * a unit systemd tried to run and could not, the other a unit the
     * manager has never heard of. */
    const STATE_COLOR = {
      running: '#3fb950',
      starting: '#d29922',
      stopped: '#6e7681',
      failed: '#f85149',
      missing: '#f85149',
    }
    const TONE_COLOR = {
      ok: '#3fb950',
      warn: '#d29922',
      critical: '#f85149',
      unknown: '#6e7681',
    }
    const stateColor = (state) => STATE_COLOR[state] || STATE_COLOR.stopped
    const toneColor = (tone) => TONE_COLOR[tone] || TONE_COLOR.unknown

    /* ------------------------------------------------------------- helpers */

    function str(value) {
      return typeof value === 'string' ? value : ''
    }
    function num(value) {
      return typeof value === 'number' && Number.isFinite(value) ? value : 0
    }
    function arr(value) {
      return Array.isArray(value) ? value : []
    }
    function asList(value) {
      return arr(value).map(str).filter(Boolean)
    }

    /* A state the host sends but this build does not know is kept, not
     * dropped, and `attention` decides whether it joins the red filter. */

    function basename(path) {
      const trimmed = str(path).replace(/\/+$/, '')
      return trimmed.split('/').pop() || trimmed
    }

    /* ------------------------------------------------------------ fetching */

    function normalizeJob(raw) {
      if (!raw || typeof raw !== 'object') return null
      return {
        id: str(raw.id),
        kind: str(raw.kind),
        label: str(raw.label),
        running: raw.running === true,
        code: typeof raw.code === 'number' ? raw.code : null,
        startedAt: str(raw.startedAt),
        elapsedSeconds: num(raw.elapsedSeconds),
        steps: arr(raw.steps).map((step) => ({
          name: str(step.name),
          running: step.running === true,
          code: typeof step.code === 'number' ? step.code : null,
          elapsedSeconds: num(step.elapsedSeconds),
        })),
        tail: asList(raw.tail),
      }
    }

    function normalize(payload) {
      const source = payload && typeof payload === 'object' ? payload : {}
      if (source.ok !== true) {
        return {
          ok: false,
          reason: str(source.reason) || 'unknown',
          message: str(source.message),
          job: normalizeJob(source.job),
          tiles: [],
          env: {},
          database: {},
          services: { total: 0, counts: {}, states: [], units: [], nats: null },
          health: { level: 'unknown', reasons: [] },
        }
      }
      const env = source.env && typeof source.env === 'object' ? source.env : {}
      const database = source.database && typeof source.database === 'object' ? source.database : {}
      const services = source.services && typeof source.services === 'object' ? source.services : {}
      const health = source.health && typeof source.health === 'object' ? source.health : {}
      const remedies = source.remedies && typeof source.remedies === 'object' ? source.remedies : {}
      const counts = services.counts && typeof services.counts === 'object' ? services.counts : {}
      /* The vocabulary and its order are the host's; this half only colours
       * what it is sent, so a state this build has never heard of still
       * renders. */
      const states = arr(services.states)
      return {
        ok: true,
        generatedAt: str(source.generatedAt),
        env: {
          name: str(env.name),
          label: str(env.label) || str(env.name),
          preset: str(env.preset),
          worktree: str(env.worktree),
          worktreeName: str(env.worktreeName) || basename(env.worktree),
          scope: str(env.scope),
          slice: str(env.slice),
          envVersion: num(env.envVersion),
          requiredEnvVersion: env.requiredEnvVersion === null
            || env.requiredEnvVersion === undefined ? null : num(env.requiredEnvVersion),
          envStale: env.envStale === true,
          activities: arr(env.activities),
          vcpkgWarning: str(env.vcpkgWarning),
        },
        database: {
          reachable: database.reachable === true,
          name: str(database.name),
          restoredAt: str(database.restoredAt),
          restoredAge: str(database.restoredAge),
          restoredAgeSeconds: database.restoredAgeSeconds === null
            || database.restoredAgeSeconds === undefined ? null : num(database.restoredAgeSeconds),
          restoredLevel: str(database.restoredLevel) || 'unknown',
          schemaVersion: str(database.schemaVersion),
          builtFrom: str(database.builtFrom),
          builtAt: str(database.builtAt),
          driftLabel: str(database.driftLabel),
          driftLevel: str(database.driftLevel) || 'unknown',
          driftSeconds: database.driftSeconds === null
            || database.driftSeconds === undefined ? null : num(database.driftSeconds),
          bootstrapMode: database.bootstrapMode === true,
          warning: str(database.warning),
          confirmPhrase: str(database.confirmPhrase) || str(env.name),
        },
        services: {
          total: num(services.total),
          counts: counts,
          states: states.map((state) => ({
            id: str(state.id),
            title: str(state.title) || str(state.id),
            count: num(state.count),
            broken: state.broken === true,
          })),
          /* Which states count as broken is the host's rule, not a second
           * list here: the filter reads this set. */
          brokenStates: states.filter((state) => state.broken === true)
            .map((state) => str(state.id)),
          units: arr(services.units).map((unit) => ({
            unit: str(unit.unit),
            selector: str(unit.selector) || str(unit.service),
            service: str(unit.service),
            label: str(unit.label) || str(unit.unit),
            state: str(unit.state) || 'unknown',
            detail: str(unit.detail),
          })),
          nats: services.nats && typeof services.nats === 'object'
            ? { label: str(services.nats.label), state: str(services.nats.state), detail: str(services.nats.detail) }
            : null,
          logDir: str(services.logDir),
        },
        health: { level: str(health.level) || 'unknown', reasons: asList(health.reasons) },
        tiles: arr(source.tiles).map((tile) => ({
          id: str(tile.id),
          label: str(tile.label),
          value: str(tile.value),
          tone: str(tile.tone) || 'unknown',
          detail: str(tile.detail),
        })),
        remedies: {
          startServices: str(remedies.startServices),
          stopServices: str(remedies.stopServices),
          recreateDatabase: str(remedies.recreateDatabase),
          configureEnv: str(remedies.configureEnv),
        },
        tree: source.tree && typeof source.tree === 'object'
          ? { root: str(source.tree.root), source: str(source.tree.source) }
          : { root: '', source: '' },
        job: normalizeJob(source.job),
      }
    }

    function useEnvironment(sessionId, cwd) {
      const [snapshot, setSnapshot] = React.useState(null)
      const [stale, setStale] = React.useState(false)
      const [loading, setLoading] = React.useState(true)
      const [notice, setNotice] = React.useState('')
      const [nonce, setNonce] = React.useState(0)
      const lastGood = React.useRef(null)
      const lastKey = React.useRef(sessionId)

      React.useEffect(() => {
        let alive = true
        if (lastKey.current !== sessionId) {
          lastKey.current = sessionId
          lastGood.current = null
          setSnapshot(null)
          setStale(false)
          setNotice('')
        }
        setLoading(true)
        const params = ['session=' + encodeURIComponent(str(sessionId))]
        /* The host's second rung: the session's workspace root, never a guess. */
        if (str(cwd)) params.push('cwd=' + encodeURIComponent(str(cwd)))
        fetch('/plugins/ores-dsh-environment/state?' + params.join('&'),
          { credentials: 'same-origin', cache: 'no-store' })
          .then((response) => response.json())
          .then((payload) => {
            if (!alive) return
            const next = normalize(payload)
            if (next.ok) {
              lastGood.current = next
              setSnapshot(next)
              setStale(false)
              setNotice('')
            } else {
              setNotice([next.reason, next.message].filter(Boolean).join(': '))
              /* A failed refresh keeps the last good model on screen and marks
               * it stale, so a transient fault does not blank the panel. */
              if (lastGood.current) setStale(true)
              setSnapshot(lastGood.current || next)
            }
          })
          .catch((error) => {
            if (!alive) return
            setNotice('state request failed: ' + str(error && error.message ? error.message : error))
            if (lastGood.current) setStale(true)
            else setSnapshot(normalize(null))
          })
          .then(() => { if (alive) setLoading(false) })
        return () => { alive = false }
      }, [sessionId, str(cwd), nonce])

      const refresh = React.useCallback(() => setNonce((value) => value + 1), [])
      const jobRunning = !!(snapshot && snapshot.job && snapshot.job.running)

      /* While an action runs, the panel follows it. Two seconds is the read
       * cache's own interval, so a poll never misses it. */
      React.useEffect(() => {
        if (!jobRunning) return undefined
        const timer = setInterval(() => setNonce((value) => value + 1), 2000)
        return () => clearInterval(timer)
      }, [jobRunning])

      return {
        snapshot,
        firstLoad: loading && snapshot === null,
        pending: snapshot !== null && loading,
        stale,
        notice,
        refresh,
      }
    }

    async function postAction(sessionId, cwd, body) {
      const response = await fetch('/plugins/ores-dsh-environment/action', {
        method: 'POST',
        credentials: 'same-origin',
        cache: 'no-store',
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify({
          session: sessionId,
          ...(str(cwd) ? { cwd: str(cwd) } : {}),
          ...body,
        }),
      })
      let payload = null
      try { payload = await response.json() } catch { payload = null }
      if (payload && typeof payload === 'object') return payload
      return { ok: false, reason: 'transport', message: 'the action route answered no data' }
    }

    /* -------------------------------------------------------------- pieces */

    function Dot(props) {
      return h('span', {
        'aria-hidden': 'true',
        style: {
          flex: '0 0 auto', width: '0.5rem', height: '0.5rem', borderRadius: '50%',
          background: props.color, boxShadow: '0 0 0 1px rgba(0,0,0,0.15)',
        },
      })
    }

    function Chip(props) {
      return h('span', {
        style: {
          fontFamily: MONO, fontSize: '0.68rem', padding: '0.05rem 0.3rem',
          borderRadius: '0.25rem', border: '1px solid var(--dsw-alias-border-l2)',
          color: 'var(--dsw-alias-label-secondary)', whiteSpace: 'nowrap',
        },
      }, props.children)
    }

    function Badge(props) {
      return h('span', {
        'data-env-state': props.state,
        style: {
          flex: '0 0 auto', fontSize: '0.68rem', fontWeight: 700,
          letterSpacing: '0.02em', color: props.color, minWidth: '3.6rem',
        },
      }, props.state)
    }

    function MicroLabel(props) {
      return h('div', {
        style: {
          fontSize: '0.68rem', textTransform: 'uppercase', letterSpacing: '0.06em',
          color: 'var(--dsw-alias-label-tertiary)',
        },
      }, props.children)
    }

    function Tile(props) {
      const tile = props.tile
      return h('div', {
        'data-env-tile': tile.id,
        style: {
          flex: '1 1 0', minWidth: '9rem', padding: '0.5rem 0.6rem',
          border: '1px solid var(--dsw-alias-border-l1)', borderRadius: '0.5rem',
          borderLeft: '3px solid ' + toneColor(tile.tone),
          background: 'var(--dsw-alias-bg-layer-2)',
        },
      }, [
        h(MicroLabel, { key: 'l' }, tile.label),
        h('div', {
          key: 'v',
          style: { fontSize: '1.25rem', fontWeight: 600, lineHeight: 1.2, color: toneColor(tile.tone) },
        }, tile.value),
        h('div', {
          key: 'd',
          style: { fontSize: '0.7rem', color: 'var(--dsw-alias-label-tertiary)', marginTop: '0.15rem' },
        }, tile.detail),
      ])
    }

    function Skeleton() {
      return h('div', {
        'data-env-loading': 'skeleton',
        style: { padding: '1rem', color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.8rem' },
      }, 'Reading the environment…')
    }

    function Notice(props) {
      if (!props.notice) return null
      return h('div', {
        style: {
          margin: '0.4rem 0', padding: '0.4rem 0.5rem', borderRadius: '0.35rem',
          border: '1px solid ' + WARN, color: WARN, fontSize: '0.75rem',
        },
      }, props.notice)
    }

    function KeyValue(props) {
      return h('div', {
        style: { display: 'flex', gap: '0.5rem', fontSize: '0.75rem', padding: '0.12rem 0' },
      }, [
        h('span', {
          key: 'k',
          style: { flex: '0 0 6.5rem', color: 'var(--dsw-alias-label-tertiary)' },
        }, props.label),
        h('span', {
          key: 'v',
          style: { flex: 1, minWidth: 0, fontFamily: props.mono ? MONO : undefined, wordBreak: 'break-all' },
        }, props.value),
      ])
    }

    function JobStrip(props) {
      const job = props.job
      const tailRef = React.useRef(null)
      React.useEffect(() => {
        const node = tailRef.current
        if (node) node.scrollTop = node.scrollHeight
      }, [job && job.tail ? job.tail.length : 0, job && job.running])
      if (!job) return null
      const done = !job.running
      const failed = done && job.code !== 0
      const tone = done ? (failed ? TONE_COLOR.critical : TONE_COLOR.ok) : TONE_COLOR.warn
      return h('div', {
        'data-env-job': job.kind,
        style: {
          marginTop: '0.5rem', border: '1px solid var(--dsw-alias-border-l1)',
          borderLeft: '3px solid ' + tone, borderRadius: '0.5rem',
          background: 'var(--dsw-alias-bg-layer-2)', padding: '0.45rem 0.55rem',
        },
      }, [
        h('div', {
          key: 'head',
          style: { display: 'flex', alignItems: 'center', gap: '0.4rem', fontSize: '0.78rem' },
        }, [
          h(Dot, { key: 'd', color: tone }),
          h('span', { key: 'l', style: { flex: 1, minWidth: 0, fontWeight: 600 } }, job.label),
          h('span', {
            key: 't',
            style: { fontFamily: MONO, fontSize: '0.7rem', color: 'var(--dsw-alias-label-tertiary)' },
          }, job.running ? `${job.elapsedSeconds}s` : (failed ? `exit ${job.code}` : `${job.elapsedSeconds}s, done`)),
        ]),
        h('div', {
          key: 'steps',
          style: { display: 'flex', gap: '0.4rem', flexWrap: 'wrap', marginTop: '0.25rem' },
        }, job.steps.map((step, index) => h('span', {
          key: step.name + index,
          style: {
            fontSize: '0.68rem', fontFamily: MONO,
            color: step.running ? WARN
              : (step.code === 0 ? STATE_COLOR.running : STATE_COLOR.failed),
          },
        }, (step.running ? '… ' : (step.code === 0 ? '✓ ' : '✗ ')) + step.name))),
        job.tail.length > 0 ? h('pre', {
          key: 'tail',
          ref: tailRef,
          style: {
            margin: '0.35rem 0 0', padding: '0.35rem', maxHeight: '10rem', overflowY: 'auto',
            fontFamily: MONO, fontSize: '0.68rem', lineHeight: 1.35,
            background: 'var(--dsw-alias-bg-layer-1)', borderRadius: '0.3rem',
            color: 'var(--dsw-alias-label-secondary)', whiteSpace: 'pre-wrap',
          },
        }, job.tail.join('\n')) : null,
      ])
    }

    /* ------------------------------------------------------ seat 1: the chip */

    function NowChip(props) {
      const sessionId = props.sessionId
      const cwd = useSessionCwd()
      const state = useEnvironment(sessionId, cwd)
      const [open, setOpen] = React.useState(false)
      const wrapRef = React.useRef(null)

      React.useEffect(() => {
        if (!open) return undefined
        const onKey = (event) => { if (event.key === 'Escape') setOpen(false) }
        const onClick = (event) => {
          if (wrapRef.current && !wrapRef.current.contains(event.target)) setOpen(false)
        }
        document.addEventListener('keydown', onKey)
        document.addEventListener('mousedown', onClick)
        return () => {
          document.removeEventListener('keydown', onKey)
          document.removeEventListener('mousedown', onClick)
        }
      }, [open])

      const snapshot = state.snapshot
      const ready = !!(snapshot && snapshot.ok)
      const health = ready ? snapshot.health.level : 'critical'
      const color = toneColor(health)
      const counts = ready ? snapshot.services.counts : {}
      const label = ready ? (snapshot.env.label || snapshot.env.name) : ''
      const dbAge = ready && snapshot.database.reachable ? snapshot.database.restoredAge : '—'
      const up = `${num(counts.running)}/${ready ? snapshot.services.total : 0}`

      return h('div', { ref: wrapRef, style: { position: 'relative', display: 'inline-block' } }, [
        h('button', {
          key: 'button',
          type: 'button',
          'data-ores-environment': 'now',
          'aria-expanded': open ? 'true' : 'false',
          'aria-haspopup': 'dialog',
          title: ready
            ? [
                `${label} (${snapshot.health.level})`,
                ...snapshot.health.reasons,
              ].join('\n')
            : (state.notice || 'the environment could not be read'),
          onClick: () => setOpen((value) => !value),
          style: {
            display: 'inline-flex', alignItems: 'center', gap: '0.35rem',
            padding: '0.15rem 0.45rem', borderRadius: '0.35rem',
            border: '1px solid var(--dsw-alias-border-l2)',
            background: 'transparent', cursor: 'pointer', maxWidth: '24rem',
          },
        }, [
          h(Dot, { key: 'd', color }),
          h('span', {
            key: 'l',
            style: { fontFamily: MONO, fontSize: '0.72rem', fontWeight: 600 },
          }, label || 'no environment'),
          h('span', {
            key: 'db',
            style: { fontSize: '0.7rem', color: 'var(--dsw-alias-label-tertiary)' },
          }, 'db ' + dbAge),
          h('span', {
            key: 'svc',
            style: {
              fontSize: '0.7rem', fontFamily: MONO,
              color: ready && (num(counts.failed) > 0 || num(counts.missing) > 0)
                ? STATE_COLOR.failed : 'var(--dsw-alias-label-tertiary)',
            },
          }, up),
        ]),
        open ? h(NowPopover, { snapshot, state }) : null,
      ])
    }

    function NowPopover(props) {
      const snapshot = props.snapshot
      const state = props.state
      const ready = !!(snapshot && snapshot.ok)
      const rows = []
      if (ready) {
        rows.push(h(KeyValue, { key: 'n', label: 'environment', value: snapshot.env.name }))
        rows.push(h(KeyValue, { key: 'p', label: 'preset', value: snapshot.env.preset, mono: true }))
        rows.push(h(KeyValue, {
          key: 'w', label: 'work tree', value: snapshot.env.worktreeName, mono: true,
        }))
        rows.push(h(KeyValue, {
          key: 'sc', label: 'scope', value: snapshot.env.scope || '(none)', mono: true,
        }))
        rows.push(h(KeyValue, {
          key: 'db',
          label: 'database',
          value: snapshot.database.reachable
            ? `restored ${snapshot.database.restoredAt} (${snapshot.database.restoredAge} ago)`
            : 'unreachable',
        }))
      }
      return h('div', {
        'data-ores-environment': 'popover',
        role: 'dialog',
        'aria-label': 'Environment',
        style: {
          position: 'absolute', top: 'calc(100% + 0.35rem)', left: 0, zIndex: 60,
          width: '24rem', maxWidth: '90vw', maxHeight: '24rem', overflowY: 'auto',
          padding: '0.55rem 0.6rem', borderRadius: '0.5rem', textAlign: 'left',
          background: 'var(--dsw-specific-menu, var(--dsw-alias-bg-layer-2))',
          border: '1px solid var(--dsw-alias-border-l2)',
          boxShadow: 'var(--dsw-elevation-prominent)',
          color: 'var(--dsw-alias-label-primary)', fontSize: '0.78rem',
        },
      }, [
        !ready ? h('div', { key: 'why' }, [
          h('div', { key: 'h', style: { fontWeight: 600 } }, 'No environment'),
          h('div', {
            key: 'm',
            style: { marginTop: '0.25rem', color: 'var(--dsw-alias-label-secondary)' },
          }, state.notice || 'the state route resolved no work tree'),
        ]) : null,
        ...rows,
        ready ? h('div', { key: 'reasons', style: { marginTop: '0.35rem' } },
          snapshot.health.reasons.length > 0
            ? snapshot.health.reasons.map((reason, index) => h('div', {
                key: index,
                style: { fontSize: '0.72rem', color: WARN, padding: '0.1rem 0' },
              }, '⚠ ' + reason))
            : h('div', {
                key: 'ok',
                style: { fontSize: '0.72rem', color: STATE_COLOR.running },
              }, 'No problems reported.')) : null,
      ])
    }

    /* ------------------------------------------------------ seat 2: the view */

    const FILTERS = [
      { id: 'all', title: 'All' },
      { id: 'not-running', title: 'Not running' },
      { id: 'attention', title: 'Failed or missing' },
    ]

    function matchesFilter(unit, filter, broken) {
      if (filter === 'not-running') return unit.state !== 'running'
      if (filter === 'attention') return broken.has(unit.state)
      return true
    }

    function ServiceRow(props) {
      const unit = props.unit
      const busy = props.busy
      const color = stateColor(unit.state)
      const canStop = unit.state === 'running' || unit.state === 'starting'
      /* compass resolves a registry service, so a replicated service's rows
       * all act on the service. The title says so, because the row names one
       * replica and the action does not. */
      const selector = unit.selector
      const shared = props.replicas > 1
      return h('div', {
        'data-env-unit': unit.label,
        style: {
          display: 'flex', alignItems: 'flex-start', gap: '0.45rem',
          padding: '0.28rem 0.15rem', borderBottom: '1px solid var(--dsw-alias-border-l1)',
        },
      }, [
        h('span', { key: 'dot', style: { paddingTop: '0.3rem' } }, h(Dot, { color })),
        h(Badge, { key: 'badge', state: unit.state, color }),
        h('div', { key: 'body', style: { flex: 1, minWidth: 0 } }, [
          h('div', { key: 'l', style: { fontSize: '0.78rem', display: 'flex', gap: '0.4rem', alignItems: 'baseline' } }, [
            h('span', { key: 't', style: { fontWeight: 600 } }, unit.label),
            h('span', {
              key: 'u',
              style: { fontFamily: MONO, fontSize: '0.66rem', color: 'var(--dsw-alias-label-tertiary)', wordBreak: 'break-all' },
            }, unit.unit),
          ]),
          unit.detail ? h('div', {
            key: 'd',
            style: {
              fontSize: '0.7rem', color: 'var(--dsw-alias-label-tertiary)',
              whiteSpace: 'nowrap', overflow: 'hidden', textOverflow: 'ellipsis',
            },
            title: unit.detail,
          }, unit.detail) : null,
        ]),
        selector ? h('button', {
          key: 'act',
          type: 'button',
          disabled: busy,
          onClick: () => props.onToggle(unit, canStop ? 'service-stop' : 'service-start'),
          title: (canStop ? 'compass services stop ' : 'compass services start ')
            + selector + (shared ? ` (all ${props.replicas} copies of this service)` : ''),
          style: {
            flex: '0 0 auto', padding: '0.15rem 0.5rem', borderRadius: '0.3rem',
            border: '1px solid var(--dsw-alias-border-l2)', background: 'transparent',
            cursor: busy ? 'not-allowed' : 'pointer', opacity: busy ? 0.5 : 1,
            fontSize: '0.72rem', color: canStop ? WARN : STATE_COLOR.running,
          },
        }, canStop ? 'Stop' : 'Start') : null,
      ])
    }

    function RestoreDialog(props) {
      const model = props.model
      const phrase = model.database.confirmPhrase || model.env.name
      const [typed, setTyped] = React.useState('')
      const [stopServices, setStopServices] = React.useState(true)
      const inputRef = React.useRef(null)

      React.useEffect(() => {
        if (inputRef.current) inputRef.current.focus()
        const onKey = (event) => { if (event.key === 'Escape') props.onClose() }
        document.addEventListener('keydown', onKey)
        return () => document.removeEventListener('keydown', onKey)
      }, [props.onClose])

      const confirmed = typed.trim() === phrase
      return h('div', {
        'data-env-dialog': 'restore',
        role: 'dialog',
        'aria-modal': 'true',
        'aria-label': 'Rebuild the database',
        style: {
          position: 'fixed', inset: 0, zIndex: 80, display: 'flex',
          alignItems: 'center', justifyContent: 'center',
          background: 'rgba(0,0,0,0.45)',
        },
      }, h('div', {
        style: {
          width: '30rem', maxWidth: '92vw', padding: '0.9rem 1rem', borderRadius: '0.6rem',
          border: '1px solid var(--dsw-alias-border-l2)',
          background: 'var(--dsw-alias-bg-layer-2)',
          color: 'var(--dsw-alias-label-primary)', boxShadow: 'var(--dsw-elevation-prominent)',
        },
      }, [
        h('div', { key: 't', style: { fontSize: '0.95rem', fontWeight: 700 } }, 'Rebuild the database'),
        h('div', {
          key: 'w',
          style: { marginTop: '0.4rem', fontSize: '0.78rem', color: 'var(--dsw-alias-label-secondary)' },
        }, `This drops ${model.database.name || 'the database'} and its roles, then recreates the schema and reseeds it. Every row in it is lost. It takes minutes.`),
        h('label', {
          key: 'opt',
          style: { display: 'flex', gap: '0.4rem', alignItems: 'center', marginTop: '0.6rem', fontSize: '0.78rem' },
        }, [
          h('input', {
            key: 'i',
            type: 'checkbox',
            checked: stopServices,
            onChange: (event) => setStopServices(event.target.checked),
          }),
          h('span', { key: 's' }, 'Stop the services first, then start them again'),
        ]),
        h('div', { key: 'p', style: { marginTop: '0.6rem', fontSize: '0.75rem' } },
          'Type the environment name to confirm: ',
          h('span', { key: 'n', style: { fontFamily: MONO, fontWeight: 700 } }, phrase)),
        h('input', {
          key: 'input',
          ref: inputRef,
          type: 'text',
          value: typed,
          'aria-label': 'Type the environment name',
          onChange: (event) => setTyped(event.target.value),
          style: {
            marginTop: '0.3rem', width: '100%', padding: '0.3rem 0.4rem',
            borderRadius: '0.3rem', border: '1px solid var(--dsw-alias-border-l2)',
            background: 'var(--dsw-alias-bg-layer-1)', color: 'inherit',
            fontFamily: MONO, fontSize: '0.78rem',
          },
        }),
        h('div', {
          key: 'actions',
          style: { display: 'flex', justifyContent: 'flex-end', gap: '0.4rem', marginTop: '0.8rem' },
        }, [
          h('button', {
            key: 'cancel', type: 'button', onClick: props.onClose,
            style: {
              padding: '0.25rem 0.7rem', borderRadius: '0.3rem', cursor: 'pointer',
              border: '1px solid var(--dsw-alias-border-l2)', background: 'transparent',
              color: 'inherit', fontSize: '0.78rem',
            },
          }, 'Cancel'),
          h('button', {
            key: 'go', type: 'button', disabled: !confirmed,
            onClick: () => props.onConfirm(stopServices),
            style: {
              padding: '0.25rem 0.7rem', borderRadius: '0.3rem',
              cursor: confirmed ? 'pointer' : 'not-allowed', opacity: confirmed ? 1 : 0.5,
              border: '1px solid ' + STATE_COLOR.failed, background: 'transparent',
              color: STATE_COLOR.failed, fontSize: '0.78rem', fontWeight: 600,
            },
          }, 'Rebuild'),
        ]),
      ]))
    }

    function EnvironmentView(props) {
      const sessionId = props.sessionId
      const cwd = useSessionCwd()
      const state = useEnvironment(sessionId, cwd)
      const [filter, setFilter] = React.useState('all')
      const [restoring, setRestoring] = React.useState(false)
      const [error, setError] = React.useState('')
      const snapshot = state.snapshot
      const job = snapshot && snapshot.job ? snapshot.job : null
      const busy = !!(job && job.running)

      const act = React.useCallback(async (body) => {
        setError('')
        const result = await postAction(sessionId, cwd, body)
        if (!result.ok) setError([result.reason, result.message].filter(Boolean).join(': '))
        state.refresh()
        return result
      }, [sessionId, cwd, state.refresh])

      if (state.firstLoad) return h(Skeleton)

      const ready = !!(snapshot && snapshot.ok)
      const broken = new Set(ready ? snapshot.services.brokenStates : [])
      const units = ready ? snapshot.services.units.filter((unit) => matchesFilter(unit, filter, broken)) : []
      const counts = ready ? snapshot.services.counts : {}
      const total = ready ? snapshot.services.total : 0

      return h('div', {
        'data-ores-environment': 'view',
        style: { padding: '0.7rem 0.9rem', fontSize: '0.8rem', overflowY: 'auto', height: '100%' },
      }, [
        h('div', {
          key: 'head',
          style: { display: 'flex', alignItems: 'baseline', gap: '0.6rem', flexWrap: 'wrap' },
        }, [
          h('span', {
            key: 'n',
            style: { fontFamily: MONO, fontSize: '1rem', fontWeight: 700 },
          }, ready ? (snapshot.env.label || snapshot.env.name) : 'environment'),
          ready ? h('span', {
            key: 'p',
            style: { color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.75rem' },
          }, [snapshot.env.preset, snapshot.env.worktreeName].filter(Boolean).join(' · ')) : null,
          h('span', { key: 'spacer', style: { flex: 1 } }),
          ready ? h('span', {
            key: 'as',
            style: { color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.7rem', fontFamily: MONO },
          }, 'as of ' + (snapshot.generatedAt || '').slice(11, 19) + (state.pending ? ' …' : '')) : null,
          state.stale ? h('span', {
            key: 'stale',
            style: { color: WARN, fontSize: '0.7rem', fontWeight: 700 },
          }, 'stale') : null,
          h('button', {
            key: 'r', type: 'button', onClick: state.refresh,
            style: {
              padding: '0.2rem 0.6rem', borderRadius: '0.3rem', cursor: 'pointer',
              border: '1px solid var(--dsw-alias-border-l2)', background: 'transparent',
              color: 'inherit', fontSize: '0.75rem',
            },
          }, 'Refresh'),
        ]),
        h(Notice, { key: 'notice', notice: state.notice || error }),

        ready ? h('div', {
          key: 'tiles',
          style: { display: 'flex', gap: '0.5rem', marginTop: '0.5rem', flexWrap: 'wrap' },
        }, snapshot.tiles.map((tile) => h(Tile, { key: tile.id, tile }))) : null,

        ready && snapshot.health.reasons.length > 0 ? h('div', {
          key: 'reasons',
          style: { marginTop: '0.5rem', fontSize: '0.75rem' },
        }, snapshot.health.reasons.map((reason, index) => h('div', {
          key: index,
          style: { color: toneColor(snapshot.health.level), padding: '0.08rem 0' },
        }, '⚠ ' + reason))) : null,

        h(JobStrip, { key: 'job', job }),

        ready ? h('div', {
          key: 'db',
          style: {
            marginTop: '0.7rem', padding: '0.55rem 0.65rem', borderRadius: '0.5rem',
            border: '1px solid var(--dsw-alias-border-l1)', background: 'var(--dsw-alias-bg-layer-2)',
          },
        }, [
          h('div', {
            key: 'h',
            style: { display: 'flex', alignItems: 'center', gap: '0.5rem', marginBottom: '0.2rem' },
          }, [
            h(MicroLabel, { key: 'l' }, 'database'),
            h('span', { key: 's', style: { flex: 1 } }),
            h('button', {
              key: 'restore',
              type: 'button',
              disabled: busy,
              onClick: () => setRestoring(true),
              title: snapshot.remedies.recreateDatabase || 'compass db recreate -y -k',
              style: {
                padding: '0.2rem 0.6rem', borderRadius: '0.3rem',
                cursor: busy ? 'not-allowed' : 'pointer', opacity: busy ? 0.5 : 1,
                border: '1px solid ' + STATE_COLOR.failed, background: 'transparent',
                color: STATE_COLOR.failed, fontSize: '0.75rem',
              },
            }, 'Restore database…'),
          ]),
          h(KeyValue, {
            key: 'a',
            label: 'restored',
            value: snapshot.database.reachable
              ? `${snapshot.database.restoredAt}  (${snapshot.database.restoredAge} ago)`
              : 'unreachable',
          }),
          h(KeyValue, { key: 'v', label: 'schema', value: snapshot.database.schemaVersion || '?' }),
          h(KeyValue, {
            key: 'b', label: 'built from',
            value: [snapshot.database.builtFrom, snapshot.database.builtAt].filter(Boolean).join('  '),
            mono: true,
          }),
          h(KeyValue, {
            key: 'd', label: 'drift', value: snapshot.database.driftLabel || 'unknown',
          }),
          snapshot.database.bootstrapMode ? h('div', {
            key: 'boot',
            style: { marginTop: '0.25rem', fontSize: '0.75rem', color: WARN },
          }, '⚠ bootstrap mode is on: the provisioning wizard has not run') : null,
        ]) : null,

        ready ? h('div', {
          key: 'services',
          style: {
            marginTop: '0.7rem', padding: '0.55rem 0.65rem', borderRadius: '0.5rem',
            border: '1px solid var(--dsw-alias-border-l1)', background: 'var(--dsw-alias-bg-layer-2)',
          },
        }, [
          h('div', {
            key: 'h',
            style: { display: 'flex', alignItems: 'center', gap: '0.5rem', flexWrap: 'wrap' },
          }, [
            h(MicroLabel, { key: 'l' }, 'services'),
            h('span', {
              key: 'c',
              style: { fontFamily: MONO, fontSize: '0.72rem', display: 'flex', gap: '0.5rem', flexWrap: 'wrap' },
            }, snapshot.services.states.map((s) => h('span', {
              key: s.id,
              'data-env-count': s.id,
              style: { color: stateColor(s.id) },
            }, `${s.id} ${s.count}`))),
            h('span', { key: 's', style: { flex: 1 } }),
            h('button', {
              key: 'startall', type: 'button', disabled: busy,
              onClick: () => act({ action: 'fleet-start' }),
              title: snapshot.remedies.startServices,
              style: {
                padding: '0.2rem 0.6rem', borderRadius: '0.3rem',
                cursor: busy ? 'not-allowed' : 'pointer', opacity: busy ? 0.5 : 1,
                border: '1px solid ' + STATE_COLOR.running, background: 'transparent',
                color: STATE_COLOR.running, fontSize: '0.75rem',
              },
            }, 'Start all'),
            h('button', {
              key: 'stopall', type: 'button', disabled: busy,
              onClick: () => act({ action: 'fleet-stop' }),
              title: snapshot.remedies.stopServices,
              style: {
                padding: '0.2rem 0.6rem', borderRadius: '0.3rem',
                cursor: busy ? 'not-allowed' : 'pointer', opacity: busy ? 0.5 : 1,
                border: '1px solid ' + WARN, background: 'transparent',
                color: WARN, fontSize: '0.75rem',
              },
            }, 'Stop all'),
          ]),
          snapshot.services.nats ? h('div', {
            key: 'nats',
            style: {
              display: 'flex', alignItems: 'center', gap: '0.45rem',
              padding: '0.28rem 0.15rem', borderBottom: '1px solid var(--dsw-alias-border-l1)',
              fontSize: '0.78rem',
            },
          }, [
            h(Dot, { key: 'd', color: stateColor(snapshot.services.nats.state) }),
            h(Badge, { key: 'b', state: snapshot.services.nats.state, color: stateColor(snapshot.services.nats.state) }),
            h('span', { key: 'l', style: { flex: 1 } }, snapshot.services.nats.label),
            h('span', {
              key: 'n',
              style: { fontSize: '0.7rem', color: 'var(--dsw-alias-label-tertiary)' },
            }, 'starts with the fleet'),
          ]) : null,
          h('div', {
            key: 'filters',
            style: { display: 'flex', gap: '0.35rem', margin: '0.4rem 0 0.2rem', flexWrap: 'wrap' },
          }, FILTERS.map((entry) => h('button', {
            key: entry.id,
            type: 'button',
            'data-env-filter': entry.id,
            'aria-pressed': filter === entry.id ? 'true' : 'false',
            onClick: () => setFilter(entry.id),
            style: {
              padding: '0.12rem 0.5rem', borderRadius: '1rem', cursor: 'pointer',
              fontSize: '0.7rem',
              border: '1px solid ' + (filter === entry.id ? ACCENT : 'var(--dsw-alias-border-l2)'),
              background: 'transparent',
              color: filter === entry.id ? ACCENT : 'var(--dsw-alias-label-secondary)',
            },
          }, entry.title))),
          units.length === 0
            ? h('div', {
                key: 'none',
                style: { padding: '0.5rem 0.15rem', color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.75rem' },
              }, total === 0 ? 'This environment has no service units deployed.' : 'No unit matches this filter.')
            : units.map((unit) => h(ServiceRow, {
                key: unit.unit,
                unit,
                busy,
                replicas: ready
                  ? snapshot.services.units.filter((other) => other.selector === unit.selector).length
                  : 1,
                onToggle: (target, action) => act({ action, service: target.selector }),
              })),
          snapshot.services.logDir ? h('div', {
            key: 'logs',
            style: {
              marginTop: '0.35rem', fontFamily: MONO, fontSize: '0.66rem',
              color: 'var(--dsw-alias-label-tertiary)', wordBreak: 'break-all',
            },
          }, 'logs: ' + snapshot.services.logDir) : null,
        ]) : null,

        restoring ? h(RestoreDialog, {
          key: 'dialog',
          model: snapshot,
          onClose: () => setRestoring(false),
          onConfirm: async (stopServices) => {
            setRestoring(false)
            await act({
              action: 'database-restore',
              stopServices,
              confirm: snapshot.database.confirmPhrase || snapshot.env.name,
            })
          },
        }) : null,
      ])
    }

    /* --------------------------------------------------------------- style */

    function css() {
      return [
        '[data-ores-environment]{box-sizing:border-box}',
        '[data-ores-environment] *{box-sizing:border-box}',
        '[data-ores-environment] button{font:inherit;color:inherit}',
        '[data-ores-environment] input{font:inherit}',
        '[data-ores-environment="view"]{scrollbar-color:var(--dsw-alias-scrollbar-bg-l2) transparent}',
        '[data-env-unit]:hover{background:var(--dsw-alias-bg-layer-1)}',
        '[data-ores-environment] :focus-visible{outline:2px solid ' + ACCENT + ';outline-offset:1px}',
      ].join('\n')
    }

    /* The stylesheet has one element and two owners, so it is reference
     * counted: the element goes away only when the last seat unmounts. */
    let styleRefs = 0

    function useStyles() {
      React.useEffect(() => {
        styleRefs += 1
        if (styleRefs === 1) {
          const node = document.createElement('style')
          node.setAttribute('data-plugin', TAG)
          node.textContent = css()
          document.head.appendChild(node)
        }
        return () => {
          styleRefs -= 1
          if (styleRefs > 0) return
          styleRefs = 0
          const node = document.querySelector('style[data-plugin="' + TAG + '"]')
          if (node && node.parentNode) node.parentNode.removeChild(node)
        }
      }, [])
    }

    /* The session's workspace root, read from the browser session store. */
    const SessionsContext = React.createContext('')

    function SessionCwd(props) {
      const hook = typeof props.useSessions === 'function'
        ? props.useSessions
        : (props.ctx ? props.ctx.get('sessions') : null)
      let cwd = ''
      if (typeof hook === 'function') {
        cwd = str(hook((store) => {
          if (!store || !store.byId) return ''
          const entry = store.byId[props.sessionId]
          return entry ? entry.cwd : ''
        }))
      }
      return h(SessionsContext.Provider, { value: cwd }, props.children)
    }

    function useSessionCwd() {
      return str(React.useContext(SessionsContext))
    }

    function NowSeat(props) {
      useStyles()
      return h(SessionCwd, {
        sessionId: props.sessionId, useSessions: props.useSessions, ctx: props.ctx,
      }, h(NowChip, { sessionId: props.sessionId }))
    }

    function ViewSeat(props) {
      useStyles()
      return h(SessionCwd, {
        sessionId: props.sessionId, useSessions: props.useSessions, ctx: props.ctx,
      }, h(EnvironmentView, { sessionId: props.sessionId }))
    }

    /* ------------------------------------------------------------ register */

    function apply(ctx) {
      if (!ctx || !ctx.slots) return
      const Now = (props) => h(NowSeat, {
        sessionId: props.sessionId, useSessions: props.useSessions, ctx,
      })
      const View = (props) => h(ViewSeat, {
        sessionId: props.sessionId, useSessions: props.useSessions, ctx,
      })
      ctx.effect(() => ctx.slots.inject('conversation.session.header.actions', () =>
        ctx.slots.register(
          { name: 'conversation.session.header.actions', id: 'ores-dsh-environment-now', order: 21 },
          Now)),
        'ores-dsh-environment: now readout')
      ctx.effect(() => ctx.slots.inject('conversation.view', () =>
        ctx.slots.register(
          { name: 'conversation.view', id: 'environment', order: 30, label: () => 'Environment' },
          View)),
        'ores-dsh-environment: environment view')
    }

    exports.name = 'ores-dsh-environment'
    exports.inject = ['slots']
    exports.apply = apply
    /* The model builder, exposed so a test can assert that every field the
     * view reads is one this half produces. Nothing in the harness reads it. */
    exports.__test = { normalize }
    return module.exports
  },
})
