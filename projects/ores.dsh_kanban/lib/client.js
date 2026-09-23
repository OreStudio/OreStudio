/*
 * ores-dsh-kanban — browser half.
 *
 * Hand-written, no build step, no dependencies beyond the platform `react`
 * module. The frozen interface is projects/ores.dsh_kanban/CONTRACT.md.
 */
window.__ModuleLoader__.load({
  id: 'ores-dsh-kanban',
  factory: (require) => {
    var module = { exports: {} }
    var exports = module.exports

    const React = require('react')
    const h = React.createElement

    const STYLE_ID = 'ores-dsh-kanban-style'
    const TAG = 'ores-dsh-kanban'
    const PULL_REQUEST_BASE = 'https://github.com/OreStudio/OreStudio/pull/'
    const MONO = 'ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, monospace'
    const ACCENT = 'var(--dsw-alias-state-business-primary)'
    const STALE_COLOR = 'var(--dsw-alias-state-warn-primary)'

    /* The one agile state palette. Keyed by the state ids the host sends. */
    const STATE_COLOR = {
      BACKLOG: '#6e7681',
      DISCOVERED: '#6e7681',
      STARTED: '#58a6ff',
      BLOCKED: '#d29922',
      DONE: '#3fb950',
      ABANDONED: '#f85149',
      UNKNOWN: '#6e7681',
    }
    const CLOSED_STATES = { DONE: true, ABANDONED: true }
    const stateColor = (s) => STATE_COLOR[s] || STATE_COLOR.UNKNOWN

    /* Deterministic pastel per epic, so related cards share a hue. */
    const EPIC_HUES = {}
    function epicColor(epic) {
      const key = epic || ''
      if (EPIC_HUES[key] === undefined) {
        let hue = 0
        for (let i = 0; i < key.length; i++) hue = (hue * 31 + key.charCodeAt(i)) % 360
        EPIC_HUES[key] = 'hsl(' + hue + ', 70%, 72%)'
      }
      return EPIC_HUES[key]
    }

    function str(v) {
      if (typeof v === 'string') return v
      if (typeof v === 'number' && isFinite(v)) return String(v)
      return ''
    }
    function num(v) {
      const n = typeof v === 'number' ? v : parseInt(str(v), 10)
      return isFinite(n) ? n : 0
    }
    function arr(v) {
      return Array.isArray(v) ? v : []
    }
    function bool(v) {
      return v === true
    }
    function asList(v) {
      const out = []
      for (const item of arr(v)) {
        const s = str(item)
        if (s) out.push(s)
      }
      return out
    }
    function truncate(text, max) {
      const s = str(text)
      return s.length > max ? s.slice(0, max - 1) + '…' : s
    }
    function plural(n, word) {
      return str(n) + ' ' + word + (n === 1 ? '' : 's')
    }
    function isoDay(value) {
      const s = str(value)
      const m = /^(\d{4})-(\d{2})-(\d{2})/.exec(s)
      return m ? m[1] + '-' + m[2] + '-' + m[3] : ''
    }
    function todayUtc() {
      return new Date().toISOString().slice(0, 10)
    }
    function ageDays(created) {
      const then = Date.parse(str(created))
      if (!isFinite(then)) return 0
      const days = Math.floor((Date.now() - then) / 86400000)
      return days > 0 ? days : 0
    }
    function titleMatches(story, query) {
      const q = query.trim().toLowerCase()
      if (!q) return true
      if (str(story.title).toLowerCase().includes(q)) return true
      if (str(story.id).toLowerCase().includes(q)) return true
      if (str(story.epic).toLowerCase().includes(q)) return true
      if (str(story.environment).toLowerCase().includes(q)) return true
      for (const name of story.branches) if (name.toLowerCase().includes(q)) return true
      for (const task of story.tasks) {
        if (str(task.title).toLowerCase().includes(q)) return true
        if (str(task.environment).toLowerCase().includes(q)) return true
      }
      return false
    }

    /* ---------------------------------------------------------------- model */

    function normalizeTask(raw) {
      const task = raw && typeof raw === 'object' ? raw : {}
      return {
        id: str(task.id),
        slug: str(task.slug),
        title: str(task.title),
        state: str(task.state) || 'UNKNOWN',
        branch: str(task.branch),
        pr: str(task.pr),
        blockedOn: str(task.blockedOn),
        blockedSince: str(task.blockedSince),
        environment: str(task.environment),
        created: str(task.created),
        updated: str(task.updated),
        scaffold: bool(task.scaffold),
        path: str(task.path),
      }
    }

    function normalizeStory(raw) {
      const story = raw && typeof raw === 'object' ? raw : {}
      const tasks = arr(story.tasks).map(normalizeTask)
      const progress = story.progress && typeof story.progress === 'object' ? story.progress : {}
      const done = num(progress.done)
      const abandoned = num(progress.abandoned)
      const total = Math.max(num(progress.total), tasks.length)
      return {
        id: str(story.id),
        slug: str(story.slug),
        title: str(story.title),
        state: str(story.state) || 'UNKNOWN',
        epic: str(story.epic),
        description: str(story.description),
        environment: str(story.environment),
        created: str(story.created),
        updated: str(story.updated),
        path: str(story.path),
        progress: { done: done, total: total, abandoned: abandoned },
        branches: asList(story.branches),
        prs: arr(story.prs).map(num).filter((n) => n > 0),
        tasks: tasks,
      }
    }

    function normalize(payload) {
      const raw = payload && typeof payload === 'object' ? payload : {}
      const tree = raw.tree && typeof raw.tree === 'object' ? raw.tree : {}
      const sprint = raw.sprint && typeof raw.sprint === 'object' ? raw.sprint : {}
      const counts = raw.counts && typeof raw.counts === 'object' ? raw.counts : {}
      const filters = raw.filters && typeof raw.filters === 'object' ? raw.filters : {}
      const stories = arr(raw.stories).map(normalizeStory)
      /* The fleet strip is a read-only report: it never changes what the board shows. */
      const trees = arr(raw.trees).map((entry) => {
        const row = entry && typeof entry === 'object' ? entry : {}
        return {
          label: str(row.label),
          name: str(row.name),
          root: str(row.root),
          branch: str(row.branch),
          detached: bool(row.detached),
          dirty: bool(row.dirty),
          isSession: bool(row.isSession),
          currentStoryId: str(row.currentStoryId),
          currentTaskId: str(row.currentTaskId),
          storyTitle: str(row.storyTitle),
          taskTitle: str(row.taskTitle),
          state: str(row.state) || 'UNKNOWN',
          pr: str(row.pr),
        }
      })
      const columns = arr(raw.columns).map((entry) => {
        const col = entry && typeof entry === 'object' ? entry : {}
        return {
          id: str(col.id) || 'UNKNOWN',
          title: str(col.title) || str(col.id) || 'Unknown',
          count: num(col.count),
        }
      })
      return {
        ok: raw.ok === true,
        reason: str(raw.reason),
        message: str(raw.message),
        generatedAt: str(raw.generatedAt),
        tree: {
          root: str(tree.root),
          name: str(tree.name),
          label: str(tree.label),
          branch: str(tree.branch),
          detached: bool(tree.detached),
          dirty: bool(tree.dirty),
          currentStoryId: str(tree.currentStoryId),
          currentTaskId: str(tree.currentTaskId),
          by: str(tree.by),
        },
        sprint: {
          version: str(sprint.version),
          name: str(sprint.name),
          title: str(sprint.title),
          startDate: str(sprint.startDate),
          endDate: str(sprint.endDate),
          dayOfSprint: num(sprint.dayOfSprint),
          totalDays: num(sprint.totalDays),
          path: str(sprint.path),
        },
        columns: columns,
        stories: stories,
        trees: trees,
        counts: {
          stories: num(counts.stories),
          storiesDone: num(counts.storiesDone),
          storiesStarted: num(counts.storiesStarted),
          storiesBlocked: num(counts.storiesBlocked),
          tasks: num(counts.tasks),
          tasksDone: num(counts.tasksDone),
        },
        filters: { environments: asList(filters.environments), epics: asList(filters.epics) },
      }
    }

    /* The state list lives in the host only. The client never derives a column set. */
    function columnIdOf(story) {
      return story.state === 'DISCOVERED' ? 'BACKLOG' : story.state
    }

    function storiesInColumn(snapshot, column) {
      const out = []
      for (const story of snapshot.stories) {
        if (columnIdOf(story) === column.id) out.push(story)
      }
      return out
    }

    function storyMatches(story, filters) {
      const envs = filters.environments
      const epics = filters.epics
      if (envs.length > 0 || epics.length > 0) {
        const envOk = envs.length === 0 || envs.indexOf(story.environment) >= 0
          || story.tasks.some((task) => envs.indexOf(task.environment) >= 0)
        const epicOk = epics.length === 0 || epics.indexOf(story.epic) >= 0
        if (!envOk || !epicOk) return false
      }
      return titleMatches(story, filters.query)
    }

    function deriveTiles(snapshot) {
      let inFlight = 0
      let blocked = 0
      let tasks = 0
      let tasksDone = 0
      for (const story of snapshot.stories) {
        if (story.state === 'STARTED') inFlight += 1
        if (story.state === 'BLOCKED') blocked += 1
        tasks += story.progress.total
        tasksDone += story.progress.done
      }
      return [
        { key: 'stories', label: 'stories', value: str(snapshot.counts.stories) },
        { key: 'in-flight', label: 'in flight', value: str(inFlight), color: stateColor('STARTED') },
        { key: 'blocked', label: 'blocked', value: str(blocked), color: stateColor('BLOCKED') },
        { key: 'tasks-done', label: 'tasks done', value: str(tasksDone) + '/' + str(tasks) },
      ]
    }

    /* ------------------------------------------------------------- data hook */

    /* Fetch keyed by the session id alone: the board always belongs to the session's tree. */
    function useSnapshot(sessionId, cwd) {
      const [snapshot, setSnapshot] = React.useState(null)
      const [stale, setStale] = React.useState(false)
      const [loading, setLoading] = React.useState(true)
      const [notice, setNotice] = React.useState('')
      const [nonce, setNonce] = React.useState(0)
      const lastGood = React.useRef(null)
      const lastKey = React.useRef(sessionId)

      React.useEffect(() => {
        let alive = true
        /* A new fetch key starts from scratch; a refetch of the same key keeps its snapshot. */
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
        const url = '/plugins/ores-dsh-kanban/state?' + params.join('&')
        fetch(url, { credentials: 'same-origin', cache: 'no-store' })
          .then((response) => response.json())
          .then((payload) => {
            if (!alive) return
            if (payload && typeof payload === 'object' && payload.ok === true) {
              const next = normalize(payload)
              lastGood.current = next
              setSnapshot(next)
              setStale(false)
              setNotice('')
            } else {
              const reason = payload && typeof payload === 'object' ? str(payload.reason) : ''
              const message = payload && typeof payload === 'object' ? str(payload.message) : ''
              setNotice([reason, message].filter(Boolean).join(': ') || 'state route returned no data')
              if (lastGood.current) setStale(true)
              else setSnapshot(normalize(payload))
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
      /* A refresh keeps the previous snapshot visible until the new one lands. */
      return {
        snapshot: snapshot,
        firstLoad: loading && snapshot === null,
        pending: snapshot !== null && loading,
        stale: stale,
        notice: notice,
        refresh: refresh,
      }
    }

    /* ---------------------------------------------------------------- pieces */

    function Dot(props) {
      return h('span', {
        'aria-hidden': 'true',
        style: {
          display: 'inline-block', width: '0.5rem', height: '0.5rem', flexShrink: 0,
          marginRight: props.gap === false ? '0' : '0.35rem',
          borderRadius: '50%', background: stateColor(props.state),
        },
      })
    }

    function Chip(props) {
      return h('span', {
        style: {
          display: 'inline-flex', alignItems: 'center', gap: '0.25rem',
          padding: '0.05rem 0.35rem', borderRadius: '0.25rem',
          background: 'var(--dsw-alias-bg-layer-2)', color: 'var(--dsw-alias-label-secondary)',
          fontFamily: MONO, fontSize: '0.7rem', whiteSpace: 'nowrap',
        },
      }, props.children)
    }

    function Badge(props) {
      return h('span', {
        style: {
          display: 'inline-flex', alignItems: 'center', padding: '0.05rem 0.35rem',
          borderRadius: '0.25rem', background: stateColor(props.state),
          color: 'var(--dsw-alias-bg-base)', fontSize: '0.68rem', fontWeight: 700,
          letterSpacing: '0.04em', whiteSpace: 'nowrap',
        },
      }, props.label || props.state)
    }

    function MicroLabel(props) {
      return h('span', {
        style: {
          color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.72rem',
          letterSpacing: '0.06em', textTransform: 'uppercase',
        },
      }, props.children)
    }

    function KeyValue(props) {
      return h('div', {
        style: { display: 'flex', gap: '0.5rem', alignItems: 'baseline', fontSize: '0.78rem' },
      }, [
        h('span', {
          key: 'k',
          style: { flex: '0 0 auto', width: '5.5rem', color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.72rem' },
        }, props.label),
        h('span', { key: 'v', style: { minWidth: 0, wordBreak: 'break-word' } }, props.children),
      ])
    }

    function PrLinks(props) {
      const prs = arr(props.prs).map(num).filter((n) => n > 0)
      if (prs.length === 0) return h('span', { style: { color: 'var(--dsw-alias-label-tertiary)' } }, '—')
      return h('span', { style: { display: 'inline-flex', gap: '0.4rem' } }, prs.map((n) =>
        h('a', {
          key: n,
          href: PULL_REQUEST_BASE + str(n),
          target: '_blank',
          rel: 'noreferrer',
          onClick: (event) => event.stopPropagation(),
          style: { color: 'var(--dsw-alias-link)', textDecoration: 'none' },
        }, '#' + str(n))))
    }

    /* The one line that reports task progress: the bar with `N tasks · M done`, or
     * `no tasks` when the story directory holds none. A story with no tasks must
     * not report that fact twice. */
    function ProgressBar(props) {
      const progress = props.progress
      const total = progress.total > 0 ? progress.total : 0
      if (total === 0) {
        return h('div', {
          'data-ores-kanban': 'progress',
          'data-empty': 'true',
          style: { marginTop: '0.35rem', color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.72rem' },
        }, 'no tasks')
      }
      const donePct = Math.round(100 * progress.done / total)
      const abandonedPct = Math.round(100 * progress.abandoned / total)
      return h('div', { 'data-ores-kanban': 'progress' }, [
        h('div', {
          key: 'bar',
          title: str(progress.done) + '/' + str(total) + ' tasks done',
          style: {
            display: 'flex', height: '0.4rem', marginTop: '0.4rem',
            background: 'var(--dsw-alias-bg-layer-2)', borderRadius: '0.2rem', overflow: 'hidden',
          },
        }, [
          h('span', { key: 'd', style: { width: donePct + '%', background: stateColor('DONE'), opacity: 0.75 } }),
          h('span', { key: 'a', style: { width: abandonedPct + '%', background: stateColor('ABANDONED'), opacity: 0.75 } }),
        ]),
        h('div', {
          key: 'label',
          style: { marginTop: '0.35rem', color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.72rem' },
        }, plural(total, 'task') + ' · ' + str(progress.done) + ' done'),
      ])
    }

    function Skeleton() {
      return h('div', { 'data-ores-kanban': 'skeleton', style: { display: 'grid', gap: '0.8rem' } },
        h('div', { style: { color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.78rem' } }, 'Loading the sprint…'),
        h('div', { style: { display: 'flex', gap: '0.8rem' } }, [0, 1, 2, 3].map((i) =>
          h('div', {
            key: i,
            style: {
              width: '16rem', height: '9rem', borderRadius: '0.5rem',
              background: 'var(--dsw-alias-bg-layer-1)', border: '1px solid var(--dsw-alias-border-l1)',
            },
          }))))
    }

    function Failed(props) {
      return h('div', {
        'data-ores-kanban': 'failure',
        style: {
          padding: '1rem', borderRadius: '0.5rem', fontSize: '0.78rem',
          border: '1px solid ' + STALE_COLOR, color: 'var(--dsw-alias-state-warn-label)',
          background: 'var(--dsw-alias-bg-layer-1)',
        },
      }, [
        h('div', { key: 'h', style: { fontWeight: 600 } }, 'The sprint could not be read'),
        h('div', { key: 'm' }, props.notice || 'the state route returned no data'),
      ])
    }

    function TaskRow(props) {
      const task = props.task
      const [open, setOpen] = React.useState(false)
      return h('div', {
        'data-ores-task': task.slug || task.id,
        style: {
          borderTop: '1px solid var(--dsw-alias-border-l1)',
          padding: '0.4rem 0', fontSize: '0.78rem',
        },
      }, [
        h('button', {
          key: 'head',
          type: 'button',
          'aria-expanded': open ? 'true' : 'false',
          onClick: () => setOpen((value) => !value),
          style: {
            display: 'flex', width: '100%', alignItems: 'center', gap: '0.4rem', textAlign: 'left',
            padding: 0, border: 0, background: 'transparent', color: 'inherit',
            cursor: 'pointer', fontSize: '0.78rem',
          },
        }, [
          h(Dot, { key: 'dot', state: task.state }),
          h('span', { key: 'title', style: { flex: 1, minWidth: 0, wordBreak: 'break-word' } }, task.title || task.slug),
          task.environment ? h(Chip, { key: 'env' }, task.environment) : null,
          h(Badge, { key: 'badge', state: task.state }),
        ]),
        task.branch ? h('div', {
          key: 'branch',
          style: { marginTop: '0.2rem', color: 'var(--dsw-alias-label-tertiary)', fontFamily: MONO, fontSize: '0.68rem' },
        }, task.branch + (task.pr ? ' · #' + task.pr : '')) : null,
        open ? h('div', {
          key: 'detail',
          style: {
            display: 'grid', gap: '0.15rem', marginTop: '0.4rem', padding: '0.4rem 0.5rem',
            borderRadius: '0.25rem', background: 'var(--dsw-alias-bg-layer-1)',
          },
        }, [
          h(KeyValue, { key: 'bo', label: 'blocked on' }, task.blockedOn || '—'),
          h(KeyValue, { key: 'bs', label: 'blocked since' }, task.blockedSince || '—'),
          h(KeyValue, { key: 'c', label: 'created' }, task.created || '—'),
          h(KeyValue, { key: 'u', label: 'updated' }, task.updated || '—'),
          h(KeyValue, { key: 'br', label: 'branch' }, task.branch || '—'),
          h(KeyValue, { key: 'pr', label: 'pr' }, task.pr ? '#' + task.pr : '—'),
          h(KeyValue, { key: 'p', label: 'file' }, h('span', {
            style: { fontFamily: MONO, fontSize: '0.68rem', wordBreak: 'break-all' },
          }, task.path || '—')),
        ]) : null,
      ])
    }

    function CardDetail(props) {
      const story = props.story
      const close = props.onClose
      React.useEffect(() => {
        const onKey = (event) => { if (event.key === 'Escape') close() }
        window.addEventListener('keydown', onKey)
        return () => window.removeEventListener('keydown', onKey)
      }, [close])
      const age = CLOSED_STATES[story.state] ? '' : 'open ' + str(ageDays(story.created)) + 'd'
      return h('aside', {
        'data-ores-kanban': 'detail',
        role: 'complementary',
        'aria-label': 'Story detail',
        style: {
          flex: '0 0 auto', width: '22rem', maxHeight: '70vh', overflowY: 'auto',
          padding: '0.7rem 0.8rem', borderRadius: '0.5rem',
          background: 'var(--dsw-alias-bg-layer-1)', border: '1px solid var(--dsw-alias-border-l2)',
          fontSize: '0.78rem',
          /* While the panel is open it is the active surface. The shell's split-view
           * resize handles sit at z-index 8 and cross its interior, so the panel has
           * to sit above them or a band of it takes no pointer events. */
          position: 'relative', zIndex: 20,
        },
      }, [
        h('div', {
          key: 'head',
          style: { display: 'flex', alignItems: 'flex-start', gap: '0.4rem', marginBottom: '0.4rem' },
        }, [
          h('div', { key: 'title', style: { flex: 1, minWidth: 0 } }, [
            h('div', { key: 't', style: { fontSize: '0.9rem', fontWeight: 600, lineHeight: 1.3 } }, story.title),
            h('div', {
              key: 'id',
              style: {
                marginTop: '0.15rem', color: 'var(--dsw-alias-label-tertiary)',
                fontFamily: MONO, fontSize: '0.68rem', wordBreak: 'break-all',
              },
            }, story.id || '—'),
          ]),
          h(Badge, { key: 'badge', state: story.state }),
          h('button', {
            key: 'close',
            type: 'button',
            'data-ores-kanban': 'detail-close',
            'aria-label': 'Close',
            onClick: close,
            style: {
              flex: '0 0 auto', padding: '0.1rem 0.4rem', border: '1px solid var(--dsw-alias-border-l2)',
              borderRadius: '0.25rem', background: 'transparent', color: 'inherit',
              cursor: 'pointer', fontSize: '0.72rem',
            },
          }, '✕'),
        ]),
        h(MicroLabel, { key: 'label' }, 'story'),
        h('div', { key: 'fields', style: { display: 'grid', gap: '0.15rem', margin: '0.4rem 0 0.6rem' } }, [
          h(KeyValue, { key: 'env', label: 'environment' }, story.environment || '—'),
          h(KeyValue, { key: 'epic', label: 'epic' }, story.epic || '—'),
          h(KeyValue, { key: 'created', label: 'created' }, isoDay(story.created) || '—'),
          h(KeyValue, { key: 'updated', label: 'updated' }, isoDay(story.updated) || '—'),
          h(KeyValue, { key: 'age', label: 'age' }, age || 'closed'),
          h(KeyValue, { key: 'files', label: 'file' }, h('span', {
            style: { fontFamily: MONO, fontSize: '0.68rem', wordBreak: 'break-all' },
          }, story.path || '—')),
        ]),
        h(MicroLabel, { key: 'dl' }, 'description'),
        h('div', {
          key: 'desc',
          style: {
            margin: '0.3rem 0 0.6rem', whiteSpace: 'pre-wrap', wordBreak: 'break-word',
            color: 'var(--dsw-alias-label-secondary)', lineHeight: 1.5,
          },
        }, story.description || '—'),
        h(MicroLabel, { key: 'bl' }, 'branches'),
        h('div', {
          key: 'branches',
          style: { margin: '0.3rem 0 0.6rem', fontFamily: MONO, fontSize: '0.7rem', wordBreak: 'break-all' },
        }, story.branches.length > 0 ? story.branches.join(' · ') : '—'),
        h(MicroLabel, { key: 'pl' }, 'pull requests'),
        h('div', { key: 'prs', style: { margin: '0.3rem 0 0.6rem' } }, h(PrLinks, { prs: story.prs })),
        h(MicroLabel, { key: 'tl' }, plural(story.tasks.length, 'task')),
        h('div', { key: 'tasks' }, story.tasks.length > 0
          ? story.tasks.map((task) => h(TaskRow, { key: task.slug || task.id || task.title, task: task }))
          : h('div', { style: { padding: '0.4rem 0', color: 'var(--dsw-alias-label-tertiary)' } }, 'No tasks in this story directory.')),
      ])
    }

    function StoryCard(props) {
      const story = props.story
      const current = props.current
      const selected = props.selected
      const registerRef = props.registerRef
      const age = CLOSED_STATES[story.state] ? '' : 'open ' + str(ageDays(story.created)) + 'd'
      const border = (current || selected)
        ? '1px solid ' + ACCENT
        : '1px solid var(--dsw-alias-border-l1)'
      return h('div', {
        ref: registerRef,
        'data-ores-kanban': 'card',
        'data-story-id': story.id,
        'data-story-title': story.title,
        'data-current': current ? 'true' : 'false',
        onClick: () => props.onSelect(story),
        style: {
          border: border, borderLeft: '3px solid ' + epicColor(story.epic),
          borderRadius: '0.375rem', padding: '0.5rem 0.6rem', cursor: 'pointer',
          background: 'var(--dsw-alias-bg-layer-1)',
          boxShadow: current ? '0 0 0 1px ' + ACCENT : 'none',
        },
      }, [
        h('div', { key: 'title', style: { fontSize: '0.82rem', lineHeight: 1.35, wordBreak: 'break-word' } }, story.title),
        h('div', {
          key: 'meta',
          style: { display: 'flex', flexWrap: 'wrap', alignItems: 'center', gap: '0.3rem', marginTop: '0.35rem' },
        }, [
          story.environment
            ? h(Chip, { key: 'env' }, story.environment)
            : null,
          story.epic
            ? h('span', {
              key: 'epic',
              'data-ores-epic-label': story.epic,
              style: { color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.72rem' },
            }, story.epic)
            : null,
          current ? h('span', {
            key: 'current',
            'data-ores-marker': 'current',
            style: { color: ACCENT, fontSize: '0.68rem', fontWeight: 700, letterSpacing: '0.04em' },
          }, 'current') : null,
        ]),
        /* The progress line carries the task count; a story with no tasks says so once. */
        h(ProgressBar, { key: 'progress', progress: story.progress }),
        /* Branches and pull requests stay off the card face: they live in the detail panel. */
        h('div', {
          key: 'age',
          'data-ores-kanban': 'card-age',
          style: { marginTop: '0.3rem', fontSize: '0.7rem', color: 'var(--dsw-alias-label-tertiary)' },
        }, [isoDay(story.created), age].filter(Boolean).join(' · ')),
      ])
    }

    function Column(props) {
      const column = props.column
      const color = stateColor(column.id)
      return h('div', {
        'data-ores-kanban': 'column',
        'data-column-id': column.id,
        style: {
          display: 'flex', flex: '0 0 auto', flexDirection: 'column',
          width: '17rem', minWidth: '17rem', maxHeight: '70vh',
          borderRadius: '0.5rem', background: 'var(--dsw-alias-bg-layer-1)',
          border: '1px solid var(--dsw-alias-border-l1)',
        },
      }, [
        h('div', {
          key: 'head',
          style: {
            display: 'flex', alignItems: 'center', justifyContent: 'space-between',
            gap: '0.4rem', padding: '0.45rem 0.55rem',
            borderBottom: '1px solid var(--dsw-alias-border-l1)',
          },
        }, [
          h('span', {
            key: 'title',
            style: {
              color: color, fontSize: '0.72rem', fontWeight: 700,
              letterSpacing: '0.06em', textTransform: 'uppercase',
            },
          }, column.title),
          h('span', {
            key: 'count',
            style: {
              padding: '0 0.35rem', borderRadius: '999px',
              background: 'var(--dsw-alias-bg-layer-2)',
              color: 'var(--dsw-alias-label-secondary)', fontSize: '0.7rem',
            },
          }, str(props.count)),
        ]),
        h('div', {
          key: 'cards',
          style: {
            display: 'flex', flexDirection: 'column', gap: '0.5rem',
            minHeight: '3rem', overflowY: 'auto', padding: '0.5rem',
          },
        }, props.cards.length > 0
          ? props.cards.map((story) => {
            const current = story.id !== '' && story.id === props.currentStoryId
            return h(StoryCard, {
              key: story.id || story.slug || story.title,
              story: story,
              current: current,
              selected: props.selectedId !== '' && story.id === props.selectedId,
              registerRef: current ? props.registerRef : null,
              onSelect: props.onSelect,
            })
          })
          : h('div', {
            style: { padding: '1rem 0', textAlign: 'center', color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.72rem' },
          }, 'empty')),
      ])
    }

    function BoardView(props) {
      const sessionId = props.sessionId
      const cwd = useSessionCwd()
      const state = useSnapshot(sessionId, cwd)
      const snapshot = state.snapshot
      const [query, setQuery] = React.useState('')
      const [envFilter, setEnvFilter] = React.useState([])
      const [epicFilter, setEpicFilter] = React.useState([])
      const [selectedId, setSelectedId] = React.useState('')
      const currentRef = React.useRef(null)

      /* Bring the work tree's current story into view on first mount. Scroll the
       * nearest scrollable ancestor only: the session container has no bounded
       * height, so scrollIntoView would walk past it and scroll the whole page. */
      React.useEffect(() => {
        const node = currentRef.current
        if (!node) return
        let box = node.parentElement
        while (box) {
          const overflowY = getComputedStyle(box).overflowY
          if ((overflowY === 'auto' || overflowY === 'scroll') && box.scrollHeight > box.clientHeight) {
            const card = node.getBoundingClientRect()
            const frame = box.getBoundingClientRect()
            box.scrollTop += card.top - frame.top - 8
            return
          }
          box = box.parentElement
        }
      }, [snapshot])

      const selected = snapshot && selectedId
        ? snapshot.stories.filter((story) => story.id === selectedId)[0] || null
        : null
      const sprint = snapshot ? snapshot.sprint : null
      const sprintName = sprint ? sprint.title || sprint.name || 'Sprint' : 'the sprint'
      const treeName = snapshot ? snapshot.tree.label || snapshot.tree.name || '—' : 'this work tree'

      if (state.firstLoad) {
        return h('div', { 'data-ores-kanban': 'view', style: { padding: '0.9rem 1rem', fontSize: '0.82rem' } },
          h(Skeleton))
      }
      if (!snapshot) {
        return h('div', { 'data-ores-kanban': 'view', style: { padding: '0.9rem 1rem', fontSize: '0.82rem' } },
          h(Failed, { notice: state.notice }))
      }
      /* The client never derives a column set; an empty one means there is no board. */
      if (snapshot.columns.length === 0) {
        return h('div', {
          'data-ores-kanban': 'view',
          'data-columns': 'none',
          style: { padding: '0.9rem 1rem', fontSize: '0.82rem' },
        }, h('div', {
          'data-ores-kanban': 'empty',
          style: {
            padding: '1.5rem', textAlign: 'center', borderRadius: '0.5rem',
            background: 'var(--dsw-alias-bg-layer-1)',
            border: '1px solid var(--dsw-alias-border-l1)',
            color: 'var(--dsw-alias-label-secondary)',
          },
        }, [
          h('div', { key: 'm' }, 'No board for ' + sprintName + ' in ' + treeName + '.'),
          h('div', {
            key: 'w',
            style: { marginTop: '0.3rem', color: STALE_COLOR, fontSize: '0.72rem' },
          }, state.notice || 'the state route sent no columns'),
        ]))
      }

      const currentStoryId = snapshot.tree.currentStoryId
      const filters = { query: query, environments: envFilter, epics: epicFilter }
      const visible = snapshot.stories.filter((story) => storyMatches(story, filters))
      const columns = snapshot.columns
      const tiles = deriveTiles(snapshot)

      const toggle = (list, setList, value) => {
        setList(list.indexOf(value) >= 0 ? list.filter((item) => item !== value) : list.concat([value]))
      }
      const clearAll = () => { setQuery(''); setEnvFilter([]); setEpicFilter([]) }

      const activeChips = []
      for (const name of envFilter) {
        activeChips.push({ key: 'env:' + name, label: 'env ' + name, onClear: () => toggle(envFilter, setEnvFilter, name) })
      }
      for (const name of epicFilter) {
        activeChips.push({ key: 'epic:' + name, label: 'epic ' + name, onClear: () => toggle(epicFilter, setEpicFilter, name) })
      }
      if (query.trim() !== '') {
        activeChips.push({ key: 'query', label: 'search “' + query.trim() + '”', onClear: () => setQuery('') })
      }

      return h('div', {
        'data-ores-kanban': 'view',
        'data-stale': state.stale ? 'true' : 'false',
        'data-loading': state.pending ? 'true' : 'false',
        'data-refetching': state.pending ? 'true' : 'false',
        'data-cwd': cwd,
        'data-tree': treeName,
        style: {
          display: 'flex', flexDirection: 'column', gap: '0.6rem',
          padding: '0.9rem 1rem', boxSizing: 'border-box', fontSize: '0.82rem',
        },
      }, [
        h('div', {
          key: 'sprint',
          'data-ores-kanban': 'sprint-line',
          style: { display: 'flex', alignItems: 'baseline', flexWrap: 'wrap', gap: '0.5rem' },
        }, [
          h('span', {
            key: 'tree',
            'data-ores-kanban': 'tree-label',
            style: { fontFamily: MONO, fontSize: '0.85rem', fontWeight: 600 },
          }, treeName),
          snapshot.tree.branch ? h('span', {
            key: 'branch',
            style: { fontFamily: MONO, color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.72rem' },
          }, snapshot.tree.branch) : null,
          snapshot.tree.dirty || snapshot.tree.detached ? h('span', {
            key: 'dirty',
            style: { color: STALE_COLOR, fontSize: '0.7rem' },
          }, snapshot.tree.detached ? 'detached' : 'dirty') : null,
          h('span', { key: 'sep', style: { color: 'var(--dsw-alias-border-l3)' } }, '·'),
          h('span', { key: 'title', style: { fontSize: '0.9rem' } }, sprintName),
          h('span', {
            key: 'day',
            style: { color: 'var(--dsw-alias-label-secondary)', fontSize: '0.78rem' },
          }, 'Day ' + str(sprint.dayOfSprint) + ' of ' + str(sprint.totalDays)),
          h('span', {
            key: 'cards',
            style: { color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.78rem' },
          }, plural(snapshot.stories.length, 'card')),
          h('span', { key: 'spacer', style: { flex: 1 } }),
          state.stale ? h('span', {
            key: 'stale',
            'data-ores-kanban': 'stale',
            style: { color: STALE_COLOR, fontSize: '0.72rem', fontWeight: 700, letterSpacing: '0.04em' },
          }, 'stale') : null,
          state.pending ? h('span', {
            key: 'loading',
            'data-ores-kanban': 'loading',
            style: { color: 'var(--dsw-alias-label-tertiary)', fontSize: '0.72rem' },
          }, 'loading…') : null,
          h('button', {
            key: 'refresh',
            type: 'button',
            'data-ores-kanban': 'refresh',
            onClick: state.refresh,
            style: {
              padding: '0.15rem 0.6rem', border: '1px solid var(--dsw-alias-border-l2)',
              borderRadius: '0.25rem', background: 'transparent', color: 'inherit',
              cursor: 'pointer', fontSize: '0.75rem',
            },
          }, 'Refresh'),
        ]),
        state.notice ? h('div', {
          key: 'notice',
          style: { color: STALE_COLOR, fontSize: '0.72rem' },
        }, state.notice) : null,
        /* Fleet strip: which work tree is on which story and task. Reports only. */
        snapshot.trees.length > 0 ? h('div', {
          key: 'trees',
          'data-ores-kanban': 'fleet',
          style: { display: 'flex', flexWrap: 'wrap', gap: '0.3rem' },
        }, snapshot.trees.map((row) => h('span', {
          key: row.label || row.root,
          'data-ores-fleet': row.label,
          'data-this-tree': row.isSession ? 'true' : 'false',
          title: [row.label, row.branch, row.storyTitle, row.taskTitle].filter(Boolean).join(' · '),
          style: {
            display: 'inline-flex', alignItems: 'center', gap: '0.3rem',
            padding: '0.12rem 0.45rem', borderRadius: '999px',
            border: '1px solid var(--dsw-alias-border-l1)',
            background: row.isSession ? 'var(--dsw-alias-bg-layer-2)' : 'transparent',
            color: 'inherit', fontSize: '0.72rem',
          },
        }, [
          h(Dot, { key: 'dot', state: row.state }),
          h('span', { key: 'label', style: { fontFamily: MONO } }, row.label || '—'),
          h('span', {
            key: 'task',
            style: {
              color: 'var(--dsw-alias-label-secondary)', maxWidth: '14rem',
              overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap',
            },
          }, row.taskTitle ? truncate(row.taskTitle, 28) : 'idle'),
          row.isSession ? h('span', {
            key: 'this-tree',
            style: { color: ACCENT, fontWeight: 700, whiteSpace: 'nowrap' },
          }, 'this tree') : null,
        ]))) : null,
        h('div', {
          key: 'controls',
          'data-ores-kanban': 'controls',
          style: { display: 'flex', flexWrap: 'wrap', alignItems: 'center', gap: '0.4rem' },
        }, [
          h('input', {
            key: 'search',
            type: 'search',
            'data-ores-kanban': 'search',
            'aria-label': 'Search stories',
            placeholder: 'Search title, id, epic, task, environment, branch…',
            value: query,
            onChange: (event) => setQuery(event.target.value),
            style: {
              flex: '1 1 16rem', minWidth: '12rem', padding: '0.25rem 0.5rem',
              border: '1px solid var(--dsw-alias-border-l2)', borderRadius: '0.25rem',
              background: 'var(--dsw-alias-bg-layer-1)', color: 'inherit', fontSize: '0.78rem',
            },
          }),
          snapshot.filters.epics.map((name) => {
            const active = epicFilter.indexOf(name) >= 0
            return h('button', {
              key: 'epic:' + name,
              type: 'button',
              'data-ores-epic': name,
              'aria-pressed': active ? 'true' : 'false',
              onClick: () => toggle(epicFilter, setEpicFilter, name),
              style: {
                padding: '0.1rem 0.5rem', borderRadius: '999px', cursor: 'pointer',
                border: '1px solid ' + (active ? ACCENT : 'var(--dsw-alias-border-l1)'),
                background: active ? 'var(--dsw-alias-bg-layer-2)' : 'transparent',
                color: active ? ACCENT : 'inherit', fontSize: '0.72rem',
              },
            }, name)
          }),
          activeChips.length > 0 ? h('button', {
            key: 'clear',
            type: 'button',
            'data-ores-kanban': 'clear',
            onClick: clearAll,
            style: {
              padding: '0.1rem 0.5rem', borderRadius: '999px', cursor: 'pointer',
              border: '1px solid var(--dsw-alias-border-l1)', background: 'transparent',
              color: 'inherit', fontSize: '0.72rem',
            },
          }, 'Clear') : null,
        ]),
        activeChips.length > 0 ? h('div', {
          key: 'active',
          'data-ores-kanban': 'active-filters',
          style: { display: 'flex', flexWrap: 'wrap', gap: '0.3rem' },
        }, activeChips.map((chip) => h('button', {
          key: chip.key,
          type: 'button',
          onClick: chip.onClear,
          title: 'Clear this filter',
          style: {
            display: 'inline-flex', alignItems: 'center', gap: '0.3rem',
            padding: '0.1rem 0.5rem', borderRadius: '999px', cursor: 'pointer',
            border: '1px solid ' + ACCENT, background: 'var(--dsw-alias-bg-layer-2)',
            color: ACCENT, fontSize: '0.72rem',
          },
        }, chip.label + ' ✕'))) : null,
        h('div', {
          key: 'tiles',
          'data-ores-kanban': 'tiles',
          style: { display: 'grid', gridTemplateColumns: 'repeat(auto-fit, minmax(7rem, 1fr))', gap: '0.5rem' },
        }, tiles.map((tile) => h('div', {
          key: tile.key,
          'data-ores-tile': tile.key,
          style: {
            padding: '0.4rem 0.6rem', borderRadius: '0.375rem', textAlign: 'center',
            background: 'var(--dsw-alias-bg-layer-1)', border: '1px solid var(--dsw-alias-border-l1)',
          },
        }, [
          h('div', {
            key: 'v',
            style: { fontSize: '1.3rem', fontWeight: 700, color: tile.color || 'inherit' },
          }, tile.value),
          h('div', { key: 'l', style: { marginTop: '0.1rem' } }, h(MicroLabel, null, tile.label)),
        ]))),
        snapshot.stories.length === 0
          ? h('div', {
            key: 'empty',
            'data-ores-kanban': 'empty',
            style: {
              padding: '1.5rem', textAlign: 'center', borderRadius: '0.5rem',
              background: 'var(--dsw-alias-bg-layer-1)',
              border: '1px solid var(--dsw-alias-border-l1)',
              color: 'var(--dsw-alias-label-secondary)',
            },
          }, 'No stories in ' + sprintName + ' for ' + treeName + '.')
          : null,
        h('div', {
          key: 'body',
          style: { display: 'flex', alignItems: 'flex-start', gap: '0.6rem', minWidth: 0 },
        }, [
          h('div', {
            key: 'board',
            'data-ores-kanban': 'board',
            style: {
              display: 'flex', flex: 1, minWidth: 0, gap: '0.6rem',
              overflowX: 'auto', overflowY: 'hidden', paddingBottom: '0.4rem',
            },
          }, columns.map((column) => {
            const cards = storiesInColumn({ stories: visible }, column)
            return h(Column, {
              key: column.id,
              column: column,
              count: cards.length,
              cards: cards,
              currentStoryId: currentStoryId,
              selectedId: selectedId,
              registerRef: currentRef,
              onSelect: (story) => setSelectedId(story.id),
            })
          })),
          selected ? h(CardDetail, {
            key: 'detail',
            story: selected,
            onClose: () => setSelectedId(''),
          }) : null,
        ]),
      ])
    }

    /* ---------------------------------------------------------------- seat 1 */

    function Popover(props) {
      const snapshot = props.snapshot
      const taskId = snapshot.tree.currentTaskId
      return h('div', {
        'data-ores-kanban': 'popover',
        role: 'dialog',
        'aria-label': 'Current work item',
        style: {
          position: 'absolute', top: 'calc(100% + 0.35rem)', left: 0, zIndex: 60,
          width: '22rem', maxWidth: '90vw', maxHeight: '24rem', overflowY: 'auto',
          padding: '0.55rem 0.6rem', borderRadius: '0.5rem', textAlign: 'left',
          background: 'var(--dsw-specific-menu, var(--dsw-alias-bg-layer-2))',
          border: '1px solid var(--dsw-alias-border-l2)',
          boxShadow: 'var(--dsw-elevation-prominent)',
          color: 'var(--dsw-alias-label-primary)', fontSize: '0.78rem',
        },
      }, [
        !snapshot.ok || snapshot.tree.currentStoryId === ''
          ? h('div', { key: 'why' }, [
            h('div', { key: 'h', style: { fontWeight: 600 } }, 'No work item'),
            h('div', {
              key: 'm',
              style: { marginTop: '0.25rem', color: 'var(--dsw-alias-label-secondary)' },
            }, snapshot.ok
              ? 'The state route resolved the sprint, but no task carries this work tree’s branch ('
                + (snapshot.tree.branch || 'detached HEAD') + ').'
              : (snapshot.reason || 'unknown') + (snapshot.message ? ': ' + snapshot.message : '')),
          ])
          : null,
        snapshot.ok && snapshot.tree.currentStoryId !== '' ? (() => {
          const story = snapshot.stories.filter((item) => item.id === snapshot.tree.currentStoryId)[0]
          if (!story) {
            return h('div', { key: 'missing', style: { color: 'var(--dsw-alias-label-secondary)' } },
              'The resolved story is not in this sprint payload.')
          }
          return h('div', { key: 'story' }, [
            h('div', { key: 'head', style: { display: 'flex', alignItems: 'center', gap: '0.35rem' } }, [
              h(Dot, { key: 'd', state: story.state }),
              h('span', { key: 't', style: { flex: 1, minWidth: 0, fontWeight: 600 } }, story.title),
            ]),
            h('div', {
              key: 'meta',
              style: {
                margin: '0.2rem 0 0.4rem', color: 'var(--dsw-alias-label-tertiary)',
                fontSize: '0.7rem', fontFamily: MONO, wordBreak: 'break-all',
              },
            }, [story.environment, story.branches.join(' ')].filter(Boolean).join(' · ')),
            h('div', { key: 'tasks', style: { borderTop: '1px solid var(--dsw-alias-border-l1)' } },
              story.tasks.length > 0
                ? story.tasks.map((task) => h('div', {
                  key: task.slug || task.id || task.title,
                  style: {
                    display: 'flex', alignItems: 'center', gap: '0.35rem',
                    padding: '0.3rem 0', borderBottom: '1px solid var(--dsw-alias-border-l1)',
                  },
                }, [
                  h(Dot, { key: 'd', state: task.state }),
                  h('span', { key: 't', style: { flex: 1, minWidth: 0, wordBreak: 'break-word' } }, task.title),
                  task.environment ? h(Chip, { key: 'e' }, task.environment) : null,
                  task.id !== '' && task.id === taskId
                    ? h('span', {
                      key: 'current',
                      'data-ores-marker': 'current',
                      style: { color: ACCENT, fontWeight: 700, fontSize: '0.68rem', whiteSpace: 'nowrap' },
                    }, 'current')
                    : null,
                ]))
                : h('div', {
                  key: 'none',
                  style: { padding: '0.35rem 0', color: 'var(--dsw-alias-label-tertiary)' },
                }, 'No tasks in this story directory.')),
          ])
        })() : null,
      ])
    }

    function NowChip(props) {
      const sessionId = props.sessionId
      const state = useSnapshot(sessionId, useSessionCwd())
      const [open, setOpen] = React.useState(false)
      const wrapRef = React.useRef(null)

      React.useEffect(() => {
        if (!open) return undefined
        const onKey = (event) => { if (event.key === 'Escape') setOpen(false) }
        const onDown = (event) => {
          const node = wrapRef.current
          if (node && node.contains(event.target)) return
          setOpen(false)
        }
        window.addEventListener('keydown', onKey)
        document.addEventListener('mousedown', onDown)
        return () => {
          window.removeEventListener('keydown', onKey)
          document.removeEventListener('mousedown', onDown)
        }
      }, [open])

      const snapshot = state.snapshot
      const story = snapshot && snapshot.tree.currentStoryId
        ? snapshot.stories.filter((item) => item.id === snapshot.tree.currentStoryId)[0] || null
        : null
      const task = story && snapshot.tree.currentTaskId
        ? story.tasks.filter((item) => item.id === snapshot.tree.currentTaskId)[0] || null
        : null
      const resolved = snapshot !== null && snapshot.ok && snapshot.tree.currentStoryId !== ''
      const label = resolved && story
        ? [snapshot.tree.label, story.title, task ? task.title : ''].filter(Boolean).join(' · ')
        : snapshot && !snapshot.ok && state.notice
          ? 'No work item · ' + state.notice
          : 'No work item'

      const children = []
      if (resolved && story) {
        if (snapshot.tree.label) {
          children.push(h('span', {
            key: 'env',
            style: {
              fontFamily: MONO, padding: '0 0.25rem', borderRadius: '0.2rem',
              background: 'var(--dsw-alias-bg-layer-2)',
            },
          }, snapshot.tree.label))
        }
        children.push(h(Dot, { key: 'dot', state: story.state }))
        children.push(h('span', { key: 'story', style: { maxWidth: '14rem', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' } },
          truncate(story.title, 40)))
        if (task) {
          children.push(h('span', { key: 'sep', style: { color: 'var(--dsw-alias-label-tertiary)' } }, '›'))
          children.push(h('span', {
            key: 'task',
            style: {
              maxWidth: '11rem', overflow: 'hidden', textOverflow: 'ellipsis',
              whiteSpace: 'nowrap', color: 'var(--dsw-alias-label-secondary)',
            },
          }, truncate(task.title, 30)))
        }
      } else {
        children.push(h('span', {
          key: 'none',
          style: { color: 'var(--dsw-alias-label-tertiary)' },
        }, 'No work item'))
      }

      return h('span', {
        ref: wrapRef,
        'data-ores-kanban': 'chip-wrap',
        style: { position: 'relative', display: 'inline-flex', alignItems: 'center' },
      }, [
        h('button', {
          key: 'button',
          type: 'button',
          'data-ores-kanban': 'chip',
          'data-resolved': resolved ? 'true' : 'false',
          'data-story-title': story ? story.title : '',
          'aria-expanded': open ? 'true' : 'false',
          'aria-haspopup': 'dialog',
          title: label,
          onClick: () => setOpen((value) => !value),
          style: {
            display: 'inline-flex', alignItems: 'center', gap: '0.35rem', maxWidth: '32rem',
            padding: '0.15rem 0.5rem', border: '1px solid var(--dsw-alias-border-l1)',
            borderRadius: '999px', background: 'transparent',
            color: resolved ? 'inherit' : 'var(--dsw-alias-label-tertiary)',
            cursor: 'pointer', fontSize: '0.75rem', lineHeight: 1.4,
          },
        }, children),
        open ? h(Popover, { key: 'popover', snapshot: snapshot || normalize(null) }) : null,
      ])
    }

    /* ----------------------------------------------------------------- style */

    function css() {
      return [
        '[data-ores-kanban]{box-sizing:border-box}',
        '[data-ores-kanban] *{box-sizing:border-box}',
        '[data-ores-kanban] button{font:inherit;color:inherit}',
        '[data-ores-kanban] input{font:inherit}',
        '[data-ores-kanban] a{color:var(--dsw-alias-link)}',
        '[data-ores-kanban="board"]{scrollbar-color:var(--dsw-alias-scrollbar-bg-l2) transparent}',
        '[data-ores-kanban="column"]>div{scrollbar-color:var(--dsw-alias-scrollbar-bg-l2) transparent}',
        '[data-ores-kanban="card"]:hover{border-color:' + ACCENT + '}',
        '[data-ores-kanban="chip"]:hover{border-color:' + ACCENT + '}',
        '[data-ores-kanban] :focus-visible{outline:2px solid ' + ACCENT + ';outline-offset:1px}',
      ].join('\n')
    }

    /* The stylesheet has one element and two owners, so it is reference counted:
     * the element goes away only when the last seat unmounts. */
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

    function NowChipSeat(props) {
      useStyles()
      return h(SessionCwd, {
        sessionId: props.sessionId,
        useSessions: props.useSessions,
        ctx: props.ctx,
      }, h(NowChip, { sessionId: props.sessionId }))
    }

    function BoardSeat(props) {
      useStyles()
      return h(SessionCwd, {
        sessionId: props.sessionId,
        useSessions: props.useSessions,
        ctx: props.ctx,
      }, h(BoardView, { sessionId: props.sessionId }))
    }

    /* -------------------------------------------------------------- register */

    function apply(ctx) {
      if (!ctx || !ctx.slots) return
      const NowSeat = (props) => h(NowChipSeat, { sessionId: props.sessionId, useSessions: props.useSessions, ctx: ctx })
      const ViewSeat = (props) => h(BoardSeat, { sessionId: props.sessionId, useSessions: props.useSessions, ctx: ctx })
      ctx.effect(() => ctx.slots.inject('conversation.session.header.actions', () => ctx.slots.register(
        { name: 'conversation.session.header.actions', id: 'ores-dsh-kanban-now', order: 20 },
        NowSeat)),
        'ores-dsh-kanban: now readout')
      ctx.effect(() => ctx.slots.inject('conversation.view', () => ctx.slots.register(
        { name: 'conversation.view', id: 'kanban', order: 25, label: () => 'Kanban' },
        ViewSeat)),
        'ores-dsh-kanban: board view')
    }

    exports.name = 'ores-dsh-kanban'
    exports.inject = ['slots']
    exports.apply = apply
    return module.exports
  },
})
