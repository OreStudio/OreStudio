// Work tree facts and journal reading. This is the only module that spawns a
// process or reads a file.

import { execFile } from 'node:child_process'
import { createReadStream, promises as fs } from 'node:fs'
import { join } from 'node:path'
import { parseJournalEntry } from './agile.js'

const GIT_TIMEOUT_MS = 10000
const JOURNAL_TAIL_BYTES = 64 * 1024
const CONCURRENCY = 8

function git(args, cwd) {
  return new Promise((resolve) => {
    execFile(
      'git',
      args,
      { cwd, timeout: GIT_TIMEOUT_MS, maxBuffer: 8 * 1024 * 1024 },
      (error, stdout) => resolve(error ? null : stdout),
    )
  })
}

function nameOf(root) {
  return String(root).replace(/\/+$/, '').split('/').pop() ?? ''
}

// `label` is the directory name with the ores_dev_ prefix removed, and it is the
// same token the org files carry in `#+environment:`.
function labelOf(root) {
  const name = nameOf(root)
  return name.startsWith('ores_dev_') ? name.slice('ores_dev_'.length) : name
}

// The porcelain block already reports each work tree's branch, so listing the
// repository's trees costs one git call and nothing per tree.
export async function listWorkTrees(cwd) {
  const list = await git(['worktree', 'list', '--porcelain'], cwd)
  if (list === null) return null
  const trees = []
  for (const block of list.split('\n\n')) {
    const lines = block.split('\n')
    const root = lines.find((line) => line.startsWith('worktree '))?.slice('worktree '.length)
    if (!root || lines.includes('bare')) continue
    const branch = lines.find((line) => line.startsWith('branch refs/heads/'))
    trees.push({
      root,
      name: nameOf(root),
      label: labelOf(root),
      branch: branch ? branch.slice('branch refs/heads/'.length) : '',
      detached: lines.includes('detached'),
      dirty: false,
    })
  }
  return trees
}

// Only the resolved tree needs its working state read, one call per fact.
export async function readWorkTree(tree) {
  const [head, status] = await Promise.all([
    git(['rev-parse', '--abbrev-ref', 'HEAD'], tree.root),
    git(['status', '--porcelain'], tree.root),
  ])
  // Detached HEAD is what rev-parse prints as HEAD. A failed call prints
  // nothing, and null must not be reported as a confident false.
  const printed = head === null ? null : head.trim()
  return {
    ...tree,
    branch: printed === null || printed === 'HEAD' ? '' : printed,
    detached: printed === 'HEAD',
    dirty: status !== null && status.trim() !== '',
  }
}

// The journal is append-only and long; only the last entry is ever needed, so
// read the file's tail rather than the whole thing.
async function readLastJournal(root) {
  const path = join(root, '.journal.org')
  let size
  try {
    size = (await fs.stat(path)).size
  } catch {
    return null
  }
  const start = Math.max(0, size - JOURNAL_TAIL_BYTES)
  const chunks = []
  try {
    const stream = createReadStream(path, { start, encoding: 'utf8' })
    for await (const chunk of stream) chunks.push(chunk)
  } catch {
    return null
  }
  const lines = chunks.join('').split('\n')
  if (start > 0) lines.shift()
  let lastStart = -1
  for (let index = lines.length - 1; index >= 0; index -= 1) {
    if (/^\* /.test(lines[index])) {
      lastStart = index
      break
    }
  }
  if (lastStart < 0) return null
  const entry = parseJournalEntry(lines.slice(lastStart).join('\n'))
  return entry.taskId || entry.storyId ? entry : null
}

export async function readJournals(roots) {
  const entries = new Map()
  for (let index = 0; index < roots.length; index += CONCURRENCY) {
    const slice = roots.slice(index, index + CONCURRENCY)
    const found = await Promise.all(slice.map((root) => readLastJournal(root)))
    slice.forEach((root, offset) => entries.set(root, found[offset]))
  }
  return entries
}
